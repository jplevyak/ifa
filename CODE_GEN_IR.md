# CODE_GEN_IR — Investigation: a normalization pass / codegen-time IR

A design investigation, not a plan. The question on the table:
**should we introduce a normalization pass that lowers the IF1
data structures (`Sym` / `Code` / `PNode` / `Var` / `Fun`) into a
new IR designed for codegen consumption, with both the C and
LLVM backends emitting from that new IR instead of from IF1
directly?**

Short answer: **yes, eventually, scoped tightly to the
post-clone / post-DCE state**. The current dual-backend
divergence is real and the issues filed in `ifa/issues/`
(004-016, especially 014 and 016) trace to four specific places
where codegen has to interpret IF1's analyzer-facing shape on
the fly. A focused normalization pass — call it `CG_IR` —
collapses those interpretations into explicit operations the
backends just translate.

But this is a several-week refactor with real test infrastructure
implications, so the value-proposition has to be earned before
landing. This document lays out:

1. What codegen consumes today, and what each backend has to do
   to interpret it (§§1-2).
2. The specific pain points that motivated this investigation,
   keyed to the open issues and the bugs found in the last
   session (§3).
3. The design space — what a normalized codegen IR could look
   like, with three concrete shapes ranked by ambition (§4).
4. A recommended shape (§5) and a sketch of the normalization
   pass itself (§6).
5. Tradeoffs, risks, and what we lose by introducing it (§7).
6. A staged migration plan if we decide to land it (§8).

Sister docs: [IR.md](IR.md) (the input format), [CFG_SSU.md](CFG_SSU.md)
(the SSU pass that produces the current PNode form),
[CODEGEN_C.md](CODEGEN_C.md) / [CODEGEN_LLVM.md](CODEGEN_LLVM.md)
(the two consumers).

---

## 1. What codegen consumes today

After `ifa_analyze` and `ifa_optimize` (clone + DCE + inline +
SSU), the codegen-facing state of IF1 is:

| Owner | Shape | Notes |
|---|---|---|
| `if1->allsyms` | linear `Vec<Sym*>` | Includes types, constants, functions, vars, modules — interleaved. Codegen filters by flag soup. |
| `fa->funs` | `Vec<Fun*>` | One `Fun` per specialized closure, post-clone. |
| `Fun::entry` / `Fun::exit` | `PNode*` | The CFG roots; everything else is reached via `cfg_pred` / `cfg_succ`. |
| `PNode::code->kind` | enum: `MOVE` / `SEND` / `IF` / `GOTO` / `LABEL` | The operation discriminator. |
| `PNode::rvals` / `lvals` | `Vec<Var*>` | Operands. `Var::sym` carries the value's symbol / type / live bits. |
| `PNode::phi` / `phy` | `Vec<PNode*>` | SSU merge nodes attached to this PNode. Each phi/phy is itself a `MOVE` PNode that the main CFG doesn't see. |
| `PNode::live`, `fa_live`, `Var::live`, `Sym::live` | bitfields | Three orthogonal liveness analyses, none of which alone tells codegen "should I emit this?". |
| `Sym::cg_string` / `Sym::llvm_value` / `Sym::llvm_type` | three side channels | Codegen-emitted state directly mutated on the source IR. The C backend writes one, the LLVM backend writes the other two. |

The structure is **symbol-centric** (everything carries a `Sym`)
and **analyzer-shaped** (SSU was built for backward-flow type
analysis, not for emission). Codegen is the last consumer.

## 2. What each backend has to do, today

### 2.1 The C backend (`cg.cc:write_c_pnode`)

```cpp
if (n->live && n->fa_live) switch (n->code->kind) {
  case Code_LABEL: emit "L<id>:;"; break;
  case Code_MOVE:  simple_move(...); break;
  case Code_SEND:  write_c_prim || write_send; break;
  case Code_IF: case Code_GOTO: break;   // handled below
}
switch (n->code->kind) {                  // outside the live gate
  case Code_IF:
    if (live) emit conditional + do_phi/phy on the taken branch;
    else      do_phi_nodes(succ, 0);
  case Code_GOTO: do_phi_nodes + emit goto if live;
  case Code_SEND: if dead reply, emit "return 0;" else do_phi_nodes;
  default: do_phi_nodes(succ, 0);
}
recurse cfg_succ DFS;
emit extra_goto if fall-through to LABEL.
```

Three things to notice:

1. **Two-stage emission** — per-kind emission is gated; phi/phy
   materialization runs unconditionally afterward.
2. **Recursive DFS** over `cfg_succ` with a `done` set, ending
   with an `extra_goto` to handle fall-through into a label
   whose body has already been visited.
3. **The "Code_SEND with dead reply" special case** — gives a
   non-live `prim_reply` a `return 0;` so the C function has a
   terminator.

The C backend is ~900 lines (cg.cc), with another ~200 in
`codegen_common.cc` shared with LLVM.

### 2.2 The LLVM backend (`llvm_codegen.cc::translatePNode`)

```cpp
bool is_live = pn->live && pn->fa_live;
if (!is_live) return;                     // EARLY RETURN
switch (pn->code->kind) {
  case Code_LABEL: translate_code_label(...); break;
  case Code_MOVE:  translate_code_move(...); break;
  case Code_SEND:  translate_code_send(...); break;
  case Code_IF:    translate_code_if(...); break;   // contains phi/phy
  case Code_GOTO:  translate_code_goto(...); break; // contains phi/phy
}
```

The structure is BFS over a worklist (in `translate_pnodes_worklist`),
and the per-kind handlers each call `do_phi_nodes` / `do_phy_nodes`
inline. Crucially: when `!is_live`, the function returns early —
**no phi/phy materialization happens**. That's the immediate cause
of issue 016 (the SSU formal-arg binding being skipped on
non-live nodes).

The LLVM backend is ~600 lines + ~950 lines of primitives.

### 2.3 The shared interpretation work

Both backends have to re-derive, every codegen run:

- **Field-name → index** for `obj.x`-style getter/setter
  emission. C does it by walking `obj->has[i]->name` (cg.cc:188);
  LLVM does the same but with an `atoi("eN")` fallback that
  silently misindexes on non-contiguous field naming (see the
  range/loop bug from the last session).
- **"Heap aggregate is by pointer"** convention. The C
  backend's `assign_type_cg_strings_pass1` writes `_CG_psN` for
  `Type_RECORD`; the LLVM backend re-derives this via
  `getLLVMVarType` (added in commit 06bec4a). Neither reads it
  from a single field on the Sym.
- **Closure-creation form** of `P_prim_period` (cg.cc:177-185).
  Both backends key on `n->lvals[0]->type->type_kind == Type_FUN
  && n->creates`. The actual structure ("a closure is two fields
  — selector + receiver") is implicit in the field iteration.
- **Constructor sequencing** for `y = A()`. The IF1 emits this
  as `P_prim_new(A)` → set up methods → call `__init__`, but the
  binding from the `new` result to the lvalue (`y`) is left
  implicit; issue 014 traces a SEGV to exactly this gap on the
  LLVM side.
- **`live` vs `fa_live` semantics**. Three liveness flags exist
  (`Sym::live`, `Var::live`, `PNode::live`, `PNode::fa_live`).
  The C backend uses `pn->live && pn->fa_live` for per-kind; the
  LLVM backend tried `fa_live` alone for a while (broke
  for-loops), then `live && fa_live` (broke iterators —
  issue 016), then variants. Nobody can say at-a-glance which
  flag means what at the codegen interface.

Each interpretation rule is re-encoded slightly differently in
each backend.

## 3. The bugs that motivated this investigation

The last session worked through five LLVM-backend issues and
landed code that brought the pyc-suite from 8/74 to 37/74. Of
the open follow-ups, four trace directly to the codegen-time
interpretation work above:

| Issue | Symptom | What's actually wrong |
|---|---|---|
| [014](issues/014-llvm-construction-flow-to-slots.md) | `y = A()` SEGV — `@y` never receives the GC-malloc'd pointer | The IF1 emits `__new__()` as a void-returning side-effect, with no MOVE binding the result to `y`. Both backends have to recognise the pattern. C accidentally gets it via the `phi` side door; LLVM doesn't. |
| [016](issues/016-llvm-ssu-formal-arg-binding.md) | Iterators read garbage `self` — `for i in range(3)` doesn't terminate | The SSU rename of the formal arg leaves the renamed local unbound. The C backend's `do_phi_nodes` runs unconditionally; the LLVM backend's runs inside the live gate. |
| Range field indexing | `for i in range(N): ...` GEPs the wrong field offsets | LLVM struct has more fields than the C struct (e.g. 7 vs 4); the IF1 names them e2, e3, e5, e6 (non-contiguous), and the LLVM `atoi("e5") = 5` fallback overshoots into a different slot than the position-in-`has[]` lookup would give. |
| Tuple `a = (...)` not visible to `a[i]` | tuple_mixed_types SEGV; `@a` is `ptr null` after the assignment | The MOVE PNode binding the constructed tuple to the global `a` had `live=0`, so the LLVM gate dropped it. The C backend gates the same way but the dropped MOVE still materialises through the SSU phi list. |

Common pattern: a structurally simple operation
(`bind-this-result-to-that-slot`) is encoded *across* an IF1
PNode plus a SSU phi list plus a liveness flag with confusing
semantics. Both backends paper over the encoding differently;
the LLVM backend's paint job has more cracks.

A normalization pass that produced explicit
`BindResultOf(X) → Slot(Y)` operations would eliminate the
interpretation drift.

## 4. The design space

Three shapes, ranked by ambition.

### 4.1 Option A — light: a `cg_pnode` rewrite in place

Smallest possible footprint: keep `PNode` as the carrier, but
add a pre-codegen pass that **mutates** each `Fun`'s PNode chain
to make implicit operations explicit:

- Materialize SSU phi/phy as ordinary `Code_MOVE` PNodes
  inserted at the predecessor's end / successor's start, so
  codegen doesn't need a separate phi/phy walk.
- Insert an explicit `Code_MOVE` after every constructor SEND
  whose result is implicitly bound to an lvalue.
- Resolve every field-name reference to its `has[]` position
  and stash the integer in a new `PNode::field_idx` field, so
  codegen never re-walks.
- Compute a single `PNode::cg_live` bit that the backends gate
  on, derived from `live && fa_live && (not-shadowed-by-clone)`.

**Pros**: smallest diff. Both backends touch a handful of sites.
The pass itself is small (~300 LOC).

**Cons**: PNodes still carry analyzer-shaped baggage (`avars`,
`creates`, `cfg_pred_index`, `phi`/`phy`). Codegen still has to
know which fields it can ignore. Two backends still re-implement
the same translation, just over a slightly tidier input. The
phi/phy fields exist but are now empty after the pass — easy to
revert into incorrect by accident.

### 4.2 Option B — medium: separate `CG_IR` data structure

Introduce a parallel set of structs that codegen consumes:

```cpp
// ifa/codegen/cg_ir.h
struct CGFun;
struct CGBlock;     // basic block
struct CGInst;      // single instruction
struct CGValue;     // SSA value (use)
struct CGSlot;      // addressable slot (alloca / global)
struct CGType;      // explicit kind: value/struct/ptr/fun-signature

enum CGOp {
  CG_LOAD,           // CGValue = load CGSlot
  CG_STORE,          // store CGValue → CGSlot
  CG_CONST,          // CGValue = imm
  CG_ALLOC,          // CGSlot = GC_malloc(CGType)
  CG_GETFIELD,       // CGValue = field i of CGValue (struct value)
  CG_SETFIELD,       // store CGValue → field i of CGSlot
  CG_GEP,            // CGSlot = field i of CGValue (pointer)
  CG_CALL,           // CGValue = call CGFun(CGValue...)
  CG_BR,             // unconditional branch CGBlock
  CG_COND_BR,        // CGValue ? CGBlock : CGBlock
  CG_RET,            // ret CGValue
  CG_PRIM,           // dispatch to PrimEmitter (write/writeln/to_string/...)
  CG_INTRINSIC,      // backend-specific (printf, snprintf, GC_malloc)
};

struct CGInst {
  CGOp op;
  CGType *result_type;
  Vec<CGValue> rvals;
  CGValue lval;           // produced value (or none)
  unsigned src_line;      // for debug info
  // ... per-op fields (field index for GETFIELD/SETFIELD, etc.)
};

struct CGBlock {
  Vec<CGInst> body;
  CGInst terminator;      // br / cond_br / ret — required
  Vec<CGBlock*> preds, succs;
};

struct CGFun {
  cchar *name;
  CGType *return_type;
  Vec<CGType*> arg_types;
  Vec<CGSlot*> formal_arg_slots;   // pre-bound by the normalization pass
  Vec<CGBlock*> blocks;
  CGBlock *entry;
};
```

The normalization pass walks each post-clone-post-DCE `Fun` and
produces a `CGFun`. SSU phi/phy collapses into explicit
`CG_STORE`s. Constructor flow gets explicit `CG_ALLOC` +
`CG_SETFIELD` + `CG_STORE` to the lvalue slot. Field accesses
carry numeric indices, not names. Liveness is folded in (dead
PNodes simply don't produce CGInsts). The `Sym → CGSlot` /
`Var → CGValue` mapping is built during the pass and discarded
after.

**Pros**: codegen becomes a pure transliteration. The C backend
walks the CGBlocks and emits `T t<id>; t<id> = ...;` text; the
LLVM backend walks the same blocks and emits `Builder->Create*`.
The shared logic — basic block management, register numbering,
type-string assignment — lives in one place. Issue 014, 016,
and the field-indexing class of bug become structurally
impossible.

**Cons**: substantial new surface. ~2k LOC of CGFun/CGBlock/CGInst
construction code. The C backend's printer and the LLVM backend
each need rewriting. Tests need re-blessing (fixture goldens
will all change). Two non-trivial migrations have to coexist with
the old path for a window.

### 4.3 Option C — heavy: drop straight to LLVM IR, then transpile back to C

The most aggressive shape: skip the intermediate and write
**only** the LLVM backend going forward. For the C output, run
the LLVM module through a transpiler (e.g., `llvm-cbe` lives in
LLVM's tree as a separate project, and `c-backend` plugins
exist) that produces equivalent C. Then both backends share 100%
of the IR translation logic; the C path is just a downstream
codegen target.

**Pros**: maximum sharing. Bug fixes land once. The pyc runtime
and the LLVM module need only one ABI to agree on.

**Cons**: pyc's `Makefile.cg`-driven C build path is documented
and works. Switching the C backend to a transpiler means losing
the `Makefile.cg` integration, the C-as-runtime story
(the C runtime is hand-written C, and the emitted code includes
`#include "pyc_c_runtime.h"`), and the per-PNode `/* name */`
comments that make the generated C readable. The C backend is
the production path; replacing its source-of-truth with a
transpilation is a significant risk.

Option C also doesn't actually address issues 014/016 — they're
about IF1 → LLVM IR translation, and a C-transpilation downstream
doesn't help. So this is the wrong tool.

## 5. Recommended shape

**Option B**, scoped tightly to the post-clone / post-DCE state,
landed in stages so the dual-backend path keeps working
throughout.

The justification:

- **Issue surface**: 014, 016, the range-field-indexing bug, the
  tuple-construction SEGV, the `live`/`fa_live` confusion — all
  five are about IF1 carrying analyzer-shaped state that
  codegen has to interpret. Option A papers over the worst of
  it but leaves the interpretation drift; Option B makes the
  interpretation a single, testable pass.

- **Existing precedent**: pyc already maintains a partial
  codegen-side IR via the `cg_string` / `llvm_value` /
  `llvm_type` side channels on `Sym` and `Var`. Those fields
  exist precisely because the analyzer-facing IR doesn't carry
  what codegen needs. Today they're cached lazily, mutated in
  place, and shared by string equality. A `CGFun` formalizes
  exactly the same caching.

- **Bounded ambition**: scope is "post-clone-and-DCE Fun → CGFun",
  with the rest of the pipeline unchanged. The IF1 layer above
  doesn't move. The C runtime ABI doesn't move. Only the
  ~1500-line span between `ifa_optimize`'s exit and the two
  `*_codegen_print_*` printers changes.

- **Avoids re-relitigating SSU**: the normalization pass
  *consumes* SSU phi/phy and writes explicit moves into the
  CGFun. The SSU pass itself doesn't change. We're paying the
  conversion cost once, at codegen entry, not asking the
  analyzer or the optimizer to change shape.

The shape of `CGFun` itself should be biased toward **easy LLVM
codegen** (basic blocks, explicit terminators, SSA values,
explicit type kind) because LLVM IR is the more constrained
backend. The C backend can transliterate a more-structured form
trivially; the reverse is harder.

## 6. Sketch of the normalization pass

`ifa/codegen/cg_normalize.cc`. One entry point:

```cpp
CGProgram *cg_normalize(FA *fa);
```

`CGProgram` owns the `CGFun` per live `Fun`, the `CGType` table,
the per-function `CGBlock` lists, plus the post-pass-only
maps `sym_to_slot` (for globals) and `fun_to_cgfun` (for direct
calls).

The pass runs per-Fun. Pseudo-algorithm:

```cpp
CGFun *normalize_fun(Fun *ifa_fun) {
  CGFun *cf = new CGFun(ifa_fun);

  // 1. Type and slot setup.
  cf->return_type = cg_type_for(ifa_fun->rets[0]->type);
  for (Var *arg : ifa_fun->positional_args()) {
    cf->arg_types.add(cg_type_for(arg->type));
    cf->formal_arg_slots.add(new CGSlot(cg_type_for(arg->type)));
  }
  // SSU rename mapping: every local Var → CGSlot (alloca).
  // Issue 016's "formal-arg-to-renamed-local MOVE never emitted"
  // becomes a no-op: the slot exists, the formal-arg binding
  // becomes a CG_STORE issued at entry below.

  // 2. CFG topology. One CGBlock per PNode (LABEL/IF/GOTO
  // boundaries) with explicit predecessor/successor links.
  build_block_skeleton(cf, ifa_fun);

  // 3. Entry block: bind formal args into their slots, run any
  // entry-resident phi/phy as explicit CG_STOREs. Issue 016
  // closes structurally here.
  for (Var *arg : ifa_fun->positional_args()) {
    cf->entry->body.add(CGInst::Store(arg, cf->formal_arg_slots[arg.idx]));
  }

  // 4. Per-PNode lowering, in CFG order.
  for (PNode *pn : reachable_pnodes(ifa_fun->entry)) {
    if (!pn_should_emit(pn)) {
      // Still process phi/phy — they may bind into live successors.
      lower_phi_phy(pn, cf);
      continue;
    }
    lower_pnode(pn, cf);          // emits 1-N CGInsts into the
                                  // CGBlock owning pn.
  }

  // 5. Constructor-flow patch. Issue 014: walk back through
  // CGFun looking for CG_CALL into a __new__/__init__ whose
  // result isn't stored anywhere; insert the missing CG_STORE
  // to the receiver's slot. This is a peephole-style fixup;
  // the proper fix is in the frontend's IF1 emission, but the
  // peephole closes the LLVM-side gap.
  patch_construction_flow(cf);

  return cf;
}
```

Each `lower_pnode` handler maps one PNode kind to CGInsts:

| PNode | Lowering |
|---|---|
| `Code_MOVE` | `CG_STORE rvals[0] → slot(lvals[0])` |
| `Code_SEND` with prim | `CG_PRIM prim` or specific op (CG_ALLOC for `P_prim_new`, CG_GETFIELD for `P_prim_period`, CG_CALL for `__pyc_more__`, etc.) |
| `Code_SEND` without prim | `CG_CALL target_fun(rvals...)` |
| `Code_IF` | terminator `CG_COND_BR rvals[0], succ_true, succ_false` |
| `Code_GOTO` | terminator `CG_BR succ` |
| `Code_LABEL` | block boundary — no CGInst, just establishes the block |

phi/phy materialization happens in `lower_phi_phy(pn, cf)`,
which iterates `pn->phi` / `pn->phy` and emits
`CG_STORE rvals[pred_idx] → slot(lvals[0])` into the predecessor
block (for phi) or the successor block (for phy). This runs
**unconditionally**, including on PNodes the live filter rejects
— that closes issue 016.

Field-name resolution is done once in `lower_pnode` for
`P_prim_period` / `P_prim_setter` / `P_prim_index_object`:

```cpp
int field_idx = lookup_field_index(obj_type, field_sym);
// field_idx is a position in the CGType's field list, computed
// once and stored on the CGInst.
```

The CGType's field list and the IF1 `obj_type->has[]` are
1-to-1 (the CGType carries the same order), so codegen sees the
exact same field positions regardless of whether the IF1 names
fields contiguously.

The C backend's `c_codegen_print_c` becomes:

```cpp
void c_codegen_print_c(FILE *fp, CGProgram *prog) {
  emit_runtime_includes(fp);
  for (CGFun *f : prog->funs) emit_c_function(fp, f);
  emit_main_wrapper(fp, prog->entry);
}
```

with `emit_c_function` doing a per-CGInst switch that's
~200 LOC. The LLVM backend's `llvm_codegen_print_ir` is similar
in shape but emits `Builder->Create*` calls.

## 7. Tradeoffs, risks, what we lose

### Real costs

- **~3000 LOC of new code** (CGFun and friends, normalization
  pass, two new printers, plus tests).
- **Goldens rebless across the board** — all 12 codegen-llvm and
  6 codegen-c fixtures. The C-suite (74 tests) and LLVM-suite
  (37 → ?) get run end-to-end; the fixtures get reblessed
  mechanically.
- **A second IR to maintain**. Two IRs is more
  surface than one. Every new IF1 feature (e.g. async, generators)
  has to learn how to lower into CGFun too.
- **A normalization layer has to stay current with IF1**. If IF1
  adds a new `Code_*` kind, normalization needs to handle it.
- **Diagnostic noise during the migration window**. While both
  paths exist, "the C backend says X, the LLVM backend (via
  CGFun) says Y" diagnoses are confusing.

### Things we lose

- The lazy `Sym::cg_string` / `Var::llvm_value` caches — these
  go away. Today they're a partial pre-IR; CGFun replaces them.
  Anything currently inspecting them in a debugger needs to
  learn the new accessors.
- The "everything is reachable from `if1->allsyms`" property —
  CGFun fragments live in a separate arena.
- The `IF1::callback->c_codegen_pre_file(fp)` hook the pyc
  frontend uses to emit `__pyc_insert_c_code__` blobs has to be
  replumbed to fire at `cg_normalize` time or at C-printer time;
  same hook, new injection point.

### Risks

- **Test infrastructure rework**. The `print_codegen.cc`
  pipeline assumes "run pyc to LLVM IR text". With CGFun, we
  may want to add a `print_cg_normalized` phase that dumps the
  CGFun shape for golden testing — that's another ~400 LOC.
- **Performance regression**. The normalization pass walks every
  PNode and allocates CGInsts. Profiling under
  `ifa/codegen/PERFORMANCE.md` would need a re-baseline; the
  expectation is single-digit-ms additional codegen time per
  pyc compilation. Acceptable for the C backend, possibly
  noticeable for `--dparse_only`-style benchmarks.
- **Coverage gap during the migration**. The C backend has its
  own bug surface that's been stable for years; replacing
  `c_codegen_print_c` runs a real risk of introducing
  regressions the test suite doesn't catch.

### Things that *don't* go away

- The IF1 itself. Nothing about `Sym::has`, `PNode::phi`, or
  the SSU pass changes. The frontend, the analyzer, the
  optimizer, the cloner all keep their current shape.
- The pyc C runtime ABI. The C backend emits the same
  `_CG_*` calls; the LLVM backend emits the same calls (when
  CODEGEN_LLVM §14.5's linking gap closes).
- The `RegisteredPrim` / `PrimitiveLLVMCGPtr` / phase-5
  `PrimEmitter` work. Those plug into the per-prim dispatch
  inside `lower_pnode`; no rework.
- The CI workflow. `make test` still runs; `make USE_LLVM=1`
  still works. The migration is an internal swap of the
  printer's input.

## 8. Staged migration plan

If we decide to land CGFun, a five-PR sequence keeps the
existing path working throughout:

**Stage 1** — header-only, no semantic change.
- `ifa/codegen/cg_ir.h` declaring `CGFun`/`CGBlock`/`CGInst`/etc.
- An empty `cg_normalize.cc` implementing `cg_normalize(fa)` as
  a stub that allocates an empty CGProgram.
- One unit test fixture verifying the stub's malloc/free.

**Stage 2** — wire the normalization pass, run alongside the
existing codegen.
- Real implementation of `cg_normalize`.
- A new ifa-test phase `cg-normalize` whose golden is a textual
  dump of the resulting CGProgram. Goldens for the existing
  IR fixtures (under `tests/ir/codegen-llvm/`) get a `.cgir`
  sibling.
- The existing C and LLVM backends are unchanged; CGProgram is
  built and discarded.

**Stage 3** — switch the LLVM backend to consume CGProgram.
- `llvm_codegen_print_ir` rewrites to walk CGProgram.
- Rebless the codegen-llvm fixture goldens (the IR changes
  marginally as the per-kind dispatch goes through CGFun).
- Verify `make USE_LLVM=1 ./test_pyc` pass count holds or
  rises (issues 014 and 016 should close in this stage).

**Stage 4** — switch the C backend to consume CGProgram.
- `c_codegen_print_c` rewrites to walk CGProgram.
- Rebless the codegen-c fixture goldens.
- Verify `make test` and the full pyc-suite (74/1/0/2 today).

**Stage 5** — clean up the old paths.
- Remove `Sym::cg_string`, `Sym::llvm_value`,
  `Var::cg_string` — they're now redundant with the CGProgram's
  internal maps.
- Remove `PNode::phi` / `phy` access from the codegen layer
  (they remain on PNode for the analyzer; codegen just doesn't
  touch them).
- Remove the C and LLVM "interpret the IF1" code paths now
  that the normalizer owns interpretation.

Each stage is independently committable, individually testable,
and reversible.

## 9. Decision: should we?

**Yes if**: the LLVM backend is going to remain a first-class
target with regular updates. The interpretation drift between
the two backends is real and growing; each fix lands in two
places. Issue 016 in particular is a textbook structural bug
that disappears once SSU phi/phy materializes into explicit
moves.

**No if**: the LLVM backend stays at "work in progress, stretch
goal" indefinitely. The C backend is healthy; the work to bring
it to parity with CGFun is significant; if no future bugs are
going to land in either backend, the existing duplication is
fine.

The recommendation here, given the LLVM backend's recent
progress (8/74 → 37/74 over one focused session, with most
remaining failures tracing to four categorized issues), is to
plan for Stage 1 in the next CODEGEN_PLAN revision and gate
Stages 2-5 on the resolution of issue 016 (which would either
demonstrate the value of the CGFun shape concretely, or fix the
specific bug and reduce the marginal case for the broader
refactor).

A tactical alternative that captures most of the benefit at much
lower cost: **Option A** (in-place PNode rewrite) implementing
just the SSU-materialization and explicit-bind-result steps. That
would close issues 014 and 016 with ~300 LOC of changes and zero
new IR. The remaining "interpretation drift" between backends
would remain but would be smaller. Worth doing as a step on the
way to Option B, or as the stopping point if Option B never
earns its keep.

## 10. References

- [IR.md](IR.md) — the IF1 IR being normalized away from.
- [CFG_SSU.md](CFG_SSU.md) — the SSU pass whose output the
  normalizer consumes.
- [CODEGEN_C.md](CODEGEN_C.md) / [CODEGEN_LLVM.md](CODEGEN_LLVM.md)
  — the two consumers that would simplify under CGFun.
- [CODEGEN_PLAN.md](codegen/CODEGEN_PLAN.md) §8 (phase 5) —
  the unified-architecture work, where the `PrimEmitter`
  interface lives. CGFun complements it; `PrimEmitter`
  becomes the per-op handler called by `lower_pnode`.
- [PRIMITIVES.md](PRIMITIVES.md) §15 — the SEND PNode shape
  the normalization pass has to consume.
- [issues/014-llvm-construction-flow-to-slots.md](issues/014-llvm-construction-flow-to-slots.md)
  — closed structurally by Stage 3.
- [issues/016-llvm-ssu-formal-arg-binding.md](issues/016-llvm-ssu-formal-arg-binding.md)
  — closed structurally by Stage 3.
- [issues/002-codegen-llvm-normalizer.md](issues/002-codegen-llvm-normalizer.md)
  — a *different* normalizer (output-text post-processing for
  fixture stability). Don't confuse the two.
