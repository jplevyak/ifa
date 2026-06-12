# LIVENESS — the four liveness flags

A working reference for `Sym::live`, `Var::live`, `PNode::live`,
and `PNode::fa_live`. These four bits together govern what
codegen emits; before this audit, no document explained who
sets what when, and the LLVM backend's gate swung three times in
one session before settling on `live && fa_live` to match the C
backend. The closing note from that session — that the C
backend ALSO runs phi/phy materialization OUTSIDE the live gate
— is the structural difference that motivates issue 016.

This doc is one of the Phase 0 mandatory prerequisites for
[CG_IR_PLAN.md](codegen/CG_IR_PLAN.md) §7 (Phase 2). The
`pn_should_emit()` predicate inside `cg_normalize` must derive
from documented semantics, not from a swing through gate
variants under test.

Sister docs: [IR.md](IR.md) §3.2 (the `Sym` flag soup, including
`live`), §5 (the `PNode` declaration), §5.1 (the `Var`
declaration); [CFG_SSU.md](CFG_SSU.md) (SSU pass that produces
the phi/phy machinery the liveness gates interact with).

---

## 1. The four flags at a glance

| Flag | Type | Width | Default | Meaning |
|---|---|---:|---|---|
| `Sym::live` | bitfield | 1 | 0 | IF1-DCE result: this Sym (constant, variable, function, type) is reachable from `if1->top` and has at least one observable use. |
| `Var::live` | bitfield | 1 | 0 | Post-FA-DCE result: this Var contributes to the program's observable behavior. |
| `PNode::live` | bitfield | 1 | 0 | Post-FA-DCE result: this PNode's effect is observable (its lvals are live, or it has a side effect, or it dominates exit). |
| `PNode::fa_live` | bitfield | 1 | 0 | FA-reachability result: at least one `EntrySet` for this PNode's containing Fun was reached during flow analysis. |

The two PNode flags are NOT redundant. They answer different
questions:

- **`fa_live`** answers "did the analyzer's *forward flow*
  actually arrive here?" In post-clone code, each `Fun` is
  specialized into one or more `EntrySet`s; only PNodes
  reachable in some EntrySet get `fa_live=1`. Templates that
  every EntrySet ended up shadowing stay at `fa_live=0`.
- **`live`** answers "did *backward DCE* prove this PNode's
  result is needed?" A PNode can be `fa_live=1` (the analyzer
  reached it) but `live=0` (its result flows nowhere
  observable). Conversely, a PNode can be `live=1` (DCE marked
  the surviving SSU rename as needed) but `fa_live=0` (the
  template version got shadowed during cloning).

The codegen gate is the **intersection**: `live && fa_live`. Both
analyses must agree.

---

## 2. `Sym::live` (`sym.h:?`, `if1.h` flag-soup)

### Setters

- `if1.cc:383` (`mark_live`, inside
  `if1_simple_dead_code_elimination`). Walks `if1->allsyms`,
  re-marks every Sym whose containing Code chain is live. Iterates
  until fixed point.
- `if1.cc:607` (initial frontend-emission marker). When DCE is
  disabled (`fdce_if1 = 0`, the early-development path), every Sym
  starts `live=1` and stays that way.
- `if1.cc:415` and adjacent (per-Code mark via `mark_code_live`).

### Clearers

- `if1.cc:495` (DCE's reset phase). At the start of each DCE pass,
  every `live` bit is cleared and then re-derived from the live
  seeds (entry point + side-effect-bearing primitives).

### Readers

- **Frontend** (pyc, V): reads `s->live` before emitting per-Sym
  setup code. A `!live` Sym is allocated in IF1 but contributes
  nothing to the emitted program.
- **Cloner** (`clone.cc`): seeds the per-clone work list from
  `live` Syms.
- **Codegen**: reads transitively via `Fun::live` (which mirrors
  `Sym::live` for a function's defining Sym) — non-live Funs
  aren't emitted at all.

### Semantics

> "This Sym is part of the surviving program after whole-program
> DCE."

The strictest of the four flags — set only for things that
survive IF1's `mark_live` fixed point.

---

## 3. `Var::live` (`var.h`)

### Setters

- `dead.cc:55` (`mark_live_avar`). When the FA-level
  reachability analysis decides an `AVar` is live, the AVar's
  underlying `Var` is also marked live.
- `inline.cc:152` (`new_live_Var`). The inliner creates fresh
  Vars during inlining; these get `live=1` directly.

### Clearers

- `dead.cc` resets `Var::live` at the start of each DCE pass
  (alongside the PNode bits — see §4 below).

### Readers

- **Codegen**: every backend filters its emission lists by
  `v->live`. Non-live Vars don't get C declarations or LLVM
  allocas.
- **Cloner**: live Vars participate in `vmap` rewriting; non-live
  ones are dropped from the cloned body.

### Semantics

> "This Var is referenced in some PNode whose effect survives
> DCE."

In SSU form, the Vars `live` bit reflects whether the
SSU-renamed alias is consumed downstream. **SSU renames of
formal args are often `live=0`** because the rename alias gets
folded back to the original in some optimizer pass — but the
binding MOVE is still necessary for the renamed local to hold
the value. This is the proximate cause of [issue 016](issues/016-llvm-ssu-formal-arg-binding.md).

---

## 4. `PNode::live` (`pnode.h:?`)

### Setters

- `dead.cc:155` (`mark_live_pnode_loop`). A PNode becomes live
  if any of these holds, and propagated until fixed point:
  - One of its `lvals` is live (the assignment's destination is
    used downstream).
  - It has an unconditioned side effect (a non-functional
    primitive: `prim_print`, `prim_setter`, etc.).
  - It dominates `f->exit` and is needed for control flow.
  - It's a `Code_LABEL`/`Code_GOTO` referenced by a live
    branch.
  - It calls into a Fun whose entry is live (live propagation
    across call edges).
- `inline.cc:212`. When the inliner inserts a fresh PNode at a
  call site, the new PNode is `live=1` directly.

### Clearers

- `dead.cc:32` (DCE start). Resets every PNode in `f->fa_all_PNodes`
  to `live=0` before re-running the mark phase.

### Readers

- **Codegen per-kind gate** (`cg.cc:586` and parallel in
  `llvm_codegen.cc:579`): the per-PNode kind emission switch
  is wrapped in `if (n->live && n->fa_live)`.
- **C backend phi/phy out-of-gate fallthrough** (`cg.cc:604-650`):
  the `default` case in the second switch — phi/phy
  materialization — runs **unconditionally**, regardless of
  `live`. This is the structural divergence the LLVM backend
  inherited the wrong half of.

### Semantics

> "This PNode's effect is observable in the post-DCE program."

This bit alone is NOT sufficient to gate codegen emission. A
PNode can be `live=1` but `fa_live=0` if it's a template version
shadowed by a clone (the surviving clone has its own
`live=1, fa_live=1` copy).

---

## 5. `PNode::fa_live` (`pnode.h:?`)

### Setters

- `clone.cc:882` (`fixup_clone_ess`). After cloning, for each
  `EntrySet` of each `Fun`, every PNode in
  `es->live_pnodes` gets `fa_live=1`. Crucially, this is set on
  the **specialized clone's** PNode — the template's PNode stays
  at `fa_live=0` if every reaching EntrySet ended up with its
  own clone.
- `inline.cc:213, 418`. Inserted PNodes are `fa_live=1` so
  subsequent DCE/codegen treats them as reachable.

### Clearers

Never explicitly cleared after initial zero-init. Once an
EntrySet's `live_pnodes` includes a PNode, the bit sticks.

### Readers

- **Codegen per-kind gate** (`cg.cc:586`, `llvm_codegen.cc:579`):
  paired with `live` as the intersection.
- **Per-primitive emitters in the LLVM backend** sometimes
  consult `pn->live` separately (e.g., `P_prim_period` skips
  silently when the field can't be resolved AND `!live` — handles
  builtin-scalar method binding without crashing on
  unresolvable getters).

### Semantics

> "The forward flow analysis reached this PNode in at least one
> EntrySet of its containing Fun."

The complement of `live`. Where `live` is backward DCE's verdict,
`fa_live` is forward FA's verdict. The codegen needs both —
forward reachability alone isn't enough (the FA may reach a
PNode whose result is dead), and backward DCE alone isn't
enough (the DCE may mark a template's PNode live based on the
template's lvals, when the surviving clone has its own copy
with the same lvals).

---

## 6. The three gates

The four flags combine into three semantically distinct gates
that codegen has to pick from:

### 6.1 `live && fa_live` — strict intersection

What the C backend uses (`cg.cc:586`). The strictest filter —
both analyses agree this PNode survives.

**Use for**: per-kind emission. The MOVE/SEND/IF/GOTO/LABEL body
each backend emits.

**Skips**: PNodes that are live by one analysis but not the
other. Specifically:
- The for-loop body PNodes in iterator-style functions whose
  SSU template has `live=1, fa_live=0` (FA shadowed by clone).
- The SSU formal-arg → renamed-local MOVEs whose alias was
  optimized away (`live=0, fa_live=1`).

### 6.2 Unconditional — the phi/phy fallthrough

What the C backend uses for phi/phy materialization (`cg.cc:604-650`).
Runs regardless of either liveness flag. The phi/phy MOVEs are
SSU bookkeeping; dropping them leaves renamed locals
uninitialized even if neither analysis marked them live.

**Use for**: phi/phy materialization. The successor-side and
predecessor-side moves SSU inserts to bind SSU-renamed Vars
across CFG joins/splits.

**Skips**: Nothing. Always runs.

### 6.3 `fa_live` alone — over-permissive

What the LLVM backend used before commit f5b6200. Emits any
PNode the analyzer reached, even if DCE proved its result is
dead. Surfaces template PNodes the clone shadowed; emits
duplicate work; can also surface PNodes whose Vars have no
allocated slots (because the Var was DCE'd).

**Don't use**: nothing. Mentioned only because it's the trap the
LLVM backend fell into.

### 6.4 `live` alone — partially-permissive

What the LLVM backend tried mid-session. Closer to right than
`fa_live` alone but still misses the template-PNodes that DCE
kept alive because the surviving clone is also live.

**Don't use**: same reason — incomplete coverage. The C backend's
choice of `live && fa_live` plus unconditional phi/phy is the
documented contract.

---

## 7. The contract for `cg_normalize::pn_should_emit(pn)`

CG_IR_PLAN §7.1 (Phase 2.3 per-PNode lowering) needs a
`pn_should_emit(pn)` predicate. Given the audit above:

```cpp
// Returns true iff the per-kind lowering should run.
// (phi/phy materialization always runs, in a separate pass —
// see lower_phi_phy in CG_IR_PLAN §6.2.4.)
bool pn_should_emit(PNode *pn) {
  return pn && pn->code && pn->live && pn->fa_live;
}
```

The contract:
- Per-kind lowering gates on `live && fa_live` (matches C
  backend §6.1).
- phi/phy materialization runs unconditionally (matches C
  backend §6.2, fixes issue 016 structurally).
- No third path. The two are exhaustive.

---

## 8. Common pitfalls

### 8.1 "fa_live is more accurate than live"

The original LLVM backend comment claimed this. It's wrong.
`fa_live` is **forward** reachability; `live` is **backward**
DCE survival. Neither is more accurate; they answer different
questions. Per-kind emission needs both.

### 8.2 "If I add the LLVM equivalent of phi/phy emission inside the live gate, that's enough"

It isn't. The phi/phy moves for SSU formal-arg binding are on
PNodes with `live=0` (the SSU rename alias is unused) but
`fa_live=1` (forward flow reached). Gating phi/phy on `live`
drops them. Issue 016's gold standard fix is to materialize
phi/phy **outside** the live gate.

### 8.3 "What about Code::live? Sym::live? Label::live?"

These exist and are set by the IF1 DCE pass (`if1.cc`). The
codegen doesn't read them per-PNode; they govern what gets
emitted at the file-scope level (which functions, globals, type
declarations are needed). Out of scope for per-PNode gating.

### 8.4 "What about Fun::live?"

`Fun::live` is set in `dead.cc:156` whenever any PNode of the
function is marked live. Codegen reads it as the per-function
emission gate: `if (!f->live) skip;`. Doesn't participate in the
per-PNode gating audit.

---

## 9. References

- `ifa/if1/if1.cc:383` — `mark_live` for Sym/Code.
- `ifa/optimize/dead.cc:55, 155` — FA-level DCE for AVar/Var/PNode.
- `ifa/analysis/clone.cc:882` — `fa_live` propagation post-clone.
- `ifa/codegen/cg.cc:586, 604-650` — the C backend's two-stage
  gate (per-kind gated, phi/phy unconditional).
- `ifa/codegen/llvm_codegen.cc:579` — the LLVM backend's current
  per-kind gate.
- `ifa/issues/016-llvm-ssu-formal-arg-binding.md` — the bug that
  motivated this audit.
- `ifa/codegen/CG_IR_PLAN.md` §5.2 — the audit's role in the
  CG_IR migration.
- [IR.md](IR.md) §3.2 — Sym flag-soup overview.
- [CFG_SSU.md](CFG_SSU.md) §4 — SSU's phi/phy generation.
