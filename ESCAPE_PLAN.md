# Escape Integration into IFA — Plan

Status: **Phase 1 in progress** (2026-06-19). Following phases
not yet scheduled.

The plan: move escape analysis from a post-IFA codegen pass
(today's [`closed/023-v2-is-value-type-consumer.md`](issues/closed/023-v2-is-value-type-consumer.md)
Stage 3) into IFA proper, so escape is a first-class lattice
that IFA propagates and clones on alongside types.

## Goals and non-goals

**Goals.**
- Escape status becomes a first-class IFA lattice, propagated
  and refined alongside types.
- Cloning triggers on escape divergence at call sites the
  same way it triggers on type divergence today.
- Codegen consumes the per-clone escape annotation directly
  (no post-hoc analysis in `cg_normalize_v2`).
- Soundness: an arg marked NoEscape genuinely doesn't leak
  its memory through that call.

**Non-goals.**
- We are not proving full memory safety or doing region
  inference.
- We are not eliminating GC; we are reducing heap pressure
  where escape is provably absent.
- We are not handling escape across language boundaries
  (`__pyc_c_call__` will be modeled conservatively).

## Opt-in flag

Wire the integration behind a build/runtime flag so the old
post-IFA pass stays intact during phased rollout.  Both paths
coexist for phases 1-4; phase 5 deletes the legacy path.

- Env var: `IFA_ESCAPE_IN_FA=1` enables the new in-FA
  analysis.  Default off — codegen still reads from Stage 3's
  `cg_normalize_v2:compute_arg_escapes`.
- CLI flag on `pyc` / `ifa`: `--escape_in_fa` mirrors the env.
- IFA-internal: `bool ifa_escape_in_fa` (declared next to
  other phase toggles in `fa.h` or `defs.h`); FA reads at
  init time.
- Codegen interface: if the flag is on AND IFA produced
  `arg_escapes` on the `Fun`/`EntrySet`, `build_fun_decl`
  copies those into the CGv2Fun and skips the local
  recompute.  Otherwise the existing Stage 3 path runs.

## Architecture

### Lattice

Two-point monotonic lattice on `AVar`:

```
                 Escape   (top — conservative)
                   ▲
                   │
                NoEscape  (bottom — wins)
```

Reasoning: a three-point lattice (No / May / Must) gives more
precision but doubles clone count for marginal gain.  Start
with two-point; revisit if the lift looks plateau'd.

**Where it lives.**
- On `AVar`: `EscapeStatus escape;` — current best knowledge
  for this analysis variable.
- On `EntrySet`: `Vec<EscapeStatus> arg_escapes;` parallel to
  the existing type signature, part of the spec key.
- On `Sym` (Fun's formals): derived from the EntrySet they're
  bound to.

### Transfer functions

Per IF1 `Code::kind`:

| Code kind | Transfer |
|---|---|
| `Code_MOVE` | `lval.escape := lval.escape ⊔ rval.escape` (escape is transitive through aliasing). |
| `Code_SEND` (prim) | Per-prim rules below. |
| `Code_SEND` (concrete fn call) | For each rval `i`, `rval.escape := rval.escape ⊔ (target_entryset.arg_escapes[i] ? Escape : NoEscape)`. |
| `Code_IF` / `Code_GOTO` | Join at merge points. |
| `Code_LABEL` | Phi-join. |
| `Code_SEQ` / `Code_CONC` / `Code_NOP` | Pass-through. |

Per-prim escape rules (subset):

| Prim | Effect |
|---|---|
| `prim_period` (field load) | `lval.escape := self.escape` if the field type is a ptr type (transitive). Bottom otherwise. |
| `prim_set_period` (field store) | `value.escape := value.escape ⊔ self.escape`. |
| `prim_index_object` / `prim_set_index_object` | Same as period. |
| `prim_reply` (return) | `arg.escape := Escape`. |
| `prim_make` / `prim_new` | `lval.escape := NoEscape` (fresh allocations start non-escaping; refined by subsequent uses). |
| `prim_clone` / `prim_clone_vector` | Same as `prim_make`. |
| `prim_capture` (closure-build, value position) | `captured.escape := captured.escape ⊔ closure.escape`. |
| `__pyc_c_call__` | Conservatively `Escape` for every ptr-typed arg. |
| Pure value prims (`add`, `sub`, `lt`, ...) | NoEscape. |

Global accesses (`MOVE` into a global Sym) escape the source.

### Cloning policy

Today's cloning fires when a call-site's arg ATypes don't fit
any existing EntrySet for the target Fun.  Extend the
EntrySet key:

```
EntrySetKey = (Fun, [AType per arg], [EscapeStatus per arg])
```

Cloning fires on:
1. Type divergence (today).
2. Escape divergence on **any arg whose CGv2Type is a ptr**
   (escape is meaningless for value types).

Escape-only divergence is bounded: at most 2 versions of each
fn per type signature.

**Cap**: a per-Fun clone budget (e.g. 8).  If exceeded,
collapse to conservative (all args Escape).  Same shape as
the existing type-cloning budget.

### Fixed point

Reuse IFA's worklist scheduler.  Each AEdge re-fires when
either:
- Caller's arg AType changes (today).
- Caller's arg escape changes (new).

Convergence: monotonic lattice → guaranteed termination.
Cost: proportional to total escape-flips across all AVars,
bounded by 2 × |AVars| × |EntrySets|.

### Codegen interface

`cg_normalize_v2`'s post-IFA escape pass becomes a simple
readback (under the flag):

```cpp
void build_fun_decl(NormCtx &c, Fun *f) {
  ...
  if (ifa_escape_in_fa && f->entryset->arg_escapes.n) {
    cf->arg_escapes.copy(f->entryset->arg_escapes);
  }
  ...
}
```

`CGv2Value::escapes` populated similarly from each AVar's
escape.

## Phased implementation

The plan splits into 6 milestones, each independently
verifiable.

### Phase 1 — Lattice plumbing (no behavior change)

- Add `EscapeStatus` enum + `escape` field on `AVar`.
- Add `arg_escapes` vector on `EntrySet` and `Fun`.
- Wire the `--escape_in_fa` / `IFA_ESCAPE_IN_FA` flag.
- Initialize everything to `Escape` (conservative top).
- Codegen reads from IFA fields when the flag is on; no
  analysis yet — so the flag-on path produces identical
  output to flag-off.

**Verification:** Behavior identical with flag on or off.
Pyc suite 82/0.

### Phase 2 — Local transfer functions

- Implement per-Code transfer rules for `Code_MOVE`,
  `prim_period`, `prim_set_period`, `prim_reply`,
  `prim_make`/`prim_new`, `prim_clone`.
- Initialize fresh allocations to `NoEscape`; let uses pull
  them up.
- Single-function fixed point (no inter-fn yet — treat every
  call as Escape).

**Verification:** Constructor wrappers' fresh allocs prove
non-escaping locally; per-fn arg_escapes match current
Stage 3 results within a function.  Pyc suite 82/0.

### Phase 3 — Inter-procedural propagation

- Wire `Code_SEND` (concrete fn call) transfer to read
  callee's `arg_escapes`.
- Re-fire callers when callee's `arg_escapes` refines.
- Indirect calls / closure dispatch: take the union over
  the set of possible callees.

**Verification:** Match Stage 3's corpus results (3 struct
allocas, same locations).  Pyc suite 82/0.  ifa unit + IR
tests clean.

### Phase 4 — Cloning trigger

- Extend `EntrySet` matching to include escape signature.
- On call-site escape divergence, allocate a new EntrySet →
  trigger `clone_for_entryset`.
- Add the per-Fun clone budget (default 8).

**Verification:**
- Synthetic test: `def store(self, x): self.items.append(x);
  if rare(): register(self)`, called from one site with
  `rare()` provably false (NoEscape clone) and one where it
  can be true (Escape clone).  Expect 2 clones in the IR.
- Pyc suite: clone count rises measurably but stays within
  budget.  Coverage probe across the corpus.

### Phase 5 — Codegen simplification

- Delete `compute_arg_escapes` in `cg_normalize_v2`.
- `value_escapes_in_fun` collapses to `return v->escapes`
  (already does after Stage 3 — no change).
- Make `IFA_ESCAPE_IN_FA=1` the default; deprecate the flag.

**Verification:** Identical IR output to Phase 4.  Reduces
code surface.

### Phase 6 — Tuning and audit

- Run on a real benchmark beyond the test suite.
- Measure: clone count, peak heap allocation, runtime.
- Refine clone budget / cap based on observed behavior.
- Document the lattice + transfer in `ifa/IR.md` and
  `CG_IR_PLAN.md`.

**Verification:** 4-quadrant table (test, pre-clones,
post-clones, alloca delta) committed to `closed/023` as
evidence.

## Verification gates

Each phase gates on:
1. Pyc suite ≥ baseline pass count on both backends.
2. ifa unit tests 99/0.
3. ifa IR-phase tests all green.
4. LLVM `verifyModule` clean (strict mode).
5. Phase-specific signal (see above).

Land each phase as its own commit; revert is `git revert
<sha>` of that commit.

## Risks and mitigations

| Risk | Mitigation |
|---|---|
| **Unsound transfer rule → UB.** A bug in a transfer function silently marks an escaping value as NoEscape. Codegen alloca's it; the program corrupts memory. | Property test: for each Phase 2-3 commit, run a generated-AST stress test asserting that the union of all NoEscape-marked values forms a valid GC-rooted-otherwise-discarded set.  LLVM strict-verify gate catches some shapes. |
| **Clone explosion.** Per-arg escape combined with per-arg type → 2^k blowup. | Per-Fun clone budget. Plus the ptr-only filter (escape divergence for non-ptr args doesn't trigger). |
| **Fixed-point divergence / slowdown.** New lattice dimension extends convergence time. | Monotonic lattice guarantees termination. Telemetry on per-Fun iteration count; flag any that hit > 10. |
| **Closure capture handling.** Closures alias captured vars; getting the transfer wrong corrupts closures' lifetimes. | Phase 3 dedicates a sub-test (closure construct → escape → field-store) before relying on the rule elsewhere. |
| **Cross-pass impedance with cg_normalize_v2.** Today's Stage 3 fields stay populated; new fields must not contradict. | Phases 1-4 leave both populated and assert equality in debug builds.  Phase 5 deletes the old path. |
| **Debug story degrades.** Multiple `_CG_f_*_NoEscape` and `_CG_f_*_Escape` clones look identical in stack traces. | Name suffix the clones (`_ne`, `_e`) in `cg_string`. Update `print_clone.cc` to surface the escape sig in IR dumps. |

## Estimated scope

| Phase | Code surface | Effort estimate |
|---|---|---|
| 1 | ~150 LOC, 4 files | ½ day |
| 2 | ~300 LOC, 3 files (fa.cc, prim_data.cc, IR.md) | 1–2 days |
| 3 | ~200 LOC, 2 files (fa.cc, clone.cc) | 1 day |
| 4 | ~250 LOC, 2 files (clone.cc, EntrySet) + 2 fixtures | 2 days |
| 5 | -200 LOC (deletes), 1 file | ½ day |
| 6 | benchmarks, doc updates | 1 day |
| **Total** | ~700 net LOC | **6–8 working days** |

## When to start

Trigger: a concrete benchmark where heap allocation rate is
the limiting factor for runtime, AND the workload exhibits
conditional-escape patterns the per-Fn analysis can't
capture.  Without that signal, the lift is bounded at ~3-5
additional struct allocas across the current pyc test suite.

Phase 1 alone is a 4-hour change and gives the data-structure
plumbing without changing behavior — that's the safest hedge
if you want to "land the architecture" now and finish the
rest later.
