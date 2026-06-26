# CG_IR_B10_10_DESIGN.md — design for the next ratchet jump

Phase B.10's per-commit ratchet plateaued at **41/34** under
`IFA_LLVM_V2=1` (vs v1 LLVM **38/37**). The remaining 22 EXEC
failures share two cross-cutting root causes that no single-
commit fix has been able to resolve. This document chooses one,
proposes a concrete path, and bounds the scope.

## Provenance

| Source | Role |
|---|---|
| [CG_IR_SEMANTICS.md](CG_IR_SEMANTICS.md) §6 | the migration plan we're executing |
| [CG_IR_META_PLAN.md](CG_IR_META_PLAN.md) | the meta-plan that produced Phase B |
| B.10.1 – B.10.9 commits | the empirical ratchet that revealed the two issues |

## 1. The two cross-cutting issues

Each remaining EXEC failure falls into one of these two
buckets (or, occasionally, both).

### Issue A — Analyzer specialization gap

Pyc's IF1 analyzer specializes function bodies based on call-
site constants. When a caller passes a literal `range(10)`,
the analyzer can inline the `10` into the specialized `range.__init__`,
producing a body whose effect is "set `self.j` to constant 10."

v1's LLVM emitter walks the specialized IF1 PNodes and produces
the corresponding `store i64 10, ptr (field 3)` directly. v2's
`cg_normalize_v2` walks the same PNodes but doesn't observe the
specialization — the literal 10 gets routed through the wrong
field, or gets dropped entirely.

**Symptom**: `range(10)`'s `__init__` stores 10 into `self.i`
(field 0) instead of `self.j` (field 3). The for-loop's
`__pyc_more__()` compares `i < j` and gets `10 < 0`, exits
immediately.

**Tests blocked**: `for_range_from_zero`, `for_over_range`,
`for_over_list`, `for_over_tuple`, `list_index_assign`,
`list_print`, `bitwise_operators` (loop variant), maybe
`list_comprehension`. ~7 tests.

### Issue B — Class-prototype global initialization

Pyc represents `class A: x = 2` as two pieces:
1. A class-body function that sets fields on a prototype object
2. A global pointer (`@g` in the v2-emitted IR) that points to
   the prototype

v1 either (a) initializes the prototype during module load via
side-effects of `createGlobalVariables` + class-body execution,
or (b) DCE eliminates the prototype-store path entirely (the
class object isn't a stable runtime entity in v1's emission).

v2's `cg_normalize_v2` emits the prototype-set IF1 PNodes
verbatim, producing `store ... ptr @g` — but `@g` is declared
`internal global ptr null` and never assigned. The store goes
through a null pointer and segfaults.

**Symptom**: any test that touches a class attribute via the
shared prototype crashes.

**Tests blocked**: `attr_augmented_assign`, `class_attr_mutation`,
`class_init`, `operator_overload`, `polymorphic_function`,
`logical_operators` (segfault variant). ~6 tests.

## 2. Which to tackle first

**Recommendation: Issue B.**

Reasoning:

| Criterion | Issue A | Issue B |
|---|---|---|
| Test impact (direct) | ~7 | ~6 |
| Test impact (indirect) | medium (cascades through iterators) | medium (class machinery is foundational) |
| Localization | distributed (per-call-site specialization) | localized (a single module-init prelude) |
| v1 reference | analyzer-side; hard to mirror in cg_normalize_v2 | createGlobalVariables-side; clean v1 code to read |
| Reversibility | partial — changes thread through many tests | high — failure mode is "prototype is null"; adding init can't make it more null |
| Composes with later work | likely; Issue A bleeds into closures | yes; class-init enables A's runtime to behave more deterministically |

Issue B is the better target. The fix has a cleaner v1 reference
(`createGlobalVariables` + module-init ordering), produces a more
localized change in `cg_normalize_v2` (one new pass), and unblocks
a meaningful chunk of tests without forcing a redesign of the
per-PNode walk.

## 3. Issue B: design

### 3.1 What "initialized" means

For a global `@g` of type `ptr-to-A`:
- v1 either leaves it null but never dereferences it before
  it's been set by the class body, or sets it via store-from-
  malloc somewhere we haven't pinpointed.
- v2 stores into `@g` via field-by-field assignment from the
  class body but never first allocates the underlying object.

The narrowest fix: before any field-store-through-`@g` runs,
the program needs to have done `@g = GC_malloc(sizeof(A))`.

### 3.2 Three implementation options

**Option 1 — class-init prelude in `cg_normalize_v2`.**

Add a new pass after `build_globals`: walk each Type_RECORD
Sym; for each, find the corresponding class-prototype global
Var. Emit an "init body" sequence at the head of the main
function that does:

```
@A_proto = GC_malloc(sizeof(A))
@g       = @A_proto
```

This synthesizes an init prelude in CGv2 IR. The v2 emit
materializes it as the first basic block of main.

Cost: ~80 LOC in cg_normalize_v2.cc. Touches main_fun's body.

**Option 2 — LLVM-level module init.**

Generate the prelude during `cg_v2_emit_llvm_module` rather
than in cg_normalize_v2. After the globals declaration pass,
walk prog->globals; for each ptr-to-struct global, emit a
sequence in a new `@__pyc_v2_init` LLVM function that runs
before main.

Use LLVM's `llvm.global_ctors` to register the init function
so the linker schedules it at module load.

Cost: ~100 LOC in cg_ir_v2_emit_llvm.cc. Doesn't touch
cg_normalize_v2. Risk: global ctors interact with the C
runtime and Boehm GC in ways v2 hasn't tested.

**Option 3 — investigate v1 first; adopt whatever v1 does.**

Trace v1's IR for a test like `class A: pass; a = A()` and
find exactly when `@g`-like prototypes get their initial
non-null value. Replicate the same mechanism. If v1 doesn't
initialize them and the test passes anyway, find out why
v1's IR doesn't reach the null-deref path.

Cost: 1-2 hours of investigation, then probably 0-50 LOC of
implementation depending on what v1 does.

### 3.3 Recommended: Option 3, falling back to Option 1

Investigation comes first because there's a real chance v1's
mechanism is something we can copy directly. If it isn't —
e.g., v1 relies on `cg_get_string`-driven name-mangling or
on DCE eliminating the null deref — we fall back to Option 1.

Option 2 stays in reserve as a structural alternative if the
prelude can't fit cleanly in main_fun's body (e.g., for
multi-module programs).

### 3.4 Investigation plan

1. Minimal repro: `class A: x = 2\nprint(A.x)` — does v1 even
   compile this? does it run? what's in the IR around the
   class definition?
2. Look at v1's `createGlobalVariables` for special-casing of
   Type_RECORD globals.
3. Look at `python_ifa_*` (the pyc frontend) for how the IF1
   for `class A: x = 2` is emitted. Is there a SEND that
   allocates the prototype? Where?
4. Compare v1's main IR vs v2's main IR for the same test:
   what's the first 10 instructions of each? Which one stores
   a non-null ptr into `@g` first?

Cost: 1-2 hours.

## 4. Implementation plan (assuming Option 1 path)

Each step is a separate commit per the meta-plan's per-CG_OP
cadence.

**B.10.10.1** — investigation document.

Write up what v1 does for `class A: x = 2`. Either:
- "v1 relies on M; v2 needs to do M" — proceed to B.10.10.2
- "v1 doesn't initialize, DCE saves it" — pivot to a v2-side
  DCE pass for prototype stores

**B.10.10.2** — Sym→prototype-Var mapping.

In `cg_normalize_v2.cc`, add a helper that walks prog->globals
post-build and identifies which CGv2Values are class
prototypes. The criterion: scope=GLOBAL + type is ptr-to-
struct (the B.10.5 wrapping) + the underlying struct is in
sym_to_struct (an actual user-defined record, not a builtin
ptr).

Test: unit test that builds a small IF1 + asserts the
prototype list has the expected entries.

**B.10.10.3** — emit the init prelude into main_fun.

After `build_funs` has produced main_fun, prepend an init
block to its first block. The block:
```
for each prototype global gv:
  alloc_inst = CG_ALLOC with type_arg = gv->type->element
  alloc_inst lvals = (a fresh local)
  move_inst = CG_MOVE with rval = the fresh local, lval = gv
```

Verify: build_terminator continues to work (it sees the new
block as the new entry). verifyModule passes.

**B.10.10.4** — run pyc-suite, ratchet.

Expected outcome: 41 → 46 or so. If the count moves much less
or there's a regression, B.10.10.3's emission has a bug.

**B.10.10.5** (if needed) — handle nested-class / multi-module
edge cases.

## 5. Pass criteria

- Pyc-suite under `IFA_LLVM_V2=1` increases by ≥ 4 tests.
- No regression in the 41 currently-passing tests.
- v1 baseline preserved (LLVM 38/37, C 74/0).
- Unit tests stay at 105/0.
- `verifyModule` clean on every passing test.

## 6. Risks and trade-offs

| Risk | Mitigation |
|---|---|
| Prototype init creates a circular dependency (proto needs class fields set before class body runs) | Run init in a strict order: structural fields zero-init via GC_malloc, then class body. Defer any field-store to AFTER the GC_malloc. |
| Init prelude breaks main's argv/return layout | Prelude is pure side-effect inside the existing main; doesn't change signature. |
| Init prelude bloats IR for programs without classes | The walk skips Vars with no struct payload; net-zero for class-free programs. |
| Issue A reasserts itself after Issue B is fixed | Likely. Phase B.10.11+ would tackle A. Adding the prelude can't make A worse. |
| The investigation finds v1 does something we can't replicate (e.g., relies on `cg_get_string` name choices we're not making) | Option 2 (LLVM-side global_ctors) becomes the backup. |

## 7. Effort estimate

- B.10.10.1 (investigation): 1-2 hours
- B.10.10.2 (mapping helper): 30 min
- B.10.10.3 (prelude emission): 1-2 hours
- B.10.10.4 (verify + ratchet): 30 min
- B.10.10.5 (edge cases, if needed): 1 hour

Total: 3-6 hours of focused work.

## 8. What this design deliberately omits

- A full re-derivation of pyc's class semantics. The plan
  trusts the v1 reference behavior; if v1's mechanism turns
  out to be subtler than expected, B.10.10.1 catches it.
- Issue A's path forward. That belongs in a separate
  Phase B.10.11 design once we understand whether B's fix
  cascades into A's tests.
- Closures (SQ3) and sum types (SQ2). Per the meta-plan,
  those wait for project-owner direction.
- Production-mode flip (the migration plan's Phase C). That
  requires the ratchet to land somewhere v2 ≥ v1 with high
  confidence across diverse programs; Phase B.10 hasn't
  reached that bar yet.

## 9. Pass criterion for this document

The project owner reads it and either:
- agrees with the recommended path (Issue B via Option 3 →
  Option 1 fallback) and B.10.10 work can begin; or
- flags a specific assumption as wrong — e.g., "Issue A is
  actually a bigger blocker than Issue B, do A first" — and
  the design pivots; or
- chooses one of the alternative options (e.g., Option 2's
  LLVM-side init) and B.10.10 follows that path.

If none of the above — if the disagreement is at a level
that requires re-investigation of what's actually failing —
B.10.10 reverts to fresh investigation before any code
lands.
