# Issue 031: Globals live outside all of FA's precision machinery

**Status:** open (analysis; no crash outstanding). Filed 2026-07-04
as the common-root-cause writeup for pyc issues
[001](../../issues/001-fa-crash-captured-locals.md) /
[002](../../issues/002-fa-crash-escaped-closure.md) /
[005](../../issues/005-while-true-fa-crash.md), all three of which
are individually fixed. This issue tracks the underlying structural
gap so future symptoms are recognized as instances of it, and
proposes the design-native cleanup.
**Affects:** `analysis/fa.cc` (`make_AVar`, `update_in`,
`GLOBAL_CONTOUR` sentinel and its ~15 scattered guards),
`optimize/ssu.cc` (locals-only renaming), `analysis/clone.cc`
(concretization over shared global AVars), both codegen backends
(consumers of the resulting union types).
**Related:** `029`/`030` (polymorphic dispatch — the *other* half of
"a callable-valued cell with more than one shape").

## The core issue

FA has one first-class context mechanism — EntrySet contours, with
per-call-site `filtered` AVars, restrict predicates (issue-025
narrowing), per-ES liveness and cloning. Values that stay on that
track get refined types everywhere. But `make_AVar`
(`analysis/fa.cc:206`) routes variables onto three tracks:

1. **EntrySet contour** (locals, internals) — first-class.
2. **Display** (`nesting_depth != 0` and not this function's own
   frame) — classic Algol displays; assumes stack-disciplined
   nesting. Broke for escaping closures (issue 001); pyc's fix
   moved captured state onto the CreationSet track (closure-carrier
   classes), i.e. *onto a first-class track* — the right pattern.
3. **`GLOBAL_CONTOUR`** (`(void*)1` sentinel) — one shared AVar per
   global Var, program-wide.

Track 3 is second-class in two independent ways, and each way has
already produced a shipped bug:

- **Not a real EntrySet.** Any per-ES machinery that touches a
  global AVar must special-case the sentinel or crash. Issue 005
  (`while True:` → `is_if_arg` set on `sym_true`'s global AVar →
  `update_in` dereferenced `(EntrySet*)1`) was exactly this; the
  fix was one more guard (`fa.cc:283`), joining ~15 existing
  `!= GLOBAL_CONTOUR` checks scattered through fa.cc/clone.cc.
  Every new per-ES feature is a fresh opportunity for the next 005.
- **No flow-sensitivity and no per-read refinement.** Globals are
  exempt from SSU renaming (`ssu.cc` `new_Var`/`get_Var`:
  `if (!v->sym->is_local) return v;`), so all stores union into one
  cell, observed identically at every read — a global's type is the
  eternal union of everything ever assigned. Issue 002 Case B was
  exactly this: the idiomatic `stash = None` initializer made every
  read of `stash` a `SUM{nil_type, closure}`. (For locals, the same
  program shape is precise: SSU splits the None def from the
  closure def.) The 002B fix taught codegen the nullable-closure
  SUM; the *union itself* remains, and any global that ever holds
  two structurally different non-nil values still degrades to a SUM
  that only 029/030's dispatch machinery could call through.

Issue 001 belongs to the same family via track 2: all three issues
are cases of "a value needing type refinement passed through a
contour mechanism that doesn't refine."

## What already works (and should be the model)

- Issue 001's resolution is the template: don't patch the
  second-class track, move the data onto a first-class one
  (captured locals → CreationSet fields on a carrier class).
- `SUM{nil_type, T}` is already a first-class codegen idiom
  (nullable pointer, `assign_type_cg_strings_pass2`), now including
  `T = closure` (002B fix, `closure_fun_type`).
- FA dispatch is already sound over global-typed unions (it
  resolved `stash()` correctly even when the static type was
  `None ∪ closure`).

## Proposed cleanup (in design order, independently landable)

1. **Promote the global contour to a real EntrySet.** Replace the
   `(void*)1` sentinel with a distinguished singleton EntrySet
   (natural candidate: the top-level Fun's unique ES, which
   `make_top_edge` already guarantees exists, or a dedicated
   always-empty `GlobalEntrySet`). Then `update_in`'s `is_if_arg`
   enqueue, `creation_point`'s `es->split` path, and every other
   `(EntrySet*)av->contour` deref work uniformly; the ~15 sentinel
   guards (including the 005 guard) become dead and can be deleted.
   Mechanical but wide; needs care at the few places that use
   `contour == GLOBAL_CONTOUR` as a *semantic* test ("is this a
   global?") — those should switch to an explicit predicate
   (`av->contour == fa->global_es` or a flag), notably
   `clone.cc:initialize` (global_avars collection),
   `escape.cc` ("globals always escape"), and
   `make_AVar`'s own routing.
2. **Per-read-site refinement for globals ("load temps").** Treat a
   global as a memory cell, not a register: at IF1 build (or as an
   FA-side transform mirroring `get_filtered`), each *read* of a
   global flows through a fresh internal local temp
   (`global_cell → read_temp` MOVE). The temp is on the first-class
   track: SSU-renamed, ES-contoured, eligible for issue-025
   narrowing (`if g is not None: g()` then refines the *temp*, not
   the shared cell) and for precise concretization. The cell keeps
   the sound flow-insensitive union; reads stop being forced to
   carry it verbatim. This is the FA-native analogue of what SSU
   already does for locals, and it composes with narrowing exactly
   the way `filtered` args compose with formal filters.
3. **Multiple closure shapes in one global** — out of scope here;
   that is 029/030 (dispatch over a union of callable types), and
   after (2) the residual need for it shrinks to programs that
   genuinely keep differently-shaped callables in one cell.

Step 1 removes the crash class (005-alikes) structurally; step 2
removes the imprecision class (002B-alikes) structurally. Neither
is urgent — the shipped guards and the 002B nullable-closure fix
cover today's known symptoms — but any new FA feature that touches
per-ES state should check this issue first, and if a new
GLOBAL_CONTOUR guard is about to be added, that is the signal to do
step 1 instead.

## Verification sketch

- Step 1: full suites (both backends) + `ifa-test` unchanged;
  delete the 005 guard and confirm `while True:` tests still pass;
  grep proves no `(void *)1` sentinel remains.
- Step 2: `closure_in_global.py` stays green; add a narrowing
  variant asserting the *temp's* type at the guarded call site is
  closure-only (e.g. via `-l F` var-type log), not `None ∪ closure`.
