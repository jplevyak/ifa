# 121 — codegen emits clones nothing references (LLVM half) and narrows call sites without checking equivalence

**Closed 2026-10-14.** Dead clones are fixed before codegen for both
backends; every point that narrows a call to one clone now proves the
choice (clone equivalence) or calls the clone the site chose; and the one
remaining unprovable shape is warned, and refused under `--strict`. See
the two 2026-10-14 sections at the end. The residual (five call sites
where an ARGUMENT's type chooses the clone) is the one-class-two-clones
population of [190](../190-CODEGEN-run-time-dispatch-census.md).

**Status at the time (kept as the record):** the C backend was fixed
2026-09-01. Rewritten 2026-09-28; history in git:
`git show 3f36072b:ifa/issues/121-CGEN-dead-clones-emitted.md`.

## Root cause

There are two notions of reachable, never reconciled. `mark_live_funs`
walks `Fun::calls`, FA's CANDIDATE set per call site. Codegen then
narrows each site to ONE target: `get_target_fun_core` returns
`fns->v[0]` when the candidates' C signatures agree, and
`cg_build_new_to_val_map` installs one winner per `(constructor, slot)`.
Nothing recomputes liveness afterwards, so every discarded candidate is
still emitted. On pygmy that was 39 of 244 functions, named nowhere.

## Fixed (C backend)

`c_codegen_print_c` buffers each body and emits only what `init`
transitively NAMES, reading references back out of the emitted bytes
(`_CG_f_<symid>_<index>` names the exact clone). Hooking
`cg_get_string(Fun *)` instead is NOT sufficient: `c_rhs` names
function-valued Vars through `cg_get_string(Var *)`, and that attempt
dropped 9 live functions. pygmy's C went down 57% with an identical
image, and linalg's six C errors were all inside dead functions. Dead code
can be WRONG code, and that, not the size, is the value. Corpus-wide only
2.8% of clones were dead, so moving the pruning before `clone` is not
worth it (the narrowing needs concrete C types, which only `clone`
produces). Typeless struct placeholders are now zero-width (`char
eN[0]`), keeping the `eN` numbering dense.

## Open

1. **LLVM backend** (`cg_emit_llvm.cc`) still emits dead clones. Buffer
   its bodies the same way and reuse the reachability.
2. **The narrowing assumes equivalence it never checks.**
   `get_target_fun_core` picks `fns->v[0]` whenever the C signatures
   agree. That is sound only if the candidates are INTERCHANGEABLE. In
   2026-09 FA's stage 5 produced per-CreationSet contours of
   `tuple.__getitem__` that each folded a DIFFERENT literal, and the call
   site bound to one of them (`test_heapq` printed "medium" for every
   item). The source is removed (stage 5 now declines when the receiver
   CSs are identical by type; see [134](../134-remove-the-frontend-forced-split-opt-in.md)),
   but the narrowing is unchanged. It should require the candidates to be
   clone-equivalent (`ES_FN::equivalent`), or dispatch. It must never
   pick silently.
3. **Creation, not emission, is the real excess:** 72 of pygmy's
   surviving functions are byte-identical to a sibling. That is contour
   minimality, [170](../170-FA-contours-minted-on-transient-types-are-never-remerged.md).
   `ES_FN::equivalent`'s unconditional `return 0` in the creation-point
   block is load-bearing (removing it gives `missmatched offsets`), so do
   not "fix" it in isolation.

## Verification

LLVM: the emitted functions equal those the C backend emits, program by
program, with identical output. Narrowing: a fixture where two candidates
agree on the C signature but differ in behaviour (per-literal folds) is
refused or dispatched, never bound to one clone.

## 2026-10-14: liveness before codegen, and equivalence instead of signatures

**Dead clones: root-caused, fixed before codegen.** `PYC_DBG_CGDCE=2`
now classifies each body the C backend drops. Every one in the corpus was
a function with a LIVE call site in an EMITTED function that the emitted C
never named: identity functions (`str.__str__`, every `__iter__` that
returns `self`, `int.__hash__`). `simple_inlining` (`ifa_optimize`, before
codegen) turns such a send into a MOVE (`convert_to_move`) but left its
`f->calls` entry, and `mark_live_funs` follows `calls` at every live node,
so the callee stayed live for every backend. `inline_single_pnode` and
`inline_prim_chain` already replace the entry; `convert_to_move` now
clears it. Liveness is then exact before codegen, so the LLVM backend,
which emits what is live, emits what the C backend emits:

| program | C emitted (byte scan drops) | LLVM `define`s |
| --- | --- | --- |
| pygmy | 183 (0, was 4) | 183 |
| richards | 140 (0) | 140 |
| chull | 283 (0) | 283 |
| sunfish | 512 (0, was 7) | 512 |
| msp_ss | 313 (0) | 313 |
| bh | 188 (3) | 191 |
| pygasus | 477 (3) | 480 |

The C byte scan stays as the backstop. What it still drops, 17 bodies in
the corpus and 6 in the suite, are all method-slot losers, the open item
below, and they are the whole LLVM difference.

**Clone equivalence, before codegen.** `compute_clone_equivalence`
(`codegen_common.cc`, called from `pyc.cc` after optimization) partitions
the live clones into INTERCHANGEABLE classes: the same nodes in the same
walk order, the same primitives, variables of the same type Syms (unions
are interned, ifa/120) numbered locally, the same constants and globals,
and callees in the same classes -- refined until stable. 43-124 ms on
pygasus, sunfish and rubik. Corpus: 16,915 live functions in 16,593
classes. (So 322 structural duplicates corpus-wide; the "72 of pygmy's
functions are byte-identical" above predates ifa/120 and is now 2.)

**The direct-call narrowing now requires it.** `get_target_fun_core`, the
C emitter's untagged-direct merge, and the LLVM emitter's default-arm
merge collapse candidates only when they are `clones_interchangeable` as
well as `identical_c_signature`. Measured first: `get_target_fun_core`
collapses NOTHING today anywhere in the corpus or the suite (after clone,
each direct call site already has one target), and no collapse is refused
under the new rule, so no output changes. It closes the hazard rather
than fixing an observed miscompile: a future collapse of clones that
differ in behaviour now falls to dispatch, or to ifa/102's refusal,
instead of binding silently.

**Open: the method-slot narrowing.** A dispatch calls through the
object's method slot, and `cg_build_new_to_val_map` fills each
(constructor, slot) with ONE clone -- the most specific self type, or the
more-derived override. When two clones of the same method compete for one
slot, the loser is never called, even at a dispatch site whose FA edge
chose it. `IFA_DBG_CLONEEQ` reports each competition: 50 in the corpus,
and two are between clones that are NOT interchangeable:

- **bh `Body.load_tree`** (slot 24): the clones have the same formal types,
  but one folded an `isinstance` to `true` (its arm dead) that the other
  evaluates. A fold is a fact about that contour's callers.
- **pygasus `_exec`** (slot 13, three competitions).

Both programs match CPython today, so either the folds hold for every
caller or the difference is not exercised. The general fix is the same
shape as the rest, and most of it is before codegen: FA knows, per
dispatch site and receiver CreationSet, which target EntrySet it chose
(`out_edge_map`, filtered by receiver). Build that table before codegen,
and have each dispatch arm call the clone the SITE chose for that class
directly after its tag test (the closure-carrier route already does
this). A slot is then needed only where one class at one site reaches two
clones that differ by creation point, and a slot competition between
non-interchangeable clones becomes a refusal, not a choice.

Corpus check -R `check__default__45e73015+6a18af72` against
`b908709e+a279682e`: every column identical on all 77.

## 2026-10-14: dispatch arms call the clone the site chose

`cg_build_dispatch_arm_plans` (`codegen_common.cc`, built after
`cg_build_new_to_val_map` and before either backend emits) decides each
class-tag arm of each dispatch. For every receiver CreationSet the site
reaches -- read from the candidates' in-edges at THIS call, filtered per
edge, matched to the clone's PNode by its shared `code` -- it compares
the clone the site chose with the clones the object's constructors
install in its method slot, using clone equivalence:

| plan | when | emitted |
| --- | --- | --- |
| `ARM_SLOT` | every object's slot holds the site's clone for it | the slot call, as before (ifa/186 can still make the chain one call) |
| `ARM_DIRECT` | the site has one clone for the tag, and some slot may hold another | a direct call to the site's clone after the tag test (the closure-carrier route, slot `-1`) |
| `ARM_AMBIGUOUS` | one object reaches two non-interchangeable clones, chosen by an argument | the slot call, with a warning; refused under `--strict` |

`apply_dispatch_arm_plans` rewrites the arms in both emitters.
`PYC_DISPATCH_ARM_PLANS=0` restores slot-only arms, for A/B.

**It was a real miscompile.** `tests/dispatch_slot_clone_per_site.py`:
`A.m` has an `int` clone and a `str` clone, one per call site, both sites
dispatch over `{A, B}`, and `A`'s constructor installs one clone in the
slot. With slot-only arms `o.m(21)` ran the `str` clone and printed a
pointer (`132719551492056`) on both backends; with the plans it prints
`42`, as CPython does.

Two details mattered. The class PROTOTYPE's CreationSet (made at class
definition, no constructor contour) never reaches a dispatch and must be
skipped, or every arm looks unproven. And the receiver set must come from
the edges, not the candidate EntrySet's own formal, which is the union
over all its callers: that made quameon's `coulomb_pot` look ambiguous
when each of its objects in fact gets the right clone through its slot.

**What it found.** bh `Body.load_tree` and pygasus `mmc*Write._exec`
(the slot competitions above) are `ARM_DIRECT` now: the site's clone is
called, and the C byte scan drops nothing (bh 191 emitted, 0 dropped). The
`ARM_AMBIGUOUS` sites are exactly ifa/190's one-class-two-clones calls:
fysphun `Link.applyme`, softrender `Vector4.add` (twice), `sub` and
`transform`. Both clones share one class tag, so neither a tag test nor
the receiver's slot can tell which one an argument needs. Refusing them
would refuse two corpus programs over a choice that is right at run time
(both match CPython), and keeping it silent is what this issue forbids, so
it is an accommodation: warned, and refused under `--strict`
(issues/171).

`make test` green (458 on each backend). Corpus check -R
`check__default__45e73015+faad267c` against `45e73015+6a18af72`: every
verdict identical on all 77; warnings fysphun 5 -> 6, softrender 7 -> 11
(the five sites above).
