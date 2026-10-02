# 132 — Arity is representation, not provenance

**Status: open on the residual representation conflicts; quameon resolved
2026-09-30.** The principle landed 2026-09-05. Rewritten 2026-09-28; the
full record is in git:
`git show 3f36072b:ifa/issues/132-arity-is-representation-not-provenance.md`.

## The principle (landed)

Two list literals of different length cannot share one RECORD layout. `[]`
merged into `[2, 3]`'s `struct { e0; e1; }` printed `[0, 0]`, because
`len()` folded to the CS's static field count. Arity is not provenance.
It is what the target can REPRESENT, AGENTS.md's legitimate third
category. So it is part of CreationSet identity:
`CreationSet::static_arity` (−1 = undecided), and `creation_point` refuses
a contour whose creation points agree on a different arity. A CS that has
already lost its static arity (`no_static_arity`: list layout, runtime
length) absorbs any arity. Result at the time: every `pyc` crash and hang
under the flag disappeared (plcfrs, softrender, pygasus, quameon,
othello3), and container CSs stayed 27.5% below the old default.

## What is open: two CreationSets of one sym, different arity, one slot

Identity correctly keeps `(1,2)` and `(1,2,3)` apart. But when ONE AVar
must hold both, `determine_basic_clones` puts them in different layout
classes on `cs1->vars.n != cs2->vars.n` (clone.cc) BEFORE `get_sym_tup`'s
`tup = false` path can see the disagreement. One stays a record and the
other becomes a list, and the slot holding both has no C type: emitted
`_CG_void`, read as `_CG_any`, with every access resolved from the FA type.

**Census (`IFA_DBG_SLOTREP`, 2026-09-14): 976 conflicts in 13 programs,
and every one of the 13 compiles and then fails or prints the wrong
answer.** Nine of them emit zero warnings. amaze, linalg and quameon abort
naming the untyped value. Re-take this census before starting: linalg now
matches CPython, so its two conflicts are gone or harmless, and so does
amaze (2026-10-02, below) with its conflicts still present.

### The known shapes

- **quameon's member.** `self.charges` is `[]` filled by `append`
  (element channel) on one path and `charges`, an arity-1 literal record,
  on the other. `cs=2912` keeps `no_static_arity=0` through the
  confluence. The needed rule: **when an arity-recorded literal and a
  non-arity list meet at a member, the static arity is dropped.** The
  member is the confluence, and arity is exactly the representation
  property that cannot survive it.
- **amaze's placeholder.** `points2 = [()]*len(points)` (upstream
  shedskin's `# SS` line), then every slot is overwritten with a 2-tuple
  before any read. The union `{(), (int, int)}` is REAL. One creation
  point, nothing merged, so no split applies. Widening `()` to a zero
  2-tuple is only sound if the arity-0 values are provably never read
  (`len(())` is 0 and `() == (0, 0)` is False in CPython). That is a
  liveness question. First check what shedskin emits for `points2`.

  **2026-10-02: amaze's failure was never this conflict, it was a missing
  builtin.** `list.index` took no `start`, so `distances2.index(dist,
  idx+1)` (amaze.py:322) resolved to nothing and left `idx` untyped: the
  untyped value amaze aborted on, and a compile error once every
  violation became fatal (ifa/158). With `list.index(x, start, stop)`
  added, amaze compiles warning-free and matches CPython on both
  backends (sweep `check__default__92ac5912+65417f57`). The conflict is
  still there: `IFA_DBG_SLOTREP` prints 393 lines, e.g.
  `MazeSolver._current` holding `tuple#1738(arity=2)` + `#1767(arity=0)`.
  It is LATENT: the arity-0 values are overwritten before any read, which
  is the liveness argument above, holding dynamically. amaze is
  therefore no longer a witness for this issue. A program that READS a
  slot across the arity conflict is still needed, and quameon is the
  one left.

### Mechanism in the tree: `PYC_SLOTARITY=1` (default 0)

Each pass, it groups conflicting CSs transitively and demotes a group to
list layout only if the group is **jointly homogeneous** over **settled**
slots. Each of those conditions cost a measurement to find: an empty AType
and a non-basic one both map to `nullptr` in `basic_type`, and
per-CS homogeneity is not enough once 16 differently-typed 2-tuples share
an element. It is correct and conservative: `quameon` compiles clean and
runs. But no verdict changes, and it found a real bug on the way (a raw
CS → CS flow edge in `make_kind`'s element seeding, fixed). It is pending
here: land it once a program's verdict depends on it, or drop it.

## Resolved 2026-09-30: quameon was an untagged dispatch ambiguity, not arity

**Census at HEAD (`IFA_DBG_SLOTREP`, `5b7270b7`): 1900 conflicts in 13
programs**: ant 7, chaos 35, chess 96, dijkstra2 2, neural2 15, plcfrs 316,
pylife 550, quameon 28, rubik2 21, sat 18, sha 18, sieve 2, sudoku3 792.
Almost all are an arity-0 list (`[]`) and an arity-N literal meeting in one
slot. **A conflict is not a failure.** `sat`, `sha` and `neural2` match
CPython, and small programs of the same shape run correctly on both
backends. The 2026-09-14 claim that every one of the 13 fails no longer
holds.

**quameon's actual mechanism.** One contour of
`coulomb_pot.compute_en_value` received BOTH `coulomb_pot` CreationSets as
`self` (`IFA_DBG_FUNES`: es=701, `self = {coulomb_pot#1960, #3451}`). One
object's `charges` is the literal `[atom[1][0]]` (an int list) and the
other's is `[]` filled by `append(1.0)` (a float list). So
`self.charges[j]` had two `list.__getitem__` candidates, returning int64
and float64 (`PYC_DBG_DISPATCH`: `IDENT ... ret[0] type _CG_int64 vs
_CG_float64`), and a `list` carries no runtime tag. Codegen emitted
`matching function not found`.

**Why the planned member-confluence rule was not the fix.** Demoting the
literal to list layout (`PYC_SLOTARITY`, after fixing two gaps that blocked
it: an element-filled `[]` read as "non-basic", and `{int64, float64}` read
as mixed) leaves the two lists with different ELEMENT types behind one C
type. Making that representable needs widening the int list to float. That
ran, but printed `nuclear charges = [2.0]` for CPython's `[2]`, i.e.
shedskin's answer, not CPython's. Dropped.

**The fix (demand splitting, no boxing):**

1. **An untagged dispatch ambiguity is a demand** (`split_css_by_defs`,
   `IFA_DBG_UNTAGGED`). A send whose receiver holds two CreationSets of one
   untagged sym is dispatched to two contours of one function that return
   different types. The receiver for a bound-method call is the closure's
   bound value. The demand names the confluence. That is a CreationSet whose
   member or element holds both receivers, found directly, because a
   bound-method receiver leaves no `backward` link to its member. When no
   CreationSet holds both, it is the calling contour's formal, holding
   several CreationSets of one record class; that formal is split per
   receiver CreationSet (`split_edges`, PER_CS_RECEIVER's primitive), so the
   partition is bounded by the receivers. Asked only when no higher stage
   acted this pass.
2. **Codegen no longer collapses clones whose RECORD arguments' members
   hold different contents** (`get_target_fun_core`). After the split the
   two `compute_en_value` clones had identical C signatures, because both
   records share one struct and `charges` is `_CG_list` either way. The
   collapse called one clone for both receivers and read the int list as
   float64. Refused, the call goes through the instance's method slot,
   which each instance stores per creation point.

**Measured.** quameon runs and matches CPython on the C backend (run_rc 134
-> 0). Corpus `check` against the previous sweep: quameon is the ONLY
program whose contours moved (ess 925 -> 928, css 2594 -> 2617), and no
other verdict changed. `make test`: 378 passed, 0 failed.

**Open, filed separately:**
- [178](178-FA-route4-declines-records-built-through-one-constructor.md):
  a synthetic test of this fix needs two separate records of a class, and
  in small programs both creation points merge through the constructor.
  Route 4 declines them ("every creation point on the same assign sets").
  quameon is the witness until then.
- [179](179-LLVM-quameon-computes-nan.md): on LLVM quameon computes `nan`,
  independently of this fix.

## Next

1. 178, then a synthetic test of the untagged-dispatch demand.
2. `PYC_SLOTARITY` (default 0): no verdict depends on it. Drop it, or land
   the two gap fixes recorded above if a program ever needs it.
3. amaze: done. It runs and matches CPython with the conflict latent
   (see its shape above). The liveness question matters only if a
   program reads across the conflict.

## Verification

Six gates; `IFA_DBG_SLOTREP` conflicts fall; `quameon` runs; a
genuinely dynamic list built by `append` in a loop still gets ONE contour
with list layout.
