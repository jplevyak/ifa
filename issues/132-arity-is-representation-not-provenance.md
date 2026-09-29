# 132 — Arity is representation, not provenance

**Status: open on the cross-CreationSet residual.** The principle landed
2026-09-05. Rewritten 2026-09-28; the full record is in git:
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
matches CPython, so its two conflicts are gone or harmless.

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

## Next

1. Re-take `IFA_DBG_SLOTREP` at HEAD.
2. Implement the member-confluence rule for quameon's shape, and measure
   quameon (run_rc=134 at `4b61e721`, `matching function not found` at an
   `(_CG_ps…, _CG_list)` signature).
3. amaze: shedskin's output first, then the liveness question.

**Stop condition:** a demotion that makes a heterogeneous group's element
irrepresentable (`mixed basic types`) is wrong. Refuse it. Do not widen
the union.

## Verification

Six gates; `IFA_DBG_SLOTREP` conflicts fall; `quameon` runs; a
genuinely dynamic list built by `append` in a loop still gets ONE contour
with list layout.
