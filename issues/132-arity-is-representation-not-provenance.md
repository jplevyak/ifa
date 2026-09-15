# 132 — Arity is representation, not provenance

**Status:** open. Guard landed (inert at the default); the real fix is
below.

**2026-09-14 — this issue is now the blocker for
[152](152-FA-backtrack-the-demand-to-the-merged-creation-set.md)'s default.**
A second shape of the same defect, measured on `quameon`: one member holding
BOTH an arity-recorded literal and an appended element-channel list has no
slot representation, so codegen emits `_CG_void e19; /* charges */` and reads
it as `_CG_any` at nine sites, resolving each index from the FA type instead.

```python
def __init__(self, npos=[], charges=None):
    if charges == None:
      self.charges = []                 # element-channel list
      for ...: self.charges.append(1.0)
    else:
      self.charges = charges            # [atom[1][0]] -- an ARITY-1 RECORD
```

`cs=2912` keeps `no_static_arity=0` right through that confluence. The needed
rule: **when an arity-recorded literal and a non-arity list meet at a member,
drop the static arity** — the member is the confluence, and arity is exactly
the representation property that cannot survive it. Present at the default as
well as under `PYC_CSBACKTRACK=1`, so this is latent today and only becomes
fatal when finer contours leave one of those nine reads without a single
dispatch target. Root-caused while investigating
[131](131-demand-driven-constant-splitting.md)'s falsified premise, in
service of [128](128-cs-identity-over-discriminates-vs-element-type.md)'s
start-merged posture (`PYC_CSDCPA1`).

## Symptom

Under `PYC_CSDCPA1=2`, `tests/empty_list_print.py`:

```python
b = [2, 3];  print(b)     # [2, 3]
k = [];      print(k)     # expected []   got [0, 0]
```

## Root cause

The two literals share one `list` CreationSet under the flag, and that CS
is emitted as a **record**:

```c
struct _CG_s11690 { _CG_int64 e0; _CG_int64 e1; };   // [2,3]'s shape
```

`len()` on it folds to the CreationSet's static field count:

```c
_CG_int64 _CG_f_3827_10/*len*/(_CG_ps11690 a1) { ...  return 2; }
```

so `__str__` builds `range(0, 2)` and walks two elements of a list that
has none. The `(_CG_ps11690)_CG_prim_tuple_list(int*, 0)` blind cast at
the `[]` site is **not** the defect — it is ifa/055's deliberate and
correct handling of `sizeof` on a zero-field record, which is an
incomplete type.

**The record-vs-list decision already implements the right rule, at the
wrong granularity.** `get_sym_tup` (`clone.cc:1268`) compares arity
across the CreationSets of an equivalence class:

```c
if (n < 0) n = cs->vars.n;
else if (n != cs->vars.n) tup = false;   // arities disagree -> list layout
```

Under a merged CreationSet the disagreement is *inside one CS*, where
`cs->vars.n` is a single number that cannot express it. `make_kind`
(`fa.cc`) fills it per creation point — `[2, 3]` fills 2, then `[]` calls
`fill(0)`, a no-op — so the CS silently keeps the larger arity and the
zero-arity creation point is misrepresented.

**Arity is therefore part of a record-able container's TYPE, not its
provenance.** A CreationSet's identity fixes its layout, and a 2-field
record cannot hold a 0-element value.

## The guard that landed

`make_kind` now sets `cs->no_static_arity = 1` when a creation point's
arity disagrees with the CS's — the same rule `get_sym_tup` applies
across a class, applied within a CS. That makes `tuple_able()` false and
clone gives list layout, exactly as the `prim_make` path already does for
dynamic-length containers.

It needs a second half. ifa/104 leaves the generic element **bottom** on
purpose, because that is what `tuple_able()` tests and what keeps a
record-able container's per-index types precise — but list layout *reads
through* that element, so leaving it bottom types every element
`_CG_void_type` and codegen fails outright. So when the arity check
fires, `make_kind` also flows each per-index var into the generic
element, as the `prim_make` path seeds its containers from the source
element.

**Measured.** `empty_list_print` now prints `[]` under `PYC_CSDCPA1=1`,
`=2` **and** the default. `./test_pyc.py` under `PYC_CSDCPA1=2`:

| | EXEC (wrong answers) | COMPILE-OUT | COMPILE | total |
| --- | --- | --- | --- | --- |
| before | **14** | 9 | 7 | 30 |
| after | **2** | 6 | 47 | 55 |

The total rose and the **kind** improved: silent wrong answers went 14 →
2, and what is lost is now a diagnosed compile refusal rather than a
program that runs and prints garbage. That is the trade this repo's
[102](102-corpus-programs-compile-then-abort-at-runtime.md) exists
to name.

The guard is **inert at the default** — `IFA_DBG_ARITY` counts zero
firings on `empty_list_print`, `list_element_type_union`,
`tuple_arity_union` and `nested_tuple_repr` — so the default's 311/0 is
unchanged by construction rather than by luck. All six gates green.

## The real fix — DONE 2026-09-05

**Arity now participates in CreationSet identity.** `CreationSet` gains
`static_arity` (`fa.h`), the arity its creation points agree on, `-1`
before any has been seen — `vars.n` cannot serve, because a zero-arity
creation point calls `vars.fill(0)`, a no-op, so an empty container is
indistinguishable from an undecided one. `creation_point` takes the
arity and refuses a contour whose creation points agree on a different
one:

```c
if (arity >= 0 && x->static_arity >= 0 && x->static_arity != arity && !x->no_static_arity) continue;
```

A CreationSet that has already lost its static arity absorbs any, because
it is on list layout and reads its length at runtime — the unknown-arity
case, and the only one where merging arities is representable. The
`no_static_arity` guard above stays, demoted to a **safety net** for a
merge arriving by some other route (the `cs_map` memo, split-parent
inheritance, the mold).

`./test_pyc.py`, 311 tests:

| | `PYC_CSDCPA1=2` | `PYC_CSDCPA1=1` |
| --- | --- | --- |
| before the guard | 30 (14 EXEC) | 49 |
| guard only | 55 (2 EXEC) | 64 |
| **arity in identity** | **16** (8 COMPILE / 6 COMPILE-OUT / **2 EXEC**) | **19** |

`=1` — tuples included — is now within 3 of `=2`, so **the tuple
exception is very nearly unnecessary**, which is what "arity in identity,
not a per-class exception" predicts.

Corpus `check`, `check__PYC_CSDCPA1_2__5e012d78+8450a439`:

| | default | dcpa1, guard only | dcpa1 + arity |
| --- | --- | --- | --- |
| container CS / shapes | 3748 / 626 = 5.99 | 2540 / 570 = 4.46 | 2716 / 595 = **4.56** |
| compile_fail | 2 | 20 | 19 |
| `pyc` segfaults (`rc=139`) | 0 | **2** | **0** |
| compile timeouts (`rc=124`) | 1 | **3** | **0** |

Container CreationSets rise slightly against the guard-only arm (2540 →
2716) because arity now correctly *separates* what that arm was unsoundly
merging — and still sit **27.5% below the default**.

The headline is the bottom two rows: **every compiler crash and hang is
gone.** `plcfrs` 139→1, `softrender` 139→0, `pygasus` 124→0, `quameon`
124→1, `othello3` 124→1; `chess`, `life`, `sudoku2` and `solitaire` now
compile. Those were latent `pyc` bugs that merging different arities into
one record layout had been feeding.

All six gates green, and the default is untouched: `make test` rc=0,
`PYC_FLAGS=-b ./test_pyc.py` 311/0. Note `fa.h` changed, so this needs
`make clean` (CLAUDE.md).

## The cross-CreationSet case — census, and one bug fixed (2026-09-14)

The landed fix puts arity in CreationSet identity, which is right and keeps
different arities apart. What it does not cover is the case where **one AVar
must hold two CreationSets of one sym whose arities disagree.** Identity
correctly keeps them apart; `determine_basic_clones` then splits them into
different layout equivalence classes on `cs1->vars.n != cs2->vars.n`
(clone.cc), so one is a RECORD and the other a LIST — and the slot holding
both has no type at all. `c_type()` names it `_CG_void`, every read of it is
`(_CG_any)`, and each access is resolved from the FA type instead of the C
type.

`get_sym_tup`'s `tup = false` path is designed for exactly this disagreement,
but it only sees WITHIN one class, and the `vars.n` split happens first.

### Census — `IFA_DBG_SLOTREP`, 84 corpus programs

**976 conflicts in 13 programs. Every one of the 13 COMPILES and then fails
or prints the wrong answer.**

| program | conflicts | outcome (default sweep) |
| --- | --- | --- |
| `amaze` | 393 | `run 134` "getter not resolved" |
| `pylife` | 330 | `run 124` (CPython finishes) |
| `chess` | 88 | `run 124` (CPython also 124) |
| `chaos` | 45 | runs; CPython timed out, undetermined |
| `neural2` | 36 | **wrong stdout** |
| `quameon` | 26 | **wrong stdout** |
| `rubik2` | 21 | `run 124` (CPython also 124) |
| `sha` | 18 | **wrong stdout** |
| `ant` | 7 | **wrong stdout** |
| `pygasus` | 4 | `run 134` |
| `dijkstra2` | 3 | `run 124` (CPython finishes) |
| `loop` | 3 | `run 124` (CPython finishes) |
| `linalg` | 2 | `run 134` "list element type mismatch" |

The other **71 programs have zero conflicts**, so this is not a proxy for
program size. And three of the aborts name the untyped value directly, which
is causation rather than correlation:

```
amaze    Assertion `!"runtime error: getter not resolved"'
linalg   _CG_f_2935_307(_CG_any, _CG_int64): `!"runtime error: list element type mismatch"'
quameon  _CG_f_15731_653(_CG_ps26965, _CG_any): `!"runtime error: matching function not found"'
```

Nine of the thirteen emit **zero warnings**. An unresolved dispatch reaching
codegen as an `assert(!"runtime error: ...")` with nothing said about it is
[149](149-the-largest-diagnostic-class-reports-nothing.md)'s family.

### A real bug, found and FIXED

`make_kind`'s element seeding was

```c
if (cs->no_static_arity)
  if (AVar *gelem = get_element_avar(cs)) flow_vars(iv, gelem);
```

`iv` is CS-contoured and so is `gelem`, so this is a **raw CS → CS flow
edge** — which `compute_setters` asserts against
(`assert(x->contour_is_entry_set)` over an AVar's backward list), and which
`vector_elems` builds a trampoline by hand to avoid. Latent until something
walked that element's setters; `PYC_SLOTARITY` below sets `no_static_arity`
on far more CreationSets and aborted `quameon` inside `compute_setters`.

Fixed by seeding from `atv` instead of `iv` — the same value (the chain is
`av -> atv -> iv`), but `atv` is the ES-contoured temp the loop already
makes and already calls `set_container` on. All six gates green.

### `PYC_SLOTARITY=1` — the demotion, and why it does not pay yet

Default 0. Each pass, group every conflicting pair transitively and demote a
group to list layout only if it is **jointly homogeneous**. Three conditions,
each of which cost a measurement to find:

1. **Settled slots.** `basic_type` maps an EMPTY AType and a non-basic one to
   the same `nullptr`, so an undecided record reads as homogeneous. At pass 0
   on `quameon` that demoted 45 CreationSets, 40 of them `tuple`.
2. **Homogeneous.** A list has ONE element type; demoting a heterogeneous
   record collapses its fields into an element union — the hazard ifa/104's
   comment in `make_kind` already records. 24 errors of
   `expression has mixed basic types: ( int64 str )`.
3. **Jointly, over the whole group.** Per-CreationSet is not enough: sixteen
   2-tuples whose slots each agreed internally — `(str, str)` here,
   `(int64, int64)` there — still union to `{int64, str}` once they share one
   element channel. Same 24 errors.

With all three, `quameon` compiles clean and runs; **but no program's verdict
changes.** The honest status: this mechanism is correct and conservative, it
found the CS → CS bug above, and it does not yet pay.

### Why — and it sequences behind [152](152-FA-backtrack-the-demand-to-the-merged-creation-set.md)

`amaze`'s 393 conflicts are all one CreationSet: `tuple#1611`, **arity 0**,
held in `MazeSolver._current` alongside a dozen 2-tuples. The source never
writes an empty tuple — `self._current = (0,0)` — so `tuple#1611` is a
MERGED arity-0 contour, and because it conflicts with everything, the
transitive group chains in a 4-tuple and an 8-tuple too. The group is then
genuinely not homogeneous and is correctly skipped.

`_current` alone only ever holds `()`-shaped and `(int, int)` values, which
IS homogeneous. **So the demotion is blocked until that merged arity-0
contour is SPLIT** — the same shape as `chull`'s `cs=1112`, one step further
out: `tuple#1611` is representable on its own, raises no demand, and
152's nomination therefore does not reach it either.

The order is fixed by this: split the merged arity-0 contour first, then
demote per group. A candidate for 152's next rung is a CreationSet that some
slot cannot REPRESENT alongside its neighbours — an arity disagreement is
exactly such a demand, and it is one 152 does not currently take.

## What is still wrong

The residue is a **precision** problem, not a
correctness one: forcing list layout on every merged different-arity
container throws away per-index types that those programs need. Making
the numbers work by tuning the guard would be the retreat — the guard is
a soundness invariant (you cannot represent 0 elements in a 2-field
record) and it is doing its job.

What remains is 16 suite failures and 19 corpus compile failures under
the flag, none of them crashes: 8 COMPILE / 6 COMPILE-OUT / 2 EXEC. The
two EXEC failures are the only remaining wrong answers, down from 14.

*Verify:* `PYC_CSDCPA1=1` needs no tuple exception; the 47 compile
failures fall; corpus `check` under the flag improves on
`check__PYC_CSDCPA1_2__3c388f22+adf4abe8` (20 compile failures);
`ess`/`css` do not grow at the default; and a genuinely dynamic list
(built by `append` in a loop) still gets ONE contour with list layout,
asserted by a new test.
