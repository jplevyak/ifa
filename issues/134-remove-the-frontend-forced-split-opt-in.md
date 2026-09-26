# 134 — Remove the frontend's forced-split opt-in

*Sequenced as [129](129-plan-demand-driven-creation-set-splitting.md) step 4
(with [151](151-split-an-entryset-on-a-constant-argument-on-demand.md), which
is the mechanism that lets this close). 129 is the single integrated plan.*

**Status: 53 of 62 annotated lines removed 2026-09-26; 9 remain, each
with a named reason** -- see "DONE 2026-09-26" below. Earlier progress,
same day: with
[151](151-split-an-entryset-on-a-constant-argument-on-demand.md)'s demand
stage built, the 30 annotated lines in `int`'s arithmetic, bitwise, in-place
and formatting methods (`02_numeric.py`, 39 -> 9) are gone; 32 annotated
lines remain program-wide (62 before), all on fold consumers --
comparisons, truthiness, `range`, `isinstance`/`issubclass`, container
keys. Corpus neutral to better; see 151's "LANDED" section. The gating mechanism behind
[131](131-demand-driven-constant-splitting.md),
[133](133-split-a-container-on-its-element-type.md) and closed issue
[045](closed/045-receiver-cs-method-cloning.md), and the clearest
remaining case of splitting driven by something other than demand.

## What it is

**The primary purpose of IFA is the demand splitting of Creation Sets.** A
contour exists because something *observed* a distinction. `pyc` has one
place where the frontend instead **asserts** a distinction and FA obeys:

```python
class range:
  def __init__(self, ai, aj, ak = 1):
    self.i = __pyc_clone_constants__(ai)      # __pyc__/05_builtins.py
```

`__pyc_clone_constants__` has a single origin —
`python_ifa_build_if1.cc:1113`, `ast->rval->clone_for_constants = 1` — and
from there:

1. `Sym::clone_for_constants` is set on the formal, and inherited by
   wrapper formals (`python_ifa_sym.cc:250, 330`).
2. Any `clone_for_constants` ctor param marks the **class**, the `__new__`
   wrapper and `__init__` with `clone_methods_per_cs`
   (`python_ifa_build_syms.cc:2854-2858`).
3. Those two flags gate **20 sites in `fa.cc`**, including the hard
   edge incompatibility in `entry_set_compatibility` (`fa.cc:1672`), the
   unstripped comparison in `collect_type_confluence` (`fa.cc:5350`),
   the split-parent CS reuse exclusion, the `csmold` exclusion, and the
   `PER_CS_RECEIVER` method-cloning stage (issue 045).

**Corrected 2026-09-12:** this used to say "the whole program-wide list of
annotated sites is four". It is **93 call sites across 8 files** —
`02_numeric.py` 39, `04_sequence.py` 8, `05_builtins.py` 6, `07_dict.py` 3,
`01_str.py` 2, `00_runtime.py` 2, `06_bytearray.py` 1, `08_set.py` 1 — so
the annotation is not a handful of special cases but the mechanism most of
the builtin library's precision rests on. `PYC_NO_FORCED_SPLIT=1` makes the
single line `print(min(2, 9))`, clean at the default, emit 10 warnings.

**A user class with exactly the same shape gets none of it.** The
demand-driven replacement for the EntrySet half is
[151](151-split-an-entryset-on-a-constant-argument-on-demand.md); closing
this issue means closing that one first.

## Why it should go

It is structure-driven splitting with a human in the loop, which is the
defect the goal statement names — the frontend is asserting a distinction
that the analysis is supposed to *discover*. It is also hand-maintained,
unavailable to user code, and the mechanism this issue tracker keeps
running into from other directions:

| mechanism | gated on today | wants |
| --- | --- | --- |
| per-constant contours, CreationSet side | `__pyc_clone_constants__` | [131](131-demand-driven-constant-splitting.md) |
| per-constant contours, EntrySet side | `__pyc_clone_constants__` | [151](151-split-an-entryset-on-a-constant-argument-on-demand.md) |
| method contours per receiver CS | `clone_methods_per_cs` | issue 045, demand-driven |
| element-type separation | per-site CS identity, incidentally | [133](133-split-a-container-on-its-element-type.md) |

**The mechanisms are not the problem — the gating is.** Per-constant
contours and per-receiver-CS method contours are both legitimate and both
earn their keep. Nothing here proposes deleting them.

## What it buys, measured

`PYC_NO_FORCED_SPLIT=1` (added with this issue, at the single origin
above) turns the entire opt-in off. `./test_pyc.py`, 311 tests:

| | failed |
| --- | --- |
| default | **0** |
| `PYC_NO_FORCED_SPLIT=1` | **69** |
| `PYC_CSDCPA1=2` | 16 |
| `PYC_CSDCPA1=2 PYC_NO_FORCED_SPLIT=1` | **76** |

Two findings, and the second is the one that matters for sequencing.

**It is heavily load-bearing — but almost entirely for PRECISION, not
correctness.** Of the 69, **60 are COMPILE-OUT**, 8 are COMPILE, and
exactly **1 is EXEC**. So what the annotation buys is overwhelmingly
diagnostic quality: without per-constant contours, `range(0, 0)`'s loop
header stops folding and its dead body gets type-checked, which is issue
040's original trace and shows up as warnings rather than wrong answers.

**Start-merged does NOT reduce the dependence.** `PYC_CSDCPA1=2` goes 16 →
76 when the opt-in is removed, a bigger absolute jump than the default's
0 → 69. So this cannot be waited out: [128](128-cs-identity-over-discriminates-vs-element-type.md)'s
architecture does not subsume it, and the two are independent pieces of
work.

## DONE 2026-09-26 -- 53 of 62 lines gone, and why each of the 9 stays

Built on [151](151-split-an-entryset-on-a-constant-argument-on-demand.md)'s
CONST_DEMAND stage. Removed: every annotation on `int`'s comparisons and
`__bool__`/`__pyc_to_bool__`/`__not__`, `str.__str__`/`__repr__`,
`isinstance`/`issubclass`, `bytearray`, `list.__getitem__`/`__setitem__`
keys, `list.__add__`'s lengths, `tuple.__setitem__`/`__len__`, and the
constructor fields of `dict` and `set`.

**What the demand stage needed to carry them:**

| gap | found by | fix |
| --- | --- | --- |
| a SELECTING operand is not on the data path: `index_object(self, key)` returns a field, never `key` | `tuple_list_mix` (`t[0]`/`t[1]` on `(int, str)`) | follow `index_object`'s key from its result |
| no violation at all: a heterogeneous tuple indexed by a merged key compiled clean and died at run time ("list element type mismatch") | `nested_tuple_repr`, `tuple_unpack_target_arity_union`, `match_map_star` | post-convergence check: a `tuple`/`list` record whose fields mix a NUMERIC scalar with anything else, indexed by a key that is not one constant, is a `PRIMITIVE_ARGUMENT` violation, which the demand stage answers. A genuinely runtime key is now a compile error instead of a crash |

The check was narrowed twice, each time on a measurement: to sequences (the
frontend lowers closure and instance access through `index_object` too),
then from `atype_irrepresentable` to "numeric scalar mixed with anything",
because `[[1, 2], "hello"]` iterated by `for` is two pointers and works
(`isinstance_dynamic`).

**Two walk extensions were built, measured, and removed.** Following EVERY
primitive operand cost `rubik` 15.6 s -> 34 s in extra nominations; only
`index_object`'s key is followed. Seeding the conditions each visited
definition is control-dependent on (so the walk sees a value CHOSEN by a
branch, `bool.__not__`'s `if self: return False`) fixed `minmax_3arg` but
took `rubik` past 150 s and `ac_encode` from 8 s to the 400 s cap. It was
needed only to remove `bool`'s lines, which stay for the reason below.

**The pyc suite is not enough evidence for this change.** With 56 lines
removed it was green, and the corpus lost `hq2x` and `sudoku2` (both
matched CPython), four programs to the compile cap, and 48% compile time.

**The 9 that stay:**

| line | without it | why |
| --- | --- | --- |
| `bool.__not__`, `bool.__pyc_to_bool__` (`00_runtime.py`) | `hq2x` 38 s -> over 150 s, corpus-wide slowdown | Every `if` goes through `__pyc_to_bool__`; one shared contour makes its `self` `{True, False}`, so every condition folded upstream stops folding downstream. What is lost is a FOLD, not a demand -- dead code that types fine raises no violation -- so no demand can ask for it back. This is the stop condition in "The work" below, met. The cost is bounded: `bool` has two values |
| `range.__init__` x4 (`05_builtins.py`) | `empty_list_print`, `match_seq_star`, `builtin_type_factory` | the constant is a CreationSet FIELD (`range.j`): `range(0)` and `range(2)` build into one `range` CreationSet. Separating them splits a CreationSet by content -- [131](131-demand-driven-constant-splitting.md)'s side, with no demand mechanism yet |
| `__list_iter__.__init__` (`04_sequence.py:21`) | `sudoku2` (33 errors; matches CPython with it) | not constants at all: the annotation makes the iterator class `clone_methods_per_cs`, so each list gets its own iterator contours. Without it iterators over different lists share an element channel. A per-receiver-CreationSet demand ([133](133-split-a-container-on-its-element-type.md)) is what should replace it |
| `int.__ne__` (`02_numeric.py:94`) | `itertools_module` aborts at run time | `product`'s `if repeat != 1:` merged over 1 / 3 makes `p` a `{list, tuple}` union with no runtime tag; codegen emits "matching function not found" and NOTHING reports it, so there is no demand. The fix is to make an untagged container union a violation |
| `tuple.__getitem__` key (`04_sequence.py:454`) | `sudoku3`, `plcfrs` (corpus); was also `test_heapq` | **The narrowing half is FIXED (2026-09-26, see below)**; what still needs the annotation is that CONST_DEMAND cannot yet reach every merged key. See "tuple.__getitem__: the narrowing fix" |

### tuple.__getitem__: the narrowing fix, and what the annotation still buys

**Root cause of `test_heapq`'s wrong output (fixed).** With the key merged,
the `{int, str}` result of `item[1]` is a violation, and stage 5
(`split_edges`) answered it by fanning `__getitem__` across its receiver's
CreationSets -- three `(prio, name)` literals. Those three are identical BY
TYPE (one class, one arity, every slot the same constant-stripped type); they
differ only in the literals their slots hold. So the fan separated nothing a
type names and nothing the runtime can tell apart: they share one C layout
and carry no tag. Each contour folded slot 1 to its own string, and the call
site -- which codegen can bind to one clone only (issue
[121](121-CGEN-dead-clones-emitted.md)'s narrowing) -- printed "medium" for
every item. `split_edges` now DECLINES when every receiver CreationSet is
identical by type (`cs_slot_sig_equal`, constants stripped). It only
declines; it never splits on the signature, so ifa/157's non-terminating
signature key is not reintroduced. With the annotation removed, `test_heapq`
passes on the decline alone.

With the annotation KEPT the decline is corpus-neutral:
`sweeps/check__default__5c7bdd94+a790d1bb.tsv` against the 134 commit's
sweep -- contours identical on every program but `fysphun` (218 -> 208),
verdicts identical (`tonyjpegdecoder` sits on the 120 s run cap).

**Why the annotation still stays.** Removing it also needs CONST_DEMAND to
reach every merged key, and three reach gaps were found and fixed
experimentally, then NOT landed:

| gap | case | fix tried |
| --- | --- | --- |
| a merged value reaches the violation only through container storage (tuple fields, list elements) | `tuple_arity_union_slice` | pass through CreationSet-contoured AVars to their writers |
| ... or through a module-level variable (GLOBAL_CONTOUR), including an actual a formal climbs to | same | pass through globals; hand a global actual to the walk |
| `index_object`'s RECEIVER, not just its key, carries the merge | same | follow both operands of `index_object` |
| TYPE_CONFLUENCE never goes quiet, so the last rung never runs | `pygmy` (stall guard at p35) | on the stall guard's stop, give CONST_DEMAND one quiet pass (rescue terminates: each needs a new bit) |

With all four and the annotation removed: `test_heapq`,
`tuple_arity_union_slice` and `pygmy` (image identical to CPython) pass --
but the corpus loses `sudoku3` and `plcfrs` (whose keys are merged along
paths the walk still misses; `sudoku3` nominates at p30 and keeps `key` =
`{int64, str}`), and compile time rises 21%. With the annotation KEPT the
four cost +5.5% contours in programs that already fail to compile and buy
nothing. So they are recorded here, not landed; the next attempt starts from
them and from `sudoku3`.

`PYC_NO_FORCED_SPLIT=1` is therefore not a no-op; it fails the tests above
and slows the corpus.

**Corpus `-m check`** (`sweeps/check__default__233b9ee4+e9dc9c65.tsv`):

| | pre-151 (`63888fd6+7e402769`) | 151 alone (`e70bcffa+eb5931f5`) | now |
| --- | --- | --- | --- |
| contours, all 77 | 32503 | 29983 | **27685 (-14.8%)** |
| compile s, all 77 | 1734 | 1650 | **1669 (-3.7%)** |
| contours / compile s, 44 compiling in both | 16088 / 528 | -- | **13924 / 496** (-13.5% / -6.1%) |
| stdout matching CPython | 14 | 14 | 14 |

Verdict moves against 151 alone: `chull` run-crash -> compile error, `life`
compile error -> compiles and aborts, `quameon` back to a compile error,
`tonyjpegdecoder` now finishes inside the run cap. `chaos` reads as a run
timeout; its binary takes 112 s alone under both trees against a 120 s cap,
so that is the cap, not this change.

## The work

1. **Classify the 60 COMPILE-OUT failures.** They are the bill, and the
   claim "it is only precision" needs to survive reading them — a spurious
   warning on a dead branch and a genuinely lost fold look the same in
   that column.
2. **For each thing the annotation buys, name the demand signal that
   should produce it.** For the `range` case the signal is visible:
   `range(0, 0)` and `range(0, 2)` differ in a constant that a loop header
   folds on, which is [131](131-demand-driven-constant-splitting.md)'s
   question. For `isinstance`/`issubclass` the constant *is* the answer,
   so the signal is different again.
3. **Then delete the annotations**, keeping the FA mechanisms and driving
   them from the signals. `PYC_NO_FORCED_SPLIT=1` becomes the regression
   test for having finished: it should eventually be a no-op.

*Stop condition:* if a case turns out to need the annotation because no
demand signal can exist for it — the frontend genuinely knows something
the analysis cannot observe — that is a real answer worth recording.
Record which case and why. Do **not** keep the annotation for cases where
the signal exists but is merely unimplemented; note those as blocked on
131/133 instead.

*Verify:* `PYC_NO_FORCED_SPLIT=1` reaches 0 failures on both backends;
corpus `check` neutral; `ess`/`css` do not grow (these splits are supposed
to be *replaced* by demand-driven ones, not added to); and
`tests/empty_list_print.py`, issue 040's original case, still prints `[]`.
