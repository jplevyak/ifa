# 134 — Remove the frontend's forced-split opt-in

**Status:** open, measured. The gating mechanism behind
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

The whole program-wide list of annotated sites is four: `range.__init__`
(both overloads), `isinstance`, `issubclass`, and one in
`__pyc__/00_runtime.py`. **A user class with exactly the same shape gets
none of it.**

## Why it should go

It is structure-driven splitting with a human in the loop, which is the
defect the goal statement names — the frontend is asserting a distinction
that the analysis is supposed to *discover*. It is also hand-maintained,
unavailable to user code, and the mechanism this issue tracker keeps
running into from other directions:

| mechanism | gated on today | wants |
| --- | --- | --- |
| per-constant contours | `__pyc_clone_constants__` | [131](131-demand-driven-constant-splitting.md) |
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
