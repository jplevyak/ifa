# 134 — Remove the frontend's forced-split opt-in (`__pyc_clone_constants__`)

**Status: open. 53 of 62 annotated lines removed 2026-09-26; 9 remain, each
with a named reason.** Rewritten 2026-09-28. It now also carries the
residuals of [131](closed/131-demand-driven-constant-splitting.md) (the
CreationSet-field constant) and [151](closed/151-split-an-entryset-on-a-constant-argument-on-demand.md)
(the CONST_DEMAND stage, landed). The full record is in git:
`git show 3f36072b:ifa/issues/134-remove-the-frontend-forced-split-opt-in.md`.

## What it is

The one place the frontend ASSERTS a distinction and FA obeys:

```python
class range:
  def __init__(self, ai, aj, ak = 1):
    self.i = __pyc_clone_constants__(ai)      # __pyc__/05_builtins.py
```

That sets `Sym::clone_for_constants` on the formal, and a ctor param
marks the class, `__new__` and `__init__` `clone_methods_per_cs`. Those two
flags gate ~20 sites in `fa.cc`: a hard edge incompatibility in
`entry_set_compatibility`, an unstripped comparison in
`collect_type_confluence`, the split-parent and mold exclusions, and the
`PER_CS_RECEIVER` stage. That is splitting driven by an annotation rather
than by demand.

## What replaced most of it: CONST_DEMAND (151, landed)

`EntrySet::const_positions` is set only by the CONST_DEMAND stage,
inherited through the durable `split_origin` lineage, and consulted
wherever `clone_for_constants` used to be. The demand: from each violation,
walk back through the offending value, and through the conditions the
violating statement is control-dependent on, into callees via call results
and into callers via formals. Nominate a formal whose in-edges disagree on
a constant. It runs as the LAST rung. `index_object`'s key is followed as
a selecting operand, and a heterogeneous tuple or list indexed by a
non-constant key is now a violation, so the demand has something to
answer. Result: `int`'s arithmetic, comparisons, `bool`, `isinstance`,
list keys and dict/set ctor fields went unannotated, corpus contours
−14.8%, compile time −3.7%, stdout matches unchanged.

**Principle found on the way:** a FOLD is not a demand. Generated code must
not make a fold depend on a callee split. `sudoku5`'s tuple compare guards
relied on `int.__ge__` folding per arity. The fix was to emit the guards as
inline primitives that fold in the tuple method's own contour, not to
restore the annotation.

## The 9 that stay

| line | without it | what would retire it |
| --- | --- | --- |
| `bool.__not__`, `bool.__pyc_to_bool__` (`00_runtime.py`) | `hq2x` 38 s → >150 s; every `if` goes through `__pyc_to_bool__`, and one shared contour stops every upstream fold | a **logical-not primitive** that FA folds as a lattice function (`{True}`→`{False}`), used by the `is not` / `not in` lowerings. No contour at all. A lost fold raises no violation, so no demand can ask for it back. That is this issue's stop condition, met. |
| `range.__init__` ×4 (`05_builtins.py`) | `empty_list_print`, `match_seq_star`, `builtin_type_factory` | the constant is a CreationSet FIELD (`range(0)` vs `range(2)` in one `range` CS). A **CS-side constant demand** (131's side) does not exist. It would partition `cs->defs` by the constant a folding consumer needs, through route 4's machinery. |
| `__list_iter__.__init__` (`04_sequence.py:21`) | `sudoku2` (33 errors) | not constants: it makes each list's iterator its own contour. Iterators over different lists otherwise share an element channel. Needs the per-receiver-CS demand of [129](129-plan-demand-driven-creation-set-splitting.md). |
| `int.__ne__` (`02_numeric.py:94`) | `itertools_module` aborts at run time; also closed/175's zero-trip loop fold (`__pyc_more__`'s `n != 0`) depends on it | [072](072-FA-empty-container-notype-current-mechanism-and-plan.md)'s bottom-element fact removes the second use. For the first: `product`'s `if repeat != 1:` merged over 1 / 3 gives `{list, tuple}` with no runtime tag, and codegen emits a silent abort stub. Make an untagged container union a VIOLATION ([102](102-corpus-programs-compile-then-abort-at-runtime.md)), and CONST_DEMAND will answer it. |
| `tuple.__getitem__` key (`04_sequence.py:454`) | `sudoku3`, `plcfrs` | CONST_DEMAND does not yet reach every merged key. Four reach extensions (through CS-contoured AVars, through globals, both `index_object` operands, one quiet pass on stall) fixed `test_heapq`, `tuple_arity_union_slice` and `pygmy`, and cost `sudoku3`, `plcfrs` and +21% compile time. Not landed. The next attempt starts from them and from `sudoku3`, where `key` stays `{int64, str}` at p30. |

## Found on the way (landed)

- `split_edges` now DECLINES when every receiver CreationSet is identical
  by type (`cs_slot_sig_equal`). Stage 5 used to fan `tuple.__getitem__`
  over three `(prio, name)` literals that share one C layout and no tag,
  and codegen bound the call site to one clone ([121](121-CGEN-dead-clones-emitted.md)),
  so `test_heapq` printed "medium" for every item.
- `pygmy`'s method-slot winner: an override now beats the method it
  overrides on a specificity tie (`cg_build_new_to_val_map`). pygmy's
  image is byte-identical to CPython.

## Stop condition

If a case needs the annotation because no demand signal can exist for it
(the frontend knows something the analysis cannot observe), record which
case and why. Do NOT keep an annotation where the signal exists but is
merely unimplemented. Name the blocker instead, as the table does.

## Verification

`PYC_NO_FORCED_SPLIT=1` reaches 0 failures on both backends. Corpus
`check` neutral or better. `ess`/`css` do not grow: these splits are
REPLACED by demand-driven ones, not added to them.
`tests/empty_list_print.py` still prints `[]`.
