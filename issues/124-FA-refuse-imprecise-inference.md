# 124 — `--refuse-imprecise`: refuse a program whose inference left a type untyped, instead of emitting codegen's guess

**Status:** option **landed 2026-09-01**, off by default. **Root-caused
2026-09-02** with a 10-line repro (`ifa/issues/repro/`) and a regression
test (`tests/comprehension_index_untypes_list.py`, `.known_issue`): two
comprehensions with different element types share one `list::append`
contour, so their element types unify. Fix not written.

**Affects:** `ifa/codegen/cg.cc` (`cg_note_imprecise`,
`cg_check_imprecise`, the list-creation site and the formal-parameter
scan), `pyc.cc` (`--refuse-imprecise`), `ifa/ifa.h` / `ifa/if1/if1.cc`
(`frefuse_imprecise`).

## Why

shedskin compiles `shedskin_examples/go` to

```cpp
void *UCTNode::update_path(Board *board, __ss_int color, list<UCTNode *> *path);
    path = (new list<UCTNode *>(1, node));
        node->losses = (node->losses + __ss_int(1));
```

pyc compiles the same source to

```c
t158 = (_CG_list)_CG_prim_list(_CG_void,1);              /* path = [node]    */
_CG_f_12733_138/*update_path*/(..., _CG_any a4)          /* path parameter   */
((_CG_ps17224)t86)->e26 = (_CG_int64)t87;                /* node.losses += 1 */
```

The difference is **not** that shedskin has a better union
representation — it has none either (see
`shedskin-template-monomorphization`). It is that where inference does
not resolve, **shedskin refuses the program and pyc guesses a layout**.
That guess is [123](123-CGEN-union-receiver-field-access-has-no-discrimination.md):
`node.losses += 1` resolved against `Square`'s layout and incremented
`UCTNode`'s `unexplored` POINTER, two increments, then SIGSEGV.

Across shedskin's whole `go.cpp`: `list<__ss_int>` ×21,
`list<Square *>` ×12, `list<str *>` ×9, `list<UCTNode *>` ×8 — **no
untyped element list and zero `void *` field accesses.** pyc: 12 of go's
26 list constructions have `_CG_void` elements.

## What it reports

An untyped (`_CG_void` / `_CG_any` / `_CG_void_type`) **container
element type** at its creation site, and an untyped **function
parameter**. Both are the points where codegen stops having a type and
starts having a layout guess.

```
PYC_DBG_IMPRECISE=1   report every site, with source location
--refuse-imprecise    make it a compile error (PYC_REFUSE_IMPRECISE=1)
```

Off by default; measured to emit nothing and change nothing when off.
It runs **before** 122's layout-contract check deliberately: imprecision
is the cause and the layout violation the consequence, and reporting the
consequence first buries the cause.

## Only constructors WITH elements count

The first version flagged every `_CG_prim_list(_CG_void, ...)`, which
over-reported badly: a comprehension, and any `xs = []` + `append`,
legitimately builds an EMPTY list first and acquires its element type
from the appends. An untyped element type there means nothing. The check
now fires only when the constructor has elements in hand and still has
no element type for them.

That took `go` from five list sites to **one**, and it is the right one:

```
go.py:330:  path = [node]        <- the list behind ifa/123's crash
```

## MINIMAL REPRO (2026-09-02): 10 lines, and the trigger is the INDEX

`ifa/issues/repro/124-comprehension-index-untypes-list.py`, reduced from
`go` 635 -> 10 lines against this diagnostic:

```python
def moves():
    return [p for p in range(4)]
class N:
    def __init__(self):
        self.kids = [None for x in range(4)]
    def play(self):
        path = [self]                        # <- element type UNTYPED
        path.append(self.kids[moves()[0]])
        return len(path)
print(N().play())
```

Three variants pin it. Only the third reports:

| index expression | `moves()` returns | reports? |
|---|---|---|
| `self.kids[0]` | — | **no** |
| `self.kids[moves()[0]]` | `[0, 1, 2, 3]` (literal) | **no** |
| `self.kids[moves()[0]]` | `[p for p in range(4)]` | **YES** |

So it is **not** `path = [self]` on its own, **not** the recursive tree,
**not** the dynamically-added attribute (`go`'s `unexplored`), and **not**
two class hierarchies interacting — every one of those was guessed during
this investigation and every one is refuted by the table above. What is
required is that the INDEX flows from a **comprehension built in another
function**. Swap the comprehension for a list literal of the same values
and the untypedness disappears.

CPython prints 2 and so does pyc: this is imprecision, not yet a
miscompile. `go` needs the additional layout divergence to crash.

`ifa/issues/repro/124-go-reduced.py` is the 71-line intermediate, kept
because it still exercises the shape through `Board`/`UCTNode` methods.

### ROOT CAUSE: two comprehensions share one `list::append` contour

FA's own count settles it (`IFA_DBG_ELEMTYPE=1` on the 10-line repro):

```
ELEMTYPE p=3 | list: 3 CS / 2 elemtypes / 2 shapes
```

**Three list CreationSets, two element types.** The repro has exactly
three lists -- `moves()`'s `[p for p in range(4)]` (ints), `kids`'s
`[None for x in range(4)]` (None), and `path` -- so two of them have
been given the SAME element type.

Which two is visible in the emitted C. `list::append` gets two clones,
and three call sites share them like this:

| caller | appends | clone |
|---|---|---|
| `moves()` -- `[p for p in range(4)]` | an **int** | `_18` |
| `N.__init__` -- `[None for x in range(4)]` | **None** | `_18` |
| `N.play` -- `path.append(...)` | an **N** | `_2` |

The int-appending comprehension and the None-appending comprehension
**share one `append` contour**, so their element AVars unify. The shared
clone's formal comes out `_CG_int64`, and the `None` append is emitted
as `(_CG_int64)NULL`:

```c
t12 = _CG_f_2845_18/*list::append*/(t4, t13);            /* in moves()      */
t12 = _CG_f_2845_18/*list::append*/(t4, (_CG_int64)NULL); /* in N::__init__ */
```

Note the direction: cloning runs AFTER flow analysis, so the shared
clone is a CONSEQUENCE of FA having unified the two contours' types, not
the cause. The cause is that FA did not split `list::append` per element
type for these two calls -- the CS-element splitting
[072](072-FA-empty-container-notype-current-mechanism-and-plan.md) refers
to as "data-polymorphism splitting, which pyc's `split_css` already
does" did not fire here.

The merged element spans an int and None, which has no single C
representation, so `c_type` yields `_CG_void`; `self.kids[...]` then
carries that into `path.append(...)`, and `path = [self]` is reported.

**This is exactly why the discriminator table looks as it does.** A list
LITERAL (`[0, 1, 2, 3]`) is constructed positionally and calls `append`
at all -- no shared contour, no merge, `path` typed. A constant index
never calls `moves()`, so its comprehension is dead -- same result. Only
the comprehension, whose construction goes through `append`, merges.

### WHY the split does not happen: the receiver fan exists and is OFF

The expected mechanism is the one that works for instance variables:
back-flow the writes through the callee, reach the creation point, split
the CreationSet. `list.append` writes an ELEMENT, so it should split the
two lists the same way.

**RETRACTION.** An earlier version of this section claimed element AVars
are "outside the setter graph" because their `setters` and `container`
are null. That is wrong, and the reasoning was backwards: `update_setter`
records a setter on the **container** AVar and propagates **backward** to
the creation point, so a null `setters` on the element is normal by
design. `P_prim_set_index_object` -- which is what `append` lowers to --
already does the full ivar-style wiring, `set_container(tval, vec)` then
`flow_vars(tval, get_element_avar(cs))`. The wiring is not missing.

What `IFA_DBG_ELEMSETTER` actually shows, per container CS:

| CS | element types | backward writers | `setter_class` | creation point `setters` |
|---|---|---|---|---|
| 1041 | 2 | 3079, 3128 | 0 | none |
| 1048 | 3 | 3188, 3189, 3219 | 1 | **4** |
| 1051 | 2 | 3079, 3128 | 0 | none |

The machinery works fine for `cs=1048`. The two lists sharing the
`append` contour, 1041 and 1051, have **the same backward writers** --
so nothing at the element looks like a confluence, and there is nothing
for a setter-driven split to key on. The split has to happen on the
**callee** side: `append` needs one contour per receiver CreationSet.

**And it is NOT a quiescence problem.** `PYC_DBG_STAGEDELTA` shows the
SETTER stage does run, twice:

```
STAGEDELTA p=2 TYPE_CONFL returned=0 confluences=40 ...
STAGEDELTA p=2 SETTER      returned=0 ...
STAGEDELTA p=3 TYPE_CONFL returned=0 confluences=40 ...
STAGEDELTA p=3 SETTER      returned=0 ...
```

TYPE_CONFL reaches `returned=0` at pass 2, the gate opens, SETTER runs
-- and finds nothing (`returned=0`). `IFA_DBG_SETTERCONF` says why:

```
SETTERCONF p=2 confluences=40 cs_contoured=27 container_elements=1
```

`compute_setters` only ever visits `confluences`, and of the three
container CSs **exactly one** element AVar is collected as a type
confluence -- `cs=1048`, the one that duly got `setter_class=1` and four
setters. The two that need splitting are never collected, so their
writers are never classed, no setter reaches their creation points, and
the SETTER stage has nothing to key on.

Why they are not confluences follows from the table above: 1041 and 1051
have **the same two writers**. From either element's local view the
incoming types are consistent -- the conflict is not *at* either
element, it is *between* the two CreationSets, and a per-AVar confluence
test cannot see it. That is precisely why the split has to happen at the
callee's receiver, where the two lists are still distinguishable.

**That mechanism exists, and it is disabled.** `recvfan_enabled()`
(`fa.cc` ~8124) gates the PER_CS_RECEIVER fan, defaulting to 0, under a
comment that names exactly this problem:

> This is what shedskin gets for free: `list<T>::__getitem__` and
> `tuple2<A,B>::__getitem__` are separate template instantiations, so no
> single `__getitem__` ever sees a union.

`PYC_RECVFAN=1` **fixes the repro completely**: `3 CS / 3 elemtypes /
3 shapes` instead of `3 CS / 2 elemtypes`, no imprecision reported, and
the program still prints 2.

Two other gates were tried and are NOT the cause: `PYC_SETTERGATE=1`
(lifts the SETTER stage's quiescence gate -- ifa/055's plcfrs fix)
leaves the repro unchanged, and so does `PYC_RECVFAN=2`.

**Why it is off: `PYC_RECVFAN=1` fails 25 of the suite** -- 8 COMPILE,
10 COMPILE-OUT, 7 EXEC (`deepcopy_list`, `genexpr_basic`,
`generator_yields_nonint`, `bytes_from_list`, ...). So the answer to
"why isn't the CreationSet split happening" is not that the analysis
lacks the mechanism, and not that a heuristic declines to fire: **the
mechanism is implemented, correct on this repro, and switched off
because it is not yet sound elsewhere.** Making those 25 pass is the
work, and it is the same work as ifa/072's "data-polymorphism
splitting" and shedskin's monomorphization.

### Superseded: an earlier reading of the setter machinery

The expected mechanism is the one that works for instance variables:
back-flow the writes to a field through the callee, reach the creation
point, and split the CreationSet so two objects written with different
types get different contours. `list.append` writes an ELEMENT, so the
same thing should split the two lists. It does not, and
`IFA_DBG_ELEMSETTER` (added here) says why in one line per container CS:

```
ELEMSETTER cs=1041 sym=list elem_av=3076 setters=-1 container=-1 lvalue=0 cs_map=0 ntypes=2
ELEMSETTER cs=1048 sym=list elem_av=2920 setters=-1 container=-1 lvalue=0 cs_map=0 ntypes=3
ELEMSETTER cs=1051 sym=list elem_av=3147 setters=-1 container=-1 lvalue=0 cs_map=0 ntypes=2
```

**Every element AVar has `setters == null` and `container == null`.**
Those are exactly the two fields the setter-driven splitter runs on:

- `compute_setters` (`fa.cc` ~7320) drives `update_setter(x->container, x, avs)`
  -- no `container`, no setter recorded;
- `collect_setter_confluences` (~7349) only looks at `av->setters`, and
  only seeds a `setter_starter` from an AVar that has a `cs_map`;
- `build_setter_marks` (~6815) pairs `x == y->container`.

So a write through `append` is **invisible** to the machinery that would
split the CreationSet. The contrast is structural, not accidental: the
ivar path calls `set_container(cav, result)` when it adds the ivar
(`fa.cc` ~2108), and `vector_elems` sets one for tuple POSITIONAL
elements (~2065), but the generic element AVar that
`get_element_avar()` hands back is never given a container by anything.
In `structural_assignment` (~2637) the container is even set on the
temporary that carries the element value, `set_container(tval, result)`,
and never on `get_element_avar(new_cs)` itself.

That is the whole answer to "why isn't it happening": **not a heuristic
declining to fire, and not a splitting decision going the wrong way --
the element AVar is not wired into the setter graph at all**, so there
is nothing for the splitter to see.

The fix therefore is not in `split_css` or the splitter stages. It is to
give a container's element AVar the same `container`/`lvalue` wiring an
ivar gets, so element writes become setters of the container CS and the
existing back-flow-and-split machinery applies unchanged. Whether that
is safe for the generic element -- which, unlike an ivar, is written
from many sites and read positionally as well -- is the open question,
and `ifa/issues/072`'s prototype note is the warning to read first.

### How it was reduced, and the two oracle bugs found on the way

`ddmin.py` (now parameterised by `DDMIN_CHECK`/`DDMIN_CAND`/`DDMIN_OUT`)
against `check124.sh`. **Three of the first four reduction runs produced
INVALID results**, each caught only by building a sharper check:

1. `nameck.py` alone was not enough. The reducer deleted
   `Square.set_neighbours` and `Board.reset` while keeping
   `square.neighbours` and `neighbour.color` -- undefined ATTRIBUTES, on
   paths it had also made unreachable, so CPython never complained and
   pyc inferred over the garbage. 7 orphans on the first pass. Hence
   `attrck.py`.
2. `attrck.py`'s first version used a stdlib allowlist, and
   `Square.find` collides with `str.find`: a 132-line reduction passed
   while `neighbour.find()` referred to a deleted method. Fixed by
   comparing against the ORIGINAL -- an attribute the original defined,
   the candidate reads, and the candidate no longer defines is drift by
   construction. No allowlist, no guessing.

A third hazard was checked and cleared rather than assumed: the reduced
program still contains unreachable code, and pyc analyses dead code, so
the signal could have come from there. Replacing the dead body of
`useful()` with `return True` (-14 lines) leaves the diagnostic intact,
so it does not.

## Superseded: what does NOT reproduce it

Four reductions were tried against `go.py:330`'s shape. **All four
compile cleanly and print the right answer**, so none of them is it:

```python
path = [a]                                  # one-element literal        CLEAN
path = [a]; path.append(None)               # + a later None             CLEAN
slots = [None for i in range(4)]            # go's pos_child shape:
slots[0] = A(); path = [slots[0]]           #   None-list, index-assign  CLEAN
xs = [A() for i in ...]; ys = [B() for ...] # two lists + shared helper  CLEAN
def count(items): ...                       #   (reports imprecision at
                                            #    the EMPTY constructors
                                            #    only -- benign)
```

The last one is worth keeping in mind: two source-monomorphic lists
sharing a helper DO both come out untyped at their empty constructors,
but that is the benign case above and their uses are correctly typed —
`A` and `B` keep their own member lists, no field-name pollution, and
the program runs correctly.

So `go.py:330` needs an ingredient not yet isolated. The untypedness
propagates INTO the literal (`node` is already untyped when `[node]` is
built), so the reduction has to reproduce whatever makes `node` untyped
— plausibly the recursive tree (`UCTNode.pos_child` holds `UCTNode`s
which hold `pos_child`...) rather than any of the flat shapes above.
Reducing this is the next step, and these four are ruled out.

## What it says about `go` — 66 sites

(66 was the first version's count, before the empty-constructor
over-reporting was fixed above; the list half is now one site,
`go.py:330`, and the rest are untyped function parameters.)

`path = [node]` produces nothing but `UCTNode` and still has no element
type — the untypedness arrives with `node`, which is already untyped
when the literal is built.

FA already says so, in warnings that were previously just noise among
go's 30 and are damning next to the above:

```
go.py:165: warning: illegal call argument type 'square'    illegal: UCTNode
go.py:184: warning: illegal call argument type 'square'    illegal: UCTNode
go.py:217: warning: illegal call argument type 'neighbour' illegal: UCTNode
```

**FA believes a `UCTNode` can be an element of `Board.squares`.** It
cannot — nothing in `go.py` ever puts one there. So the element
CreationSets of two unrelated lists have been merged, and that merge is
the root of 123's crash.

That points the fix at container-element precision —
[072](072-FA-empty-container-notype-current-mechanism-and-plan.md),
[043](closed/043-empty-container-inference-options.md),
[040](closed/040-empty-list-shared-clone-type-inference.md) — not at
codegen. 123's classtag-discriminated field access remains worth having
as a safety net, but it would be making a guess safe rather than
removing the need to guess.

## Verification plan

1. Off by default changes nothing: `make test` and the corpus sweep
   unchanged (verified — the flag emits nothing when off).
2. `--refuse-imprecise` on a program whose inference IS precise must
   accept it. The corpus census below is that measurement, and is not
   yet taken.
3. The useful regression: once container-element precision improves,
   `go`'s five list sites should disappear one by one, and 122's layout
   violations with them.

## What this unblocks

A way to ask "is this program fully inferred?" and get source lines
instead of a crash. Immediately: it turned `go` from "segfaults
somewhere" into five named list literals, one of which is provably
monomorphic in the source.

---

## ROOT CAUSE (2026-09-02): `->type` strips a pure-nil AType, and the
## splitter's every comparison is guarded by `->n &&`

Measured on the 10-line repro, on `list.append`'s shared contour
(es=60). Two edges reach it, one passing `int64` and one passing
`None`, and the splitter never separates them:

```
DECIDE   p=3 es=60 av=3065 nedges=2 es_type: int64#6 __pyc_None_type__#13
   edge from_es=46 ety(n=0): RAW: __pyc_None_type__#13  compat=1
   edge from_es=51 ety(n=1): int64#6                    compat=0
DECIDE-OUT p=3 es=60 av=3065 do=0 stay=2 groups=0
```

`do=0 stay=2 groups=0`, identically on every pass. Two mechanisms
compose to produce it:

**1. `make_AType` projects a lone `{None}` to bottom.** `nil_type` is
`is_unique_type`, so it lands in `nil_cs` rather than `nonconsts`; the
issue-060 carve-out that KEEPS nil in `->type` fires only when the same
AType also carries a `num_kind` scalar. A lone `{None}` has none, so
`nonconsts.n == 0` and `tt->type = bottom_type`. The MERGED formal
`{int64, None}` does keep nil (int64 supplies `has_scalar`) — which is
why the confluence is *detected*: `type_diff({int64,None}, {int64})` is
`{None}`. Detected, but with nothing to partition by.

**2. Every edge-compatibility test treats a bottom `->type` as
"compatible with anything"**, and it costs the split twice:

```c
// edge_type_compatible_with_entry_set
if (etype->n && stype->n && etype != stype) return ++ic_arg, 0;
// edge_type_compatible_with_edge
if (etype->n && eetype->n && etype != eetype) return ++ic_arg, 0;
```

First the nil edge is judged compatible with the ES and put in
`stay_edges`. Then the int edge — correctly placed in `do_edges` by the
first loop — is re-tested against that stay edge by
`edge_type_compatible_with_edge`, whose `eetype->n` is 0, so it is
pulled back OUT into `stay_edges`. `do_edges` empties, no group forms.

The `->n &&` guards are right in general: an unanalyzed edge has no type
yet and must not force a split. The bug is that a bottom `->type` is
overloaded — it means both "nothing known" and "carries only nil".

## The fix (all five CI gates green)

Do NOT change `->type` itself. Tried first, and it regresses three
tests, all in places where nil is *meant* to be transparent:
`is_not_none_narrow` (narrowing), `minmax_3arg` (a `None`-defaulted
parameter), `expr_evaluator` (the recursion-separability gate — whose
comment already records it regressing both ways).

Instead give the SPLITTER a local view, `split_type_view()`, that
distinguishes "not analyzed" (raw `out` empty too) from "carries only
nil" (raw non-empty), and unstrips only in the latter case. Constants
are deliberately left stripped — a raw single-element `out` can be a
constant CS (`3` rather than `int64`) and partitioning on that is
clone-per-constant (survey B5). Used at three sites:

- `edge_type_compatible_with_edge` — both operands
- `edge_type_compatible_with_entry_set` — the edge and the ES
- `apply_entry_set_split`'s `part` — **required**: without it the
  splitter decides to split on nil and then builds the product's filter
  from the stripped view, so the pure-nil group's partition is bottom
  and the formal STARVES in the product contour (`illegal call argument
  type 'c' illegal:` with an empty type). Decide and apply must read the
  same view.

## Measured result

| | baseline | with fix |
|---|---|---|
| `go` compile | **refuses** — 20 blind-cast layout violations (ifa/123) | rc=0 |
| `go` run | no binary | **rc=0, plays a full game** |
| `go` `--refuse-imprecise` sites | 55 | 39 |
| repro `append` split | `do=0 stay=2 groups=0` forever | `groups=2` at p=0 |

CI: `make test` rc=0 (308 passed / 17 known / 0 failed, unchanged),
`ifa --test` + `test-ir` clean, `make -C ifa test_llvm` passed,
`PYC_FLAGS=-b ./test_pyc.py` 309 passed / 16 known / 0 failed,
`make test_dparse` all passed.

**Not yet measured: the corpus `check` A/B.** This is a core FA
behaviour change and `make test` is not sufficient evidence for one —
run `./corpus_sweep.sh -m check` on this tree against
`check__default__635d26b6+c1a28ccc` (compile_fail=5 run_fail=42
stdout_differs=23 with_warnings=42) before treating it as settled.

## Residual

`tests/comprehension_index_untypes_list.py` still reports 3 imprecise
sites under `--refuse-imprecise`, but they are DIFFERENT sites: the
original `[self]` element-type site is gone, replaced by "function
parameter untyped" on `append` and `len`. Those are the new pure-nil
contours — a parameter that is provably always `None` is perfectly
precise, it just has no C representation, so this looks like a false
positive of the check rather than residual imprecision. The test stays
`.known_issue` and will flip on its own if the check learns to exempt
nil-only formals.
