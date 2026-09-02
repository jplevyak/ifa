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
