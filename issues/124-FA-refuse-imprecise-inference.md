# 124 — `--refuse-imprecise`: refuse a program whose inference left a type untyped, instead of emitting codegen's guess

**Status:** option **landed 2026-09-01**, off by default. Filed with the
`go` diagnosis it was built to produce.

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

## MINIMAL REPRO: not yet, and here is what does NOT reproduce it

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
