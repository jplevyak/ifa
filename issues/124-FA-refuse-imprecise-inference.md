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

## What it says about `go` — 66 sites

The five list sites are the interesting half:

```
go.py:48    self.neighbours = []                                  empty literal
go.py:163   self.squares = [Square(self, pos) for pos in ...]     ALL Squares
go.py:177   self.history = []                                     empty literal
go.py:323   self.pos_child = [None for x in ...]                  all None
go.py:330   path = [node]                                         all UCTNodes
```

`go.py:163` is the tell. A comprehension producing **nothing but
`Square`** gets an untyped element, and so does `path = [node]`, which
produces nothing but `UCTNode`. Two lists that are each monomorphic in
the source end up sharing an untyped element type — which is exactly
what lets a `UCTNode` reach a `Square` layout.

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
