# Issue 018: v2 LLVM backend drops the loop-after block and list-literal element stores

**Status:** open.
**Affects:** `ifa/codegen/cg_normalize_v2.cc` (or the SSU
phi-by-pred handling) under `IFA_LLVM_V2=1`. The v1 LLVM and
the C backends are not affected by these specific shapes.
**Related:** [017-iterator-construction-undef-self.md](017-iterator-construction-undef-self.md)
(also a v2 LLVM lowering gap; manifested at iterator
construction). Surfaced during the Phase D FFI library
migration (commit `026891b`, while landing D.6).

## Symptom

Functions whose Python body is `loop … ; trailing statements ;
return x`, where `x` is an accumulator updated inside the loop
and finalised in the trailing statements, compile to LLVM IR
whose loop-exit edge falls through to a block that returns
`undef` instead of loading `x`.

Minimal repro (`/tmp/d6_while.py`):

```python
def f():
    s = "init"
    i = 0
    while i < 3:
        s += "a"
        i += 1
    s += "!"
    return s

print(f())
```

- C backend: `initaaa!` ✓
- v1 LLVM (`IFA_LLVM=1 -b`): also broken (segfaults on the
  pre-loop iterator setup, but the post-loop tail isn't reached
  to compare). Not analysed further here.
- v2 LLVM (`IFA_LLVM_V2=1 -b`): prints `(null)`. The loop
  iterates correctly inside; the function then returns `undef`
  because the post-loop `s += "!"; return s` was dropped.

Same shape is the root cause of v2 LLVM `print([1, 2, 3])`
printing `(null)` — `__pyc__/04_sequence.py:list.__str__` has
the exact pattern (build `x = "["`, loop, then `x += "]"; return
x`). D.6 worked around it by routing list.__str__ through a
runtime helper instead of the Python loop, but other library
code with the same shape (currently no other tested call) will
fail the same way.

## What the IR looks like

Inspecting the v2 LLVM emit for `list.__str__` (function
`_CG_f_1497_3`) shows the structure:

```llvm
entry:
  store ptr @.str.lit.15, ptr %x, align 8    ; x = "["
  ...
  br label %L145

L145:                                        ; loop head
  ...check more...
  br i1 %v17, label %L241, label %L146

L146:                                        ; loop-exit edge
  br label %L144

L144:
  ret ptr undef                              ; ← BUG

L241:                                        ; loop body
  ...
  br i1 %v28, label %L239, label %L240
L239: br label %L240
L240:
  ...body work...
  br label %L145
```

L144 is the function's "after the loop" block. It should:

1. Load `x` from its alloca slot.
2. Concatenate `"]"`.
3. Store back.
4. Return.

Instead it just does `ret ptr undef`. The post-loop PNodes
appear to never have been emitted into the block — or the
block's terminator was overwritten with a bare `ret undef`
before the loop-after instructions ran.

## Suspected root cause

`cg_normalize_v2.cc`'s `lower_body` walks PNodes from the
function entry and partitions them into CGv2Blocks based on the
nearest enclosing LABEL ancestor (see `lower_send` /
`block_closer` machinery, `cg_normalize_v2.cc` around line 940).
A loop's exit edge is a `cfg_succ` from the head PNode back to
the PNode that sits structurally *after* the loop construct in
the source. If lower_body either (a) doesn't visit that
"after-loop" PNode in the right block, or (b) attributes its
work to the loop body instead of the trailing block, the
trailing block ends up with only a fall-through terminator and
no body — exactly the L146 → L144 → `ret undef` shape we see.

The v1 LLVM normalize path (`cg_normalize.cc`) handles loops
via a different SSU-aware walk that hasn't been ported to v2;
the v2 implementation has so far been small enough to live
without that machinery, but this shape demands it.

## Companion gap: list-literal element stores missing

While investigating list.__str__ for D.6 we tried to bypass the
loop-after bug by routing through a runtime helper (the
`_CG_list_str_i64(lst)` shape). The library can do that with a
single `__pyc_c_call__`, but on v2 LLVM the helper sees an
empty list. Reason: pyc's C backend emits the literal `[1, 2,
3]` as

```c
t1 = _CG_prim_tuple_list(_CG_ps3280, 4);
t1->e0 = 1;
t1->e1 = 2;
t1->e2 = 3;
```

(see `ifa/codegen/cg.cc:140`, `P_prim_make` case — note that
the count argument is `rvals.n - 2`, which is the element count
**plus one** by pyc convention; see "+1 convention" below.)
The v2 LLVM emit produces the `GC_malloc` of the list and the
final call into list.__str__ but **does not emit the field
stores** for `e0/e1/e2`. So when the runtime helper runs, it
reads from uninitialized memory.

This is independent of the loop-after bug. It's a missing v2
lowering for the field-store sequence that pyc generates for
list/tuple literals. Likely the SEND PNodes for these stores
are being attributed to the wrong CGv2Block, or aren't being
visited at all.

### "+1 convention" footnote

`P_prim_make`'s emit (`cg.cc:140`) writes `n->rvals.n - 2` as
the count argument. The SEND rvals layout is `[marker,
sym_make, type, e0, e1, ...]`, so this value equals **element
count + 1**. The runtime header faithfully stores it, and the
runtime's macro-level `_CG_prim_len` reads it back. Other code
paths (`_CG_TUPLE_TO_LIST_FUN`, slice/concat helpers,
`list.append`) store the semantic count. So the same list-
header `len` field carries different meanings depending on how
the list was constructed. Any runtime helper that wants a
single semantic-length API would have to know which path
created the list.

This invariant inconsistency surfaced when D.6's runtime helper
naively subtracted 1 (and broke 9 C-backend tests that exercise
non-literal lists). Documenting it here so a future fix knows
to either normalize the convention or expose two distinct
accessors.

## Verification plan

When fixing:

1. The repro program above prints `initaaa!` under
   `IFA_LLVM_V2=1 -b`.
2. Revert D.6's runtime-helper workaround for
   `list.__str__` (commit landing this issue + D.6 referenced
   from there) and confirm `print([1, 2, 3])` produces
   `[1, 2, 3]`.
3. `tests/list_print.py`, `tests/list_concat.py`,
   `tests/list_or_concat.py` move to the pass column under the
   v2 LLVM pyc-suite ratchet.
4. Unit suite stays clean.

## What this unblocks

- Pure-Python `list.__str__` works on v2 LLVM. Pyc-suite gains
  `list_print`, `list_concat` (after concat), `list_or_concat`,
  and `logical_operators` flips from FAIL to PASS once both
  this issue and the literal-init companion gap are closed.
- `tuple.__str__`, `dict.__str__`, and any other library code
  with a "loop-then-return-accumulator" shape becomes usable
  on v2 LLVM without runtime-helper escape hatches.
- Phase D.6 of `FFI_LIBRARY_MIGRATION.md` is currently blocked
  on this issue — the runtime-helper escape hatch the doc
  suggested doesn't work in practice (see "Companion gap"
  above), so list/tuple/dict `__str__` migration has to wait
  for the lowering fix.
