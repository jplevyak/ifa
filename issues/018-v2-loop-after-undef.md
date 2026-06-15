# Issue 018: v2 LLVM backend drops the loop-after block and list-literal element stores

**Status:** **closed** by commits `01994fd` (loop-after, via
strcat lowering + length-prefixed str literals), `32419ca`
(list-literal field stores + named-prim dispatch), and
`4d2253f` (INDEX_LOAD/STORE stride for struct-as-array lists).
The two structural gaps this issue tracked are repaired; the
follow-on consequence ("`list_print` still fails because of
arg routing in CG2_CALL for `self[k].__repr__()`") is tracked
separately and is **not** what 018 was about — see "Closing
notes" below.
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

## Closing notes (June 2026)

The diagnosis ended up smaller than the symptom suggested. The
loop-after block was never actually being dropped; reading the
v2 IR carefully showed it was reached and had its terminator,
just with no body. The real bug was that
`cg_normalize_v2`'s `lower_send` had no arm for
`P_prim_strcat`, so every Python `s += other_s` was a SEND
that produced no IR at all — and the post-loop `s += "]"; return s`
was simply two such SENDs followed by a return that loaded
the SSU-renamed accumulator. With nothing writing the
accumulator the load returned `undef`, which surfaced as
`(null)` once printf got hold of it.

The two coordinated fixes that closed this:

1. `lower_send_strcat` in `cg_normalize_v2.cc` — routes
   `P_prim_strcat` through `CG2_C_CALL → _CG_strcat` (which
   D.3.5's `libpyc_runtime.a` already exports).
2. String-literal layout in `cg_ir_v2_emit_llvm.cc` — pyc str
   pointers expect an 8-byte length prefix at `s[-8..-1]`
   (`_CG_string_len` and friends rely on it). The v2 I_STR
   emit was materializing raw `[N x i8]` globals; it now uses
   a packed `<{ i64, [N x i8] }>` struct and returns a GEP
   that lands at the chars. Runtime helpers see the layout
   they expect.

For the companion gap (list-literal init):

3. `lower_send_alloc` now emits per-field stores after the
   alloc for `P_prim_make` on tuple/list literals, matching
   the C backend's `t1->e0 = v1; t1->e1 = v2; ...` loop in
   `cg.cc:142`. The freshly-allocated `[1, 2, 3]` literal
   holds its values instead of uninitialized memory.
4. `lower_send_prim` dispatches named primitives
   (`index_object`, `set_index_object`, `sizeof`,
   `sizeof_element`, `len`) to their structural lowerings.
   Without this, the FA's habit of leaving `pn->prim` as the
   outer `P_prim_primitive` meant the existing per-prim arms
   never fired for the `__pyc_primitive__("name", ...)`
   shape — the SEND went into a generic CG2_PRIM that
   `dispatch_prim` silently dropped.
5. `CG2_INDEX_LOAD` and `CG2_INDEX_STORE` emit pick the
   first struct field's type as the GEP stride. For pyc's
   homogeneous list-of-int64 layout this gives the per-element
   stride instead of stepping by one whole struct.

Pyc-suite ratchet after all three fixes: C **74/0** (held),
v2 LLVM **47/28 → 49/26** (+2), unit tests **107/0** (held).
Both `list_print.py` and `list_concat.py` are no longer
producing null/garbage — the loop in `list.__str__` now runs
its body and produces an output of the right *shape*
(`[N, N, N, N]`). What's left is a separate CG2_CALL arg-
routing issue (next paragraph) which 018 does not own.

What still fails on `tests/list_print.py`: the SEND for
`self[k].__repr__()` inside `list.__str__` reaches the CG2_CALL
for `int.__repr__` with `i64 undef` as the self argument
instead of the element value loaded by index_object. The
index_object SEND lowers correctly in isolation — the gap is
how CG2_CALL routes the SEND's rvals into the callee's formal
arg slots when there's a primitive-call result feeding into a
method-call SEND in the same block. Worth its own
investigation; not in scope for 018.
