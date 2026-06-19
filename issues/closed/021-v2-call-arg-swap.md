# Issue 021: v2 LLVM CG2_CALL swaps args on cross-clone calls

**Status:** **closed June 2026** — fix landed in commit
`0f2c37e` ("ifa/codegen: issue 021 — walk MPositions for v2
LLVM signature").  Root cause was the signature-build path
disagreeing with the call-site arg-routing path about which
formal goes in which slot (hypothesis #1 from below).  Pyc
suite 80/0 on v2 LLVM; the swap-coercion pattern
(`ptrtoint` + `inttoptr` on adjacent args) is gone from the
entire 79-file test corpus IR audit.

**Affects:** `ifa/codegen/cg_normalize_v2.cc`'s `lower_send_call`
MPosition-based arg routing. Surfaced when issue 020's
fixes made `list.__add__` actually call `_CG_list_add`, which
caused `list.__str__` on the concat result to reach a wrapper
function (`_CG_f_2947_10`) whose internal call to range's
`__init__` had positionally-swapped + ptr/int-coerced args.
**Related:** [closed/020-v2-list-add-empty-body.md](closed/020-v2-list-add-empty-body.md)
(the alloc/sizeof_element/len fixes that unblocked this gap).

## What landed (closes the issue)

Of the three hypotheses below, **#1 was the actual cause**:
`build_fun_decl` (the v2 LLVM signature builder) iterated
`f->args.get_values(arg_vars)`, which returns args in map-
insertion order rather than positional MPosition order.
`lower_send_call` was already walking
`target->positional_arg_positions` and indexing rvals via
`Position2int(p->pos[0]) - 1`, so when the two orderings
diverged (which they did for any clone whose args entered the
map in a non-positional sequence), the LLVM signature had its
formal slots in one order and the call-site rvals routed
against a different order.

The v2 emit's type-checker then inserted `ptrtoint` +
`inttoptr` coercions to bridge the resulting type mismatch —
those coercions *looked* like type fixes but were actually
covering up a positional swap.  The visible footprint was a
pair of opposite-direction casts on adjacent call args
(`%v` ptr coerced to i64, `%arg` i64 coerced to ptr).

**Fix:** `build_fun_decl` now walks
`positional_arg_positions` itself (same iteration order as
`lower_send_call`), so the LLVM signature's formal order
agrees with the call site's rval routing for every clone.

## Verification (June 2026)

- `x = [1] + [2,3,4,5,6]; print(x)` → `[1, 2, 3, 4, 5, 6]`
  under `IFA_LLVM_V2=1 -b`.
- Wrapper function for `range(0, len(x))` is now:
  ```llvm
  define internal ptr @_CG_f_2954_10(i64 %arg) {
  entry:
    %v = call ptr @GC_malloc(i64 56)
    %v1 = call ptr @_CG_f_1695_5(ptr %v, i64 %arg)
    ret ptr %v
  }
  ```
  Direct call with no ptrtoint/inttoptr — matches the
  inlined-loop reference shape from the original report.
- `tests/list_concat.py`, `tests/list_or_concat.py`,
  `tests/list_multiply.py` all PASS on v2 LLVM.
- **Corpus-wide audit**: dumped IR for all 82 pyc test files
  (79 reached codegen; 3 are scope-test exits before
  codegen).  `grep "ptrtoint\|inttoptr"` across the 79 .ll
  files returns **zero hits** — the diagnostic shape is gone
  from the entire suite, not just the list cohort.
- pyc suite v2 LLVM: 80/0 (parity with C backend).
- ifa unit tests: 99/0.

## Symptom

For `x = [1] + [2, 3, 4, 5, 6]; print(x)` under
`IFA_LLVM_V2=1 -b`:

- `list.__add__` correctly calls `_CG_list_add` and returns
  a list with `header.len = 6` and `header.ptr` pointing at
  the freshly-concatenated 48-byte buffer (issue 020's fix).
- `list.__str__(x)` then builds a `range(0, len(x)) = range(0, 6)`
  via:

```llvm
define internal ptr @_CG_f_2947_10(i64 %arg) {
entry:
  %v = call ptr @GC_malloc(i64 56)
  %0 = ptrtoint ptr %v to i64
  %1 = inttoptr i64 %arg to ptr
  %v1 = call ptr @_CG_f_1695_5(i64 %0, ptr %1)
  ret ptr %v
}
```

The `_CG_f_1695_5` callee (range's `__init__` clone for this
context) declares signature `(i64 %aj, ptr %self)` — `aj` is
the limit, `self` is the range pointer. The call site passes:

- `ptrtoint(%v)` as the i64 `aj` slot — that's the range
  pointer reinterpreted as an i64 length.
- `inttoptr(%arg)` as the ptr `self` slot — that's the length
  reinterpreted as a pointer.

So `__init__` writes the wrong values into the range struct.
The subsequent `__pyc_more__`/`__next__` loop in `list.__str__`
then reads garbage and the program segfaults inside the
element-load path.

For comparison, the equivalent **inlined** loop body (same
source, just written verbatim in user code instead of relying
on `list.__str__`) emits:

```llvm
define internal ptr @_CG_f_2967_10(i64 %arg) {
entry:
  %v = call ptr @GC_malloc(i64 56)
  %v1 = call ptr @_CG_f_1686_5(ptr %v, i64 %arg)
  ret ptr %v
}
```

— direct call with no coercion. The arg ordering matches the
callee's `(ptr %v, i64 %arg)` declaration, so the range
constructor receives the right values and the loop works.

Both clones share the same source function (`range.__init__`)
but differ in their LLVM signature (which arg comes first).
The v2 emit's CG2_CALL apparently routes the SEND's rvals into
the callee's formal slots via different paths in the two
contexts.

## Suspected root cause

`lower_send_call` (cg_normalize_v2.cc) uses
`target->positional_arg_positions` to walk the callee's
formal slots, computing the rval index for each formal via
`Position2int(p->pos[0]) - 1`. The logic handles closures via
a special `is_closure_var` check that shifts the index by the
closure's field count.

For the failing call, one of three things is likely wrong:

1. **`positional_arg_positions` is in a different order than
   the LLVM signature was built from.** The v2 emit's
   `to_llvm_fn_type(sig)` (cg_ir_v2_emit_llvm.cc:140) walks
   `sig->args` linearly. If `lower_send_call`'s walk and
   `build_funs`'s signature-construction walk disagree on the
   order, the rvals get routed to mismatched slots.

2. **MPosition rval-index computation drops a position.**
   The skip-on-`p->pos.n > 1` clause (cg_normalize_v2.cc
   around line 815) elides positions with nested tuple
   fields, which can leave the surviving formals shifted
   relative to the LLVM signature.

3. **A coercion site (the `ptrtoint`/`inttoptr` in the IR)
   was inserted to bridge a CGv2Value whose type didn't
   match the LLVM formal's type.** Look at where CG2_CALL's
   emit inserts these coercions — they normally indicate a
   type-system mismatch between the SEND's rval and the
   callee's formal. Here both formal slots get inserted
   coercions of opposite kinds (ptr → int and int → ptr),
   which is the signature of a positional swap rather than
   a genuine type mismatch.

## Verification plan

When fixing:

1. The minimal repro
   (`x = [1] + [2,3,4,5,6]; print(x)`) prints
   `[1, 2, 3, 4, 5, 6]` under `IFA_LLVM_V2=1 -b`.
2. The two-arg range constructor emits a direct
   `call ptr @_CG_f_<init>(ptr %v, i64 %arg)` with no
   ptrtoint/inttoptr.
3. `tests/list_concat.py`, `tests/list_or_concat.py`,
   `tests/list_multiply.py` flip from FAIL to PASS in the
   v2 LLVM pyc-suite ratchet.
4. Unit suite stays at 105/0 and C-backend stays at 74/0.

## What this unblocks

- The three list-shape tests above (current concat / mult /
  or-concat fails). Pyc-suite v2 LLVM should gain ~2-3 tests.
- Any other library method whose specialization makes the
  v2 emit choose a different LLVM signature order than what
  `lower_send_call` routes against. Worth grepping the pyc-
  suite IR output for `ptrtoint`+`inttoptr` pairs on adjacent
  call args — each one is a candidate for the same shape.
- Closes the chain that issue 019 (list-header) and issue 020
  (list runtime helpers + len/sizeof_element opaque-ptr
  fallbacks) set up. With 019+020+021 all resolved the
  list-operator family fully works on v2 LLVM.
