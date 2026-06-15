# CG_IR_019_DESIGN — v2 LLVM list-literal header alignment

Plan to close [issue 019](../issues/019-v2-flat-list-header.md):
pyc's runtime list helpers expect a 16-byte header at
`ptr - 16` (total_len, len, payload-ptr), and the v2 LLVM
backend's `CG2_ALLOC` doesn't carve that header out. Today
`tests/list_concat.py`, `tests/list_or_concat.py`, and any
program that routes a literal list through `list.__add__` /
`__mul__` / slicing segfaults under `IFA_LLVM_V2=1 -b`. C
backend is unaffected; the 53/22 ratchet held throughout the
June 2026 attempt that produced this plan.

The full landscape — including why naive fixes break — is in
the issue file's "June 2026 attempt" section. This doc picks
one of the two candidate paths and breaks it into committable
steps.

## Recommendation: option (a) — mirror the C backend's two-stage construction

Both paths can plausibly close the issue. We recommend (a) for
the same reason the C backend uses it: the runtime contract
(`_CG_list_add`, `_CG_list_mult`, `_CG_list_str_*`, slicing
helpers) is already defined against the post-conversion shape.
Option (b) — skip the +1 convention and route everything
through INDEX_STORE — looks cleaner on paper but forces
agreement on element-size semantics across every primitive
that touches a list (CG2_SIZEOF_ELEMENT being the worst
offender), which is a much bigger blast radius.

Option (a) is also cheaper to revert if a sub-step regresses:
each commit changes one alloc path at a time, and the runtime
helper is additive (does not touch existing call sites until
the normalize change opts into it).

The two-stage shape, mirroring `cg.cc:140` / `pyc_c_runtime.h:261`:

1. **Stage 1 — allocate** via `_CG_prim_tuple_list_internal(s, n)`
   (the existing runtime helper). For tuple/struct shapes, `s` =
   `sizeof(struct)` and `n` = `element_count + 1` (pyc's +1
   convention; intentional over-allocation that the conversion
   step corrects). For flat shapes, `s` = `sizeof(element)` and
   `n` = `element_count`.

2. **Stage 1.5 — initialize** the payload via per-field
   CG2_FIELD_STORE (struct shape) or per-index CG2_INDEX_STORE
   (flat shape). No change from the current implementation.

3. **Stage 2 — convert** (struct shape only) via a new
   `_CG_to_list_runtime(struct_ptr, size, n)` helper that
   mirrors `_CG_TUPLE_TO_LIST_FUN`'s body — alloc a fresh
   list with `header.len = n` (semantic length) and memcpy the
   struct's payload. The runtime contract (`sizeof_element`
   etc.) is defined against the result of this stage.

Flat shape skips stage 2 because its stage 1 already produces
the runtime contract's expected shape.

## Sub-commits

### E.1 — Add the conversion runtime helper

Add `_CG_to_list_runtime` (or whatever name; the
`_CG_TUPLE_TO_LIST_FUN` macro's signature has `_s` and `_n`
baked into the name and is awkward to reuse from v2) to
`pyc_c_runtime.h` (static inline) and `pyc_runtime.c`
(extern). Signature mirrors the macro's body:

```c
void *_CG_to_list_runtime(void *struct_ptr,
                          unsigned int struct_size,
                          unsigned int semantic_n);
```

Body:

```c
char *fresh = (char *)GC_MALLOC(SIZEOF_LIST_HEADER + struct_size);
void *result = fresh + SIZEOF_LIST_HEADER;
_CG_list_len(result) = semantic_n;
_CG_list_total_len(0, result) = semantic_n;
_CG_list_ptr(result) = result;
memcpy(result, struct_ptr, struct_size);
return result;
```

**No v2 normalize change yet.** This commit is the runtime
prerequisite; everything else is wired up in E.2-E.4.

**Verification.** `libpyc_runtime.a` builds and exports
`_CG_to_list_runtime` (verify with `nm`). C-backend 74/0
unchanged (the inline copy isn't called by anything yet but
must compile cleanly with `g++ -fpermissive`).

### E.2 — Route flat-shape list make through `_CG_prim_tuple_list_internal`

The simpler path: single-element / homogeneous flat lists
already have stage-1 producing the right shape. Update
`lower_send_alloc` so when `unwrap_struct(dst->type)` is not
`CG2T_STRUCT`, we route through `CG2_C_CALL` to
`_CG_prim_tuple_list_internal(sizeof_element, element_count)`
instead of bare `CG2_ALLOC`.

Element-type discovery: read the first rval's CGv2Type
(`pn->rvals[3]->type`). For `[1]` that gives `int64`; for
empty literals (rvals.n == 3) we can either skip the routing
(equivalent to today's broken state) or pick a default i64.
**Skip is safer.**

Per-element stores stay on INDEX_STORE with the value's type
as the stride override (the current code), so `print([1])`
keeps working.

**Verification.** `print([1])` still prints `[1]` under v2
LLVM. `tests/list_concat.py` (`[1] + [...]`) goes from
segfault to either correct output or a *different* failure
mode (e.g. struct-shape sibling now mismatches; that's E.3).
Confirm with `nm` that the binary now references both
`_CG_prim_tuple_list_internal` and `GC_malloc`.

**Risk.** None new — the flat path was already broken, so
worst case is an unchanged failure.

### E.3 — Route struct-shape list make through the two-stage construction

The harder path. When `unwrap_struct(dst->type)` is a
`CG2T_STRUCT`:

1. Mint a fresh temp `CGv2Value` (`%list_tmp`, type =
   the original struct ptr).
2. Emit `CG2_C_CALL` for
   `_CG_prim_tuple_list_internal(sizeof(struct),
   element_count + 1)` into `%list_tmp`.
3. Emit existing FIELD_STORE sequence into `%list_tmp` (NOT
   into the user's `dst`).
4. Emit a `CG2_SIZEOF` for `sizeof(struct)` (constant we
   already have at LLVM-emit time).
5. Emit `CG2_C_CALL` for
   `_CG_to_list_runtime(%list_tmp, sizeof(struct),
   element_count)` into `dst`.

After step 5, `dst` holds the correct-`header.len` list that
the runtime contract expects. The intermediate `%list_tmp` is
garbage-collected naturally because nothing else references it.

**Crucial:** the conversion preserves dst's CGv2Type as the
original struct-ptr, so downstream
`CG2_FIELD_LOAD` / `CG2_SIZEOF_ELEMENT` / etc. see the same
type they always did. **No dst type rewriting** — that was the
June 2026 attempt's mistake.

**Verification.** `print([1, 2, 3, 4])` still works (the
test that broke during the June attempt — likely the first
regression to watch for). `tests/list_concat.py` now produces
`[1, 2, 3, 4, 5, 6]`. Pyc-suite v2 LLVM ratchet moves from
53/22 to ≥ 54/21.

**Risk — high.** The 4-instruction sequence inside one SEND
breaks an assumption in the current `lower_send_alloc` /
`block_closer` machinery (one inst per closer PNode would be
the simplest mental model — we're emitting four). Verify
during emit that the block's terminator isn't accidentally
overwritten by the conversion call.

### E.4 — Ratchet check and follow-ups

Run the full pyc-suite. Expected list-shape tests that should
pass after E.3:

- `tests/list_concat.py` — `[1] + [2,3,4,5,6]` → `[1, 2, 3, 4, 5, 6]`.
- `tests/list_or_concat.py` — `[] or [1,2,3]` shape.
- `tests/list_multiply.py` — `[0] * 4` shape (compile-failure
  fix; may need E.3 + a `_CG_list_mult` debugging round).

If list_print regresses (the test we just got passing with
the issue 018 round), the conversion's CGv2Type handling
needs review.

**Verification.** Full pyc-suite C 74/0, v2 LLVM ≥ 54/21,
unit tests 105/0.

## Risks and mitigations

| Risk | Mitigation |
|---|---|
| The struct-temp / conversion sequence in E.3 confuses `block_closer` or phi-by-pred handling | Add a unit test in `cg_ir_v2_test.cc` that parses a textual fixture with a four-inst alloc-init-convert sequence and verifies the LLVM IR shape (one block, no spurious terminator). |
| `_CG_to_list_runtime` allocates twice per list literal (once for the struct, once for the converted list); GC pressure rises on list-heavy code | Same as C backend's behavior — we're matching its allocation profile exactly. Worth measuring on `tests/sieve.py` (already in the suite) but not worth pre-optimising. |
| Heterogeneous tuples (mixed-type fields, `(1, "hi")`) hit a CG2T_STRUCT shape but the runtime helper assumes homogeneous payload | The runtime helper only does memcpy; field types don't matter. The `sizeof(struct)` we pass covers the heterogeneous layout naturally. |
| Empty literals `[]` reach the flat-shape branch with `rvals.n == 3` and no element type | Detect and skip routing; fall back to current `CG2_ALLOC` (the existing broken-but-not-segfaulting behavior). Empty lists rarely go through `__add__` / `__mul__` in the pyc-suite, so this is bounded. |

## Pass criteria

- All three sub-commits land independently without regressing
  the unit suite (105/0) or the C-backend pyc-suite (74/0).
- v2 LLVM pyc-suite reaches ≥ 54/21 (one or more list-shape
  tests flip). 56/19 is the optimistic target if E.4 closes
  list_multiply / list_or_concat together.
- The v2 emit references `_CG_prim_tuple_list_internal` and
  `_CG_to_list_runtime` exactly where the runtime contract
  expects them — no other call sites changed.

## What this design omits

- A library-side rewrite of `list.__add__` /
  `__mul__` etc. The runtime helpers are already in
  `pyc_runtime.c`; this design only changes how literals are
  *constructed*, not how they're consumed.
- Tuple-only shapes (`(1, 2, 3)` not `[1, 2, 3]`). Tuples
  don't go through `_CG_list_add` so the header isn't strictly
  required — but `list_or_concat.py` uses both, so the
  conversion may need to apply to tuples too. Worth checking
  during E.3.
- Vector specializations (`@vector("s")` decorator), nested
  containers (`[[1,2], [3,4]]`), dict literals. Each is its
  own pattern; close 019 first, then audit.

## Pass criterion for this design

The next investigator either implements E.1-E.4 verbatim or
explicitly chooses option (b) instead with a counterproposal
explaining the tradeoff (the issue file's "June 2026 attempt"
section names what option (b) would have to handle).
