# CG_IR_PARITY_PLAN — v2 LLVM → C-backend parity (74/0)

Closing the 19 remaining v2 LLVM pyc-suite failures so v2 reaches
the C backend's 74/0.  Snapshot: v2 LLVM 56/19 at commit
`0f2c37e` (after the issue 019 → 020 → 021 chain).  C backend
holds 74/0.  Unit tests 105/0.

The 19 sort into four buckets by underlying gap and effort.
Numbered F.1, F.2, ... within each bucket.

## Bucket F.1 — Quick wins on the issue-019/020/021 chain

Five tests that share the now-fixed list-allocator + opaque-ptr
infrastructure but still need small follow-ups.

| Test | Failure | Fix |
|---|---|---|
| list_or_concat.py | COMPILE | Empty literal `[]` in E.3's struct-shape path: `rvals.n == 3` means no first-element type to discover. One arm in `lower_send_alloc`. |
| list_slicing.py | EXEC | `_CG_list_getslice` already exported in `libpyc_runtime.a`; library's `__pyc_getslice__` SEND needs `__add__`-shape opaque-ptr fallbacks. |
| list_multiply.py | EXEC | Partial pass: `[1] * 4` works, `[' '] * 3` returns `[]`. Element-size dispatch when element is `str` (ptr) not `int64`. |
| list_comprehension.py | EXEC | `[i+j+1 for i in x for j in y]` lowers to range + append; should fall out once the list family is solid. |
| sieve.py | EXEC | Heavy list workload; most likely unblocked when the others pass. |

**Target:** v2 LLVM 61/14. **Estimate:** 1-2 sessions.

## Bucket F.2 — String / tuple shape work

Five tests that need `INDEX_LOAD` / `FIELD_LOAD` shapes tailored
to non-int64 element types.

| Test | Failure | Fix |
|---|---|---|
| string_index.py | EXEC | `s[i]` reads a byte (i8) not an i64 stride. INDEX_LOAD shape for string element. |
| string_unpack.py | EXEC | `e, f = "ab"` — destructuring assignment via two byte reads. |
| for_over_string.py | COMPILE | `for c in "abcd"` — iter shape over `char*`; library has `__base_iter__` but its v2 INDEX_LOAD path mismatches stride. |
| multi_type_unpack.py | EXEC | `b, c = (1,2)` etc.: mixed-shape unpack — first piece of the destructuring + literal-tuple path. |
| string_format.py | EXEC | `"foo %d bar %f" % t`: `__pyc_format_string__` primitive is currently C++-side only. Library-side via `__pyc_c_call__("snprintf", ...)` is the FFI plan's natural next move. |
| tuple_mixed_types.py | EXEC | `(1, "asdf", 2.0)` — heterogeneous tuple. Can't use "first-field stride" trick; needs FIELD_LOAD/STORE with per-field type. |

(Listed six because tuple_mixed_types lives at the same boundary.)

**Target:** v2 LLVM 67/8. **Estimate:** 2-3 sessions.

## Bucket F.3 — Class / analyzer specialization

| Test | Failure | Fix |
|---|---|---|
| attr_augmented_assign.py | EXEC | `y.x += 2` on class with `x = 2` default. B.10.10 family. |
| class_attr_mutation.py | EXEC | A.n / B.n inheritance + mutation. |
| class_init.py | EXEC | `__init__` setting self.x from class default. |

B.10.10 already added a prelude that GC_mallocs class-prototype
globals at main entry; what's missing is the **initialization**
of the prototype's fields so the field-store path reads sane
defaults before mutation.  See
`ifa/codegen/CG_IR_B10_10_DESIGN.md` for the existing analysis.

**Target:** v2 LLVM 70/5. **Estimate:** 1-2 sessions of design + careful change.

## Bucket F.4 — Wholesale new infrastructure

Five tests that need a whole new runtime / lowering layer rather
than a tweak.

| Test | Failure | Blocking gap |
|---|---|---|
| dict_basic.py | EXEC | Dict runtime (hash table) + `__getitem__`/`__setitem__`/`len` primitive lowerings. None of this exists yet. |
| lambda_closure.py | EXEC | Closures (the SQ3 gap deferred all session). |
| default_args.py | EXEC | Mutable default args: `def f(a, L=[])`'s `L` must be a module-level global with state surviving across calls. |
| builtins.py | EXEC-TIMEOUT | Likely depends on string ops; runaway loop somewhere downstream. |
| bitwise_operators.py | EXEC-TIMEOUT | Same shape: probably depends on a list/string op that loops. |

**Target:** v2 LLVM 75/0. **Estimate:** dict + closures are each
their own 10-15 hour project; default_args is 3-5 hours; the two
TIMEOUTs may collapse to bucket F.2 follow-ups once their loops
have working primitives.

## Recommended order

1. **F.1** — keeps the issue 019/020/021 momentum.  Targets 61/14.
2. **F.2** — string/tuple shape lowerings.  Reuses opaque-ptr
   fallback pattern.  Targets 67/8.
3. **F.3** — class prototype init.  Targets 70/5.
4. **F.4** — pick one of dict / closures / default_args based on
   which gates the most downstream work.

**Realistic to 70/5 in 4-6 focused sessions.**  The last 5 need
the bucket-4 commitments.

## What this plan omits

- Re-runs against the v1 LLVM backend (still 38/37; closing v2
  doesn't help v1 directly).
- Performance / allocation profile measurement.
- Library code consolidation (some `__pyc_c_call__` patterns
  might become idiomatic helpers once F.1-F.2 lock in their
  shape).
- The next layer of FFI library moves: now that `_CG_str_from_int`,
  `_CG_to_list_runtime`, `_CG_list_add`, etc. are in
  `libpyc_runtime.a`, the C++ prim dispatchers for `write`,
  `writeln`, and the float-format printer (`pyc_llvm_to_string_cgfn`)
  could follow.  Not strictly required for parity.
