# CG_IR_PARITY_PLAN — LLVM → C-backend parity

## Status (June 2026 update)

- **C backend:** 99/0/1xfail/1skip.
- **LLVM backend (`-b`):** 92/7/0/1.  Gap is exactly the
  7 tests we added recently (ring_splice, fibheap_*,
  expr_evaluator) that landed C-side fixes the LLVM
  emitter hasn't yet absorbed.

## Architecture clarification (correcting an earlier
## confusion in this session)

There is **only one LLVM backend**.  The naming "v1" /
"v2" referred to two historical *implementations* of the
LLVM emission:

- **v1**: a direct IF1 → LLVM emitter that lived in
  `llvm.cc` (`createGlobalVariables` +
  `translateFunctionBody`).  **Retired in issue 014.**
- **v2**: an IF1 → CGv2Program → LLVM pipeline
  (`cg_normalize_v2.cc` + `cg_ir_v2_emit_llvm.cc`) that
  strictly subsumed v1.

Today, `llvm_codegen_print_ir` in `llvm.cc` IS the v2
path — it just orchestrates init + v2 emit + main
wrapper synthesis + verify + write.  The "v1" code is
gone; the file is just scaffolding around v2.

The `-b` flag selects this single LLVM backend.  92/7 is
the current parity number.

## Policy (June 2026)

Every codegen-affecting change must land such that BOTH
backends compile and run it correctly.  Validation:

```
./test_pyc                 # C backend (canonical)
PYC_FLAGS=-b ./test_pyc    # LLVM backend
```

The C backend remains canonical because (a) it's the
default, (b) gcc serves as a free type checker on the
generated code, (c) the generated C is human-readable
for debugging codegen bugs.  The LLVM backend is the
parity follower — production-quality codegen with a 7-
test lag that needs closing.

## What's currently failing on LLVM and why

The 7 failures are exactly the recent additions:

- `ring_splice`, `fibheap_*`, `expr_evaluator` — landed
  C-side fixes that the LLVM emitter doesn't yet apply:
  - `prim_is` (issue 028 step 4) — new identity primitive.
  - Voidish-arg cast (issue 028 step 5 / 029) — implicit
    `(formal_t)arg` at call boundaries when arg's C type
    is `_CG_any`/`_CG_void`/`_CG_nil_type`.
  - `resolve_union_receiver` — picks a non-nil component
    of a union receiver for the period/setter cast.

Two FA-level fixes (`concretize_var_list_type`, SSU phi
cascade) are already shared and don't need an LLVM port.

## Closing the gap

Each remaining v2 emission gap is bounded.  Triage:

1. `prim_is` — add a CG2_BINOP-EQ case in
   `cg_ir_v2_emit_llvm.cc` (the parallel of cg.cc:709).
2. Voidish-arg cast — at the v2 emit level, when a
   CG_CALL argument's CGv2Type is `_CG_any`/`_CG_void`
   and the formal expects a typed pointer, emit a
   `bitcast`.  Parallels cg.cc:write_send_arg.
3. `resolve_union_receiver` — when emitting GEP for a
   union-receiver period/setter, pick a non-nil
   component for the cast.

After these three, run the suite, see what remains, and
file the residuals as parity tickets.

---

# Historical record (pre-June 2026)

**v2 LLVM reached 74/0 at one point — full parity with the C
backend.**  Unit tests 105/0.  All test suites green.

Closure landed via F.4.8 with a two-piece shortcut (see
F.4.8 below): a class-default method-slot init now stores
the real function pointer via CG2V_FUN_REF, and `a.x` for a
function-typed field MOVEs the bound `a` instead of
materializing a closure struct (the FA resolves the function
pointer statically at the call site, so the lambda receives
`a` as its sole real arg).  Doesn't generalize to closures
with captured locals or multi-method dispatch — those are
deferred to a future "real closures" project — but the
shortcut suffices for `lambda_closure.py`.

## Bucket F.1 — Quick wins on the issue-019/020/021 chain

Five tests that share the now-fixed list-allocator + opaque-ptr
infrastructure but still need small follow-ups.

| Test | Failure | Fix |
|---|---|---|
| list_or_concat.py | EXEC | COMPILE → EXEC after F.1.1 (commit `4d2080c`).  Now: `[] or [1,2,3]` prints `[]`.  Root cause is `list.__str__` specialization for "might be empty" inputs — analyzer constant-folds `len(self)` to a wrong value.  Same shape blocks list_slicing, list_multiply (str case), list_comprehension. |
| list_slicing.py | EXEC | `_CG_list_getslice` is exported in `libpyc_runtime.a`; some prints work (`[1, 2, 6, 7, 8]` is correct) but most prints return `[]` — same `list.__str__` specialization issue as list_or_concat. |
| list_multiply.py | EXEC | `[1] * 4` works (correct: `[1, 1, 1, 1]`); `[' '] * 3` returns `[]` (expected `[' ', ' ', ' ']`).  Same root cause. |
| list_comprehension.py | EXEC | `[i+j+1 for i in x for j in y]` prints `[0, 0, 0, 0, 0, 0, 0, 0, 0]` (length right, values zeroed).  Comprehension lowering loses the computed element value. |
| sieve.py | EXEC | Wrong prime counts (35539 instead of 9592); index loads/stores into the sieve list misread. |

### F.1.1 progress (commit `4d2080c`)

Two structural fixes landed:

- **Empty literal `[]` allocation** in `lower_send_alloc` — `[]`
  used to fall through to bare `CG2_ALLOC` of a 0-field struct
  (`GC_malloc(0)`).  Now routes through
  `_CG_to_list_runtime(..., 0)` for the header.
- **Struct-name uniqueness** in `build_struct_type` — pyc
  specializes a fresh list/tuple struct per shape but multiple
  CGv2Types of different field counts shared the name "list" /
  "tuple", and LLVM's `getTypeByName` cache silently inherited
  the first struct's body.  Now `<name>.<sym->id>` is unique
  per Sym.  Without this, list_or_concat's COMPILE failure
  (`Invalid indices for GEP pointer type` on field 2 of a
  struct with fewer than 3 fields) didn't surface in the
  pre-F.1 baseline because the second `[1,2,3]` literal
  reused the empty list's struct.

Net-zero on the ratchet (list_or_concat moved COMPILE → EXEC)
but both fixes unblock downstream F.1 / F.2 work.

### F.1.3 SSU accumulation fix (LANDED — v2 61/14)

The four remaining list-family failures (list_or_concat,
list_multiply, list_slicing, list_comprehension) collapsed to
**one root cause**: v2's `materialize_phi_phy` was reading
`succ_pn->cfg_pred_index.get(closer)` from a stale
SSU-time map.  After clone + later optimization, the map's
key for a non-entry predecessor was missing, so `Map<>::get`
returned the 0-default — making every loop-back edge resolve
to `phi->rvals[0]` (the entry pred's source).  In
`list.__str__`'s broken clone, that meant the L240→L145 phi
for `x` stored the entry constant `"["` back into the
accumulator slot every iteration instead of the strcat
result.

The v1 C backend dodges this by calling
`rebuild_cfg_pred_index(f)` in `write_c_fun` (cg.cc:718)
before walking PNodes.  Fix: do the same in v2's
`build_fun_body` before `materialize_phi_phy`.

Tests recovered: list_or_concat, list_multiply, list_slicing,
list_comprehension (list_comprehension was an EXEC fail in the
pre-fix snapshot — counted in the +4 net).  default_args also
flipped: its mutable-default-arg loop was hitting the same
phi-pred-index trap.

**v2 LLVM ratchet:** 57/18 → 61/14.  Bucket F.1 complete.

### F.1 leftover — sieve only

`sieve.py` still EXEC-fails (wrong prime count: 35539 vs
9592).  The fix above didn't touch it, so the root cause is
something else in the index-store path.  Move into F.2 / F.3
investigation as appropriate.

## Bucket F.2 — String / tuple shape work

### F.2.1–F.2.3 LANDED (v2 66/9)

Five of the six tests in this bucket flipped to PASS in one
focused session:

- **F.2.1 string indexing**: `lower_send_index_load` now
  routes `s[i]` to `_CG_C_CALL "_CG_char_from_string"` when
  obj's type is in the `sym_string` family.  Unblocks
  `string_index.py` and `for_over_string.py` (the latter's
  iterator goes through the same path).
- **F.2.2 put_result stores to globals**: `put_result` in
  `cg_ir_v2_emit_llvm.cc` was only handling alloca slots and
  per-fun value_map.  Module-scoped assignments
  (`e = "ab"[0]` writing to `@e`) silently dropped because
  the global path was missing.  Added a `CG2V_GLOBAL` →
  `CreateStore(r, gv)` branch.  Unblocks `string_unpack.py`
  and `multi_type_unpack.py`.
- **F.2.3 heterogeneous-tuple constant index**:
  `lower_send_index_load` dispatches to `CG2_FIELD_LOAD` when
  obj's type is `Type_RECORD` with a constant integer index.
  Mirrors v1's `obj->eN` emit at `cg.cc:289`.  Unblocks
  `tuple_mixed_types.py`.

### F.2.4 remaining — string_format

| Test | Failure | Fix |
|---|---|---|
| string_format.py | EXEC | `"foo %d bar %f" % t`: `__pyc_format_string__` primitive is currently C++-side only. Library-side via `__pyc_c_call__("snprintf", ...)` is the FFI plan's natural next move. |

**Target:** v2 LLVM 67/8 after F.2.4 lands.

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

### F.3.1 LANDED (v2 69/5)

**Root cause** of the broken `y = A()`: the IF1 SEND for
class instantiation is `(sym_primitive, sym_clone, proto)`
per `gen_class_pyda` (`python_ifa_build_syms.cc:614`).  The
`proto` is `cls->self`, the class's prototype global, which
is initialized at startup by the class's ___init___ (line
581 in the same builder — same flow that produces the C
backend's `_CG_f_2160_3(g7)` call from main).

The existing `lower_send_clone` was reading `rvals[1]` —
which is the **`sym_clone` primitive marker**, not the
prototype.  Its global (`@clone`) is uninitialized, so the
earlier "memcpy from null" symptom (B.10.7) was the wrong
diagnosis: the issue wasn't that the prototype was
uninitialized, it was that we were reading the wrong rval.

**Fix:** `lower_send_clone` now uses
`compute_prim_arg_offset(pn)` (= 2 when rvals[0] is
sym_primitive) so the proto comes from `rvals[o]`, not
`rvals[1]`.  CG2_CLONE then memcpys from the genuinely
initialized prototype.

Tests recovered:

- `class_init.py` — class default `x = 2` reaches `y.x`.
- `class_attr_mutation.py` — A.n / B.n inheritance + mutation.
- `attr_augmented_assign.py` — `y.x += 2` reads `2` then writes `4`.
- **Bonus:** `dict_basic.py` also flipped to PASS — dict's
  internal struct construction relied on the same clone path
  that was silently being bare-alloc'd.

**v2 LLVM ratchet:** 66/9 → 69/5.  Bucket F.3 complete.

## Bucket F.4 — Wholesale new infrastructure

### F.4.1 / F.4.2 LANDED (v2 71/3)

- **F.4.1 prim_lsh/rsh dispatch**: `prim_to_binop` was
  missing `P_prim_lsh` → `CG2B_SHL` and `P_prim_rsh` →
  `CG2B_SHR`.  Without these, `x = x >> 1` lowered to a
  silent no-op (the binop fell through to a generic call
  that produced nothing).  `bitwise_operators.py` was an
  infinite loop because `x >> 1` returned `x`; the test now
  passes.
- **F.4.2 typed stride in header-indirection fallback**:
  CG2_INDEX_LOAD/STORE's no-stride fallback used a hardcoded
  i64 stride.  For boolean lists (`[True] * N`), the
  underlying buffer is 1 byte per element, so `gep i64`
  walked 8x too far and read out-of-bounds bytes.  The
  fallback now picks stride from `lvals[0]->type` (LOAD) /
  `rvals[2]->type` (STORE), defaulting to i64 only when
  the result/value type is missing or unsized.  Fixed
  `sieve.py` and unblocked the `dict_basic` adjacent path.

### F.4.3 LANDED — `__pyc_format_string__` (v2 72/2)

`"foo %d" % t` lowered to a `P_prim_primitive` SEND with
name `__pyc_format_string__`, but `lower_send_prim` had no
handler — the CG2_PRIM fall-through emits no LLVM IR, so
`a = "foo %d" % (3,4)` left `@a` null and the program
printed `(null)`.

Fix lives in two places:

1. **`lower_send_format_string`** (cg_normalize_v2.cc) —
   route `__pyc_format_string__` to `CG2_C_CALL
   "_CG_format_string"`.  If rvals[3] (the value) is a
   Type_RECORD, emit a `CG2_FIELD_LOAD` per field into a
   fresh local and append each to the call's rvals (so the
   tuple gets unpacked into varargs).  Otherwise pass the
   value directly.  Mirrors v1's `format_string_codegen`
   (python_ifa_main.cc:80).

2. **Varargs declaration** in `CG2_C_CALL` emit
   (cg_ir_v2_emit_llvm.cc) — when declaring
   `_CG_format_string`, mark the LLVM `FunctionType` as
   varargs with only the first param fixed.  Without this,
   distinct call sites with different arg counts trip
   `Incorrect number of arguments passed to called function`
   at verifyModule.

### F.4.4 PARTIAL — builtins.py (still failing on residual bugs)

Three landings (doesn't flip builtins yet — other bugs
remain — but each is a real semantic fix):

- **`lower_send_lnot`**: `not x` (P_prim_lnot via
  `__pyc_operator__("!", x)`) was being silently dropped —
  the SEND fell through to CG2_PRIM which emits nothing.
  Without the boolean result, the surrounding Code_IF lost
  its rval and the CFG fell into `unreachable` after the
  first loop iteration.  Now emits `x == 0` (CG2_BINOP EQ
  vs zero) which works for any numeric type.
- **int/float → bool in CG2_CAST**: the int-int path used
  Trunc, which kept only the LSB → `bool(10)` returned False.
  Fixed: `icmp ne x, 0` for int→bool, `fcmp one x, 0.0` for
  float→bool.
- **`lower_send_coerce` for P_prim_coerce**: routes `int(x)`,
  `float(x)`, bytearray's internal `coerce(__pyc_char__,
  value)` etc. through `CG2_CAST`.  Required two fixes:
    1. **Follow Type_ALIAS chains**: pyc's `int` is a
       Type_ALIAS for `sym_int64` (`int(s->num_kind) == 0`
       on the alias).  Calling `build_type` on the alias
       returned an opaque ptr instead of `i64`.  Solution:
       `tgt_sym = unalias_type(tgt_sym)` before
       `build_type` — mirrors the FA's own dispatch at
       `analysis/fa.cc:1879`.
    2. **CG2_CAST emit must classify by LLVM type, not CGv2
       kind**: upstream `CG2_INDEX_LOAD` can produce a `ptr`
       LLVM value when the CGv2Type claims `uint8` (in
       bytearray clones where the FA leaves the element
       type unresolved — `__pyc__:507: expression has no
       type`).  Dispatching on the CGv2 kind alone called
       `CreateZExt(ptr, i64)` — invalid IR.  Now derives
       `src_int`/`src_flt`/`src_ptr` from
       `src->getType()->isIntegerTy()`/`isFloatingPointTy()`/
       `isPointerTy()` so a ptr value falls into the
       `src_ptr && dst_int` branch and emits `ptrtoint`.

Tests recovered downstream: `all([1,0,3])`, `bool(10)`,
`int(3.4) → 3`, `float(3) → 3.0`, the for-loop-with-if
pattern.  bytearray byte indexing now lands at correct middle
bytes (`'a'` at pos 1, `'b'` at pos 2 — were random before).

Debug tool added: `PYC_DUMP_LL=/path/to/dump.ll` writes the
pre-verify LLVM module so verify failures can be diagnosed
even when the regular `.ll` output is truncated.

### F.4.5 LANDED — bytearray `@vector("s")` layout

pyc's `@vector("s")` classes (bytearray) use v1's
`T v[0]` flexible-array idiom: the data area lives PAST the
struct's regular fields.  v2 was treating bytearray like a
normal struct and indexing from offset 0 — `x[1] = 'a'`
wrote into the `length` field of the struct prefix.

Three landings:

1. **`CGv2Type::is_vector_struct`** new flag — set in
   `build_struct_type` from `s->is_vector`.
2. **CG2_INDEX_LOAD/STORE emit** — for vector structs, GEP
   by `sizeof(struct)` bytes first (i8 stride) to reach the
   data area, then GEP by `idx*sizeof(element)` with the
   element's LLVM type.
3. **`lower_send_clone` handles P_prim_clone_vector** —
   emits `CG2_C_CALL` to a new runtime helper
   `_CG_prim_clone_vector_runtime(proto, struct_size, extra)`
   (added to `pyc_runtime.c`) that GC_mallocs `struct_size
   + extra` bytes, memcpys the proto, and zero-inits the
   trailing data area.

Result: `bytearray(10)` now allocates `sizeof(struct) + 10`
bytes correctly, `x[1] = ord('a')` writes byte 1 of the data
area, and `print(x)` reads all 10 bytes verbatim.

### F.4.6 LANDED — `__pyc_to_str__` class repr

`print(str)` was emitting nothing because the
`__pyc_to_str__` SEND dropped silently (no handler in
`lower_send_prim`).

Fix: added a handler that compile-time-resolves to a
constant string CGv2Value — `"<class 'X'>"` for meta-types
with a name, `"<instance>"` otherwise.  Mirrors v1's
`to_str_codegen` at `python_ifa_main.cc:93` verbatim.

### F.4.7 LANDED — `-x` unary minus (P_prim_minus)

`bin(-1)` was returning a 50-digit binary string only when
`bin(10)` was ALSO present in the file.

Root cause: unary minus (P_prim_minus, from
`__pyc_operator__("-", x)`) was being silently dropped, so
`x = -x` inside the negative branch of `bin()` was a no-op.
The next-iteration loop `while x > 0` then iterated over -1's
two's-complement bits (signed compare against 0 is false for
negative, BUT the FA's specialization had inlined the cmp).

The bin(-1)-isolated case worked because the FA constant-
folded the call to start with `x=1` directly.  With `bin(10)`
also present, the FA generated ONE shared clone taking `i64
%x` with the runtime if/else branch — exposing the missing
unary-minus emit.

Fix: `lower_send_neg` emits `0 - x` via `CG2_BINOP SUB`.
Symmetric in shape with the earlier `lower_send_lnot` fix
(F.4.4).

### F.4 remaining (2 tests)

| Test | Failure | Blocking gap |
|---|---|---|
| builtins.py | EXEC | See F.4.4 notes above. |
| lambda_closure.py | EXEC | Full closure representation: method binding (`a.x` → bound closure), closure call dispatch, captured-var struct. |

dict_basic flipped to PASS as a bonus from F.3.1; default_args
flipped from F.2.2 (put_result-to-global).  Of the originally
listed 5 F.4 tests, only lambda_closure remains.

**Target:** v2 LLVM 74/0.  **Estimate:** builtins ~2-3
sessions of small fixes; lambda_closure ~10-15 hours (full
closure design); string_format ~3-5 hours (snprintf FFI plumbing).

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
