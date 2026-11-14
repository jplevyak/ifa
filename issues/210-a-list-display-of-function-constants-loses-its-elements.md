# 210 — A list display of function constants loses its elements

**Status: open.** Filed 2026-11-14, found while reproducing
[200](200-llvm-backend-parity.md) A3's lambda divergence. Fixture:
`tests/lambda_list_literal.py` (`.known_issue`).

## Symptom

```python
fs = [lambda x: x - 7, lambda x: x * x]
for k in fs:
    print(k(5))          # CPython: -2, 25
```

- **C backend:** a C++ compile error.
  `t6->e0 = _CG_f_18149_8;` assigns a function to the element's
  `_CG_pf8` (`void *`) member with no cast.
- **LLVM backend:** `_CG_prim_tuple_list_internal` and `_CG_to_list_runtime`
  are emitted, and no element is ever stored. The dispatch on `k` then
  matches no candidate. Before 200 A3 its fallback fell through and
  printed `0 0`; it now traps (`poly_no_branch_matched`).

The same holds for `[inc, lambda x: x - 7]`. Two `def`s in a list work on
both backends. A `def` reaches the display through its global slot
(`g1 = (_CG_function)&_CG_f_…`), so the display stores a variable. A
lambda reaches it as a function CONSTANT, a Var whose `sym->fun` is set
and which has no backing variable.

## Mechanism, as far as read

- **C:** `cg.cc`'s tuple/list construction writes each element with
  `cg_get_string(rval)` (or a nil-union cast). For a function constant,
  that is the bare function name. `simple_move` and the global
  initialisers write `(_CG_function)&name` instead.
- **LLVM:** function-typed values are excluded from signatures and storage
  (the issue 036 comment in `emit_send_call_impl`'s bare-callable pass),
  so `value_for_var` on the constant returns nothing, and the element
  store is skipped. The bare-callable pass already substitutes the
  function's address for a constant callee
  (`TheModule->getFunction(v0->sym->fun->cg_string)`). The element store
  has no such step.

## Plan

One rule for a function constant used as a VALUE, on both backends: its
address, typed `_CG_function` / `ptr`. Find every place a Var can be
read as a value (element stores, field stores, call arguments, returns)
and route it through that rule, rather than patching the display alone.
The C side probably wants `c_rhs`; the LLVM side `value_for_var`.

**Stop condition:** `tests/lambda_list_literal.py` prints CPython's output
on both backends, and its `.known_issue` is deleted. If `value_for_var`
cannot return an address for a function constant without breaking the
issue-036 exclusion, say what in that exclusion depends on it.

## Verification

`make test` on both backends; the corpus `check` sweep unchanged.
