# 173 — keyword arguments to methods bind two slots late; `x is True` folds to False

**Status: FIXED 2026-09-26.** Both are silent wrong answers. Found root-causing
why `shedskin_examples/sat` failed to type (`illegal call argument type 'cause'
illegal: str` at `cause.cacl_reason2()`), which took both fixes to compile and
run: it now prints CPython's output and runs faster than it (26 s vs 35 s for
the timed half).

## 1. A keyword argument to a method bound to the previous formal

```python
class S:
    def f(self, lit, reason=None, reason_txt=None): ...
s.f(1, reason_txt="learnt")     # pyc: reason="learnt", reason_txt=None
```

A plain function binds correctly; only calls through a bound method were
wrong. In sat, `self.enqueue(lit, reason_txt="considering")` stored the string
in `var_info.reason`, which later reached `cause.cacl_reason(p)` as the
receiver. That is the typing failure.

**Cause** (`partial_application`, fa.cc). A call through a closure assembles
the argument vector `[fun, captured values..., call args...]` and a parallel
names vector. It placed the call's names at `def->rvals.n + i - 1`, where
`def` is the PNode that made the closure. For a partial application that
equals the captured count. For a bound method `def` is the `obj.f` period
send, rvals `[__operator, obj, ., f]` = 4, while the closure captures
`[fun, self]` = 2, so every name landed two slots late and the pattern
match resolved it to the formal before.

**Fix:** place names at `cs->vars.n + i - 1` and size the vector from
`cs->vars.n`. `def`'s own names are copied only when its arity matches the
closure's.

## 2. `r is True` folded to False on a runtime bool

```python
def f(x):
    if x < 0: return False
    return True
r = f(3)
r is True                        # pyc: False (a compile-time constant)
```

**Cause** (`P_prim_is`, fa_prims.cc). The transfer folds to False when the
two operands' CreationSets are disjoint. That is sound for objects and wrong
for values: the constant `True` and the runtime `bool` are different
CreationSets, yet a runtime bool can be True. Wherever `r`'s constants had
been stripped, `r is True` became the constant False. sat's `assert r is
True` then failed on a True (the empty `Unhandled exception:`).

**Fix:** a constant overlaps the non-constant CreationSet of its own type.
Two different constants stay disjoint. The C backend then emits `is` on
numeric operands as a plain `==`, since casting a `_CG_bool` or `double` to
`void *` warns or does not compile.

## Pinned by

`tests/method_keyword_argument_binding.py`,
`tests/is_true_on_runtime_bool.py`.
