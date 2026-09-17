# 165 — `None` reaching an operation is silently accepted and read as the zero value

**Status: OPEN.** Pre-existing; found while measuring
[164](164-nil-union-at-a-primitive-argument-is-a-nullable-pointer.md),
which did not introduce it and does not widen it.

## Symptom

```python
def f(n):
    a = [None] * n
    a[0] = "x"
    return a[1] + "y"      # a[1] IS None
print(f(3))
```

| | result |
| --- | --- |
| CPython | `TypeError: unsupported operand type(s) for +: 'NoneType' and 'str'` |
| pyc | prints `xy`, exit 0 |

No warning, no error, no runtime check. The `None` is represented as a
null pointer and the string primitive reads it as `""`.

This reproduces on today's `main` **and on every build before
[164](164-nil-union-at-a-primitive-argument-is-a-nullable-pointer.md)** —
it goes through the RECEIVER position, which has always read the
nil-stripped `->type` projection. 164 made the ARGUMENT position agree
with the receiver; the residual wrongness is that the position they now
agree on has no runtime check.

## Root cause

`{None, T}` for pointer-shaped `T` is represented as a nullable `T`
(issue 060), and every consumer of `->type` sees only `T`. That is the
right *representation* — see 164, and shedskin compiles the same programs
with `str *` and `NULL`. What is missing is the *check*: nothing tests
the pointer for null before the operation, so a `None` that really does
arrive is read as the zero value instead of raising.

## Why this is not 164's business

164 is about a `None` that provably never arrives — `[None] * n` followed
by a full overwrite, where the union is temporal and the program is
correct. Rejecting those at compile time is a false positive on valid
CPython programs, and it was rejecting them on ONE of the two operand
positions.

This issue is the opposite error on the other position: a `None` that
DOES arrive produces a wrong answer rather than a `TypeError`. Fixing it
is a runtime check, not a type rule.

## Proposed fix

Under `fruntime_errors` (the default), emit a null test on the nil member
of a `{None, T}` union at the point of use and raise `TypeError`, the way
CPython does. Under `--strict`, keep a compile-time refusal — CLAUDE.md's
rule for permissive-only devices: *"an automatic coercion is a
PERMISSIVE-only device ... `--strict` must error on anything that would
otherwise require boxing"* (see
[145](145-numeric-coercion-is-not-gated-on-permissive-mode.md) for the
same shape).

The cost to measure first is how many such checks survive optimisation —
most nullable unions in the corpus are `self.next = None` linked
structures where the null test is already present in the source as an
`is None` guard, and `RP_IsNotNilType` narrowing (fa.cc:4383) should
remove the redundant one.

## Verification plan

- The repro above must print CPython's `TypeError`.
- `tests/nil_union_prealloc.py` must keep passing — a `None` that never
  arrives must cost nothing observable.
- Corpus: no program may regress from match to run-fail. `richards`,
  `life` and `sokoban` carry the most nullable class fields and are the
  ones to watch.
