# 109 — tuple slicing is unimplemented: `t[0:2]` aborts

**Status: FIXED 2026-08-18** (tuple slicing works; `sunfish` still needs
[018](../../issues/018-dict-mixed-key-types-boxing-failure.md)). Found
while digging into `sunfish`'s crash.
Repro: `tests/tuple_arity_union_slice.py` (`.known_issue`) — and a much
smaller one below.

> **CORRECTION.** This issue was first filed as *"slicing a
> `{tuple(N), tuple(M)}` union aborts"*, blaming mixed arity. **That was
> wrong**, and the correction is the whole point of the issue:

## Two lines are enough

```python
t = (1, 2, 3, 4)
print(t[0:2])
```

| | result |
|---|---|
| CPython | `(1, 2)` |
| **pyc** | **SIGABRT** — `runtime error: list index type mismatch` |

No union. No dict. No differing arity. **A plain tuple, a constant
slice.**

## Cause

`__pyc_getslice__` is defined **only on `class list`** in
`__pyc__/04_sequence.py`. `class tuple` has `__getitem__`, `__setitem__`,
`__iter__`, `__len__`, `__contains__` — but no slice method — so `t[i:j]`
resolves to something that cannot index a tuple and the emitted guard
fires.

`list` slicing works fine (`[1,2,3,4][0:2]` → `[1, 2]`).

## What the mixed-arity theory got wrong

The original diagnosis came from `sunfish`, where the sliced value *does*
have unioned arity, and from a `DISPATCH FAIL` line showing two
`__pyc_getslice__` candidates. But the controls disprove it:

| variant | result |
|---|---|
| mixed-arity tuples in a dict, sliced | aborts |
| **same-arity** tuples in a dict, sliced | **also aborts** |
| **plain tuple, constant slice, no union at all** | **also aborts** |
| mixed-arity tuples, *iterated* and `len`-ed (no slice) | **works** |

Arity is not the variable; **slicing** is. The union in `sunfish` is
incidental, exactly as the tuples in
[104](closed/104-unify-list-and-tuple-in-analysis.md) turned out to be
incidental passengers in a degenerate type.

So [104](closed/104-unify-list-and-tuple-in-analysis.md)'s conclusion —
that mixed-arity tuples cause no corpus failures — **stands after all**.
The correction I made to it on the strength of this issue has itself been
retracted.

## FIXED 2026-08-18 — and the answer was "lists can do it, so can tuples"

`class tuple` now has `__pyc_getslice__`, mirroring `list`'s. The reason
that works is the point:

> `__pyc_getslice__` reads `sizeof_element`, which **populates the
> tuple's generic element**. A populated element makes `tuple_able()`
> false, so `clone.cc` gives that CreationSet **list layout** — *unknown
> arity, known element type*, exactly the representation a list has.

The only thing missing was that `sym_tuple` had **no element sym to
populate**, which is why the first attempt segfaulted the compiler:
`sizeof_element` on a type with no element. `PYC_TUPELEM` — built in
[104](closed/104-unify-list-and-tuple-in-analysis.md) as an experiment
and left off — supplies it, and is **now on by default** because it is
load-bearing rather than experimental.

All seven forms match CPython, including the runtime-bound cases whose
result arity is not static:

```
t = (1, 2, 3, 4);  n = len(sys.argv)
t[0:2] -> (1, 2)     t[1:]  -> (2, 3, 4)    t[:3] -> (1, 2, 3)
t[:]   -> (1,2,3,4)  t[0:n] -> (1,)         t[n:] -> (2, 3, 4)
("a","b","c")[0:2] -> ('a', 'b')
```

`tests/tuple_slice.py` pins them.

### Measured cost

Corpus, like-for-like over the 69 programs that reach FA in both runs:

| | before | after |
|---|---|---|
| exit codes | — | **no regressions** |
| `pass_limit_hit` | — | **no changes** |
| `ess` | 24 027 | 24 497 (**+2.0 %**) |
| `css` | 83 594 | 85 219 (**+1.9 %**) |
| analysis time | 243 s | 252 s (**+3.5 %**) |
| violations | 3 895 | 5 995 (+53.9 %) |

**The entire violation increase is `plcfrs`** (2232 → 4332 — the only
program whose count changes at all), and `plcfrs` does not compile either
way. Giving every tuple an element sym pulls tuples into container-keyed
paths, which is where that comes from.

So: a basic Python operation that used to abort at runtime now works, for
+2 % contours and +3.5 % analysis time, with no exit-code or pass-limit
regressions. Worth it.

### Still open

The `sunfish` reproducer (`tests/tuple_arity_union_slice.py`) now fails
**differently** — `sizeof_element of non-container type '<anonymous>'`,
which is [018](../../issues/018-dict-mixed-key-types-boxing-failure.md),
the scalar/container union family. Tuple slicing is fixed; that program
needs 018 as well.

## Superseded: the first attempt, and why it is not one line

Adding a `tuple.__pyc_getslice__` mirroring `list`'s — on the reasoning
that `cg.cc` builds every tuple with `_CG_prim_tuple_list`, which sets a
real list header, so `_CG_list_getslice` should apply — **crashes the
compiler** (SIGSEGV during compilation). Reverted.

The real difficulty is the return type. `list.__pyc_getslice__` returns a
`list`; a tuple slice must return a **tuple**, and with a runtime range
its arity is unknown — which pyc's fixed-arity record tuples cannot
express. `sunfish`'s `table[i*8:i*8+8]` has a runtime start and a
constant length, so a constant-length special case would cover it, but
the general case wants the variable-length tuple representation
[104](closed/104-unify-list-and-tuple-in-analysis.md) prototyped.

## Verification plan

- `t = (1, 2, 3, 4); print(t[0:2])` prints `(1, 2)`.
- `tests/tuple_arity_union_slice.py` prints
  `(0, 1, 2, 0) (0, 5, 6, 0)`; delete its `.known_issue` tag.
- `sunfish` compiles **and runs**.
- `list` slicing is unaffected.


## How shedskin handles sunfish — and a regression my fix caused

shedskin translates and **builds** `sunfish` cleanly, no warnings. The
generated C++ says exactly what it does with the table:

```cpp
dict<str *, tuple<__ss_int> *> *directions, *pst;
...
table->__slice__(3, i*8, i*8+8, 0)
```

`pst` is a dict of **variable-length homogeneous `tuple<__ss_int>`** — the
64-element and 120-element tuples are *the same type* — and `__slice__`
returns that same type. That is precisely the representation pyc now uses
for a sliced tuple, so the approach is confirmed correct by the reference
implementation.

### But sunfish went backwards under pyc

| | `sunfish` |
|---|---|
| before the `divmod` fix | compile error (missing builtin) |
| after `divmod`, before tuple slicing | **compiled**, then SIGABRT at runtime |
| **after tuple slicing (now)** | **compile error** — `sizeof_element of non-container type '<anonymous>' (in __pyc_getslice__)` |

Verified by rebuilding at the previous commit: `compile_rc=0` there,
`compile_rc=1` now. The failure is inside the **new**
`tuple.__pyc_getslice__` (`__pyc__.py:1251` maps to it), where a
`<anonymous>` union receiver reaches `sizeof_element`. `sunfish` slices
both tuples (`table[i*8:i*8+8]`) and **strings** (`board[:i]`,
`self.board[::-1]`), so a slice site whose receiver FA could not separate
now lands in the tuple method.

### Why the corpus sweep missed it

The sweep compared against `rc_107.txt`, taken **before** the `divmod`
fix — when `sunfish` was already `rc=1` for a different reason. So
"no exit-code changes" was true and useless: the program had regressed
between the two snapshots and back to the same code. **A baseline must be
re-taken after every landed change**, not reused across them.

### Is it a net loss?

Arguably not, by
[102](102-corpus-programs-compile-then-abort-at-runtime.md)'s own
argument — a compile-time diagnostic beats a runtime SIGABRT, and
`sunfish` produced no useful output either way. But it is a real
behaviour change in the wrong direction for that program, it was not
caught by the sweep, and it should not be discovered later as a surprise.

The underlying cause is [018](../../issues/018-dict-mixed-key-types-boxing-failure.md):
a `{tuple, str}` slice receiver has no single representation. shedskin
avoids it because `str` and `tuple<T>` are separate C++ types with
separate `__slice__` instantiations, and its analysis keeps the receiver
monomorphic.