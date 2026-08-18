# 109 — slicing a `{tuple(N), tuple(M)}` union aborts: sunfish's crash

**Status:** open, found 2026-08-18 while digging into `sunfish`'s runtime
crash. Repro: `tests/tuple_arity_union_slice.py` (`.known_issue`), seven
lines.

**This corrects [104](closed/104-unify-list-and-tuple-in-analysis.md)**,
which concluded mixed-arity tuples "cause no failures in the corpus".
They do — `sunfish` is one.

## Symptom

```python
pst = {"P": (1, 2, 3, 4), "N": (5, 6, 7, 8)}
for k, table in list(pst.items()):
    pst[k] = table[0:2]
    pst[k] = (0,) + pst[k] + (0,)
print(pst["P"], pst["N"])
```

| | result |
|---|---|
| CPython | `(0, 1, 2, 0) (0, 5, 6, 0)` |
| **pyc** | compiles, then **SIGABRT**: `runtime error: matching function not found` |

## Cause

`pst[k]` is reassigned with a **different-length** tuple inside the loop,
so the dict's value type — and therefore `table` — becomes
`{tuple(N), tuple(M)}`: *same element type, different arity*. In pyc each
arity is a distinct fixed-arity record type, so slicing that value has
two candidate clones:

```
DISPATCH FAIL in __main__: fns=2 | cand=__pyc_getslice__ cand=__pyc_getslice__ r1=_:?
```

Codegen cannot discriminate them (their C-level receiver type is the
same), so `cg.cc:2055` emits an abort stub and the program dies when the
slice executes — [102](102-corpus-programs-compile-then-abort-at-runtime.md)'s
class B.

In `sunfish` the same shape appears at `sunfish.py:74-77`: a table of
64-element tuples padded to 120 elements in place.

```python
for k, table in list(pst.items()):
    pst[k] = sum((padrow(table[i*8:i*8+8]) for i in range(8)), ())
    pst[k] = (0,)*20 + pst[k] + (0,)*20
```

## Why 104 missed it

104 measured mixed-arity impact with `IFA_DBG_ARITYVIOL`, which counts
**violations** whose operand type holds tuples of differing arity. This
failure produces **no violation at all** — it is a *dispatch* failure,
resolved at codegen. The probe was looking in the wrong place, and the
resulting "no corpus failures" conclusion was wrong.

The lesson generalises: a violation count does not see failures that
codegen turns into abort stubs. `PYC_DBG_DISPATCH` sees them; violation
probes do not.

## Fix direction

This is precisely the case for a **variable-length homogeneous tuple**
— shedskin's `tuple<T>` versus its fixed `tuple2<A,B>`. If tuples of the
same element type shared one type regardless of arity, `table` would have
a single type and one `__pyc_getslice__`.

[104](closed/104-unify-list-and-tuple-in-analysis.md) built exactly that
representation (`PYC_TUPELEM` + `PYC_TUPLE_AS_LIST`, both off) and closed
as "correct mechanism, no demonstrated need". **The need is now
demonstrated**, so it is worth re-testing those flags against this
reproducer — with the caveat recorded there that the mechanism is
post-FA in `clone.cc`, while this failure is a *dispatch* decision, so it
may well not be reached by them.

## Verification plan

- `tests/tuple_arity_union_slice.py` prints `(0, 1, 2, 0) (0, 5, 6, 0)`;
  delete its `.known_issue` tag.
- `sunfish` compiles **and runs** (it already compiles as of the
  `divmod` fix).
- ~~Re-check `PYC_TUPELEM=1 PYC_TUPLE_AS_LIST=1` against the reproducer~~
  **Done, 2026-08-18: they do NOT fix it** — the reproducer still aborts
  (`run_rc=134`) with both flags on. The caveat above was right: those
  flags change the *representation* in `clone.cc`, after FA, whereas this
  is a **dispatch** decision made from the FA-level type. An
  arity-independent tuple type would have to exist *during* FA for
  `__pyc_getslice__` to have one candidate instead of two.
