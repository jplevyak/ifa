# 125 — sunfish: every `dict::__setitem__` clone degenerates to `getter not resolved`

**Status:** open, filed 2026-09-02 while doing
[090](closed/090-CGEN-tuple-arity-cant-vary-across-loop-iterations.md)'s
follow-up. Succeeds 090 as the holder of sunfish's residual failure —
090's own named blocker is **refuted**, see "What 090 said, and why it
was wrong" below.

**Affects:** `ifa/codegen/cg.cc` `P_prim_getter` (~line 777) is where it
surfaces; the cause is upstream in FA/clone, which produces a
`dict::__setitem__` clone whose key and value formals are gone.

**Severity:** compiles with warnings and exit 0, then aborts at runtime.
Class B of [102](102-corpus-programs-compile-then-abort-at-runtime.md).

**Reproducer:** `shedskin_examples/sunfish/sunfish.py` only. **No minimal
repro yet** — four hypotheses tested and refuted, see below. This is
filed without one deliberately, because the measurements are worth
recording and the next person should not repeat them.

## Symptom

```
$ pyc -D . shedskin_examples/sunfish/sunfish.py -o sf     # rc=0, warnings only
$ ./sf
sf: sunfish.py.c:1580: _CG_void_type _CG_f_7501_60(_CG_ps27272):
    Assertion `!"runtime error: getter not resolved"' failed.
```

The sweep records this as `compile_rc=0 run_rc=134`
(`check__default__635d26b6+c1a28ccc`).

## What the emitted C shows

`_CG_f_7501_60` is a `dict::__setitem__` clone. Its whole body is:

```c
_CG_void_type _CG_f_7501_60/*dict::__setitem__*/(_CG_ps27272 a1) {
  ...
  t13 = a1;
  t11 = t13;
 L499:;
  assert(!"runtime error: getter not resolved");
  assert(!"runtime error: matching function not found");
}
```

Two things to notice, both measured:

1. **The clone takes ONE argument.** A `__setitem__` is
   `(self, key, value)`; this has only the receiver. The key and value
   formals were dropped.
2. **This is not one bad clone — it is all of them.** Every
   `dict::__setitem__` in the emitted C (`_7501_24`, and `_7501_60`
   through `_7501_70` — 12 clones) has arity 1, and they are called
   consecutively on the same fresh dict:

   ```c
   t51 = _CG_f_15947_27/*__new__*/();
   _CG_f_7501_60/*dict::__setitem__*/(t51);
   _CG_f_7501_61/*dict::__setitem__*/(t51);
   ...
   ```

   That is a dict LITERAL being built one entry at a time. 12 = the 6
   entries of `pst` plus the 6 of `directions`, sunfish's two
   `str`-keyed table literals.

So both dict literals lower to `__setitem__` calls whose key and value
have no type, the bodies collapse, and the first assert fires.

## The trigger is in `main()`, and it is NOT the containment loop

Cut sunfish at successive top-level boundaries, appending a use so the
tables stay live. Oracle: the emitted C contains `getter not resolved`.

| candidate | lines | result |
|---|---|---|
| through `class Position` | 260 | compiles, **0** asserts |
| through `class Searcher` | 418 | compiles, **0** asserts |
| through `parse`/`render`/`print_pos` | 437 | compiles, **0** asserts |
| **+ `main()`** | 484 | compiles, **17** asserts |

`main()` is necessary. But cutting `main()` down does not isolate it —
each reduced variant hits a *different* failure:

| variant of `main()` | result |
|---|---|
| `move = None; while move not in hist[-1].gen_moves(): ...` | compiles, **0** asserts |
| `for _depth, (move, score) in searcher.search(hist[-1], hist)` | **compile error** — `'x' has mixed basic types:( closure tuple int64 str )` |
| both together | **compile error** — `assigning to '_CG_ps26692' from incompatible type '_CG_nil_type'` |

Three neighbouring programs, three distinct diagnostics. That is the
finding: sunfish's residual state is a **cluster** of union/representation
failures around `move` (which is `None`, then a 2-tuple from `parse`,
then rebound by a nested for-target unpack) and around
`Searcher.search`'s yielded `(depth, (move, score))` — not one bug with
one witness.

## What 090 said, and why it was wrong

[090](closed/090-CGEN-tuple-arity-cant-vary-across-loop-iterations.md)
closed with a "sunfish is NOT fully cleared" section naming the blocker
as `sunfish.py:448`, `unresolved call '__not__'` — containment on a
GENERATOR (`move not in hist[-1].gen_moves()`), and said it "wants its
own issue". Both halves are refuted:

- **The symptom is gone.** `__not__` appears **0 times** in sunfish's
  current compile output, and nothing is reported at line 448 at all.
- **The shape is not the trigger.** Reproducing exactly that construct
  (variant 1 above — `move = None` and the `gen_moves()` containment
  loop, on the real `Position`) compiles with **zero** asserts.

So the issue 090 wanted filed does not exist. This one replaces it.

## Four refuted hypotheses (do not re-test these)

All compile clean and print the CPython answer:

```python
d = {'a': (1,2,3,4), 'b': (1,2,3,4,5,6,7,8)}   # mixed-arity tuple VALUES
for k, t in list(d.items()): print(k, t)        # items() tuple unpacking
for k in list(d.keys()): d[k] = (0,) + d[k] + (0,)   # self-referential mutation
```

and a faithful transcription of sunfish's own table-padding loop —
`sum((padrow(table[i*4:i*4+4]) for i in range(2)), ())` with a `k`-capturing
lambda, a genexpr inside `tuple()`, a `piece[k]` lookup and a slice —
prints the correct padded tuple.

The mixed-arity one matters most: 090 was *named* for tuple arity, and
arity in a dict value position is demonstrably fine.

## Verification plan

1. `pyc shedskin_examples/sunfish/sunfish.py` emits no
   `getter not resolved` assert, and the binary runs.
2. A minimal repro exists and is pinned in `tests/`. Reduction against
   the "emitted C contains `getter not resolved`" oracle is set up and
   was abandoned only on cost (each trial is a full sunfish compile,
   ~700 trials worst case); `ifa/issues/repro/ddmin.py` with
   `DDMIN_CHECK` pointing at that oracle is the way back in, and it
   should start from the 484-line `main()`-inclusive cut above rather
   than the whole file.
3. The two neighbouring compile errors above are separately explained —
   they may be the same cause or two more.

## What this unblocks

`sunfish` is one of the corpus's `compile_rc=0 run_rc≠0` programs, the
population [102](102-corpus-programs-compile-then-abort-at-runtime.md)
exists to shrink and the one that
[corpus-compiles-but-crashes](102-corpus-programs-compile-then-abort-at-runtime.md)
makes the case is invisible to `-m compile` sweeps. It is also the last
thing standing between sunfish and a working chess engine, having
already survived tuple slicing
([109](closed/109-mixed-arity-tuple-slice-dispatch.md)), `divmod`, and
090's two library fixes.
