# 174 — a dispatch violation was not backtracked to the load over a union receiver that made it

**Status: FIXED 2026-09-26.** `shedskin_examples/richards` failed to type:

```
richards.py:272: error: illegal call argument type 'h' illegal: ( DeviceTaskRec IdleTaskRec WorkerTaskRec )
                    h.workInAdd(pkt)
```

It now compiles, matches CPython, and runs its timed half in 40 s against
CPython's 207 s.

## The shape

```python
class Task(TaskState):
    def __init__(self, i, p, w, initialState, r):
        self.handle = r                    # each subclass stores its OWN record
    def runTask(self):
        ...
        return self.fn(msg, self.handle)   # one method, four receiver classes

class HandlerTask(Task):
    def fn(self, pkt, r):
        h = r
        assert isinstance(h, HandlerTaskRec)
        h.workInAdd(pkt)
```

The fields are precise: each task class's `handle` holds exactly its own
record (`IFA_DBG_CSVARS`). The union is made at ONE place: `runTask` had a
single contour whose `self` spanned all four task classes, so the load
`self.handle` was the union of four records. That union was then passed
to every class's `fn`. The receiver and the argument are correlated
(a HandlerTask always carries a HandlerTaskRec), and one contour cannot
express that.

shedskin separates them. Its generated `HandlerTask::fn` begins with
`HandlerTaskRec *r = (HandlerTaskRec *)__r;` and folds the isinstance
assert to `ASSERT(True, 0)`, which it can only do after analysing
`runTask` per receiver class. It then emits one C++ method with the hoisted
`TaskRec *` parameter. The class union is legitimate polymorphism (AGENTS.md:
hoist it). The union leaking into the ARGUMENT is not.

## Root cause

The demand was there (a dispatch that cannot resolve) and never reached
the contour that made the union:

1. The violation is recorded on the `self` captured by `h.workInAdd`'s
   bound-method closure, a CreationSet-contoured AVar. Stage 5
   (`collect_violation_imprecisions`) turns a violation into a split
   candidate only if the violating AVar is itself a load over a union
   receiver (`v->av->container`), or via ifa/146 E if it sits in an
   EntrySet. This one is neither, so "2 violations -> 0 imprecisions".
2. The union was made three hops upstream: runTask's `self.handle` load
   -> fn's formal `r` -> `h` -> the closure. Nothing walked there.

## Fix

In `collect_violation_imprecisions`, when the violating AVar is not
itself such a load, walk its value flow backward (following every writer
that contributes part of the offending union) to the first
`P_prim_period` load whose receiver spans several CreationSets, and hand
that receiver's FORMAL to the type splitter. Two details were measured:

- **The formal, not the in-body `self`.** A load's `container` is the
  SSU-renamed `self` used inside the body. `split_ess_for_type` acts only
  on formals (`Var::is_formal`, set at pattern-build time, so valid during
  FA), and skipped the in-body AVar as "non-formal rval".
- **Contributing writers, not only union-typed ones.** In the reduced test,
  `h` is the join of the arms of `assert isinstance(h, ARec)` (pyc rejoins
  the failing arm), whose inputs are each single-typed. Following only
  unions stopped there.

The split itself is `split_edges`, which separates the receiver's
CreationSets in two groups per pass. `runTask` becomes four contours, one
per task class.

`PYC_LOADBT=0` disables it; with it off, both richards and
`tests/receiver_union_correlated_field.py` fail again.

## Related

`assert isinstance(h, X)` does not narrow `h` for the code after it,
because the failing arm (which raises) is rejoined. That is why the walk
has to cross a join of narrowed arms. Narrowing on an assert would give
the same answer here without a split. It is a separate improvement, not a
substitute: the split is what makes each `fn` contour precise.

## Left open

- **richards compiles with one C warning**, `implicit conversion of NULL
  constant to '_CG_int64'`, on `__main__`'s dead fall-through after
  `__pyc_unhandled_exception__()` (the failing-assert path). The reply
  value is the None constant (`NULL`) in a function whose C return type
  is `_CG_int64`, and that function's return Var has no `type`, so a
  guard keyed on the return type in `c_rhs`'s callers cannot see it.
  Cosmetic, dead code. It is reachable only now that richards compiles.
- **The intermittent pyc segfault compiling tonyjpegdecoder: FIXED,
  unrelated to this issue.** Seen in two sweeps (first at `cdff0749`, before
  this change) and about 1 in 50 compiles under parallel load. A
  preloaded SIGSEGV backtrace caught it in `inject_tuple_methods ->
  dparse_python_buf_to_ast -> py_node_indent`. The grammar's indent
  actions scan BACKWARD from a token over spaces/tabs to the previous
  newline with no lower bound, so for a token on a buffer's first line they
  read before the allocation, and ran off into unmapped memory when the
  bytes there happened to be spaces or tabs. Every buffer handed to dparse
  now carries a leading '\n' sentinel (`prepare_parse_buffer`,
  python_parse.cc). 96 parallel compiles, 0 crashes.

Corpus `-m check` (`fd35a71d+bbba17b7` vs `cdff0749+2f333ed4`): compile
failures 30 -> 28. richards compiles and runs (its CPython reference
exceeds the 120 s cap; run standalone, the output matches). tonyjpegdecoder
compiles and its binary now finishes, and its output DIFFERS from CPython.
That was never checked before, because the binary always timed out; it is
not attributable to this change: the backtrack never fires on
tonyjpegdecoder (same contour count), its output is identical with
`PYC_LOADBT=0` apart from the timing line, and the only
difference from CPython is the file object's repr (`<instance>` vs
`<_io.BufferedReader name='tiger1.jpg'>`).
