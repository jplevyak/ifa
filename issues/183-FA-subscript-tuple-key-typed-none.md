# 183 — a tuple subscript key `board[row-1, column]` is typed `None`

**Status:** open. Found 2026-10-05, when `life` started compiling (its
earlier refusal, `map(process, generator(...))`, was fixed by the
`make_seq` and convergence work in the same change). Reproduces on the
previous HEAD (`5eb61058`) too, so it predates that change, which only
uncovered it. **Silent: no diagnostic, exit 0 from pyc, then a run-time
`matching function not found` abort.**

## Repro

```python
from collections import defaultdict

def add(board, pos):
    row, column = pos
    return board[row-1, column] + board[row+1, column]

def snext(board):
    new = defaultdict(int, board)
    for pos in list(board):
        near = add(board, pos)
        if near == 1:
            new[pos] = 1
    return new

board = defaultdict(int)
board[(1, 1)] = 1
board[(2, 1)] = 0
print(len(snext(board)))     # CPython: 2
```

pyc aborts in `dict.__contains__(self, _CG_nil_type key)`. In the emitted
`add`, both `tuple.__getitem__` results of `row, column = pos` are
discarded, and every key is passed as a constant `NULL`:

```c
t2 = _CG_f_.../*defaultdict::__getitem__*/(t4, NULL);
```

FA types the key formal of that `defaultdict.__getitem__` contour as
`None`, so the `(row-1, column)` tuple the frontend builds for the
subscript reaches it as nothing but `None`.

## Not yet located

Where the `None` comes from. Candidates to check first, per AGENTS.md's
"locate before acting":

- `defaultdict.__init__`'s `initial=None` default with `if initial: for k
  in initial: ... initial[k]`. A `None` arm of `initial` subscripted on a
  path FA cannot prove dead would put `None` into the key channel of
  `__getitem__` (`pyc_lib/collections.py`).
- `pos` from `list(board)`: `IFA_DBG_FUNES=__getitem__` on the repro shows
  whether the key formal ever holds the tuple at all, or only `None`.

## Verification

The repro prints `2`, and `shedskin_examples/life` matches CPython
(currently `rc=134`, sweep `check__default__5eb61058+c61232b1`).
