# 178 — route 4 cannot separate two creation points of a class built through one constructor

**Status:** open. Filed 2026-09-30 from
[132](132-arity-is-representation-not-provenance.md), whose synthetic test it
blocks.

## Symptom

```python
class Pot:
    def __init__(self, n, labels=None):
        self.n = n
        if labels == None:
            self.labels = []
            for i in range(n):
                self.labels.append(i * 10)
        else:
            self.labels = labels
    def show(self):
        r = 0
        for j in range(self.n):
            r += len(str(self.labels[j]))
        return r
class Wave:
    def __init__(self):
        self.pot = Pot(3)
    def total(self):
        return self.pot.show()
def run():
    w = Wave()
    print(w.total())
    w.pot = Pot(1, ["abcd"])
    print(w.total())
run()
```

CPython prints `5` then `4`. pyc refuses it: `expression has mixed basic
types: ( int64 str )` at `self.labels[j]`.

## Cause

Both `Pot(...)` calls produce ONE `Pot` CreationSet (`IFA_DBG_FUNES=show`:
one contour, `self = {Pot#…}`), so its `labels` member holds the int-list
and the str-list. `self.labels.append(i * 10)` then runs through that merged
member and writes ints into BOTH lists' elements; both `list.__getitem__`
contours return `{int64, str}`. The union is real by then, and nothing
downstream can separate it.

The record should have split. Route 4 sees it (`IFA_DBG_CSDEFSPLIT`:
`cs=… sym=Pot defs=2`) and declines: `1 group: every creation point on the
same assign sets`. Its partition key groups creation points by which
content each can reach, and here both reach the same writes. The writes go
through the class's constructor and `__init__` contours, which serve both
creation points. `PYC_CSDCPA1=0` (per-site identity) does not help either,
because construction runs inside one shared constructor wrapper: one
allocation site.

The confluence is that shared contour. By AGENTS.md's method, the demand
(the violation at `self.labels[j]`) must be backtracked to it and the
contour split, so the two creation points reach different writes. That is
ESBLOCK's job (`find_blocking_es`), and here it is not reached. Check
first whether the CreationSet is in `demanded` at all: the violation's
backward walk may not reach the member AVar.

## Verification

The repro prints `5` and `4` on both backends, with two `Pot`
CreationSets. It then becomes 132's synthetic test, for the untagged-dispatch
demand and the clone-collapse rule.
