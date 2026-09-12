# ifa/149 — the corpus's largest warning class was reporting a blank

**Found 2026-09-11** while asking which set of programs to work on next.

## The classes, by size

Grouping the 45 corpus programs that produce diagnostics by their
DOMINANT warning kind gives disjoint groups:

| dominant warning | programs |
| --- | --- |
| `illegal call argument type expression illegal: T` | **23** |
| `expression has no type` | 14 |
| `illegal call argument type 'X' illegal: T` | 5 |
| implicit conversion / illegal primitive argument | 1 each |

So the largest single class is the first, at 23 of 45 programs.

## 55% of it said nothing at all

Of the corpus's **747** `illegal call argument type expression` warnings,
**416 (55%)** rendered as

```
warning: illegal call argument type expression illegal:
```

— nothing after the colon. Across 14+ programs: tarsalzp 51, msp_ss 49,
rubik 42, doom 38, plcfrs 31, rdb 29, othello2 21, softrender 14,
sudoku3 12, mastermind2 12, quameon 10, sunfish 9, rsync 8, timsort 7.

`show_illegal_type` printed `show_type(*v->type->type)` — the PROJECTION,
which by design drops constants — so two different conditions both came
out blank:

- the offending type is BOTTOM: the analysis never typed the argument at
  all. This is the bulk of the 416.
- the offending type is constants-only (typically nil): `make_AType` sets
  `->type` to bottom when `nonconsts.n == 0`, so a genuine
  `__pyc_None_type__` printed as nothing.

Both now print. On `tarsalzp` the 51 blanks become `(no type)`; on
`tests/minmax_3arg.py` two blanks become `__pyc_None_type__`, which is the
second case and is strictly more information than before.

## Why this matters more than its size

It is diagnostic-only — no analysis behaviour changes — but the largest
warning class in the corpus was unreadable, and both conditions it was
hiding are ones this project actively works on:

- "the argument is untyped" is the same condition as the second-largest
  class (`expression has no type`, 14 programs), reported at a call site.
  Counting them together, **roughly 37 of the 45 failing programs have an
  untyped value as their dominant complaint** — far larger than any
  contour-splitting group, and it was invisible because more than half of
  its instances printed a blank.
- a nil-only type reported as blank is exactly the `{None}`-projection
  trap recorded in issue/060 and in ifa/133's nilstore work.

Five goldens pinned the blank text and were re-blessed, one line each
except `minmax_3arg` (2 lines, to `__pyc_None_type__`) and `match_seq`
(12 lines). Every changed line is the message this issue is about.

All six CI gates pass, 315/0.

## Next

The untyped-argument class is now legible and is the biggest thing in the
corpus. Reading a sample of the 416 to find out WHY those arguments are
bottom is the obvious follow-up, and it is a different question from the
contour-splitting work that ifa/128, ifa/129, ifa/133, ifa/146 and ifa/148
have been circling.

---

# Part 2 — the class is a FAN, and 25% of every corpus warning is a duplicate

**2026-09-11**, answering "dig into those that printed nothing after the
colon". Part 1 made the blanks legible; this part asks what they ARE, and
the answer is that most of them should never have been printed.

## One failed dispatch, (arguments x contours) warnings

`illegal call argument type ...` and `unresolved call 'X'` are **the same
violation kind** — `SEND_ARGUMENT`. The report branches on whether the
offending AVar is the send's SELECTOR (rval 0, the method name) or one of
the other arguments:

```c
case ATypeViolation_kind::SEND_ARGUMENT:
  if (v->av->var->sym->is_symbol && v->send->var->def->rvals[0] == v->av->var) {
    fprintf(memfp, "unresolved call '%s'", ...);   // + show_candidates
  } else {
    fprintf(memfp, "illegal call argument type "); // + show_illegal_type
  }
```

And `collect_argument_type_violations` raises that violation **once per
rval of the send**, for **every live EntrySet of the enclosing function**
(`fa.cc:5294`):

```c
if (!dispatched_this_pass(from, m)) {
  if (p->code->partial == Partial_NEVER) {
    for (Var *v : p->rvals) {          // EVERY argument, not the guilty one
      AVar *av = make_AVar(v, from);
      type_violation(SEND_ARGUMENT, av, av->out, make_AVar(p->lvals[0], from));
```

So one call site that fails to dispatch produces *arguments × contours*
warnings. `sudoku3:54` is the clean example — fifteen lines at a single
column, 5 EntrySets × 3 rvals:

```
sudoku3.py:54:1047: warning: illegal call argument type 'x' illegal: int64     (x5)
sudoku3.py:54:1047: warning: illegal call argument type expression illegal: (no type)  (x5)
sudoku3.py:54:1047: warning: expression has no type                            (x5)
```

The fan is directly visible on the 5-line repro below, where `bool` has no
`__xor__` (it is an int subtype in CPython; `__pyc__/02_numeric.py` defines
39 dunders, `bool` in `00_runtime.py` defines 13). ONE missing method, eight
warnings — and note `unresolved call` and the two `illegal:` lines share
column 33, which is the fan at one send:

```python
def f(i, j, n):
    x = i < n
    y = j < n
    return not (x ^ y)
print(f(1, 2, 3))
```
```
x.py:4:22: warning: expression has no type
x.py:4:33: warning: unresolved call '__xor__'                        <-- THE CAUSE
x.py:4:33: warning: illegal call argument type 'x' illegal: bool
x.py:4:33: warning: illegal call argument type 'y' illegal: bool
x.py:4:28: warning: illegal call argument type expression illegal: (no type)
x.py:4:33: warning: expression has no type
x.py:4:28: warning: expression has no type
x.py:5:36: warning: illegal call argument type expression illegal: (no type)
```

## The blanks are duplicates of the NOTYPE warning, measured

A fan member whose type is bottom says only *this value has no type* — and
that is exactly what the `NOTYPE` violation for the same AVar says, in the
same place, with the value's name. `collect_var_type_violations` raises one
for every bottom `live_arg` non-internal Var, so the sibling is normally
right there.

Measured over the whole corpus (`sweeps/compile__default__ae80a6ed+eeea0b01`),
for each of the **482** `illegal: (no type)` lines, what else is reported at
the same place:

| accompanied by | blanks |
| --- | --- |
| a `has no type` warning at the SAME file:line:col | **429** |
| a `has no type` warning on the same LINE | 50 |
| only a typed `illegal:` naming a real mismatch at the same send | 3 |
| **nothing at all** | **0** |

479 of 482 (99.4%) are a second rendering of a warning already printed for
the same value; the other 3 sit beside a typed `illegal:` that names the
actual problem. **No blank anywhere in the corpus is the only signal.**

Against the corpus's warning totals:

```
total warning lines   1947
  (no type) blanks     482   <-- 24.8%
  has no type          756
  typed illegal arg    528
  unresolved call      166
```

So a quarter of every diagnostic the corpus emits was a duplicate.

## Fix: do not report a bottom-typed SEND_ARGUMENT

`is_uninformative_violation` in `show_violations` skips it. Two things it
deliberately does NOT do:

- it does **not** touch the selector branch (`is_selector_violation`), which
  prints `unresolved call` plus the candidate list and is the one member of
  the fan that names the cause;
- it does **not** remove the violation, only the report. The violation still
  reaches the splitter — where a bottom type routes nothing anyway, having
  no CreationSets to partition — so **no analysis behaviour changes**.

All 482 blanks are `SEND_ARGUMENT`; `PRIMITIVE_ARGUMENT` shares
`show_illegal_type` but produces none, so the predicate is scoped to the
kind that was measured.

## Where the primaries actually are

Of the 41 programs with blanks, **34 also report `unresolved call`** and the
blank is a fan sibling of it. Corpus-wide ranking of the unresolved
operators — every one a dunder, and the real work list:

```
__lt__ 21  __add__ 20  __iter__ 18  __eq__ 13  __ne__ 12  __gt__ 12
__mul__ 10 __sub__ 9   __lshift__ 9 __iadd__ 9 __floordiv__ 8
__xor__ 4  __isub__ 4  __le__ 3     __truediv__ 2  __rshift__ 2
```

The other **7** have blanks with no `unresolved call` at all, and they split
three ways — none of them a new phenomenon:

- **the selector is not a symbol** (`sudoku3`, `sudoku5`, `minpng`, `mao`).
  An indirect call through a function value takes the `else` branch, so the
  selector prints `illegal: closure` (or a class name) instead of
  `unresolved call`. Same fan, different rendering of its head.
- **every rval is bottom** (`dijkstra`, `pygasus`) — a pure downstream
  cascade with no local primary. `dijkstra`'s is
  [ifa/049](049-FA-raise-only-contour-notype.md): `distance()` is
  `for ... else: raise AssertionError`, its return types to bottom, and
  `G.distance(s, S)` then feeds bottom into `print` at lines 129 and 133.
  One known bug, six warnings.
- **not bottom at all** (`genetic2`) — constants-only, which Part 1's fix
  already reclassified into a real type.

A structural note that makes the accounting exact: the `else` arm of
`collect_argument_type_violations` (dispatch succeeded, an actual's type
minus every callee's filter is non-empty) narrows `t` from `av->out` and
only reports when the result is NON-empty, so **it can never produce a
blank**. Every `(no type)` comes from the complete-dispatch-failure arm,
which is why every one of them has a fan to belong to.

## Verification

Six gates green, 315/0, 19 known. Five goldens lost blank lines — the same
five Part 1 re-blessed to `(no type)`, now dropping those lines entirely —
66 deletions and zero additions, every one a `(no type)` block. On
`tests/cross_type_method.py` the suppressed `10:92` blank is covered by the
surviving `10:92 expression has no type` at the identical column, with the
real failure at `10:98` untouched.

## Next

The fan itself is still there for the typed members: `sudoku3:54` still
prints `'x' illegal: int64` five times, once per EntrySet, for one failed
call. Reporting a failed dispatch ONCE per send — with the argument tuple
and the candidate list — is the remaining half of this issue, and unlike the
suppression it needs the report loop to group violations by send rather than
print them one at a time.

`bool`'s missing int-subtype operators are a separate, concrete fix worth
doing on its own: `00_runtime.py` already carries a comment explaining why
`__lt__`/`__gt__` were added there and warns that such a gap "cascaded into
unrelated NOTYPE collapses". `__xor__` is the next instance (quameon), and
`range` has no `__contains__` at all while nine other classes do — the `in`
lowering (`emit_in_pyda`) dispatches `__contains__` directly with no
fallback to the iterable protocol, which is the same gap
`__pyc_generator__.__contains__` and `__pyc_iterator__.__contains__` were
each added to plug by hand.

---

# Part 3 — the receiver-type census, re-run: the missing-method era is over

**2026-09-12**, after [150](150-is-not-none-never-folds.md),
[125](../../issues/125-in-has-no-iterable-fallback.md) and
[126](../../issues/closed/126-bool-lacks-int-subtype-arithmetic.md) closed three
root causes. Re-run because the operator NAME had already proven a poor
proxy for the cause twice: `unresolved call '__not__'` in `othello` was
really `range.__contains__`, and `'__lt__'` in a dozen programs was really
`bool.__not__` failing to fold.

Method: Part 2 established that a completely failed dispatch raises one
violation per rval of the send, so the `illegal: T` siblings at the same
`file:line:col` ARE the argument types of the failed send. For each of the
**108** remaining unresolved-call sites, ask whether the operator actually
exists on each named type.

## Two false starts worth recording

**The first cut grouped by type and got 44 sites on `int64` alone** — which
would mean `int` lacks `__floordiv__`, `__ne__`, `__lshift__`. It does not;
all of them are in `__pyc__/02_numeric.py`.

**The second cut tested "is some rval at this position bottom?" and got
108 of 108 — which is circular.** A failed dispatch's own result is bottom,
so a `has no type` warning at that position is a CONSEQUENCE of the failure,
never evidence of its cause. The test could not have returned anything else.

The discriminator that works is non-circular and needs no probe: **if the
operator exists on every named operand type, that type cannot be why
dispatch failed**, so the receiver must be something else — and the only
something else available is an untyped value.

## The result

| | sites |
| --- | --- |
| a genuinely MISSING method on a named type | **5** |
| receiver is a `closure` / function value, or a user class | 10 |
| every named type HAS the operator — **cascade from an untyped value** | **93** |

**86% of what is left is a cascade.** The missing-method work is finished:
the whole remaining list is

```
4  list.__lt__     linalg mastermind2    `list1.sort()`
1  bool.__iand__   rdb                   `basis &= MatchRule(props, rule)`
```

`list.__lt__` is already filed as [issues/122](../../issues/122-list-ordering-comparisons-missing.md)
with a fixture. `bool.__iand__` wants a CLASS INSTANCE as its argument,
which is a `TypeError` in CPython unless `MatchRule` defines `__rand__`
(see [126](../../issues/closed/126-bool-lacks-int-subtype-arithmetic.md)); rdb is
also one of the two programs that do not compile at all.

The 10 `closure` receivers are their own shape — an indirect call, or a
comprehension's function value appearing where its RESULT should be
(`sat`'s `nrofvars = [int(n[2]) for n in cnf if n[0] == 'p'][0]`,
`sunfish`'s `sum((padrow(...) for i in ...))`). Worth a look on its own;
it is not an operator gap.

## So the work list is now "why is this value untyped"

The `NOTYPE` population, which is what every cascade terminates in:

```
named   ('X' has no type)          92
anonymous (expression has no type) 558
```

The 92 NAMED ones are the actionable roots, because they identify a
variable. Ranked:

| sites | root |
| --- | --- |
| 12 | **doom `data`** — `data = self.entry_data[b'VERTEXES']`, a dict indexed by a bytes literal whose value channel never types. Drives ALL 20 of doom's unresolved calls. |
| 6 | msp_ss `dataOut` |
| 4 | plcfrs `rule`, msp_ss `blkin` |
| 3 | othello2 `value`, msp_ss `l` |
| 2 | tarsalzp `norm`/`ilog`, rdb `l3`, msp_ss `startaddr`/`rxFrame`/`bslVerLo`/`bslVerHi` |

doom's is the highest-leverage single root in the corpus and it is a
CONTAINER ELEMENT question — the value type of a dict — which puts the next
step in [133](133-split-a-container-on-its-element-type.md)'s and
[128](128-cs-identity-over-discriminates-vs-element-type.md)'s territory
rather than in the builtin library. That is a different kind of work from
the last three fixes, and it is where the corpus now points.
