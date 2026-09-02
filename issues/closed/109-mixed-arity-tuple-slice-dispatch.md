# 109 — tuple slicing is unimplemented: `t[0:2]` aborts

**Status: CLOSED 2026-09-02** — fixed 2026-08-18, archived after
re-verifying. Found while digging into `sunfish`'s crash.

Re-verified on the current tree before archiving. Three of the four
verification items below pass: `t = (1,2,3,4); t[0:2]` prints `(1, 2)`,
`list` slicing is unaffected (`[1,2,3,4][0:2]` -> `[1, 2]`), and
`tests/tuple_arity_union_slice.py` has had its `.known_issue` tag removed
and passes in the suite. `tests/tuple_slice.py` pins the seven forms.

The fourth — "`sunfish` compiles **and runs**" — is **half met and
closed anyway**: sunfish compiles (`compile_rc=0`) and aborts at runtime
(`run_rc=134`, sweep `check__default__635d26b6+c1a28ccc`). That abort is
NOT tuple slicing, which is what matters for closing this issue.

**Correction (2026-09-02, same day):** the sentence that stood here
attributed the abort to the `{list, tuple}` union receiver documented
under "Still open" below —
[030](../030-DISPATCH-polymorphic-dispatch-fat-pointers.md), pinned by
`tests/list_tuple_union_method.py`. That was inherited from this doc's
older text, not measured. Running the binary shows the assert that
actually FIRES is `getter not resolved`, in a degenerate
`dict::__setitem__` clone; the `matching function not found` assert sits
on the very next line of the same collapsed function body and is never
reached. sunfish's residual failure is filed as
[125](../125-sunfish-degenerate-dict-setitem-clones.md). Closing here
per the README's rule that remaining scope covered by another issue
belongs to that issue, not to a second open doc.
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
[104](104-unify-list-and-tuple-in-analysis.md) turned out to be
incidental passengers in a degenerate type.

So [104](104-unify-list-and-tuple-in-analysis.md)'s conclusion —
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
[104](104-unify-list-and-tuple-in-analysis.md) as an experiment
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
which is [018](../../../issues/closed/018-dict-mixed-key-types-boxing-failure.md),
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
[104](104-unify-list-and-tuple-in-analysis.md) prototyped.

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
[102](../102-corpus-programs-compile-then-abort-at-runtime.md)'s own
argument — a compile-time diagnostic beats a runtime SIGABRT, and
`sunfish` produced no useful output either way. But it is a real
behaviour change in the wrong direction for that program, it was not
caught by the sweep, and it should not be discovered later as a surprise.

The underlying cause is [018](../../../issues/closed/018-dict-mixed-key-types-boxing-failure.md):
a `{tuple, str}` slice receiver has no single representation. shedskin
avoids it because `str` and `tuple<T>` are separate C++ types with
separate `__slice__` instantiations, and its analysis keeps the receiver
monomorphic.

## Root cause of sunfish's remaining failure: `tuple | tuple` at the receiver

`PYC_DBG_SIZEOF` (new, probe-only) prints the type that reaches
`sizeof_element`:

```
[sizeof] type='<anon>' kind=2 has=2 in fun=__pyc_getslice__
         members= tuple(kind=3,elem=1) tuple(kind=3,elem=1)
```

`kind=2` is **`Type_SUM`**. So the receiver of `__pyc_getslice__` is a
**union of two distinct concrete tuple types** — each individually a
`Type_RECORD` *with* an element (`elem=1`), but their sum has none, so
`sizeof_element(self)` has nothing to read.

It is not `{tuple, str}` — that case works. Verified directly: both a
branch-merged `(1,2,3,4) if n else "abcd"` sliced, and a shared
`def sl(v): return v[0:2]` called with a tuple and a str, compile and run
correctly.

**This is a dispatch problem, and pyc has the machinery for it.**
`split_for_per_cs_method_receivers` (`PER_CS_RECEIVER`, issue 045) fans a
receiver position per CreationSet, which would give each clone a
monomorphic receiver. It does not fire here for two reasons:

1. it is gated on `cs_is_per_cs_method_class` — `tuple` is not flagged
   `clone_methods_per_cs`;
2. `PYC_RECVFAN`'s container relaxation required the classes to **differ**
   ("mixed container"), and here both are `tuple`.

### Widening the fan: measured, and it does not work

Dropping the differ-classes requirement makes the fan fire on
`sunfish`. Results:

| `PYC_RECVFAN` | `sunfish` |
|---|---|
| 0 (default) | compile error — `sizeof_element of non-container` |
| 1 | fires, then **aborts the compiler** — `split_edges` asserted `p` |
| 1, after fixing that assert | compile error (unchanged) |
| 2 (gate lifted) | **FA non-convergence** — no progress for 120 s, 8 060 000 edges |

So fanning every same-class container receiver per CreationSet is too
aggressive: it explodes the analysis rather than resolving it. The
principled direction is right — a monomorphic receiver per clone is
exactly what dispatch should provide — but "fan every container receiver"
is not a workable formulation of it.

One real fix did come out of it, and is kept: `split_edges` asserted that
the AVar handed to it is still at an argument position of the ES. The
caller walks `positional_arg_positions` and can split an earlier position
in the same pass, rewriting `es->args` underneath the later ones — so
that is "nothing to split here", not a broken invariant. It now returns 0
instead of aborting. Unreachable at default settings, which is why it had
survived as an assert.

### What would actually fix it

The receiver needs to be monomorphic *at the call site*, which means
either splitting the **caller's** contour so `self` is one tuple type
(the [101](../101-FA-first-time-forever-splitting.md) splitting rule), or
giving the two tuple types one representation so the sum never forms —
shedskin's answer, since its `pst` is `dict<str*, tuple<__ss_int>*>` with
both lengths as one type.

Note the second is what `PYC_TUPLE_AS_LIST` was built for and it does not
reach this case, because it is a post-FA `clone.cc` decision while this
sum is formed during FA. That is the same stage mismatch
[104](104-unify-list-and-tuple-in-analysis.md) closed on.

**Default is unaffected throughout: 275 passed / 15 known / 0 failed**,
and `sunfish`/`plcfrs`/`go` are unchanged at `rc=1`.

## Splitting the caller instead: the right mechanism, blocked by stage starvation

The suggestion was that a receiver union should split the **caller's**
contour automatically, via backward analysis. That is the correct
architecture, and pyc already has the wiring. The reason it does nothing
here is more specific than "it is missing".

### FA never knew there was a problem

`P_prim_sizeof_element`'s constraint (`fa.cc`) simply unions the element
sizes across every receiver CreationSet:

```cpp
for (CreationSet *cs : t->out->sorted) {
  AVar *elem = get_element_avar(cs);
  if (elem) for (CreationSet *cs2 : elem->out->sorted) rtype = type_union(rtype, ...);
}
```

No violation, ever. A `tuple | tuple` receiver is a perfectly good AType
to FA; only **codegen** chokes, because the sum has no `element`. So the
violation-driven splitter had nothing to act on — the backward analysis
was not failing, it was never invoked.

`PYC_SIZEOF_VIOL=1` records a `BOXING` violation when the receiver spans
CreationSets that cannot share one concrete container type (different
sym, or different arity — which for a record-shaped tuple *is* a
different type). It works: `sunfish` goes from 10 to **11 violations**.

### But the VIOLATION stage is starved

```
STAGES: TYPE_CONFL SETTER SETTER_OF_SETTER
```

Stage 5 (`VIOLATION`) **never runs on sunfish**. It is gated on
`if (!analyze_again)` — full quiescence of the earlier stages — and the
first-stage-wins cascade never gets there. So the violation is recorded
faithfully and nothing ever splits on it: `ess` and `css` are unchanged
(534 / 1586) and the failure is identical.

This is the same starvation [101](../101-FA-first-time-forever-splitting.md)
found keeping `PER_CS_RECEIVER` from running, now with a second concrete
victim.

### Lifting the gate destabilises FA

`PYC_SIZEOF_VIOL=2` also lifts stage 5's quiescence gate:

```
fail: FA flow analysis made no EntrySet progress for 120s (5 640 000 edges)
      -- non-convergent input
```

**The same outcome as the receiver fan** (`PYC_RECVFAN=2`, 8 060 000
edges). Two independent, principled routes to the same fix both explode
the analysis the moment their stage is allowed to run early.

### What this establishes

The quiescence gates are **load-bearing for convergence**, not
conservatism to be relaxed. Both fixes here are correct in principle and
neither is reachable while the cascade starves the stage that implements
it — and un-starving it costs convergence.

So this is not a missing mechanism but
[101](../101-FA-first-time-forever-splitting.md)'s productivity problem:
splitting must *earn* its contours, so that a stage can run early without
diverging. Until that exists, `sizeof_element`'s violation is correct
information the splitter cannot safely use.

Both flags are off by default (`PYC_SIZEOF_VIOL`, `PYC_RECVFAN`);
default is **275 passed / 15 known / 0 failed**, `sunfish` and `plcfrs`
unchanged at `rc=1`.

## FIXED: the element type is the union of the components' — and codegen can use it

The observation that unblocked it: for a receiver typed
`tuple | tuple`, the element slot must hold *any* member's element, so
**one size is correct exactly when every member agrees on it**. The sum
having no `element` of its own does not make the question unanswerable.

That is the same reasoning `resolve_uniform_size` already applies to a
union ELEMENT type; it just had to be lifted one level, to a union
CONTAINER type. `cg.cc` now does that before failing.

### The empty tuple was the real blocker

`PYC_DBG_SIZEOF` showed the union is not two same-shaped tuples:

```
members= tuple(kind=3,elem=1,esz=0,eus=0,has=64)   <- 64 fields
         tuple(kind=3,elem=1,esz=0,eus=0,has=0)    <- ZERO fields
```

The second member is `()`, from `sunfish`'s `sum(gen, ())`. An **empty
container has no elements, so it has no element size to agree about** —
counting it as a conflict is what failed the uniformity test. It is now
skipped, which is a statement about empty containers rather than a
special case for this program.

### Result

| | |
|---|---|
| **`sunfish`** | compile error → **compiles** |
| corpus, 77 programs | **the only exit-code change is `sunfish` 1 → 0** |
| `violations` / `ess` / `css` | **byte-identical on all 72 programs that reach FA** |
| suite | 275 passed / 15 known / 0 failed |

No annotation and no hack: codegen answers the question it was already
asking, from information it already had.

### Still open

`sunfish` now aborts at *runtime* — `matching function not found` — and
that has been followed up: the receiver is a **`{list, tuple}` union**,
reduced to four lines in `tests/list_tuple_union_method.py`. The two
candidates have the same C-level receiver type and there is no runtime
tag to pick between them:

```
_CG_any  list::__pyc_getslice__(_CG_any a1, ...)
_CG_void tuple::__pyc_getslice__(_CG_any a1, ...)
assert(!"runtime error: matching function not found");
```

It is **not slice-specific** — `len(x)`, `x[0]` and `for v in x` fail
identically on the same union. It is
[030](../030-DISPATCH-polymorphic-dispatch-fat-pointers.md) /
[102](../102-corpus-programs-compile-then-abort-at-runtime.md) class B, a
separate problem from this issue.

Worth recording why a `{tuple, str}` union does **not** fail: FA
constant-folds the `str` branch — its `__pyc_getslice__` is emitted with
*no parameters* and a hardcoded literal — so no union ever reaches one
call site. That case working is an accident of constant propagation, not
evidence that polymorphic dispatch works.

The two failed approaches above (widening the receiver fan, and recording
a `sizeof_element` violation so backward splitting fires) are left
documented and off by default. They remain the *right* architecture for
the general case; this fix resolves the specific question codegen was
asking without needing either.