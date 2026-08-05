# 076 — Receiver-CS method cloning (issue 045) doesn't cover classes whose instances diverge by mutation, not constructor arguments

**Status:** open, found 2026-08-04 triaging a minimal repro
(`squares = {1: 1, 2: 4, 3: 9}; words = {"a": 1, "b": 2}`) that
produces a hard C compile error. Not fixed — filed because the fix
requires a design decision about how far to widen an intentionally
scoped opt-in mechanism (issue
[045](closed/045-receiver-cs-method-cloning.md)), and because it's a
program-wide-blast-radius change (touches every `dict`/`set`
instance, or every class it's extended to) needing full corpus
verification, not a quick patch.
**Affects:** `python_ifa_build_syms.cc` (~2033-2050, the
`clone_methods_per_cs` trigger), `ifa/analysis/fa.cc`
(`cs_is_per_cs_method_class` ~6067, `split_for_per_cs_method_receivers`
~6074 — the PER_CS_RECEIVER stage from issue 045), and, if `dict`
specifically is chosen as a fix target, `__pyc__/07_dict.py`.
**Related:** [045](closed/045-receiver-cs-method-cloning.md) (the
mechanism this issue proposes widening — read that first, its "Design"
section explains the existing trigger and why it's scoped the way it
is), [056](056-degraded-index-type-raw-c-compile-error.md) and
[077](077-primitive-equality-codegen-missing-salvage-guard.md) (the
separate, shallower codegen-level symptom this same repro also
exposes — a missing salvage guard turns the *imprecision* below into
a hard build failure specifically for `dict`; fixing 077 makes this
degrade gracefully instead of failing the build, but does not fix the
imprecision itself).

## Symptom

```python
squares = {1: 1, 2: 4, 3: 9}
words = {"a": 1, "b": 2}
```

fails to compile:

```
1.py:1:9: warning: expression has mixed basic types:( int64 str )
    squares = {1: 1, 2: 4, 3: 9}
            ^
  called from __pyc__.py:1624
...
1.py.c:114:8: error: comparison between pointer and integer ('_CG_int64' (aka 'long long') and 'char *')
  114 |   t1 = _CG_prim_equal(t2, _CG_Symbol(6488, "=="), _CG_String_n("b",1));
1.py.c:310:8: error: no matching function for call to '_CG_str_eq'
  310 |   t1 = _CG_str_eq(t2, t3);
```

`PYC_CSM=2` (issue [075](075-element-cs-method-split-idempotent-plan.md)'s
element-CS container-method split) produces **byte-for-byte identical**
output — it does not touch this case at all, and `-l s` (the
`LOG_SPLITTING` log) shows zero splitting activity of any kind for
this program.

## Root cause

`__pyc__/07_dict.py`'s `dict` class is a single, unspecialized method
contour: `__setitem__`/`__getitem__`/`get` each run through **one**
`EntrySet` shared by every `dict` instance in the whole program,
regardless of how many distinct dict *objects* exist. `squares`'s and
`words`'s `__setitem__` calls both land in that one contour, so the
`key` parameter's type is the union `(int64, str)` there — and
`__setitem__`'s own internal scan, `self._keys[i] == key`
(`__pyc__/07_dict.py:89`, mirrored at :82/:100), compares that
union-typed `key` against `self._keys[i]`, which is itself unioned for
the same reason. `int == str` at that primitive level is what produces
the hard C error above (see 077 for why it's a *hard* error rather
than a warning + runtime guard).

Per-instance method specialization already exists — issue 045's
`PER_CS_RECEIVER` stage, gated by a class flag `clone_methods_per_cs`
— but that flag is set **only** when the class's `__init__` has a
parameter marked `__pyc_clone_constants__`
(`python_ifa_build_syms.cc:2044`, `wf->clone_for_constants` check).
`dict.__init__(self)` takes no such parameter — a dict starts empty;
its instances only diverge *later*, via the sequence of `__setitem__`
calls each one happens to receive. There is no constructor argument to
hang the marker on, so 045's trigger is structurally blind to this
shape. It was never meant to cover it: 045's own doc says the flag is
"opt-in, class-gated... first user: `range`" — the mechanism was
purpose-built for classes whose per-instance identity is visible *at
construction* (`range(0, 2)` vs `range(0, 0)`), not classes whose
divergence only shows up in later mutating calls.

**Confirmed general, not `dict`-specific.** An ordinary user-defined
class with the identical shape reproduces the same underlying merge:

```python
class Box:
    def __init__(self):
        self.v = 0
    def set(self, x):
        self.v = x
    def get(self):
        return self.v

a = Box(); a.set(1)
b = Box(); b.set("hi")
print(a.get()); print(b.get())
```

produces the same `expression has mixed basic types:( int64 str )`
warning on `self.v` — `Box.set`/`Box.get` are just as unspecialized
per-instance as `dict.__setitem__`/`__getitem__`, for the identical
reason (no-arg constructor, divergence via later mutation). The only
difference from the `dict` case: `Box`'s path degrades to a warning +
`assert(!"runtime error: matching function not found")` guard instead
of a hard compile error, because the generic dispatch codegen path
happens to have that fallback and `dict`'s raw-primitive-`==` path
doesn't (issue 077).

**Why issue 075 (CSM) doesn't help.** CSM splits a method that
*accesses an already-per-CS-diverging **contained** field* (e.g. a
`list` field holding different element types per creation site) —
it assumes the *receiver* itself is already properly separated per
instance, and only patches up residual divergence nested inside one
such receiver. Here the receiver itself (the `dict`/`Box` object) was
never separated to begin with — the union is on the method's own
formal parameter (`key`, `x`), not on a nested container field CSM's
`decide_csm_split` looks at (`av->out->type->sorted` per-CS
`added_element_var` divergence). That's a structurally different
starting point, which is why CSM shows zero engagement on this repro.

## Proposed fix (design decision needed — options, not a recommendation)

**Option A — widen `clone_methods_per_cs`'s trigger to a
divergence-observed condition**, not just a constructor-constant
condition: if `PER_CS_RECEIVER`'s existing scan
(`split_for_per_cs_method_receivers`) ever finds an EntrySet whose
positional arg holds ≥2 same-class CreationSets *and* a later stage
would otherwise report a violation inside that method, treat it the
same way as a flagged class instead of requiring the flag at all.
Risk: this is close to removing the class gate entirely, which 045's
design deliberately kept narrow "to bound the blast radius" — needs
re-verification against 045's own stability reasoning (termination:
does a violation-triggered split still only route through *existing*
CSs, per 045's argument, or can it manufacture new ones each pass —
issue 033's non-idempotence concern).

**Option B — mark specific builtin classes (`dict`, `set`) unconditionally
`clone_methods_per_cs`**, narrower and lower-risk than A: these are
exactly the "grows heterogeneously via mutation, not construction"
shape by design (unlike most user classes, which usually *do* set
their diverging fields from constructor arguments and are already
covered once A or an equivalent lands, or aren't diverging at all).
Doesn't fix the general `Box`-shaped case, only the two builtin
containers most likely to hit it in practice (shedskin-style corpus
programs lean on `dict`/`set` heavily). Cheapest to build and verify;
punts on the general case.

**Option C — do nothing structural; rely on 077's codegen-guard fix
alone.** Turns the hard failure into a warning + runtime-crash risk
(matching `Box`'s current degraded-but-building behavior) without
improving precision. Lowest risk, but leaves the underlying
imprecision — and the *possibility* of a runtime crash on first
`dict[str,X]`-vs-`dict[int,Y]` collision anywhere in a compiled
program — unaddressed.

## Verification plan

1. The two repros above (`1.py`'s dict case, the `Box` case) — both
   should compile cleanly with correctly separated `int`/`str`
   handling per instance, no warnings.
2. `ifa --test` full unit suite.
3. `test_pyc.py`, both backends, `PYC_CSM` unset and `=2` — should be
   unchanged or strictly improved (this is a distinct mechanism from
   075's CSM; verify neither regresses the other — 045's stage and
   CSM's stage both live in `run_split_stages` and need to keep
   composing correctly).
4. Full shedskin corpus sweep for regressions — this touches
   `dict`/`set` (Option B) or every multi-instance class program-wide
   (Option A), so needs the same rigor as 045's and 075's own landings
   (both required full-corpus verification before shipping).
5. fysphun (the many-pass numeric splitter-stress canary, per 045's
   own verification plan) — confirm no new non-termination.

## What this unblocks

`dict`/`set` (and any no-arg-constructor class) instances that
legitimately hold different key/value or field types across separate
instances currently either hard-fail the build (`dict`, today) or
silently degrade to a runtime-crash-risking union (`Box`-shaped user
classes, today) instead of compiling with full precision. This is a
completely ordinary Python pattern (two dicts with different key
types in the same program) — likely affects real shedskin-corpus
programs beyond the two probe cases above, not yet surveyed.
