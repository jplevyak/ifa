# 110 — a method override gets a SECOND member slot, shifting every field after it

**Status:** FIXED 2026-08-22 — see the FIXED section at the bottom.
Found the same day while verifying issues/116's inheritance cases, and
**pre-existing**: reproduced unchanged on `888cb499`, the commit before
that work.

## Symptom

A subclass that overrides one method and inherits another that touches
instance fields reads the wrong field. 19 lines, no iterator protocol,
no exceptions, no generators:

```python
class B:
    def __init__(self, n):
        self.i = 0
        self.n = n
    def more(self):
        return self.i < self.n
    def step(self):
        self.i = self.i + 1

class S(B):
    def step(self):
        self.i = self.i + 2

a = B(3)
while a.more():
    a.step()
print(a.i)

b = S(3)
while b.more():
    b.step()
print(b.i)
```

    CPython:  3 / 4          pyc:  prints nothing, hangs forever

Compiles clean, exit code never arrives. Either loop **alone** is
correct (`3`, or `4`) — it takes both, so that `more` is shared.

## Cause

`S`'s instance layout carries `step` **twice**:

```c
struct /* B */ {                      struct /* S */ {
  _CG_TypeObject *__pyc_tag;            _CG_TypeObject *__pyc_tag;
  _CG_pf2  e10; /* more */;             _CG_pf2  e10; /* more */;
  _CG_pf3  e11; /* step */;             _CG_void e11; /* step */;
  _CG_int64 e13; /* i */;               _CG_void e21; /* step */;   <-- the override
  _CG_int64 e14; /* n */;               _CG_int64 e23; /* i */;
};                                      _CG_int64 e24; /* n */;
                                      };
```

`i` is the 3rd slot on `B` and the 4th on `S`. `more` is emitted **once**
for both receivers, with an `_CG_any` parameter and a blind cast to the
base's layout:

```c
_CG_bool B__more(_CG_any a1) {
  t3 = a1;
  t2 = (_CG_int64)((_CG_ps11799)t3)->e13;  /* "i" -- but on an S this is
                                              the second `step` pointer */
  return _CG_prim_less(t2, "<", 3);
}
```

On an `S` instance that reads a function pointer, compares it against
`n`, and answers True forever.

Neither half is wrong on its own — a shared method over several
receivers is the whole point of the `_CG_any` receiver, and it is sound
**provided the layouts agree on the prefix**. The duplicate slot breaks
that invariant silently.

`ifa/if1/ast.cc`'s `collect_include_vars` is where the duplicate is
introduced:

```c
static void collect_include_vars(Sym *s, Sym *in = 0) {
  Vec<Sym *> saved;
  if (!in) saved.move(s->has);
  else     in->has.append(s->has);          // <-- no name dedup
  for (Sym *ss : s->includes)
    if (ss->type_kind == Type_RECORD) collect_include_vars(ss, in ? in : s);
  if (!in) s->has.append(saved);
}
```

Every base's `has` is appended wholesale, then the class's own members
are appended after it. A name defined in both simply appears twice.

The recursion compounds it whenever a base has **already been
flattened**: `include_instance_variables` walks `collect_includes`'
dependency order, so a base's `has` normally contains its own bases'
members by the time a subclass is processed — and the subclass then
descends into that base's `includes` and appends them a second time.
issues/116's bridged classes show this plainly: `Doubling(Counter)`,
where `Counter` includes `__pyc_iterator__`, gets the bridge's **entire**
member set twice, putting `i` at `e55` where `Counter` has it at `e29`.
The recursion is not simply removable — a base whose `id <
finalized_types` is skipped by that same loop and really has not been
flattened.

## Why the suite never caught it

The failure needs all three of: an override, a *sibling* inherited
method that touches fields, and enough receiver variety that the
inherited method is shared rather than monomorphized per receiver. Drop
any one and it passes. `tests/` has overrides and it has field access,
but no test combined them with two live receiver types through one
method.

That third condition is also why issues/116's `Doubling(Counter)` case
passes today despite the identical layout damage — `__pyc_more__`
happened to be cloned per receiver there. It is one monomorphization
decision away from the same hang.

## Fix

Dedup `has` by member name in `collect_include_vars`, most-derived
winning, so a subclass's layout is a strict prefix-compatible extension
of each base's.

**This is what shipped.** It was not a one-liner, which is why it was
filed rather than fixed inline; each concern below turned out real:

- **Which duplicate survives changes codegen.** The emitted prototype
  setters for `S` write *both* `B::step` and `S::step` to `e11`
  (last-store-wins on the base's slot), while `e21` is written nowhere —
  so today's dispatch reads the base's slot and the override's slot is
  dead weight. Dropping the wrong one of the pair silently changes which
  function a call reaches.
- **Layout order is load-bearing beyond this function.** `has` order
  drives struct emission, prototype setter emission, and the field
  indices baked into every already-cast accessor.
- **The blast radius is every record in every program**, so it needs a
  full before/after sweep of both suites plus the corpus, not a
  suite-green check.

## Verification plan

- The repro above prints `3` then `4` and terminates.
- Each loop in isolation still prints the same as it does today.
- `S`'s emitted struct has exactly one `step` slot, and `i`/`n` at the
  same indices as `B`'s.
- `Doubling(Counter)` from `tests/iterator_protocol_bridge.py` carries
  one copy of the `__pyc_iterator__` members, not two.
- Both suites unchanged, and the corpus shows no new compile failures or
  new runtime divergence (see [[corpus-compiles-but-crashes]] — rc=0 is
  not evidence on its own).

## What this blocks

Any ported library or user program with an ordinary override. This is
the plainest shape in object-oriented Python there is, and it hangs.

`tests/iterator_protocol_bridge.py` had to route around it: its
"a base that speaks pyc's protocol natively" case uses a `FastBase`
that deliberately does **not** define `__next__`, because a subclass
overriding it would hit this bug rather than anything about issues/116.

---

## FIXED (2026-08-22)

Both defects live in `ifa/if1/ast.cc`'s `collect_include_vars`.

### 1. An override was appended, not substituted

Own members were appended after every inherited one, so a name defined
in both appeared twice and every field after it shifted. Now they are
merged **by name, in place**:

```c
for (Sym *own : saved) {
  int at = -1;
  if (own->name)
    for (int i = 0; i < s->has.n; i++)
      if (s->has[i]->name == own->name) { at = i; break; }
  if (at >= 0) s->has[at] = own; else s->has.add(own);
}
```

Both properties are needed and neither alone is enough: most-derived
wins (the subclass's Sym is what any name lookup finds), **and** the
index still matches the base's, which is what keeps the blind cast in a
shared method sound.

### 2. An already-flattened base was descended into again

Not in the original diagnosis; it only became visible once the first was
fixed. `include_instance_variables` walks `collect_includes`' post-order,
so a base's `has` already contains its own bases' members by the time a
subclass is processed — but the recursion descended into that base's
`includes` anyway and appended them a second time. A grandparent's
members therefore arrived twice.

This is why issues/116's `Doubling(Counter)`, where `Counter` includes
`__pyc_iterator__`, carried the bridge's **entire** member set twice —
20 fields against `Counter`'s 10, with `i` at index 55 instead of 29.
`append_new_members` now skips any name the target already carries;
first writer wins, which for multiple inheritance is Python's MRO rule.

That case never actually misbehaved, because `__pyc_more__` happened to
be cloned per receiver there rather than shared. It was one
monomorphization decision away from the same hang.

### Verification

The concern that made this a filed issue rather than an inline fix was
blast radius, so that was measured rather than assumed.

**Emitted code, all 312 suite programs:** 274 files change (54 of them
only by id renumbering). Total emitted C **shrinks 1.8%**
(225,701 → 221,552 lines) — the duplicate slots had been keeping struct
typedefs and function declarations alive. Only 7 programs grow, by 2–8
lines each, all passing.

**Layout, checked as bytes rather than by eye.** For the repro's `B` and
`S`, compiled `offsetof`: `i=24 n=32 size=40` on both. `Counter` and
`Doubling` from `tests/iterator_protocol_bridge.py` now have identical
field names *and* indices.

**Corpus, 83 programs, baseline vs fixed:**

    compile status changed   0
    run status changed       0
    output hash changed     21   (every one with IDENTICAL exit codes)

All 21 were run down. Three exit cleanly: `genetic` is byte-identical on
re-run, and `nbody` and `sudoku1` differ only in a wall-clock `TIME`
line (`sudoku1` otherwise matches CPython exactly). The other 18 already
abort, time out, or segfault, identically before and after.

Their messages differ only because an assertion string embeds emitted-C
identifiers, clone indices and line numbers, which necessarily move when
the emitted C does — `othello2` fails at line 5387 before and 5242
after. Checked rather than assumed: parsing the assertion text out of
every such pair, **all ten are identical strings** (`matching function
not found`, `getter not resolved`, `bad getter`). Same failure, same
place, renumbered.

A bare output hash was too weak a signal here and initially suggested 13
regressions. The control that settled it: re-run the *fixed* binary
twice, so a base-vs-fixed difference only counts when that program's own
output is reproducible.

`othello3` is excluded from the sweep by its compile budget — it needs
234 s. Compared on its own, it fails **identically** on both sides with
byte-identical logs, hitting the pre-existing FA non-convergence
watchdog (ifa/issues/057).

**Compile time**, since the merge adds an O(n²) name scan: neutral to
better. `othello3` 233.82 s → 233.66 s, `tonyjpegdecoder` 2.80 → 2.71,
`pygasus` 46.50 → **38.35** (fewer duplicate members, less downstream
work).

**Suites:** pyc 293/0 on both the C and LLVM backends; `ifa --test`
58/0; `make test-ir` unchanged at 29/2 (the same two pre-existing
synthetic failures, `mark_distance_skew` and `mark_setter_skew`).

Backend-independent, as the diagnosis predicted: `m2.py` under `-b`
hangs on baseline and prints `3` / `4` with the fix, matching the C
backend. The defect was in the shared `has` construction, not in either
backend's struct emission.

**Regression test:** `tests/method_override_field_offset.py` — the
minimal two-loop repro plus a three-level chain with an override at two
levels and a field only the leaf adds. It **hangs** on the pre-fix
compiler.

### Not fixed here

`class D(B, C)` where both bases override the same method still warns
`ambiguous call` — pyc resolves methods by pattern specificity on
`self`, and neither base is more specific for a `D` receiver. The
*layout* is now right and the program produces CPython's answer; the
dispatch ambiguity is the same limitation issues/116 hit with sibling
bases, and is unrelated to this issue.
