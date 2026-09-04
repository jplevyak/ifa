# 130 — identity is keyed on `Sym::name` in three load-bearing places, and names are not unique

**Status:** open, filed 2026-09-04. Found by audit while working
[129](129-plan-demand-driven-creation-set-splitting.md) step 2.
**Category:** FA (the ranked-first site is `fa.cc`/`fa.h`), with one
instance each in the C and LLVM backends.

## The invariant that does not hold

Identifier strings are interned — `cannonicalize_string` /
`if1_cannonicalize_string` (`python_ifa_sym.cc:91`,
`python_ifa_build_syms.cc:82`) — so equal text normally means an equal
`cchar *`. **Interning makes a name a fast key. It does not make it an
identity.** `Sym::name` is not unique: two classes in different modules
(issues/113 packages), a clone and its original, an override and the
member it overrides, all carry the same string.

That cuts both ways, and the tree already contains evidence of each:

- **Collision** — two distinct `Sym`s share one interned pointer, so a
  name-keyed map fuses them or a name-equality test reports "same".
- **Miss** — a name that escaped canonicalization has a distinct pointer
  with equal text, so a pointer-keyed lookup fails to find a member that
  is there.

CLAUDE.md's "Never analyse or decide by NAME" records the *wrong
measurement* half of this (ifa/issues/123's slot census, four name-based
formulations, all wrong). This issue is the other half: name as
**identity**, where the failure is not a bad number but a wrong answer
the compiler acts on.

## A1 — `CreationSet::var_map`: field resolution runs through a name-keyed map

`Map<cchar *, AVar *> var_map` (`ifa/analysis/fa.h:267`) answers "which
AVar is member `f` of this CreationSet?". It is a plain `Map`, so its
`cchar *` key compares by **pointer**.

| | site |
|---|---|
| filled | `fa.cc:273`, `fa.cc:755`, `fa.cc:2068`, `clone.cc:914` |
| read | `fa.cc:2597` (record destructuring), **`fa.cc:3010`, `fa.cc:3099`** (the `.field` selector path), `clone.cc:329`, `clone.cc:341`, `clone.cc:831` |

Both directions are live:

**Collision.** `fa.cc:755` fills the map while walking `s->has`:

```c
for (Sym *h : s->has) { ... if (h->name) cs->var_map.put(h->name, iv); }
```

Two members of one class with the same name — which is exactly
[110](closed/110-override-duplicates-member-slot.md), an override
duplicating a member slot — silently overwrite. The map cannot represent
the situation, so whichever `has` entry comes last wins, and the other
member's AVar is unreachable through the map for the rest of the
analysis.

**Miss.** `clone.cc:876` already carries the symptom, in a comment
explaining why the code next to it does *not* use the map:

> By NAME, not `cs->var_map.get()`: the map is keyed by pointer, so an
> equal-but-distinct string misses and the CS's REAL ivar is replaced by
> a pad, silently losing its type (`bh`: `EPS` `_CG_float64` became
> `_CG_void`).

So interning is not universal, and where it lapses a member vanishes and
is replaced by padding — a type loss with no diagnostic.

`clone.cc:816`, `clone.cc:853` and `clone.cc:881` are the workaround for
this: O(n·m) `strcmp` scans standing in for a map that cannot be
trusted. They are symptoms of A1, not independent defects, and they
should disappear when A1 is fixed.

### Why the fix is not simply "key on `Sym *`"

The read at `fa.cc:3010` legitimately *starts* from a name — the
selector is a symbol constant out of the IR, because Python attribute
access is by name at the source level:

```c
cchar *symbol = sel->sym->name;
if (!symbol) symbol = sel->sym->constant;
if (!symbol) symbol = sel->sym->imm.v_string;
```

The name is the *query*, and that is correct. What is wrong is using it
as the *key of the answer*. The fix is to resolve the selector name to a
member `Sym *` **once**, through the class hierarchy (`Sym::has` /
`Sym::specializes`), and key `var_map` on that `Sym *`. Then an override
and its base are two keys rather than one, a lapse in interning cannot
cause a miss, and the `strcmp` scans in `clone.cc` become map lookups.

Where a name resolves to more than one member `Sym` on one class, that
is [110](closed/110-override-duplicates-member-slot.md)'s situation and
must be *decided* by the hierarchy, not resolved by whichever `put` ran
last.

## A2 — `atype_shape`: the CS canon key is built out of names

`fa.cc:9665`, inside the shape string that keys `PYC_CSELEM=3`:

```c
out += cs->sym->name ? cs->sym->name : "?";
```

`cselem_shape_canon` (`fa.cc:9701`) is **monotone and global** — it is
deliberately never cleared per pass, because "shapes converge, so
entries only stabilize". A name collision therefore produces a contour
merge that can never be revisited, which is the worst version of this
defect: the other two sites give a wrong answer, this one gives a
permanent one.

Contrast `element_census` (`fa.cc`, [129](129-plan-demand-driven-creation-set-splitting.md)
step 1) in the same file, which keys on `cs->sym->id` and says why. The
fix is to do the same here — emit `name#id` so the key stays
diffable in `IFA_DBG_CSELEM` output while being structural.

This is on 129's path: step 2 is deciding whether `PYC_CSELEM=3` can
become the default, and `fa.cc:5891` records that it currently costs the
corpus four programs. A2 is a candidate contributor to that cost and
should be fixed before the mode is judged.

## A3 — classtag dispatch dedup compares class names, and the same file disagrees with itself

`cg.cc:2278` (mirrored at `cg_emit_llvm.cc:2950`), deduplicating the
classes of a polymorphic dispatch table:

```c
for (int ci = 0; ci < classes.n; ci++)
  if (!strcmp(classes[ci]->name, rt->name)) { found = true; break; }
```

The comment above it states the intent: *"Merge each found receiver
class into a per-class-name branch (clones of one class share a tag; the
stored slot pointer disambiguates)."* So the name is a **proxy** for
"shares a classtag" — and the structural predicate is right there,
`cg_has_classtag` / `cg_get_string` are already called on the line
above. Two distinct classes that share a name collapse into one dispatch
entry.

What makes this more than theoretical is twenty lines earlier, in the
same function. `cg.cc:2247` (mirrored at `cg_emit_llvm.cc:3020`) applies
the *identical* predicate and draws the opposite conclusion — it treats
name equality as a mis-dispatch hazard and refuses:

```c
if (classes[ci] == carrier_recv) { same = true; break; }
if (!strcmp(classes[ci]->name, carrier_recv->name)) { collide = true; break; }
...
if (collide) { ok = false; break; }
```

Note that this one gets the identity test right on the line before
(`classes[ci] == carrier_recv`, a pointer compare) and then uses the
name only to detect the hazard. **One of the two readings is wrong.**
Either name equality means "same class, dedup it" or it means "distinct
classes, bail" — it cannot mean both in one function. Settling this is
cheap and the answer is needed either way.

Related and already closed, same family:
[083](closed/083-CGEN-print-println-name-collision-risk.md),
[084](closed/084-CGEN-LLVM-bool-constant-name-matching-workaround.md).

## B — pyc semantics baked into generic ifa by name

Lower severity, and largely known territory — the `IFACallbacks`
extension-point pattern of
[082](closed/082-narrowing-wrapper-names-hardcoded-in-fa.md) /
[083](closed/083-CGEN-print-println-name-collision-risk.md) /
[084](closed/084-CGEN-LLVM-bool-constant-name-matching-workaround.md).
Recorded here so the census is complete, not as new work:

- `exc_check_fold.cc:32` — `__pyc_exc__`
- `cg_emit_llvm.cc:2223-2479` — `__pyc_c_call__`, `__pyc_net_wait_read__`,
  `__pyc_net_wait_write__`, `__pyc_sleep__`, `__pyc_format_string__`,
  `__pyc_to_str__`, `print` / `println`
- `cg.cc:2215`, `cg_emit_llvm.cc:3002` — `__closure__`
- `__bool__` / `__not__` / `__pyc_to_bool__`

A user function named `print` collides; that is 083 exactly.

## C — diagnostics, which are fine, with one exception worth a one-line fix

Every `getenv`-gated `strcmp` on a name is a display filter and is
correct as written (`fa.cc:758`, `5232`, `6167`, `6286`, `7457`, `7978`,
`10033`, `10502`, `10558`, `10598`, `graph.cc:273`, `graph.cc:490`,
`cg.cc:3288`, and the `ifa/testing/*` phase-name parsing).

One exception: `report_cs_population` (`fa.cc:10011`) keys its histogram
on `cs->sym->name`, so it merges distinct syms into one bucket. It is
`getenv`-gated and marked "Probe-only", but it is a **measurement people
reason from**, which is precisely the ifa/issues/123 trap. Key it on
`Sym *` and print `name#id`.

Two that are already right, and are the model to copy:
`element_census` / `report_demand_ratio` (`Sym *`-keyed throughout), and
the `ELEMTYPE` sort at `fa.cc:10208`, which sorts by name **then `id`**
so the id breaks ties and the ordering is stable — the correct way to
use a name for presentation without letting it become the key. Compare
[112](closed/112-CGEN-nondeterministic-emitted-c.md), where unstable
ordering was the bug.

## Proposed fix, in order

1. **A3** — settle the contradiction in `cg.cc` / `cg_emit_llvm.cc`.
   Cheapest, self-contained, and the answer is a prerequisite for
   trusting either site. Replace the name compare with the structural
   predicate the comment is describing.
2. **A1** — resolve selector name → member `Sym *` through the
   hierarchy, re-key `var_map` on `Sym *`, and delete the `strcmp`
   scans in `clone.cc:816/853/881` that exist only to work around it.
3. **A2** — `name#id` in `atype_shape`, before
   [129](129-plan-demand-driven-creation-set-splitting.md) step 2 judges
   `PYC_CSELEM=3`.
4. **C** — one-line fix to `report_cs_population`.

B is deliberately not scheduled here; it belongs to the `IFACallbacks`
line of work.

## Verification plan

- **A3**: `make test` (all six CI gates), then `./corpus_sweep.sh -m check`
  against the current default sweep. Any program whose dispatch changes
  should be inspected individually — a dedup that stops merging can
  legitimately *add* dispatch branches.
- **A1**: the `bh` symptom in `clone.cc:876` is the direct regression
  test — `EPS` must stay `_CG_float64`. Add a fixture with an override
  duplicating a member name ([110](closed/110-override-duplicates-member-slot.md)'s
  shape) and assert both members remain reachable. Corpus `check`
  neutral or better; `run_fail` is the column to watch, since a wrong
  member resolution shows up as a crash, not a compile error
  ([102](102-corpus-programs-compile-then-abort-at-runtime.md)).
- **A2**: `./corpus_sweep.sh -m check -e "PYC_CSELEM=3"` before and
  after. `fa.cc:5891` records the pre-fix cost (four programs); the
  question is how much of it A2 accounts for.
- **C**: probe-only, no test.

Because these change struct layout (`var_map`'s key type) and codegen
decisions, **`make clean` first** — CLAUDE.md's header-dependency rule
applies directly to A1.

## What this unblocks

- **Correctness that is currently silent.** A1's miss direction loses a
  field's type and pads it; A1's collision direction makes an override
  unreachable. Neither produces a diagnostic, and
  [102](102-corpus-programs-compile-then-abort-at-runtime.md) is where
  such failures surface — as a crash in a program that compiled clean.
- **[129](129-plan-demand-driven-creation-set-splitting.md) step 2.**
  A2 is a permanent-merge hazard sitting inside the exact key whose
  corpus cost step 2 is trying to measure. Measuring it before fixing A2
  risks attributing A2's damage to the keying idea itself.
- **[123](123-CGEN-union-receiver-field-access-has-no-discrimination.md).**
  Union receiver field access is the same question A1 answers — which
  member does this selector name denote on this CreationSet — and it
  cannot be answered structurally while the map underneath it is keyed
  on the name.
- **The directive in CLAUDE.md becomes checkable.** Today "never decide
  by NAME" is stated but three load-bearing violations remain; after
  this, a `grep` for `->name` in a non-diagnostic context is a
  meaningful review test.
