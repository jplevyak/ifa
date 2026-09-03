# 123 — a method whose receiver is a UNION of unrelated classes gets a `void*` receiver and blind-casts to ONE member's layout; field access has no runtime discrimination

**Status:** open (fix outstanding); **root-caused end to end
2026-09-01**, and the miscompile is a compile error rather than a
segfault as of the same day (fix 2). `go`'s crash is fully explained:
`node.losses += 1` on a UCTNode is emitted through Square's layout and
increments the `unexplored` POINTER instead.
Found by [122](122-CGEN-layout-families.md) Phase 0's layout-contract
check, which reported it at 20 sites in `go` and 1 in `bh` **at compile
time** — both programs previously only crashed.

**Affects:** `ifa/codegen/cg.cc` — the getter (~531) and setter (~578)
via `resolve_union_receiver` (~221); the union's C representation
(`_CG_any`). Contrast `poly_dispatch_classtag_targets`
(`codegen_common.cc`), which solves the same problem for method
DISPATCH and has no counterpart for FIELD ACCESS.

## Symptom

`shedskin_examples/go` segfaults (`run_rc=139`); `shedskin_examples/bh`
dies inside the collector (`GC_clear_fl_marks`), the signature of a
wrong-offset write. Both are
[102](102-corpus-programs-compile-then-abort-at-runtime.md) members.

## Root cause, with the crash line

`Square` and `UCTNode` are **unrelated** classes in `go.py` — no
inheritance. They have different numbers of method slots (17 vs 20), so
**no field index agrees between them**:

| slot | `Square` (31 members) | `UCTNode` (29 members) |
|---|---|---|
| `e24` | `used` (`_CG_bool`, 1 byte) | `pos` (`_CG_int64`, 8 bytes) |
| `e26` | `losses` (`_CG_void`) | `unexplored` (`_CG_any`, a list) |

`UCTNode::select` is emitted with a **generic receiver**, and casts it to
one class's layout:

```c
_CG_int64 _CG_f_12664_136/*UCTNode::select*/(_CG_any a1, _CG_ps17220 a2) {
  ...
  t56 = (_CG_any)((_CG_ps17236)t18)->e26;   /* unexplored */
  t54 = ...(_CG_list_ptr(t56))[...];        /* <-- SIGSEGV */
```

`a1` is `_CG_any` — a `void *`. The body blind-casts to `_CG_ps17236`
(UCTNode) and reads `e26` expecting a list; gdb confirms the crash at
exactly that line, and that `e26` reads back **`0x7ffff7d609d2` — not
null, and not 8-aligned**. No valid GC pointer looks like that, so the
object is not shaped like `_CG_s17236`: it is a wrong-layout read.

## ROOT CAUSE, complete (2026-09-01)

`go.py:378` is `node.losses += 1`, inside `UCTNode::update_path`. It is
emitted as:

```c
t89 = (_CG_int64)((_CG_ps17224)t86)->e26;   /* losses -- SQUARE's layout */
t87 = _CG_prim_add(t89, _CG_Symbol(7441, "+"), 1);
((_CG_ps17224)t86)->e26 = (_CG_int64)t87;
```

`t86` is a **UCTNode** (`__pyc_tag` confirms it at run time). The access
uses `_CG_ps17224`, a **Square** clone, where `e26` is `losses`. On a
UCTNode `e26` is `unexplored`. So `node.losses += 1` **reads the
`unexplored` POINTER as an integer, adds 1, and writes it back.**

Proven by watching the value, not inferred:

```
store   (computer_move:9696)  e26 <- 0x7ffff7d609d0   (a valid 8-aligned list)
crash   (select:8438)         e26 == 0x7ffff7d609d2   (same object)
```

Two `losses += 1` updates, pointer +2, `_CG_list_ptr` on a misaligned
pointer, SIGSEGV. The object at the fault is the ROOT `tree` node
(`pos=-1`, `parent=NULL`, `wins=5`), and its neighbouring `pos_child`
(`e25`) is still properly aligned — only the field Square's layout aims
at is damaged.

**Why Square's layout is used at all: field-name pollution.** `Square`'s
member list contains `losses`, `wins` and `bestchild` — UCTNode's fields
— and UCTNode's contains `color` and `used`. So when the receiver is a
union, `resolve_union_receiver(union, "losses")` finds `losses` on
`Square` and picks it. Either half of the fix stops this: do not merge
the field-name sets, or discriminate the layout at run time.

**What the check does and does not name.** This corrupting write IS one
of the 20 reported violations ("`UCTNode` is blind-cast to `Square` and
read at e26"). The check therefore names the CAUSE. It does not name the
faulting line — the fault is one hop downstream in `select`, reading the
field that was corrupted. That is the right behaviour for such a check,
and worth stating because a first reading of the report looks like it
missed the crash.

### Two claims retracted along the way

Recorded rather than quietly fixed; each was a plausible inference from
the emitted C that a re-check disproved.

**(a) "The crash LINE is the violation."** Wrong: all 20 obligations are casts TO `Square` and the
faulting line casts to `UCTNode`. But the opposite over-correction —
that the check did not predict the crash — is also wrong: it predicts
the corrupting write, which is the cause.

**(b) "`unexplored` is read seven times and never written."** Wrong. It is
written — `((_CG_ps17243)t73)->e26 = (_CG_any)t71;` — and the claim was
an artifact of grepping for a `/* unexplored */` comment that the SETTER
does not emit while the getter does. (A third dead end, also
disproved: the setter's dead-field elision. A `PYC_DBG_DROPSTORE` probe
showed it never fires for `UCTNode.unexplored`.)

**The union has no runtime discrimination.** FA typed the receiver as
`{Square, UCTNode}`; the C representation of that union is a bare
`void *`, and the field access picks one member's layout statically.
Method *dispatch* on a union receiver already solves this — 
`poly_dispatch_classtag_targets` emits a classtag switch and uses each
concrete class's own slot index. **Field access has no equivalent.**

Note also that each class's `has` list has been polluted with the
other's field names (`Square` carries `losses`/`wins`/`bestchild`,
`UCTNode` carries `color`/`used`), which is what lets
`resolve_union_receiver` find the field on the "wrong" component in the
first place.

**Ruled out: this is NOT clone's record-type blindness.**
[121](121-CGEN-dead-clones-emitted.md) root-caused `equivalent_es_vars`
as unable to tell one record type from another, which merges contours of
different classes. That is a real hole, and it is not this one:
`IFA_DBG_VAREQ` reports **zero** `Square vs UCTNode` pairs on `go`. The
two classes are not merged by clone; the union is FA's own typing of a
variable that genuinely holds either.

## Fix directions

1. **Classtag-discriminated field access.** Mirror
   `poly_dispatch_classtag_targets` for the getter/setter: when the
   receiver is a union with more than one member carrying the field,
   emit a tag switch and use each class's own slot. Correct, and the
   most direct; costs a branch per polymorphic field access.
2. **Refuse instead of miscompiling. DONE 2026-09-01.** 122 Phase 0's
   check is now fatal in every mode, so these two silent crashes are two
   compile errors naming the class pair and the slot. Corpus:
   `compile_fail` 3 → 5, `run_fail` 44 → 42, nothing else affected. This
   does not fix anything — it stops pyc emitting a program that lies,
   and it is why the remaining options are worth doing.
3. **Give unions a representation.** [118](118-union-field-representation-and-polymorphic-field-offset.md)
   is the same family; a union with a tag would make both dispatch and
   field access uniform.
4. **Stop FA producing the union.** The receiver is only a union because
   some call site is shared; more precision there
   ([074](074-FA-cross-pass-oscillation-plan.md)/
   [030](030-DISPATCH-polymorphic-dispatch-fat-pointers.md)) removes the
   need — but cannot be relied on in general.

Options 1 and 2 are not exclusive: 2 is a day's work and stops the
miscompile now; 1 is the real fix.

## Verification plan

1. `go` must run to completion, and `bh` must stop dying in the
   collector. Neither has a CPython stdout oracle in `check` mode
   (`go` cpy_rc=0 — it does, actually; `bh` times out), so `go`'s output
   should be compared against CPython.
2. 122 Phase 0's check must report **zero** violations on both.
3. Full gate on both backends plus a `check` sweep — this touches the
   getter/setter, which every program uses.

## What this unblocks

Two of [102](102-corpus-programs-compile-then-abort-at-runtime.md)'s
compiles-then-crashes programs, with a named cause rather than a
bisection. More broadly it is the first crash in that bucket traced end
to end from a compile-time diagnostic to the faulting line — which is
the case for keeping 122 Phase 0's check and making it louder.

## 2026-09-03: bh needs a THIRD option — the base-class prefix

bh's failure is the clearest instance of this issue, and it rules out
"just fix the precision".

`Cell.hack_cofm` (bh.py:420) walks its octree children:

```python
r = self.subp[i]
if r is not None:
    mr = r.hack_cofm()              # polymorphic method dispatch -- fine
    tmpv.mult_scalar2(r.pos, mr)    # reads .pos on a GENUINE {Cell, Body}
```

`subp` legitimately holds both — internal `Cell`s and leaf `Body`s, both
`Node` subclasses — and `pos` is a **`Node` field that both have**. So
this is a shared-field read on a real union: precisely the "modulo OOP
dispatch" case, not imprecision. No precision work removes it.

What breaks is the offset. From the emitted C:

| field | `Cell` (`_CG_s16269`) | `Body` (`_CG_s16263`) |
|---|---|---|
| `mass` (from `Node`) | e22 | e25 |
| `pos`  (from `Node`) | **e23** | **e28** |
| own | `subp` e24 | `acc` e24, `new_acc` e26, `phi` e27 |

The inherited fields land at different indices because `Body`'s own
fields sort in among them under per-class name-sorted assignment. Hence
`'Cell' is blind-cast to 'Body' and read at e28, but member absent at
e27` — e28 is `pos`, e27 is `phi`.

Note [issues/121](../../issues/closed/121-sibling-subclass-field-layout.md)
already made `promote_field` name-sort so siblings agree on ORDER. That
is not sufficient: order agreement does not give offset agreement when
one sibling declares extra fields that sort before or between the shared
ones.

### The third option

This issue currently offers (1) per-class classtag dispatch on field
access, or (2) a global slot assignment. shedskin's answer is neither:
it emits real C++ classes, so `Node`'s fields sit at a **common prefix**
in every subclass by single-inheritance layout rules, and a
heterogeneous container is simply `list<Node *>` (confirmed — shedskin
translates bh and types `subp` exactly that way, while typing
`Tree.bodies` as `list<Body *>`).

**Lay inherited fields first, in the base's order, then the class's own.**
Then any shared-field read through a sibling union is a single `->eN`
with no dispatch and no global constraint — option 2's benefit without
option 2's cost of constraining every class in the program.

It also subsumes bh's imprecision for this purpose: the phantom `acc`/
`vel` promoted onto `Cell` (issues/039) would sit after the prefix and
not disturb `mass`/`pos`. So bh compiles under this change **whether or
not** the precision bug is fixed — which is the argument for doing this
first and 039 on its own merits.

## 2026-09-03: the analysis, and three wrong levers

Built the selective machinery: measure which hierarchies need a common
prefix, and reorder only those. `PYC_PREFIX_LAYOUT=1`, off by default;
`IFA_DBG_PREFIX=1` reports without changing anything.

### The analysis works and is very selective

`collect_prefix_groups` (clone.cc) walks every `P_prim_period` against
every EntrySet, collects receivers spanning two or more record classes,
and merges them into groups. A group is then kept only if two of its
classes disagree on a shared member's index — being polymorphic is not
by itself a reason to reorder.

| program | groups needing a prefix |
|---|---|
| bh | 2 — `{Exception, StopIteration, SystemExit}`, `{Cell, Body}` |
| go | 1 |
| tictactoe | 1 |
| chess | 1 |
| pygmy | **0** |
| sudoku1 | **0** |

`pygmy` is the instructive one: it has *three* genuine sibling unions
(`plane`/`sphere`, `spotshader`/`everythingshader`,
`pointlight`/`parallellight`) and needs nothing, because each pair has
an EQUAL member set and name-sorting already aligns them. The
requirement bites only on unequal sets — bh's `Cell`(27) vs `Body`(30).

### Three levers that look right and are not

1. **Reordering `Sym::has`.** It is an OUTPUT.
   `compute_member_types` rebuilds it from `cs->vars` positionally
   (`AVar *av = cs->vars[i]` → `has[i]->type`). Reordering it directly
   emitted `__str__` as `_CG_string` and then assigned a function
   pointer to it — raw clang errors. `determine_layouts` already says
   this: "cs->vars itself is left untouched (other code indexes it
   positionally)". **`cs->vars` is the array that decides layout.**

2. **Closing a group over its inheritance hierarchy.** Tempting after
   codegen wrote `BaseException::__str__` into a struct that had been
   reordered without it. But closure pulls in `object`, and since every
   class descends from `object` the group becomes the whole program —
   destroying exactly the selectivity this is for. The real cross-class
   assumption is in codegen's slot lookup (below), not in the grouping.

3. **`Vec::set_add` then iterating densely.** plib Vecs used as sets
   carry NULL holes; `set_to_vec()` first. This segfaulted the compiler
   inside a name `qsort`, which reads as a mysterious crash rather than
   an API misuse.

### Where it stands

With `cs->vars` reordered, bh's **layout-contract violation is gone**
and the exception-hierarchy errors with it — 4 clang errors down to 1:

```
bh.py.c: error: assigning to '_CG_ps16259' (aka Vec3*) from 'void *'
  ((_CG_ps16263)t1)->e22 = (void*)_CG_f_12221_110/*Body::walk_sub_tree*/;
```

`Body`'s slots 21 and 22 (`sub_index`, `walk_sub_tree` — both methods)
came out typed `Vec3*`. That is `compute_member_types` reading
`cs->vars[i]` across an equivalence group whose members my reorder left
mutually misaligned.

**The remaining obstacle is structural, not a bug in the patch.**
`Cell` and `Body` have different member COUNTS, and shared names can
only occupy identical indices in both if the shorter class is PADDED at
the positions the other fills. `cg_member_ctype` already has the
representation for that — `if (!s->has[i]->type) return "<placeholder>"`,
emitted as a zero-width `char eN[0]` — so the missing piece is creating
the padding ivars, which is `promote_field` territory and carries its
own FA consequences (a promoted field is a field FA then believes in).

Default path is unmeasured-safe: flag off, bh fails identically, and
`make test` is 309 passed / 18 known / 0 failed.

## Padding: where it can land, and what each choice costs

**Can it be done without impacting analysis? Yes, by construction.**
`determine_layouts` runs inside `clone()`, which runs after
`fa->analyze()` has returned. FA has converged; `add_var_constraint` and
the ES worklist are inert at that point. Any padding is post-analysis.

The real question is which DOWNSTREAM stage absorbs it. Three land in
different places.

### Why padding is needed at all (measured)

Shared-name-first ordering is not sufficient because CSs of the SAME
class have different member sets:

```
Body#1170 vars=24     Cell#1171 vars=22
Body#1406 vars=30     Cell#1611/#2111/#2114 vars=27
```

A CS missing one shared name shifts every later slot, which is exactly
how `Body`'s `sub_index`/`walk_sub_tree` ended up typed `Vec3*`. Aligning
requires every CS in the group to carry the SAME name sequence — i.e.
padding to the union of names, not merely sorting.

### Option 1 — pad `cs->vars` in `determine_layouts`

- Analysis: unaffected.
- **Cloning: AFFECTED.** `determine_basic_clones` runs after and uses
  `vars.n` to keep CSs distinct; padding equalises it, so CSs currently
  separate (the 22/27 and 24/30 pairs above) may merge.
- Codegen: automatic. A bottom-typed pad yields `sym_void`
  (`concrete_type_set_to_type` of an empty set is `sym_void`, not null),
  so it emits an ordinary 8-byte `_CG_void` member.
- Cost: 8 bytes per pad per instance.
- Effort: smallest, ~30 lines. Risk concentrated in clone merging, which
  needs a corpus A/B.

### Option 2 — pad after clone equivalence, before `compute_member_types`

Move the reorder+pad to between `determine_clones` and
`build_concrete_types`.

- Analysis: unaffected. **Cloning: unaffected** — equivalence is already
  decided.
- `compute_member_types` then sees a uniform name sequence and builds
  each struct from it. Its `assert(!n || n == cs->vars.n)` is satisfied
  because every CS in every affected eqcss group is padded to the same
  sequence.
- Cost: same 8 bytes per pad.
- Effort: small-moderate. **The sweet spot: no analysis and no cloning
  impact, and the layout still falls out of the existing positional
  machinery rather than a parallel one.**

### Option 3 — codegen-side slot map, `cs->vars` untouched

Compute a per-group canonical name→slot map and have codegen consult it,
emitting gaps for absent names.

- Analysis, cloning, `compute_member_types`: all unaffected.
- Cost: **zero** — `cg_member_ctype` already emits a null-typed entry as
  a zero-width `char eN[0]`, so gaps take no space.
- Effort: largest. Every place a member becomes an `eN` must use the map:
  the struct emitter, `cg_member_ctype`, both `P_prim_period`
  getter/setter paths, the polymorphic-slot registry, and the
  blind-cast/layout contract check.
- Semantically the cleanest and the only one with no memory cost; the
  risk is breadth of call sites rather than depth.

### Option 4 — do not pad

Restrict alignment to groups whose classes already have equal member
sets (which name-sorting handles today — pygmy), and leave unequal
groups to option 1 of this issue's original list, classtag dispatch on
field access. Does not fix bh, whose entire problem is unequal sets.

**Recommendation: option 2**, with option 3 as the principled endpoint if
the 8-byte-per-pad cost or a clone-order surprise turns out to matter.

## 2026-09-03 (later): option 2 implemented — bh COMPILES

Padding placed between `determine_clones()` and `build_concrete_types()`,
so it is after clone equivalence is decided (it cannot change which CSs
are equivalent) and before `compute_member_types` builds each struct.
`PYC_PREFIX_LAYOUT=1`, still off by default.

```
PREFIX applied: names=15 css=6 pads=4  { StopIteration Exception SystemExit }
PREFIX applied: names=33 css=6 pads=41 { Cell Body }
```

- **bh compiles**, no diagnostics. Corpus `compile_fail` 3 → 2.
- Suite **with the flag on**: 309 passed / 18 known / 0 failed —
  identical to the default path. `make test` green on default.

### Four things the implementation needs, each found the hard way

1. **`has` and `vars` must be permuted TOGETHER.**
   `compute_member_types` takes a member's NAME from `sym->has[i]` (it
   clones that entry) and its TYPE from `cs->vars[i]`. Moving only
   `vars` emitted `_CG_void e11 /* EPS */` where the baseline had
   `_CG_float64`; moving only `has` fails symmetrically. Neither array
   alone is "the layout".

2. **Match ivars by STRING, not `cs->var_map.get()`.** The map is keyed
   by pointer, so a lookup with an equal-but-distinct string misses and
   the CS's real ivar is replaced by a pad — silently losing its type
   (`EPS` `_CG_float64` → `_CG_void`, `IMAX` `_CG_int64` → `_CG_void`).

3. **The whole group must be ATOMIC.** Every class's `has` and every
   CS's `vars` permutation is computed first, and nothing is applied
   unless all of them succeed. Skipping one class or one CS by its own
   guards desynchronises the pair just as surely — that is what left
   `Boom` with slot 12 named `__str__` and typed `_CG_string`, then a
   method pointer stored into it. This was the last of three failing
   tests and fixing it took the flagged suite from 306/3 to 309/0.

4. **Pads need a TYPE donor.** A pad left at bottom becomes `sym_void`
   (`concrete_type_set_to_type` of the empty set), and a store codegen
   previously SKIPPED — because the CS had no such slot — then lands on
   a `void*` member and trips the "field type mismatch" guard
   (`Node.EPS = 0.05` into a padded `Body` clone). The pad takes the
   type the field has on some other CS in the group.

One codegen change was needed alongside: the polymorphic-slot registry
resolved a method's slot in the self FORMAL's class and stored it into a
different class's struct, which is equal only by luck. It now resolves
in `cs->sym`, falling back to the old value when the name is not found
there — the fallback matters, since recomputing unconditionally is what
the existing comment records as regressing `poly_dispatch_low/high`.

### bh still segfaults, and it is not this

```
#0 GC_clear_fl_marks (q=0x... <_CG_type_Body>)
#1 clear_all_fl_marks   #2 GC_finish_collection
```

A `_CG_type_Body` classtag pointer in a GC free list — the same shape as
chess's crash (`GC_clear_fl_marks(q=0xc)`), which happens with none of
this enabled. bh had never compiled before, so there is no baseline to
compare against, but the signature says heap corruption from emitted
code rather than a layout error. See ifa/118's chess section for the two
diagnostic dead ends already measured (valgrind finds nothing in emitted
code; a large `GC_INITIAL_HEAP_SIZE` does not avoid it).

### Remaining before this can default on

A corpus A/B. `make test` is not sufficient evidence for a layout
change: padding costs 8 bytes per pad per instance (bh: 41 pads across 6
CreationSets), and the analysis fires on 0–2 groups per program, so the
population that changes at all is small but needs measuring for both
exit codes and `ess`/`css`.

## Unused method slots: measured (`IFA_DBG_SLOTUSE=1`)

Padding costs 8 bytes per pad, which raises the obvious question — how
many slots are carrying their weight at all? A member only needs a slot
if something dispatches through it, and `cg_build_new_to_val_map`
already computes both halves of that: `poly_names` (every method name at
a POLYMORPHIC call site) and `cg_new_to_val_map` (every slot the
registry stores into). A member in neither is reached only by direct
calls.

| | live members | of which method slots | never dispatched |
|---|---|---|---|
| bh | 238 | 144 | **86 — 59% of slots, 36% of members** |
| richards | 452 | 224 | **191 — 85% of slots, 42% of members** |

Per class: bh `Cell` 8 of 33, `HG` 1 of 18; richards `DeviceTask` and
`HandlerTask` 15 of 42 each.

**A correction worth keeping.** The first version of this measurement
reported 65% for bh, because it counted any member that was neither
polymorphic nor stored — which is also true of every DATA field
(`mass`, `pos` are in neither set). Restricting to members whose name
matches some function in the program gives the figures above. The
inflated number would have oversold the change by ~2x.

### Eliminating them is not just `cg_field_live` returning 0

Dropping a member changes the BYTE OFFSETS of the members after it. The
`eN` suffix keeps the has-index, so the numbering does not shift
(issues/055), but the C struct layout does — so two classes reached
through one union receiver must agree on the live SET, or the blind-cast
contract (ifa/122) breaks in exactly the way this issue is about.
Elimination therefore has to be decided **per prefix group**, using the
same grouping already built here: drop a name only if it is dead in
every class of the group.

Two further checks before it can be sound, neither done:

- **The read side.** "Never dispatched" is established from the
  registry and the poly-call-site names. Other paths read a slot by
  index — the struct-copy loop (`cg.cc`, `%s->e%d = %s->e%d`),
  `__deepcopy__`, and any `getattr`-like route. Each must either skip
  eliminated members or keep them alive.
- **Interaction with padding.** Elimination shrinks the name union a
  prefix group has to agree on, so it should be applied BEFORE the pad
  is computed — the two are complementary, and doing them in the wrong
  order pads slots that were about to be deleted.

Report-only for now; `IFA_DBG_SLOTUSE=1`, no behaviour change. Default
path `make test` 309 passed / 18 known / 0 failed, and the flagged path
matches it.

### The two checks, done

**Check 1 — the read side. It changes the answer by 2x, twice.**

Every class-member site in codegen already consults `cg_field_live`:
the getter (`cg.cc:740`), the setter (`:802`), tuple/list construction
(`:689`), the polymorphic-slot store (`:1280`), struct emission
(`:2892`), and all five slot lookups in `codegen_common.cc`. So making
`cg_field_live` return 0 for a dead slot is in fact enough to elide it
everywhere. The one unguarded walker, `destruct_prim` (`cg.cc:461`),
copies every `->eN` without asking — but it `assert(0)`s on a non-tuple
with named members, so it never sees a class with method slots.

The failure modes are asymmetric, which is worth knowing before
enabling anything: a wrongly-eliminated slot that is READ degrades to
the `getter not resolved` runtime assert (loud), while one that is
WRITTEN is silently skipped (quiet). For method slots the only writer is
the polyslot store, which tests the same `cg_field_live` predicate, so
the two stay consistent.

What the analysis was missing is that a method slot is also used when
the member is read as an ATTRIBUTE — `f = obj.method` and every
attribute access whose selector names a method — through the generic
`P_prim_period` getter, which appears in neither `poly_names` (built
from polymorphic CALL sites) nor the store registry.

Adding it name-globally collapsed the opportunity to **zero**: any class
reading `foo` marked `foo` used on every class. Tracking it PER CLASS
(receiver CreationSets × selector) gives the real figure:

| | method slots | never used | a name-global set would have spared |
|---|---|---|---|
| bh | 134 | **55 (41%)** | 98 more |
| richards | 176 | **96 (54%)** | 117 more |

So the measurement went 65% → 59-85% → 0% → 41-54% as each correction
landed. Only the last is trustworthy, and it is still a large win:
richards' `HandlerTask` and friends drop real slots, and every remaining
one is genuinely dispatched or read.

**Check 2 — ordering against the pad. Eliminate FIRST; measured.**

| bh | live members | method slots | never used |
|---|---|---|---|
| without padding | 214 | 134 | 55 |
| with padding | 238 (+24) | 144 (+10) | 64 (**+9**) |

Padding adds ten method slots to bh and **nine of them are dead** — it
is manufacturing slots that elimination would immediately remove. Per
class, `Body` goes 30 → 33 members (dead 6 → 8) and `Cell` 27 → 33
(dead 4 → 8). Running elimination first shrinks the name union each
prefix group has to agree on, so the pad that follows is smaller and
almost none of it is dead weight.

Both remain report-only (`IFA_DBG_SLOTUSE=1`); default `make test` is
309 passed / 18 known / 0 failed.

### Use the call graph, not string matching — 93-98% of method slots are never read

The name-based analysis above is the wrong instrument, and the objection
that motivated replacing it is right: FA's call graph is precise, so a
call it resolves to one target is emitted as a DIRECT call and touches
no slot at all. Matching `P_prim_period` selectors counts every `x.f()`
as a slot read and therefore measures nothing useful.

`cg_note_slot_use(class, slot, is_read)` records what codegen actually
EMITS, at the five sites that touch a class member: the getter
(`cg.cc:771`, read), the classtag dispatch calling through the slot
(`:2418`, read), the setter (`:819`), the polymorphic-slot store
(`:1282`), and record construction (`:690`). Reads and writes are kept
apart deliberately — a slot only has to EXIST if something reads it, and
the polyslot store writes a vtable entry whether or not any dispatch
ever goes through it, so counting writes lets every stored slot justify
itself.

| | method slots | no access at all | **never read** |
|---|---|---|---|
| bh | 134 | 32 (23%) | **125 (93%)** |
| richards | 176 | 65 (36%) | **169 (96%)** |
| go | 208 | 57 (27%) | **204 (98%)** |

Nine slots in bh, seven in richards, four in go are genuinely dispatched
through. Everything else is written and never read.

**The measurement history is the point.** It went

    65%   of members   -- wrong: counted DATA fields
    59-85% of slots    -- wrong: no read side at all
    0%                 -- name-global reads, uselessly conservative
    41-54%             -- per-class names; wrong in BOTH directions
                          (23 slots it called dead are emitted; 22 it
                          kept alive are never touched)
    93-98% never read  -- emission truth, reads only

Only the last is trustworthy, and every earlier figure was produced by
string matching of one flavour or another. Names were never going to
answer this: the question "is this slot dispatched through" is a
property of the resolved call graph, and codegen already knows the
answer because it decides direct-call versus dispatch itself.

### What this means for elimination

The win is far larger than the padding question that prompted it. If
93-98% of method slots are never read, the vtable is very nearly
vestigial: eliminating a never-read slot removes both the member AND the
store that initialises it. bh's `Body` and `Cell` would carry a handful
of slots instead of ~20 each, which also shrinks the name union a prefix
group has to agree on — so this subsumes most of the padding cost rather
than merely preceding it.

Still report-only (`IFA_DBG_SLOTUSE=1`). Before enabling: the emission
set must be confirmed complete for class members (the five sites above
are believed to be all of them; `destruct_prim` reads `r->eN` but is
tuple-only), and a never-read slot's elimination has to be decided per
prefix group, since dropping a member still moves the byte offsets of
those after it.

Default `make test` 309 passed / 18 known / 0 failed.

### Attempting the elision: `write_c` is not idempotent

The elision itself is one line — `has[i]->type = nullptr` — because that
is the existing issues/055 + issues/121 placeholder path rather than a
new mechanism: the struct emitter then writes a zero-width
`char eN[0]`, keeping the `eN` numbering dense (issues/055 measured that
removing a slot outright breaks bh/block/chull/doom/rubik/softrender),
and `cg_field_live` becomes false so the polymorphic-slot store, the
getter and the setter all elide it by tests they already apply. Note the
struct emitter does NOT consult `cg_field_live` — it checks
`!has[i]->type` — so nulling the type is the only lever that reaches it.

Readership can only be known by emitting, and the struct definitions are
written BEFORE the function bodies, so this needs a throwaway discovery
pass: emit every body into a memstream that is discarded, purely to
populate `cg_slot_use_*`. The bodies are already buffered that way for
function DCE, so it looked like one extra emission per function.

**It does not work, and not for the reason expected.**
`PYC_ELIDE_SLOTS=1` fails 218 of 327 tests. `PYC_ELIDE_SLOTS=2` — the
discovery pass with the elision suppressed, eliding NOTHING — fails
**exactly the same 218**. So the elision criterion is not implicated at
all: `write_c` is **not idempotent**, and emitting a function twice
corrupts compiler state. The first casualty is
`cg_get_string(pn->rvals[i])` returning null in
`emit_send_default_prim`, i.e. a Var that had a codegen name during the
first emission and lost it.

That kills discovery-by-emission as a mechanism. Two ways forward, both
untried:

1. **Make `write_c` re-entrant** for the discovery pass. Needs the state
   it mutates identified first — the `cg_get_string` loss above is one
   thread to pull, and `cg_note_blind_cast` / `cg_note_imprecise` also
   accumulate across the extra pass and would double-count.
2. **Compute readership without emitting**, by replicating the two read
   conditions: a `P_prim_period` whose receiver resolves to this class
   with this selector, and the classtag dispatch's `(class, slot)`
   pairs. This duplicates logic that codegen owns, so it can drift —
   which is exactly the objection that retired the name-based analysis.

The measurement itself is unaffected and stands: 93-98% of method slots
are never read, from the same instrumentation, in a single emission.

Default `make test` 309 passed / 18 known / 0 failed; both flags off.

### Why the surviving reads are not dead: they are the real dispatches

Naming the read slots (`IFA_DBG_SLOTUSE=1`, `READ-METHOD` lines) answers
it. Two things fell out.

**Most "reads" are DATA fields, not slots at all** — `thelist`,
`position`, `i`, `j`, `handle`, `args`, `seed`, `d0`. Filtering to
members whose name matches a function leaves a very short list, and
every entry on it is a genuine polymorphic dispatch:

```
bh (9)        Exception / SystemExit / StopIteration  .__str__
              Body.e20 / Cell.e18   .load_tree
              Body.e22 / Cell.e20   .walk_sub_tree
              Body.e17 / Cell.e17   .hack_cofm
go (4)        StopIteration / AssertionError  .__str__
              UCTNode  .__not__ , .__pyc_to_bool__
richards (7)  Exception / StopIteration / AssertionError  .__str__
              DeviceTask / HandlerTask / IdleTask / WorkTask  .fn
```

So the vtable is not vestigial by accident — it is carrying exactly the
sites where FA could not resolve a single target. bh's six are the
octree walk over `subp`, a real `{Cell, Body}` union (`hack_cofm` reads
`r.pos` and calls `r.hack_cofm()` on it); richards' four are the Task
function dispatch over four subclasses; the `__str__` ones are `str()`
on an exception whose class is not statically known. These are the
"modulo OOP dispatch" cases, and they are irreducible without runtime
type information.

**And the two findings converge.** `Body.e20 load_tree` against
`Cell.e18 load_tree` — the same method, reached through the same union,
at DIFFERENT slot indices. That is the exact conflict
`collect_prefix_groups` reports (`'load_tree' at Cell[18] vs Body[20]`).
The handful of slots that survive elimination are precisely the ones on
classes that need the prefix alignment, which is a good sign that the
two pieces of work are aimed at the same small population rather than
overlapping.

The practical consequence: eliminating the 93-98% is not competing with
dispatch — nothing reads those slots, and the ones that ARE read are
few enough (4-9 per program) to enumerate and reason about individually.
