# 030 — Polymorphic dispatch via classtag comparison

**Status:** partial / open. Core mechanism (classtag dispatch)
implemented and correct on both backends since 2026-07-04, with a
closure-carrier mixing gap fixed 2026-08-06. The one remaining item
is a performance gap (if/else chain instead of table dispatch at high
fan-out), not a correctness one.

## Problem

When FA's type inference produces a union-typed receiver at a call
site (`f->calls.get(n)` has more than one candidate `Fun`,
`get_target_fun_core` returns null — e.g. `lhs.eval()` where `lhs` is
`Const | BinOp`), codegen must choose the right concrete
implementation at runtime instead of emitting a single direct call.
This issue covers how that choice gets emitted, on both backends, for
every shape such a call site can take: ordinary receiver-polymorphic
method dispatch, a nil-typed member of the receiver union, a bare
callable value with no receiver at all, and a closure-carrier value
(the object a decorator like `@double` synthesizes to hold captured
state).

Supersedes the simpler "indirect call through a single fixed struct
slot" sketch in [closed/029](closed/029-polymorphic-dispatch.md) —
that assumed every class in a union shares one layout, which per-class
dead-field elision breaks.

## Mechanism as built (classtag dispatch)

Every class-like record gets a `__pyc_tag` header field at offset 0
(C: a real struct member; LLVM: a leading pointer field, with
`llvm_fld()` shifting all other field GEP indices by one), stamped
once into the class prototype at `prim_new` (instances inherit it
through clone's memcpy) and pointing at that class's own
`_CG_type_<name>` object (LLVM: an internal global; only address
identity matters — see `cg_has_classtag`, `codegen_common.cc`).

**Excluded from tagging** (identified structurally: unnamed fields
only, plus the tuple/list/vector predicates): tuples, list-backed
tuples, vectors. `_CG_TUPLE_TO_LIST_FUN`'s memcpy and `_CG_list_ptr`
indexing treat these as bare element arrays with no header — tagging
them broke a 15-test regression, then (after fixing that) a 2-test
tuple regression from tuple clones missing from
`sym_tuple->specializers`. If you're ever tempted to add a classtag to
a raw-layout type, re-derive why these two things needed re-fixing
first.

At each polymorphic call site, codegen emits an if/else chain on the
tag: each branch casts the receiver to that class's own layout and
calls through that class's own stored method slot (kept from 029's
per-instance stored-pointer mechanism — same-class clones need no
extra branches, only cross-class union members do). A closure-carrier
candidate (see "closure-carrier mixing" below) skips the slot
entirely and calls the statically-known implementation directly,
since a carrier's implementation is never ambiguous at a given
dispatch site. A nil-typed union member (`if not self.field:` where
the field starts as `None`) gets its own branch, tested before any
tag dereference so the tag reads stay null-safe.

Shared candidate-resolution algorithm, `codegen_common.{h,cc}`:
`poly_dispatch_directly_owned`, `poly_dispatch_classtag_targets`,
`poly_dispatch_is_nil_receiver` — both backends call these to turn a
call site's candidate `Fun`s into concrete `(receiver class, method
slot, call-site rval index)` triples; each backend still does its own
filtering and emission around the shared result (see "Known backend
divergences" below — the two are not identical).

This is a materially simpler mechanism than the fat-pointer / indirect
vtable-slot design originally sketched for this issue (kept below,
"Original design sketch," since it's still the fallback plan for the
one open item). The if/else-chain + per-class stored-slot approach
handles every fan-out tested so far correctly —
`poly_dispatch_shared_method_extra_args.py` dispatches 11 subclasses
correctly on both backends — it's simply O(n_branches) per call site
rather than O(1).

## Root causes worth remembering

**FA fixpoint sub-bug (fixed 2026-07-04, `fa.cc:make_closure_var`).**
Before this fix, clones of a polymorphic receiver ended up with
void/dead result vars ("expression has no type," papered over by
`convert_NOTYPE_to_void`). Root cause: bound-method closure
`CreationSet`s persist across analysis passes (cached in the result
`AVar`'s `cs_map`), but every pass clears all `AVar` state
(`clear_results`, including `closure_used`) and re-derives it.
`make_closure_var` flowed each field's value into
`unique_AVar(av->var, cs)` — keyed by whichever `Var` carries the
value *this* pass — while consumers (`partial_application`'s `fun =
cs->vars[0]`) read the *positional* slot created by the pass that
built the CS. After the receiver CS split between passes, the method
value arrived via a different `Var`, the re-derived flow landed in an
orphan `AVar`, `cs->vars[0]` stayed bottom, and the closure's call
site returned "incomplete" forever. Fix: when the CS's positional
slots already exist and `iv != cs->vars[i]`, also flow into
`cs->vars[i]`, keeping the positional slot fed regardless of which
`Var` carries the value that pass. This is a general FA-correctness
fix, not specific to dispatch — worth knowing if a similar
"positional slot goes stale across passes" symptom shows up elsewhere.

**Closure-carrier mixing (fixed 2026-08-06, both backends).** A
function value that's sometimes a plain top-level function and
sometimes a decorator-synthesized closure carrier (e.g. `make_dispatcher()`
returning either `add_one` or `double(add_one)` depending on a runtime
flag) crashed on the C backend
(`assert(!"runtime error: polymorphic dispatch: no branch matched")`)
and silently read garbage on LLVM (uninitialized `alloca`, a fresh
wrong value every run — worse than the C crash). Root cause: a carrier
struct only holds captured variables, never a field named after
itself, so `poly_dispatch_classtag_targets` (which looks for a live
field matching the candidate's own method name) always returns empty
for it — the carrier candidate fell through to the plain-function
*value-identity* route, which compares the runtime value against the
candidate's own code address. A carrier value is a heap pointer to
what `double()` allocated, never that address, so the branch could
never match.

Fix: when a candidate's own first live formal resolves to the
synthesized closure-carrier record (name `"__closure__"`, reserved —
no user class can be named that), route it into the classtag
`classes[]` partition with a sentinel (`slots[ci] == -1`) meaning
"call this candidate directly, no stored-slot indirection" — no slot
is needed because, unlike the classtag route's original purpose
(runtime polymorphism across implementations sharing a layout), a
carrier candidate *is* the statically-known implementation for its
own tag. Two distinct carrier `Sym`s colliding on the shared
`"__closure__"` name at the same call site (not solved by this fix —
see "Deliberately not done" below) degrades to the existing
`ok = false` → runtime-assert path instead of silently mis-dispatching.

The LLVM half needed a second, structural fix beyond porting the
carrier-direct check: `cg_emit_llvm.cc`'s per-candidate loop was
all-or-nothing — the moment `poly_dispatch_classtag_targets` returned
a *nonempty-but-not-classtag-eligible* `rts` (which happens for an
ordinary plain function too, since the resolver scans every formal
position for a same-named field, not just `self`), the loop bailed
(`ok = false; break`) and abandoned classtag dispatch for the entire
call site — unlike `cg.cc`'s shape, which falls through to try
nil-receiver/carrier/plain-function regardless of *why* nothing was
added. Fixed by restructuring `emit_send_call`'s loop to match
`cg.cc`'s control flow: merge into `classes[]` whenever `rts.n != 0`,
then unconditionally try nil-receiver, then carrier-direct, then a new
`plains[]` value-identity partition (ported from `cg.cc`'s bare-value
route, sharing IR structure via a new `emit_direct_call` lambda,
rather than the old separate fully-independent bare-callable pass). A
call site with zero classtag-eligible candidates at all is unaffected
— it still falls through unchanged to the pre-existing bare-callable
pass.

New regression test: `tests/poly_dispatch_carrier_mixed.py`.

**Unrelated bug found in the same code path:**
[closed/058](closed/058-polymorphic-classtag-dispatch-drops-extra-arguments.md)
(2026-07-19) — both backends' classtag-dispatch branch hardcoded a
single-`self`-argument call, silently dropping every formal beyond
`self` for an inherited (not overridden) method reached through a
genuinely polymorphic receiver. Independent of this issue's two
structural gaps, but evidence the if/else-chain path can have its own
latent bugs — worth a second look if touching this code again.

## What's still open

**Table/indirect dispatch for high fan-out.** No
`DISPATCH_THRESHOLD` or table-lookup code exists in either backend —
every polymorphic call site still emits an unconditional if/else
chain regardless of fan-out. Confirmed functionally correct up to
11-way fan-out (`poly_dispatch_shared_method_extra_args.py`), so this
is purely an O(n_branches)-per-call-site cost today, not a correctness
gap. Revisit if profiling shows it matters; "Original design sketch"
below (fat pointer + vtable slot indexed by classtag) is the fallback
plan.

**Deliberately not done:** unique per-call-site naming for
closure-carrier candidates, to disambiguate two distinct carriers that
both land on the shared reserved name `"__closure__"` at the same
dispatch site. That's real vtable-style infrastructure with its own
design cost; today's collision case degrades safely (runtime assert)
rather than mis-dispatching, so it's deferred until a real program
hits it.

## Known backend divergences

`cg.cc` and `cg_emit_llvm.cc` share the candidate-resolution algorithm
(`codegen_common.cc`) but still apply different filters/guards around
its result — confirmed current, not historical:

- **Classtag-eligibility filter** on a resolved receiver type: `cg.cc`
  requires `cg_has_classtag(rt) && cg_get_string(rt)` (`cg.cc:1621`);
  `cg_emit_llvm.cc` requires `rt->name && !rt->is_system_type &&
  cg_has_classtag(rt)` (`cg_emit_llvm.cc:2816`).
- **Unnamed (lambda) candidates**: `cg.cc`'s per-candidate loop only
  requires `fun_val->sym` to proceed (`cg.cc:1487`), so an unnamed
  candidate still reaches `poly_dispatch_classtag_targets` (which
  itself returns empty for it) and falls through to the plain-function
  route; `cg_emit_llvm.cc`'s loop bails the *entire call site*
  (`ok = false`) the moment any candidate has no `sym->name`
  (`cg_emit_llvm.cc:2787`).

Neither has a known failing repro today — recorded because the next
person touching this dispatch machinery should know the backends
aren't identical here before assuming a fix in one applies to the
other.

## Cross-references

- [closed/029](closed/029-polymorphic-dispatch.md) — superseded by
  this issue; the simpler fixed-slot sketch.
- [closed/058](closed/058-polymorphic-classtag-dispatch-drops-extra-arguments.md)
  — extra-args-dropped bug found in this dispatch code, unrelated root
  cause.
- [079-DISPATCH-single-candidate-dispatch-unchecked-cast.md](079-DISPATCH-single-candidate-dispatch-unchecked-cast.md)
  — a distinct, separate gap in the *single*-candidate fast path (not
  the polymorphic classtag chain this issue covers): an unchecked cast
  when the receiver's union has another member that doesn't implement
  the method at all. Not attempted.
- [025-FA-intra-function-union-narrowing.md](025-FA-intra-function-union-narrowing.md)
  — `isinstance(x, C)` against a union-typed `x` is broken, but the
  root cause is 025's own union-narrowing gap in the shared
  `isinstance()` wrapper clone, not this issue's classtag mechanism
  (monomorphic and single-class `isinstance` checks both work
  correctly).
- `../../issues/007-decorators-not-applied.md` — bare callable-value
  dispatch (no receiver at all, e.g. a plain function variable
  reassigned by a decorator) is handled by the plain-function
  value-identity route described above, landed as part of 007's own
  fix; not a gap in this issue.

## Original design sketch (kept for the still-open table-dispatch item)

The mechanism actually built (above) uses a simpler if/else chain with
per-class stored method slots, not the indirect vtable design below.
This section is retained because it's the design to pick back up if
the O(n_branches) fan-out cost above is ever worth fixing.

A **fat pointer** is a `(data*, vtable*)` pair (or equivalently a
tagged union pointer). Instead of an if/else chain at every fan-out,
choose the emission strategy by fan-out:

- **Low fan-out (≤ N, say 4):** conditional tree (today's mechanism) —
  full compiler visibility per branch, enables inlining.
- **High fan-out (> N):** table lookup or indirect call through a
  vtable pointer indexed by classtag. O(1) dispatch cost, no
  per-branch code-size growth.

The classtag this issue already materializes at allocation time is
exactly the discriminant a table-lookup implementation would index by
— no new backward-flow analysis is needed to build the tag; only the
table-emission side (in `cg.cc` / `cg_emit_llvm.cc`) is unbuilt.

## Verification

`tests/poly_dispatch_{low,high,swapped,partial_override,
carrier_mixed,shared_method_extra_args}.py` — all pass strictly (no
`.check_fail` sidecars) on **both** backends (`./test_pyc.py` and
`PYC_FLAGS=-b ./test_pyc.py`). Regression-swept against the shedskin
corpus (`shedskin_sweep.sh`, both backends) with zero behavior
changes outside the targeted fix each time. `ifa --test` unaffected
(pure codegen change, no IF1/FA surface).

## What this unblocks

Any Python program using ordinary OOP polymorphism (a method called
through a variable/field/return value typed as a union of sibling
classes) — which is most of the shedskin example corpus and any
realistic class hierarchy. Without this, such call sites either failed
to compile or picked one clone arbitrarily.
