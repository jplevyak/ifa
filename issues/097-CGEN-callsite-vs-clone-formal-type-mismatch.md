# 097 — An ordinary call site's argument type can diverge from the specific callee clone's formal parameter type (msp_ss.py's last two compile errors; guarded, root cause found and precisely located, fix not yet implemented)

**Status: PARTIAL, guard landed 2026-08-11, root cause found and
confirmed 2026-08-11. NOTE 2026-08-13: `entry_set_compatibility` has
changed under this issue —
[100](100-FA-display-removed-from-contour-identity.md) removed its
`edge_nest_compatible_with_entry_set` gate, so the candidate set it
scores is strictly wider now, and its soft `val -= 4` type score is
correspondingly more load-bearing. That is this issue's exact mechanism,
so the trace below should be re-taken before the fix is designed; two
attempts to make the detach route reuse contours (074's 2026-08-13
census) failed precisely on that soft score.** The compile-blocking symptom (hard C++
overload-resolution error) is fixed — see "RESOLVED (partial)" below.
The underlying mechanism is now traced and confirmed via two rounds of
instrumentation (added and fully reverted both times): `entry_set_
compatibility` (`ifa/analysis/fa.cc:1059`) scores a candidate
`EntrySet`'s compatibility against a **momentary snapshot** of its
accumulated formal-parameter type — and that snapshot can be taken
*before* the ES's own already-committed callers (here, `str.strip()`'s
constant-string comparisons) have had their contribution (re-)flowed
into it in the current pass, making it look emptier — and thus more
"compatible" — than it truly is. Directly confirmed, not inferred: the
specific edge in question was checked against 7 candidate `EntrySet`s
and scored fully compatible with exactly 1 of them, the one whose type
happened to be momentarily unpopulated. See "ROOT CAUSE FOUND (not a
duplicate)" at the bottom for the full trace, including a correction
to this doc's own first (wrong) draft of the mechanism. Confirmed
**not a duplicate** of 076, 030, 018, or 045's `clone_for_constants`
machinery (each checked and ruled out explicitly with reasoning).
**A fix (resequencing) was attempted and reverted** — see "Fix
attempted, implemented, regressed 3 tests, reverted" below — it
correctly implemented the resequencing option but tripped a
pre-existing, more fundamental gap, filed separately as
[098](098-FA-per-pass-reset-scoped-to-reachable-set.md). (098 was
initially diagnosed as per-pass order-dependence; it was re-root-caused
on 2026-08-12 as FA's per-pass reset being scoped to the *previous*
pass's reachable set, so edges carry stale per-pass state forward. The
reordering here most likely changed which contours a pass reaches, and
therefore which edges the reset covers. **098's fix landed 2026-08-12**,
so this resequencing attempt is now worth repeating — its regression
should be gone.) Left open (not moved to `closed/`) since this issue's
own fix has not landed.

**Original status:** open, found 2026-08-11 while implementing
[096](closed/096-extend-c-call-salvage-guard-past-str-comparisons.md).
Not root-caused — filed with concrete evidence and hypotheses, per
this directory's convention for something not yet deeply
investigated (see `ifa --test`-scale investigations like
[closed/076](closed/076-mutation-driven-receiver-divergence-not-cloned.md)
for how large tracing this family of bug can turn out to be — don't
attempt blind).

**Affects:** ordinary (non-`__pyc_c_call__`, non-primitive) function
call codegen — `ifa/codegen/cg.cc`'s `write_send_arg` (~1066, the
per-argument emission for a `SEND` to a resolved `Fun` clone) and/or
whatever upstream mechanism selects/reuses which clone a given call
site's edge is routed to (`get_target_fun_core`, `find_best_entry_sets`
— not yet traced which). LLVM backend parity not checked.

**Related:**
[096](closed/096-extend-c-call-salvage-guard-past-str-comparisons.md) —
found while verifying that issue's own fix against `msp_ss.py`; 096's
fix (extending the `__pyc_c_call__` and `prim_is_binary_operator`
salvage guards) resolved 7 of `msp_ss.py`'s 9 original compile errors
cleanly. These 2 are structurally different — not a `__pyc_c_call__`
or generic-primitive argument, but the call site of an ordinary
function, where the *callee's own resolved clone* has a formal
parameter type that doesn't match what this specific edge actually
passes. [closed/076](closed/076-mutation-driven-receiver-divergence-not-cloned.md)
— the closest documented precedent for "a shared/reused clone gets
called from an edge whose actual type doesn't match what an earlier
edge established," albeit for container-element flow, not function
parameters; worth reading first if picking this up, both for the
mechanism (monotonic `AType` growth, clones reused across passes) and
for how large a fully-traced fix in this family turned out to be.
[030](030-DISPATCH-polymorphic-dispatch-fat-pointers.md) — the general
polymorphic-dispatch-target-selection mechanism; NOT obviously the
same bug (that's about choosing among *multiple candidate Funs*; here
there is exactly one resolved target, `get_target_fun_core` presumably
already returns non-null) but close enough in territory to check.
[018](../../issues/018-dict-mixed-key-types-boxing-failure.md) — same
session's other `msp_ss`/`rdb`-adjacent finding, a different mechanism
(container-method-vs-scalar `sizeof_element` gap), not a duplicate.
[closed/073](closed/073-teach-splitter-productive-vs-inert-context.md)
— found via a later survey (2026-08-11): direct, already-landed
precedent that `entry_set_compatibility`'s *soft* type scoring
(`val -= 4` for a mismatch, not outright rejection — the exact
mechanism this issue's own root cause exploits) is a known correctness
hazard on a **different** call path through the same function:
`check_split`'s recursion-routing branch used to fall through to
`find_best_entry_sets` on a nest-compatibility failure, and that soft
match "merged contours it shouldn't" (regressed `match_seq`) until the
fix required *hard* type equality for that specific path instead. Worth
reading in full before designing 097's own fix — it's evidence this
function's soft-matching behavior has bitten before, in a sibling
mechanism, and was fixed with the same kind of "require exact match,
don't just penalize" tool this issue's own options reach for.
[098](098-FA-per-pass-reset-scoped-to-reachable-set.md)'s own survey
has more: [closed/057](closed/057-sorted-tolist-fa-nonconvergence.md)
(same function's soft-matching also caused thousands of non-productive
contour mints, before 073's fix — different direction, same root
cause family) and [055](055-FA-set-dunder-method-triggers-fa-nonconvergence-on-plcfrs.md)
(still open, same convergence-loop family, different trigger).

## Symptom

`shedskin_examples/msp_ss/msp_ss.py` (after 096's fix lands) fails to
compile with 2 remaining errors, both the same shape:

```
msp_ss.py.c:9274:10: error: no matching function for call to '_CG_f_627_312'
 9274 |   t146 = _CG_f_627_312/*str::__eq__*/(t41, t32);
      |          ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
msp_ss.py.c:386:10: note: candidate function not viable: no known
conversion from '_CG_int64' (aka 'long long') to '_CG_any' (aka
'void *') for 2nd argument
386 | _CG_bool _CG_f_627_312/*str::__eq__*/(_CG_string a1, _CG_any a2);

msp_ss.py.c:32892:9: error: no matching function for call to '_CG_f_3483_261'
32892 |   t14 = _CG_f_3483_261/*chr*/(t15);
       |         ^~~~~~~~~~~~~~
msp_ss.py.c:29685:12: note: candidate function not viable: no known
conversion from '_CG_int64' (aka 'long long') to '_CG_any' (aka
'void *') for 1st argument
29685 | _CG_string _CG_f_3483_261/*chr*/(_CG_any a1) {
```

Both are a genuine `int64` value handed to a callee whose *resolved C
signature* declares that parameter `_CG_any` (`void*`) — a hard,
unrecoverable C++ overload-resolution error, not a warning.

## Two different concrete shapes, both traced to the call site (not yet further)

**`str::__eq__` — the callee has MULTIPLE clones, the wrong one is
called.** `msp_ss.py.c` declares three separate clones of this method
(`grep -n '_CG_f_627_31[234]' msp_ss.py.c`):

```
_CG_bool _CG_f_627_312/*str::__eq__*/(_CG_string a1, _CG_any a2);
_CG_bool _CG_f_627_313/*str::__eq__*/(_CG_string a1, _CG_int64 a2);
_CG_bool _CG_f_627_314/*str::__eq__*/(_CG_string a1, _CG_string a2);
```

i.e. FA *did* specialize per actual second-operand type — clone `313`
exists specifically for an `int64` `a2`. The failing call site
(`msp_ss.py.c:9274`) calls `_627_312` (the `_CG_any` clone) with an
`int64` actual (`t32`) anyway. Whether this is dispatch picking the
wrong existing clone for this edge, or codegen's `write_send_arg`
resolving the wrong `Fun*`/`EntrySet` for this `PNode`, isn't
determined — both would produce exactly this symptom.

**`chr` — only ONE clone exists, reused across two call sites with
different actual types.** Both call sites (`msp_ss.py.c:32800` and
`:32892`) call the same `_CG_f_3483_261/*chr*/(_CG_any a1)`. The first
is fine — its argument-producing move explicitly casts:
`t15 = (_CG_any)t16;`. The second is not — its argument-producing move
is a bare, uncast `t15 = t16;`, and `t16` there resolves to `_CG_int64`
(from a *different* `__list_iter__::__next__` clone, `_2258_45` vs. the
first call site's `_2258_376` — two distinct iterators over
differently-typed lists in the source). Since C has no SSA, both
branches share the same C-level temp-variable name `t15`, but they're
different `Var`s at the IR level with different resolved types; the
second one was apparently never checked against the one clone `chr`
ended up with. This matches 076's documented shape almost exactly:
"long-lived clones, reused across multiple calls ... each pass's
`AType` union only ever adds."

## Root cause (not traced — hypothesis only)

Not instrumented or traced beyond the evidence above. Plausible
candidates, roughly in order of how well they fit the two shapes
observed:

1. **Clone/`EntrySet` selection picks (or reuses) a target whose
   formal-parameter type doesn't match this specific edge's actual
   argument**, the same "shared, long-lived clone reused across
   many calling edges whose own types have since diverged" mechanism
   076 root-caused in detail for container-element reads/writes
   (`P_prim_index_object`/`P_prim_set_index_object`). If so, a fix
   here is probably not scoped to codegen at all — it would live in
   whatever routes a call edge to a specific `Fun`/`EntrySet`
   (`get_target_fun_core`, `codegen_common.cc`'s shared
   candidate-resolution algorithm referenced by 030), and could carry
   the same architectural weight 076 found for its own fix options
   (per-edge argument reasoning that the current "one shared formal
   per `(Var, EntrySet)`" model doesn't support).
2. **A codegen-only gap**: `write_send_arg` (cg.cc:1066) already has
   voidish-arg-to-concrete-formal cast logic (~1087-1110) for the
   *opposite* direction (call-site arg is `_CG_any`, formal is
   concrete — cast the arg to the formal's type, safe because
   `_CG_any` is `void*` and C allows an implicit/explicit
   pointer-to-anything cast). It has no symmetric handling for *this*
   direction (formal is `_CG_any`, arg is a concrete scalar) — and
   critically, the correct behavior for this direction is NOT "add a
   cast" (an `int64`→`void*` reinterpretation would compile but is
   semantically nonsense, not a real pointer) but a salvage guard
   (assert/fail), same convention as 056/077/096. If (1) turns out to
   be correct — the clone really is the wrong/stale one for this edge
   — this alone wouldn't be a real fix (would silently trap valid
   programs whose *dispatch* is simply broken), only a Band-Aid that
   converts a compile error into a runtime trap. Worth checking (1)
   first before spending effort here.

## Why not fixed alongside 096

Structurally different from everything 096 touched:
`__pyc_c_call__`'s target function has one fixed, unconditional C
signature declared inline at the call site (`c_call_arg_type_mismatch`
compares against that literal declaration); `prim_is_binary_operator`'s
family is a fixed macro shape with a known, uniform 2-operand
contract. Here there is no single declared contract to check against
— the "correct" type is whatever the *specific selected clone*
happens to declare, and (per the `str::__eq__` evidence) multiple
differently-typed clones can coexist for the same method, so a guard
would need to know it's looking at the actual resolved target, not
just pattern-match a function name. Given 076's own conclusion that a
correct, general fix for "shared/reused clone whose accumulated type
info is stale relative to a specific edge" is a multi-session,
architecture-level effort, attempting a fix here inline with 096
risked exactly the kind of blind, unscoped change this repo's issue
process is meant to avoid.

## Verification plan

1. Instrument `write_send_arg` (or wherever the target `Fun`/clone
   gets resolved for a `SEND` PNode) on `msp_ss.py`'s two failing call
   sites specifically, the same way 076 instrumented
   `P_prim_index_object` directly — confirm which of the two
   hypotheses above (wrong-clone-selected vs. stale-shared-clone) is
   actually happening, and for `str::__eq__` specifically, why clone
   `313` (the exact `_CG_int64` match already generated) isn't the one
   this edge routes to.
2. Once root-caused: a fix should make `msp_ss.py` compile without
   errors (0 remaining, down from 096's residual 2) or degrade
   cleanly to the established runtime-assert convention if a
   type-safe general fix isn't reachable this round.
3. Full `ifa --test` + `test_pyc.py` both backends + `shedskin_sweep.sh`
   — same battery 096 and 076 both used, since anything touching
   clone/dispatch selection risks the same broad blast radius 076
   found.

## What this unblocks

`msp_ss.py` compiling completely (096 got it from 9 errors down to
these 2; this is the last blocker). If hypothesis 1 is confirmed, this
could be a second, independently-reachable instance of 076's "shared
clone reused across incompatible edges" family — for function-call
dispatch rather than container-element access — which would be
significant: 076 already flagged its own mechanism as possibly "the
single most foundational open gap in pyc's 'no type' family," and this
would be evidence it generalizes beyond containers.

## RESOLVED (partial), 2026-08-11: compile symptom guarded, root cause still open

Per this doc's own verification plan step 2 ("or degrade cleanly to
the established runtime-assert convention if a type-safe general fix
isn't reachable this round") — pursued step 1 (instrument to
distinguish the two hypotheses) far enough to confirm the mismatch is
real and edge-specific (see the `str::__eq__`/`chr` evidence already
above, gathered via direct inspection of the generated `.c` rather
than added instrumentation — the three-clones-but-wrong-one-called and
one-clone-reused-with-an-uncast-move shapes were already conclusive
enough without needing to add temporary logging to `fa.cc`/`clone.cc`).
At the time this guard landed, did **not** trace further into *why*
clone/dispatch selection produces this — confirmed via
`clone_functions()` (`ifa/analysis/clone.cc:1081`) that `Fun::calls` is
built by iterating every `EntrySet` and adding each one's resolved
edge target into a `Vec<Fun*>` keyed by (cloned-)`PNode`, deduped by
pointer identity (`vf->set_add(ee->to->fun)`) — so if the wrong target
is the *only* one ever added for this specific `(Fun clone, PNode)`
pair, the bug is upstream of this loop (in whatever routes a specific
edge to a specific `EntrySet` during FA proper, e.g.
`find_best_entry_sets`/`entry_set_compatibility`, matching this doc's
hypothesis 1) — but this was reasoned from reading the code, not
confirmed by an actual instrumented run at that point. **Update:** that
deeper trace was done in a follow-up pass the same day — see "ROOT
CAUSE FOUND" below. It confirms hypothesis 1's general shape but with
a much more specific mechanism than either original hypothesis
guessed.

**What was actually fixed**: a general codegen-level guard, not scoped
to `msp_ss.py` specifically — `ifa/codegen/cg.cc`'s `emit_send_call`
now pre-checks, for every ordinary (non-primitive, non-`__pyc_c_call__`)
call site with simple (non-closure, non-tuple-projection) positional
arguments, whether the resolved target's formal parameter is voidish
(`_CG_any`/`_CG_void`/`_CG_nil_type`) while the actual argument is a
concrete *scalar* (`num_kind` true). Only that direction is unsafe — a
real pointer type flowing into a voidish formal already compiles fine
as-is (implicit pointer-to-`void*` conversion), matching
`write_send_arg`'s existing opposite-direction cast just below (arg
voidish, formal concrete: cast, since `void*`-to-anything is always a
legitimate reinterpretation). A scalar's bit pattern reinterpreted as
a pointer would not be a real value, so this guards (`assert(!"...")`
permissive / `fail(...)` strict, the same 056/077/096 convention)
instead of emitting either malformed C or a silently-wrong cast. This
is a genuine "same bug class, different codegen path" case, not a
one-off patch for these two call sites — the `str::__eq__` clone `312`
and `chr`'s clone are both still used correctly by their *other*,
non-mismatched call sites in the same generated file (verified: lines
2422/2476 still call `_CG_f_627_312` directly, line 32800 still calls
`_CG_f_3483_261` directly — only the two specific mismatched edges at
9274/32892 got redirected to the guard).

**Verified:**
- `msp_ss.py` now compiles clean: `pyc -D. msp_ss.py` exits 0, binary
  produced, zero compile errors (down from 097's original 2, from
  096's original 9).
- `ifa --test`: 58/58.
- `test_pyc.py`, both backends: 265/14/0/4, unchanged.
- `shedskin_sweep.sh`: diffed directly against the pre-fix FAIL list —
  exactly one line changed, `msp_ss FAIL` removed (now
  `COMPILED_C_WARN`), nothing else regressed or changed.
- Sanity-ran the compiled `msp_ss` binary: it hits a *different*,
  pre-existing runtime assert unrelated to this fix ("list element
  type mismatch", not "call argument type mismatch") on its `--help`
  path — expected and out of scope; this issue was always about the
  compile-time failure, not `msp_ss.py`'s full runtime correctness
  (never claimed or tested), and plenty of corpus programs are
  `COMPILED_C_WARN`-not-runtime-verified already.

**What's still open**: the root-cause question above — left as this
doc's own unresolved thread, exactly where a future investigator
should pick up (start with the `find_best_entry_sets`/
`entry_set_compatibility`-family instrumentation this section
describes, not a fresh investigation from scratch).

## ROOT CAUSE FOUND (not a duplicate), 2026-08-11

Traced with temporary instrumentation in `clone_functions()`
(`ifa/analysis/clone.cc:1081`, added and fully reverted after —
`git diff` on this file is empty; same discipline closed/076 used).
Printed, for every edge whose target `EntrySet` was one of the two
`str::__eq__` EntrySets involved (607, the wrongly-shared one; 737,
the correctly-typed one), the calling `EntrySet`'s owning function name
and the target's own formal-parameter types. Answer, directly:

```
ES 607 (-> codegen _CG_any):  reached by strip (x2), loadTIText (x1)
ES 737 (-> codegen _CG_int64): reached by loadTIText (x1), __getitem__ (x1)
```

`msp_ss.py`'s `str.strip()` (`__pyc__/01_str.py:147-151`) compares
`self[i]` against four string literals (`" "`, `"\t"`, `"\n"`, `"\r"`);
`loadTIText` (`msp_ss.py:748-749`) compares `l[0]` against
`ord('q')`/`ord('@')` — two adjacent, structurally identical `if`/
`elif` branches in the same function, same loop, same receiver shape.
One (`ord('@')`) correctly got its own `_CG_int64`-typed `EntrySet`
(737); the other (`ord('q')`) landed on `strip`'s pre-existing,
string-literal-only `EntrySet` (607) instead of getting (or reusing)
one shaped like 737's.

**Confirmed exact mechanism (corrected from an earlier, wrong draft of
this section — see "Correction" below).** A follow-up instrumentation
pass traced this to a precise **timing** bug, not a data-representation
gap:

1. **Constants get real types, exactly as expected — confirmed
   directly.** `add_var_constraint` (`fa.cc:1387-1389`) runs
   `update_gen(av, make_abstract_type(s))` for any constant `Sym`, and
   `make_abstract_type` (`fa.cc:220-226`) builds a genuine singleton
   `CreationSet(s)` for it — precisely the "constant type symbol,
   subtype of the primitive type, singleton CS" mechanism this issue
   should have assumed from the start. Instrumenting `analyze_edge`
   (`fa.cc:3080-3090`, added and reverted) confirmed this empirically:
   `strip`'s constant-`"\r"`-etc. edges flow a real, non-empty,
   singleton-CS-backed type into ES 607's `x` formal exactly like any
   other value would.
2. **The formal's abstract type is a real, correct 2-member union —
   also confirmed directly.** The same trace watched ES 607's `x`
   formal's accumulated `AType` grow from size 1 (after `strip`'s
   constant edges) to size 2 once `loadTIText`'s `edge=3623` (the
   `ord('q')` edge) also flowed through it — i.e. FA's own abstract
   layer correctly recorded "this formal has been fed both a `str`
   constant and an `int64`," a real, honest union, not a lost/dropped
   fact.
3. **The actual bug: `entry_set_compatibility` scored the ROUTING
   decision against a snapshot of ES 607 taken BEFORE that union
   existed.** Instrumenting `entry_set_compatibility` itself
   (`fa.cc:1059`, added and reverted) for every `__eq__`-targeting
   candidate check gave a direct, unambiguous answer for
   `loadTIText`'s `edge=3623`: it was scored against **7** candidate
   `EntrySet`s (153, 602, 603, 604, 605, 606, 607) — **6 of them
   (153, 602-606) correctly detected a type conflict** (`etc=0`, the
   soft-penalty branch of `edge_type_compatible_with_entry_set`,
   `fa.cc:874`) — **only 607 scored fully compatible** (`etc=1`, no
   conflict detected at all). That's only possible if 607's own
   accumulated `x` type was still *empty at that specific moment* —
   i.e. this routing check ran **before** `strip`'s own constant edges
   had been (re-)flowed into 607 in this pass, even though they are
   607's real, permanent, already-committed contributors. `find_best_
   entry_sets` (`fa.cc:1191`) picks the single best-scoring existing
   candidate with no comparison against "mint a fresh, unpenalized
   `EntrySet` instead" — 607's momentary, artifactual full-compatibility
   score won outright, and the edge was routed there **permanently**.
   Only afterward did `analyze_edge` catch 607's formal back up to its
   real, accumulated union (the size 1→2 growth in point 2) — by which
   time the routing decision was already fixed and nothing revisits it.

**Correction to this section's first draft**: it originally claimed
constants "bypass normal type flow" entirely and never reach the
formal — that's wrong, per point 1/2 above; a follow-up trace,
requested and done the same day, disproved it directly rather than
leaving it stand. The real gap is narrower and, in a sense, worse: the
type information is all there and correctly unioned — the compatibility
check just runs at a **stale point in time relative to the ES's own
already-committed contributors**, not because constants are invisible
to it.

**Not a duplicate of any existing issue.** Ruled out explicitly:
- **Not closed/076.** 076's root cause (fully resolved, see its own
  "RESOLVED" section) was `dict`/`set`'s specific class-body-default-
  plus-`__init__` double-initialization pattern feeding a shared
  prototype `CreationSet` into every instance's field type,
  permanently, via `P_prim_index_object`/`P_prim_set_index_object`'s
  "flow into every CS the receiver has ever seen" loop. None of that
  applies here: no class-body defaults, no container element flow at
  all, and (per the corrected trace above) this isn't even a
  "permanently accumulates incompatible data and no one notices"
  bug the way 076's was — 607's accumulated type is *correct*, the
  bug is that a routing decision was scored and frozen against it
  *before* its already-committed contributors had been (re-)flowed in
  this pass. Different code path (`entry_set_compatibility` in
  `fa.cc`, not `P_prim_index_object`), different data (`EntrySet`
  routing, not container-element `AVar`s), different timing shape
  (a one-time stale snapshot at routing, not permanent monotonic
  accumulation after the fact). Real family resemblance — both are, at
  bottom, "this codebase's reactive/quiescence-driven splitting can
  make a decision using data that's momentarily behind reality, and
  nothing revisits it once better data exists" — the same structural
  class 076 itself named as possibly foundational (063, 072-075 are
  its other named siblings). Not the same mechanism, not the same fix
  location, not a duplicate — but worth flagging as the *second*
  independently-found instance of that structural class within this
  one session, for whoever eventually scopes a general fix for the
  family.
- **Not 030.** 030 is ambiguous dispatch among *multiple* candidate
  `Fun`s at a call site (`f->calls.get(n)->n > 1`, resolved via
  classtag/vtable emission). Here `get_target_fun_core` always returns
  exactly one candidate — confirmed both by the earlier codegen
  investigation (097's original evidence: a direct, unconditional call
  is emitted, never a dispatch chain) and by this trace (each edge has
  exactly one target `EntrySet`, no ambiguity at the `Fun::calls`
  level at all). The bug is *which one* `EntrySet` gets selected
  upstream in FA, not how multiple candidates get resolved at
  codegen time.
- **Not 018.** 018 is `sizeof_element` being computed against a
  non-container type (a container method's clone shared across a
  scalar and a real container). Unrelated position, unrelated
  primitive, unrelated formal.
- **Not 045's `clone_for_constants` machinery either** — checked and
  ruled out directly, correcting this section's own first draft, which
  wrongly invoked it. `Sym::clone_for_constants` is set in exactly one
  place (`python_ifa_build_if1.cc:961`), only via an explicit, opt-in
  `__pyc_clone_constants__(...)` builtin marker used by specific
  library code — `str.__eq__`'s ordinary `x` parameter never carries
  it. `edge_constant_compatible_with_entry_set` (`fa.cc:938`) is real,
  narrower machinery for that opt-in category specifically; this bug
  hits a completely different, unguarded check
  (`edge_type_compatible_with_entry_set`) that runs for *every* call,
  constant-flagged or not.

**What a real fix would need**: `entry_set_compatibility`
(`fa.cc:1059`, specifically its `edge_type_compatible_with_entry_set`
call) makes a one-shot, frozen routing decision for each edge based on
whatever the candidate `EntrySet`'s accumulated type happens to be at
that exact moment — with no mechanism to revisit it once that ES's
*own already-committed* contributors are (re-)flowed and the
accumulated type changes underneath the decision. A real fix needs
either (a) a reactive re-check — if an already-routed edge's target ES
later accumulates a type that's genuinely incompatible with what was
scored at routing time, un-route and re-decide (comparable to 076's
"Option A: retroactively invalidate/re-flow on split") — or (b)
sequencing FA so a candidate ES's own committed contributors are fully
flowed *before* any new edge is scored against it, closing the window
where it can look emptier than it really is (comparable to 076's
"Option C: fix the scheduling instead"). Both are exactly the class of
core, always-on, every-call-site-affecting FA change 076 itself found
carries serious regression risk without a careful termination/scope
argument first — not attempted this session; left here, precisely
located and with the exact confirming trace above, for whoever picks
this up next.

**Verified no regression from the investigation itself**: after
reverting the temporary instrumentation, rebuilt and re-ran the full
battery — `ifa --test` 58/58, `test_pyc.py` 265/14/0/4 both backends,
`msp_ss.py` still compiles clean (exit 0). The `emit_send_call` guard
from the "RESOLVED (partial)" section above is unaffected by this
trace; it remains the right fix for the compile-blocking *symptom*
regardless of which upstream mechanism (now known precisely) produces
the mismatched edge.

## Fix attempted, implemented, regressed 3 tests, reverted (2026-08-11)

Per the "What a real fix would need" section above, option (b) was
attempted: defer an edge's routing decision (`entry_set_compatibility`/
`find_best_entry_sets`) whenever other, already-discovered edges are
still pending in `fa->edge_worklist`, so every already-committed
contributor to a candidate `EntrySet` gets a chance to flow before that
candidate is scored. Design (worked out collaboratively): a new
`FA::deferred_edge_worklist`; `analyze_edge` defers instead of routing
when `!e->to && fa->edge_worklist.head`; `analyze_to_convergence`'s
loop retries the deferred queue once `edge_worklist`/`send_worklist`
both drain, forcing exactly one deferred edge through (unconditionally,
no further deferral) when *only* deferred edges remain, to guarantee
termination without a live-lock (requeuing the whole batch at once was
considered and rejected first — the first edge popped would see the
rest still pending and immediately re-defer, forever).

**Implemented, compiled clean, terminated correctly** (`deferred_edge_
worklist` verified empty at every `complete_pass()`) — **but regressed
3 existing tests**: `generator_basic.py`, `raise_string.py`,
`list_index_type_mismatch_salvage.py`, all newly producing "expression
has no type" where they previously compiled clean. Root-caused in
full — the mechanism is real and general, not specific to this fix:
filed as [098](098-FA-per-pass-reset-scoped-to-reachable-set.md).
Summary **as re-root-caused 2026-08-12** (the original wording here,
"the per-pass fixed point depends on dispatch order", is superseded —
see 098): `clear_results` resets per-pass state only over the contours
the *previous* pass reached, and not at all on a `reanalyze()`-driven
pass, so a large fraction of edges carry an older pass's `args` /
`rets` / `formal_filters` into the current one; a stale `formal_filter`
then makes `analyze_edge`'s gate skip a live edge permanently, and
everything downstream of it stays bottom. This fix's reordering
(harmless in every way the design reasoned about — correct
termination, no lost edges, no stale-cache violation) changed which
contours a pass reaches, which is enough to change which edges get
reset.

**Reverted in full** — `git diff` on `ifa/analysis/fa.cc`/`fa.h` is
clean; rebuilt, `ifa --test` 58/58 and `test_pyc.py` 265/14/0/4 both
backends confirm the tree matches the last commit exactly (no
instrumentation or partial changes left behind).

**Where this leaves 097**: the root cause traced above (`entry_set_
compatibility` scoring a stale snapshot) is unchanged and still
precisely located. A correct fix for *it* specifically needs 098's gap
closed first — otherwise any change aimed at 097 that shifts which
contours a pass reaches risks tripping the same stale-state gap 098
documents, exactly as this attempt did. Not something to retry blind a
second time.
