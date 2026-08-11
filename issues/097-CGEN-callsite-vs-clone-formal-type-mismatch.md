# 097 — An ordinary call site's argument type can diverge from the specific callee clone's formal parameter type (msp_ss.py's last two compile errors; guarded, root cause found and precisely located, fix not yet implemented)

**Status: PARTIAL, guard landed 2026-08-11, root cause found
2026-08-11.** The compile-blocking symptom (hard C++ overload-
resolution error) is fixed — see "RESOLVED (partial)" below. The
underlying mechanism — *why* clone/dispatch selection routes an edge
to a target whose formal type doesn't match that edge's actual
argument — is now traced to a specific, precisely-located gap: see
"ROOT CAUSE FOUND (not a duplicate)" at the bottom. Confirmed **not a
duplicate** of 076, 030, or 018 (each checked and ruled out explicitly
with reasoning). Left open (not moved to `closed/`) since the actual
fix — teaching `edge_type_compatible_with_entry_set`
(`ifa/analysis/fa.cc:874`) to distinguish "never touched" from "only
ever touched by constants whose type never propagated" — is real,
scoped work not yet attempted, not just an open question.

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

**Confirmed exact mechanism, not just the general shape**: ES 607's
own formal `AVar` for the `x` position has an **empty** `AType`
(`->out->type->n == 0`) even though `strip`'s two calling edges both
pass a real `str` actual — the type never flowed into the formal at
all, evidently because `strip`'s call sites pass **compile-time string
constants**, which take a different, constant-specific path through FA
(consistent with codegen's own evidence: a *separate*, single-argument
`str::__eq__` clone, `_CG_f_627_4`, exists in the same output purely
for a constant-folded comparison — this codebase already special-cases
constant arguments elsewhere, e.g. `Fun::clone_for_constants`,
`edge_constant_compatible_with_entry_set` below). `entry_set_
compatibility`'s type check, `edge_type_compatible_with_entry_set`
(`ifa/analysis/fa.cc:874`):

```cpp
if (etype->n && es_arg->out->type->n && etype != es_arg->out->type) return 0;
```

only rejects a candidate `EntrySet` when **both** the edge's actual
type (`etype->n`) **and** the ES's own accumulated type
(`es_arg->out->type->n`) are non-empty and disagree. When the ES's own
formal type is empty — as ES 607's `x` position always was, because
its only prior contributors were constants that bypassed normal type
flow — this check is silently skipped, so ES 607 reads as compatible
with *any* edge's actual argument at that position, typed or not. This
is indistinguishable, to this check, from "a brand-new `EntrySet` with
no edges yet" (which legitimately must accept its first edge's type
unconditionally) — the check has no way to tell "genuinely first-use,
safe to accept anything" apart from "has been used, but by constants
whose types never propagated here, so still reads as empty." `loadTIText`'s
`ord('q')` edge scored well enough against ES 607 (an existing,
receiver-compatible candidate needing no new `EntrySet`) to win over
minting/reusing a properly `int64`-typed one, exactly the way ES 737
correctly did for the very next line.

**Not a duplicate of any existing issue.** Ruled out explicitly:
- **Not closed/076.** 076's root cause (fully resolved, see its own
  "RESOLVED" section) was `dict`/`set`'s specific class-body-default-
  plus-`__init__` double-initialization pattern feeding a shared
  prototype `CreationSet` into every instance's field type,
  permanently, via `P_prim_index_object`/`P_prim_set_index_object`'s
  "flow into every CS the receiver has ever seen" loop. None of that
  applies here: no class-body defaults, no `CreationSet` accumulation,
  no container element flow at all — this is call-target `EntrySet`
  selection for an ordinary two-argument method, gated by a totally
  different piece of code (`entry_set_compatibility` in `fa.cc`, not
  `P_prim_index_object`). The two bugs share only a family
  resemblance ("a reused/shared analysis object's staleness lets
  incompatible data through") common to a whole class of FA bugs in
  this codebase (063, 072-075 are named siblings in that family too) —
  not the same mechanism, not the same fix location, not a duplicate.
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
- **Closest relative: closed/075/063's "no type bucket" family and
  045's `clone_for_constants`/`clone_methods_per_cs` machinery** — not
  a duplicate of either, but this bug's actual mechanism sits exactly
  at the seam between them: `edge_constant_compatible_with_entry_set`
  (`fa.cc:938`, quoted above `entry_set_compatibility`) already
  special-cases constant-vs-constant incompatibility for
  `clone_for_constants` formals specifically — this bug is the
  same general shape (a constant-argument formal position needs
  different compatibility handling than an ordinary one) but for
  `str.__eq__`, which isn't a `clone_for_constants` function, hitting
  the *unguarded* general-purpose check instead
  (`edge_type_compatible_with_entry_set`'s `es_arg->out->type->n`
  emptiness gap, not `edge_constant_compatible_with_entry_set`'s own,
  narrower, already-handled one).

**What a real fix would need**: `edge_type_compatible_with_entry_set`
(`fa.cc:874`) would need to distinguish "this `EntrySet`'s formal
position has never had ANY edge contribute a type" (genuinely safe to
accept anything — the bootstrapping case this check's current shape
protects) from "this position's contributing edges were all constants
whose types bypass the normal `AVar` flow, so it LOOKS empty but isn't
actually untouched." That distinction doesn't exist anywhere in the
current data model (`AType.n == 0` conflates both cases) — adding it
is exactly the kind of core, always-on, every-call-site-affecting FA
change 076 itself flagged as carrying serious regression risk without
a careful termination/scope argument first. Not attempted this
session; left here, precisely located, for whoever picks this up next.

**Verified no regression from the investigation itself**: after
reverting the temporary instrumentation, rebuilt and re-ran the full
battery — `ifa --test` 58/58, `test_pyc.py` 265/14/0/4 both backends,
`msp_ss.py` still compiles clean (exit 0). The `emit_send_call` guard
from the "RESOLVED (partial)" section above is unaffected by this
trace; it remains the right fix for the compile-blocking *symptom*
regardless of which upstream mechanism (now known precisely) produces
the mismatched edge.
