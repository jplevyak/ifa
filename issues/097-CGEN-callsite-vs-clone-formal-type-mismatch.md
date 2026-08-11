# 097 — An ordinary call site's argument type can diverge from the specific callee clone's formal parameter type, with no guard (msp_ss.py's last two compile errors)

**Status:** open, found 2026-08-11 while implementing
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
