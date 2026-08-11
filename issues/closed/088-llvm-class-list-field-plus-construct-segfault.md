# 088 — LLVM backend segfaults constructing a class two ways: once using a `None`-default arg, once with an explicit value

**Status:** fixed 2026-08-10 (see "Fix" section at the end). Found
2026-08-08 while verifying the fix for
[issues/044](../../../issues/closed/044-list-add-mutates-receiver.md) (`list.__add__`
mutating its receiver) on the LLVM backend. Confirmed **pre-existing**
— reproduces identically on the LLVM backend before that fix too
(bisected via `git stash`), so it is unrelated to issue 044's change
and was simply never exercised on `-b` before now. Narrowed
significantly since first filed: the original repro (list `+` inside
a method returning a new instance of its own class) turned out to be
a red herring — list `+` and the loop/multi-instance shape are both
unnecessary; see "Narrowing" below.

**Affects:** `pyc -b` (LLVM backend) only. The C backend compiles and
runs every repro below correctly.

## Minimal repro

```python
class node:
    def __init__(self, route=None):
        self.route = route or []

start = node()
n2 = node([5])
print(start.route)
print(n2.route)
```
- CPython: `[]` then `[5]`.
- pyc, C backend: matches CPython exactly.
- pyc, LLVM backend (`pyc -b repro.py`): compiles cleanly (no
  warnings), then `./repro` segfaults immediately, before printing
  anything.

## Narrowing (each tested standalone against the minimal repro above)

- Removing either construction (`start = node()` alone, or `n2 =
  node([5])` alone) — **no crash**. Both call shapes must coexist in
  the same program.
- Removing `print(start.route)` (keep both constructions, drop the
  first print) — **no crash**. Reading the field back after both
  instances exist is also required, not just constructing them.
- Replacing `self.route = route or []` with plain `self.route =
  route` (so the field is `None`-or-`list` instead of always `list`)
  — **no crash**, but now the LLVM backend's output is wrong (prints
  nothing/empty instead of `None` then `[5]`) — this looks like a
  separate, pre-existing None-handling bug on `-b`, not investigated
  further here; kept out of scope for this issue.
- The original repro's list `+` (`self.route + [move]`), the loop
  building 4 children, and the `extend` method are all **not
  required** — the 8-line repro above triggers the same crash with
  none of that machinery.

This points at pyc's per-class "prototype" instantiation model
(`gen_class_pyda` in `python_ifa_build_syms.cc`; see issue 044 and the
closed issue 017 for the same mechanism traced on the C backend) —
specifically, whatever the LLVM backend does when `__init__` is
invoked through two different argument-shape call sites for the same
class (one relying on the default `None` and the `or []` fallback,
one passing an explicit value) and the resulting field type is
unified to a single concrete type (`list`) rather than a `None|list`
union. The C backend handles this combination correctly; something in
the LLVM path's equivalent handling does not.

## What's known so far

`gdb -batch -ex run -ex bt ./repro` gives an unhelpful backtrace — no
debug symbols, top frame in library address space with no symbol:
```
Program received signal SIGSEGV, Segmentation fault.
0x00007ffff7de4d00 in ?? ()
#0  0x00007ffff7de4d00 in ?? ()
#1  0x00005555555569b0 in ?? ()
#2  0x00005555555569b6 in main ()
```
`??`/no-symbol frames directly under `main` — consistent with a bad
call through a corrupted function pointer or a bad-address struct
field read, but not confirmed either way.

`ptrace_scope` in this sandbox blocks attaching to a running process
(`gdb -p`), so live inspection is limited to `gdb -batch -ex run`
against a fresh launch; core dumps are redirected through `apport`
with no local core file produced. A build with debug symbols enabled
for the LLVM path, or a session with proper ptrace permissions, would
be needed to get a real backtrace — or instrumenting
`ifa/codegen/cg_emit_llvm.cc`'s handling of multi-call-shape
`__init__`/prototype construction directly and comparing against the
C backend's `cg.cc` equivalent for this exact program.

## Root-caused further (2026-08-10) — precise mechanism found, not yet fixed

The `gdb -p`/no-symbols blockers above turned out not to matter: `pyc`'s
own generated executable IS the thing that needs symbols, and it has
them by default (no special build flag needed) — `gdb -batch -ex run
-ex bt ./repro` against a fresh build immediately resolves real
function names once you disassemble/step within `repro`'s own `.text`
(the earlier "no symbols" reading was from stopping at the first `??`
frame instead of walking further; `up`/disassembling neighboring
addresses resolves cleanly). Also corrected an unrelated but important
methodology bug while chasing this: `IFA_LLVM=1` has
**zero effect on `pyc`** (it only matters for the standalone `ifa`
binary) — the correct way to force the LLVM backend is `-b` /
`PYC_FLAGS="-b"` / `PYC_LLVM=1`. Re-verified: the repro genuinely
segfaults with the real flag; it does not with the (silently wrong)
`IFA_LLVM=1`, which is why it read as "no crash" if anyone re-tried it
that way.

**The crash is real stack corruption from executing an LLVM
`unreachable` instruction that is reached at runtime** (not a
provably-dead path — LLVM's own semantics for `unreachable` are "the
compiler may assume this is never reached," so reaching it for real is
undefined behavior; empirically, on this LLVM/clang version it does
not trap, it falls through into whatever bytes follow in the object
file, producing the "corrupted function pointer" symptom the original
crash report described). Confirmed via `--strict_verify`, which
passes clean — this isn't a malformed-IR bug the verifier could catch;
the IR is syntactically valid, just semantically wrong for this
program. Also confirmed the `-O2` `clang++` step
`llvm_codegen_compile` always runs isn't the cause: compiling the
identical `.ll` at `-O0` still crashes (as a much easier-to-read
261,000-frame stack overflow instead of a jump-to-garbage, but the
same root defect).

The `unreachable` shows up in `_CG_f_10149_14`, the Fun implementing
`self.route = route or []` for `node.__init__` — its `entry` block is
just `alloca; store %1 (route); unreachable`, with the real logic
(build `[]` if falsy, use `route` if truthy, store into the `route`
field, `ret`) sitting in two later blocks (`L1111`/`L1112`) that are
never branched to from `entry` at all. Traced to
`ifa/codegen/cg_emit_llvm.cc`'s `emit_block_terminator`, `Code_IF`
case: temporary instrumentation confirmed that for this exact
`Code_IF` PNode, `closer->live && closer->fa_live` is true, both
`t_bb` and `f_bb` (the LLVM blocks for `L1111`/`L1112`) resolve fine
via `ctx.label_bb`, but `cond = value_for_var(ctx, closer->rvals.v[0])`
returns **null** even though `closer->rvals.n == 1` (a real condition
Var is attached) — landing in the `t_bb && f_bb` branch's `else {
Builder->CreateUnreachable(); }` fallback (not the issues/085 trap
path — `is_unresolved_condition` doesn't fire here, since this cond
Var isn't itself typed `void_type`; it's a different failure mode).

Traced one level further into `emit_pnode` (the main per-PNode
emission walker): a PNode's Code only actually gets emitted —
`Code_SEND`'s case calls `emit_send`, which is what would register the
condition's computed value into `ctx.var_map`/`ctx.alloca_map` for
`value_for_var` to find — when `pn->live && pn->fa_live` is true
*for that PNode*. The walker still **recurses into** a PNode's
successors regardless of that PNode's own liveness (only skips
*emitting* it), so it's structurally possible to walk right past a
dead SEND into a live, dependent Code_IF. That fits everything
observed: the PNode computing `route`'s truthiness (presumably a
`__pyc_to_bool__`/`or`-lowering SEND) is walked but not emitted
(`live`/`fa_live` false on *it specifically*, unlike the Code_IF
consuming its result), so nothing ever populates a value for
`value_for_var` to return — not confirmed with certainty (would need
one more instrumentation pass directly on that SEND PNode to see its
own `live`/`fa_live` bits), but is the mechanism every other piece of
evidence points at. **This hypothesis turned out to be wrong — see
the corrected root cause below.**

**"Not yet explained" above turned out to be the wrong question —
corrected below (2026-08-10, same day, continued digging).** The
"FA marks the SEND dead" framing was a false lead: further
instrumentation (`Var::def`/`PNode::live`/`fa_live` printed directly
for the condition-computing SEND) showed it's fully live
(`live=1 fa_live=1`), has a real 2-candidate callee list
(`ctx.fn->calls.get(pn)` → 2 `Fun*`, not null), and is reached
normally by `emit_pnode`'s walk. The actual defect is one level
deeper, in the *polymorphic dispatch classification* that
`emit_send_call` runs once it has those 2 candidates — and it's a
plain, well-understood classification bug, not an FA liveness
mismatch at all.

**Root cause, confirmed precisely**: `self.route = route or []` needs
a runtime truthiness check on `route`, whose type at this shared
`__init__` clone is `None | list` (both call shapes — `node()` and
`node([5])` — share one clone, per the "both call shapes must
coexist" narrowing above). That compiles to a 2-candidate
`route.__pyc_to_bool__()` dispatch: one candidate specialized to a
`list` receiver, one to `__pyc_None_type__` (`sym_nil_type`).
Confirmed via instrumentation of both candidates' declared receiver
types directly:
- The `list`-receiver candidate: `list`'s own `has` array (1 entry)
  does **not** contain `__pyc_to_bool__` — because `list` doesn't
  define its own; it inherits the universal default from
  `__pyc_any_type__`
  ([issues/089](089-DISPATCH-closure-pyc-to-bool-no-candidate.md)
  gave every type this default). `poly_dispatch_classtag_targets`
  (`ifa/codegen/codegen_common.cc:380`) only checks the receiver
  class's **own direct** `has` list for the method name (`ccls->has[k]->name
  == method_name`) — no inheritance-chain walk — so it finds
  **nothing** for this candidate and returns an empty `rts`.
- The `__pyc_None_type__`-receiver candidate: correctly classifies as
  a nil-receiver (`argv->type == sym_nil_type` matches).

Both `emit_send_call` (`cg_emit_llvm.cc`) and its C-backend
counterpart in `cg.cc` iterate ALL candidates in one loop and require
each one to classify into *some* bucket (classtag / nil-receiver /
carrier-direct / plain-function-identity) — the very first candidate
that classifies into **none** of these sets `ok = false; break;`,
abandoning the *entire* multi-candidate dispatch, not just that one
candidate. Since candidates are processed in order and the `list`
candidate (unclassifiable, per above) comes before the nil-receiver
one in this repro, the loop bails immediately — `emit_send_call`
enters its `if (ok && classes.n && recv)` gate with `ok=false`, so the
whole `merge_bb`/`res_slot` machinery that would normally call
`put_result` for the condition's result Var never runs at all. That's
the precise reason `value_for_var` found nothing: not a liveness
mismatch, but a classification bailout that skips result-materializing
code entirely. `cond` stays null → `emit_block_terminator` hits the
bare-`Builder->CreateUnreachable()` fallback → real `unreachable`
executed at runtime → the stack corruption this whole issue is about.

**Why the C backend doesn't crash on the identical classification
failure**: not fully traced (see "Still open" below) — `cg.cc`'s
`write_send` has the structurally identical per-candidate loop calling
the *same* `poly_dispatch_classtag_targets`/`poly_dispatch_is_nil_receiver`
(shared, `codegen_common.cc`), so it almost certainly hits the same
`ok=false` for the same reason. But C's failure mode is presumably
"emit nothing useful for this SEND, or fall back to some other
dispatch strategy downstream" rather than "silently produce a
branch with no live condition and no successor-block wiring, leaving
`unreachable` to execute for real" — the LLVM backend's specific
combination of (a) needing `t_bb`/`f_bb` to be independently resolved
via `discover_blocks`'s BFS (which finds them through OTHER paths in
the CFG regardless of this dispatch's own success) and (b) using a
bare `CreateUnreachable()` as the point-of-no-information fallback is
what turns "this one SEND produced no value" into "the program
executes real undefined behavior," specifically. This asymmetry is
itself worth understanding before finalizing a fix.

**This is the same class of gap
[ifa/issues/091](091-DISPATCH-nonrecord-builtin-constructor-not-first-class.md)
root-caused for *construction* (`int`/`float`/`bool`/`list`/`tuple` —
Type_ALIAS / ifa-core-primitive types whose *inherited* methods aren't
reflected in their own `has` list the way a normal Python subclass's
are) —
here it's *dispatch classification* hitting the identical "inherited,
not locally owned" gap instead of *constructor synthesis*. Both trace
back to the same root fact about how these five builtin types are
registered: methods reachable only through inheritance from
`__pyc_any_type__`/`object` don't get copied into the primitive
type's own `has` array the way `gen_class_pyda`'s `includes` loop
does for ordinary user classes.

**The "teach `poly_dispatch_classtag_targets` to walk inheritance"
fix (originally listed as option 1 here) turned out not to be
viable — corrected below, same day, before implementing anything.**
Two things killed it on inspection:
1. `cg_has_classtag()` — the filter the *caller* applies to whatever
   `poly_dispatch_classtag_targets` returns — hard-requires
   `type_kind == Type_RECORD`. Confirmed directly: `list`'s
   `type_kind` is `Type_PRIMITIVE`, not `Type_RECORD`, and this isn't
   incidental. `list`'s runtime representation
   (`pyc_c_runtime.h`: `typedef void *_CG_list`, a pointer to a
   length-prefixed resizable buffer) has **no classtag field at
   all** — the same struct shape is reused for every element type,
   with no room for a `_CG_TypeObject*` the way a real record
   instance carries one in its first slot. `list` (and, by the same
   reasoning, `int`/`float`/`bool`/`tuple` — the same five ifa-core
   primitive types issues/091 already flagged) can **never** become a
   classtag-dispatch target, regardless of what `has`-walking logic
   feeds into it.
2. Separately, the inheritance walk itself didn't even find the
   method: `sym_any` (`__pyc_any_type__`)'s own `has` array is empty
   at codegen time (confirmed by direct inspection —
   `sym_any->has.n == 0`), so even a class that *could* use classtag
   dispatch wouldn't find `__pyc_to_bool__` there. Class methods must
   be tracked some other way entirely for dispatch-resolution
   purposes (FA's own `must_implement`/`must_specialize` matching,
   independent of `Sym::has`) — `has` isn't the right signal to
   search regardless of how far it's walked.

**Real root cause, restated**: this was never about `list` failing to
*classify* as a classtag target — it structurally can't, full stop.
The actual gap is that the dispatch-classification loop
(`emit_send_call` in *both* `cg_emit_llvm.cc` and `cg.cc`) had nowhere
to put a completely legitimate candidate whose receiver carries no
runtime classtag at all, once every other bucket (classtag,
nil-receiver, closure-carrier-direct, plain-function-value-identity)
had been tried and failed.

**And a load-bearing discovery**: `cg.cc` already has exactly this
bucket — `directs[]` (`ifa/codegen/cg.cc`, `write_send`'s classtag
loop, ~line 1652), with its own comment: *"the builtin containers
(list, str, ...), whose instances are raw runtime layouts, not tagged
structs... this route is only usable when it ends up with exactly ONE
member... First user: truth-testing an `Optional[list]` field (`if
node.args:` where args starts `None`) -- `__pyc_to_bool__` over
{nil, list} is `nil_fn` plus exactly one untagged candidate, fully
decided by the null test (pyc issues/025, genetic2's crossover)."*
This is the *exact* repro shape, already solved on the C backend,
already labeled with the right name for the mechanism (`directs[]` /
"untagged direct route") and the right invariant (exactly one member,
enforced via `if (directs.n > 1) ok = false;`). This answers the
"still open" question from the previous pass — `cg.cc` doesn't crash
because it already has this bucket; `cg_emit_llvm.cc` (added later,
per the LLVM-backend-parity effort) never got the equivalent.

Also independently re-confirmed via `git checkout`-based bisection
against the pre-issues/089 commit (`00f4286d`) that this reproduces
there too — genuinely independent of and pre-dating issue 089's fix,
not exposed or caused by it.

## Why not root-caused further here

Found incidentally while confirming issue 044's fix generalizes to
the LLVM backend. Root-causing an LLVM-backend-specific codegen bug
(`ifa/codegen/llvm.cc` / `cg_emit_llvm.cc`) is a different
investigation from the C-backend/runtime-header work issue 044 did,
and wasn't pursued further to keep that fix's scope bounded — this
issue exists to carry the (now well-isolated) repro and narrowing
forward rather than lose them.

## Verification plan once fixed

- The 8-line repro above, `pyc -b repro.py && ./repro`, must match
  CPython (`[]` / `[5]`).
- The original, less-minimal repro (list `+`, loop, `extend` method —
  see issue 044) must also pass on `-b`.
- `tests/list_add_no_mutate.py` (added for issue 044) exercises a
  related but non-crashing shape (no default arg) — add a new
  `tests/` case for this default-arg-plus-explicit-arg construction
  pattern once fixed, run on both backends.

## What this unblocks

Any LLVM-backend program with a class whose `__init__` has a
default-`None` parameter that's constructed both with and without
that argument in the same program — an extremely common pattern
(optional constructor args generally) — currently crashes outright on
`-b` rather than running.

## Fix (2026-08-10)

Ported `cg.cc`'s existing `directs[]` mechanism ("untagged direct
route", see the corrected root-cause section above) to
`cg_emit_llvm.cc`'s `emit_send_call` — the LLVM backend simply never
had it. Added `Fun *default_fn` alongside the existing `nil_fn`
tracking in the classtag-classification loop: a candidate that fails
every existing bucket (classtag / nil-receiver / closure-carrier-
direct / plain-function-value-identity) is claimed as `default_fn`
*instead of* immediately bailing (`ok = false`), but **only once** —
a second such candidate is genuine ambiguity this mechanism can't
resolve and still falls back to the original conservative bail.
`default_fn`, once claimed, is called unconditionally at the existing
"fallthrough" point (previously a bare `Builder->CreateBr(merge_bb)`
with nothing computed — already a documented gap, issues/030(a)'s own
comment: *"a genuinely unmatched dispatch here reads garbage, not
0"*) using the same `emit_direct_call` helper the closure-carrier and
plain-function-value routes already share.

One additional fix needed beyond porting the classification logic:
the outer gate guarding the whole merge_bb/res_slot mechanism was
`if (ok && classes.n && recv)` — hard-requiring at least one *real*
classtag candidate. For a pure `None | list`-shaped dispatch (zero
classtag candidates, just `nil_fn` + `default_fn`), `classes.n` stays
0 forever, so this whole mechanism — including `nil_fn`'s own null-
test branch — was **never entered at all**, regardless of the
default_fn fix. Broadened to
`if (ok && (classes.n || nil_fn || default_fn) && recv)`. `recv`
itself stays a required guard deliberately: it's only ever populated
via the classtag-rts loop or the nil-receiver check, so a
`default_fn` claimed with neither of those present (no nil_fn, no
real classtag candidate at all) still correctly declines to run this
path — that narrower shape (e.g. a 2-member union of two *different*
untagged primitive types, no `None` involved, nothing to null-test
against) isn't addressed here; it stays exactly as broken/unhandled
as it already was, not made worse.

**A second, genuine regression caught by a full shedskin-corpus
before/after sweep, not by `test_pyc.py`** (which is thorough but
doesn't happen to exercise this exact shape): the first version of
`default_fn`'s classification only checked "has a real LLVM function
defined" before claiming a candidate — unlike every *other* bucket in
this loop (classtag, nil-receiver, carrier-direct, plains), which all
independently confirm the candidate's formal *types* line up with the
call site's actual arguments before accepting it. Running the fix
against all 86 vendored `shedskin_examples/` programs (compile + run,
both before and after, diffed) surfaced exactly one case where this
mattered: `adatron.py` went from a clean compile to `fail: LLVM module
verification failed: Call parameter type does not match function
signature!` — `default_fn` had been claimed by a candidate with an
unrelated `double`-typed formal, called with a `ptr` argument from a
completely different, coincidentally-shaped call site. Fixed by
adding the same per-formal type-compatibility pre-check `cg.cc`'s
`directs[]` already performs (identical, or both integer, or
pointer↔integer — the same coercions `emit_direct_call` itself
applies when actually emitting the call; anything else, e.g. float
vs. pointer, correctly falls through to the conservative `ok = false`
bail instead of mispairing). Re-verified clean after this fix:
`adatron.py` compiles again, the original 8-line repro is unaffected,
and a second full corpus sweep (86 programs, compile + run,
before/after diffed) found no differences at all versus the pre-fix
baseline outside the two already-broken cases described above
(`genetic2`, `softrender` — neither newly broken, both pre-existing
failures that just changed failure mode).

**Verified**: the 8-line repro now prints `[]` then `[5]` on `-b`,
matching CPython and the (already-correct) C backend exactly. Full
`test_pyc.py`, both backends (`PYC_FLAGS="-b"` — not `IFA_LLVM=1`,
which has zero effect on `pyc`, only on the separate `ifa` binary):
clean 264/14/0/4 on both. `ifa`'s own `make test` (all phases +
`ifa-test` UnitTest): clean. Full `shedskin_examples/` corpus sweep
(86 programs, compile + run, before/after diffed twice — once before
finding the `adatron.py` regression above, once after fixing it): zero
regressions in the final pass. New test
`tests/default_arg_plus_explicit_construction.py`.

**Bonus, verified via explicit before/after comparison**: the
"separate, pre-existing None-handling bug on `-b`" the original
narrowing section flagged and explicitly scoped out (`self.route =
route` without `or []` — a plain `None|list` field, no truthiness
check at all, just constructed and printed directly) is **also
fixed**, not still open. That shape needs `print(route)` to dispatch
`route.__str__()`/`__repr__()` over the same `None|list` union — the
identical "list has no classtag, nowhere to put the candidate" defect
this fix addresses, just reached through a different method name.
Confirmed by direct before/after rebuild: the pre-fix binary prints
two blank lines for this repro (matching the original "prints
nothing/empty" report); the post-fix binary prints `None` then `[5]`,
matching CPython exactly.

**Not done as part of this fix**, tracked separately: porting `cg.cc`'s
more general `directs[]` (a real `Vec<Fun*>`, bailing only when
`directs.n > 1`) instead of this change's single-slot `default_fn` —
functionally equivalent for every case actually reachable here (a
second unclassifiable candidate bails either way regardless of which
representation is used), so not a behavioral gap, just a smaller
diff; left as-is rather than generalizing speculatively.
