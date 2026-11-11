# 186 — pass 1 pushes a whole-program union through the shared builtins (othello3: FA is ~850 s against a 400 s cap)

**Status: open; the verification below was first met on 2026-11-10.**
Sweep `check__default__0b55bb75+ae4581cf`: othello3 compiles in 352 s at
`-j32` (cap 400 s) and its stdout matches CPython, after
[207](207-FA-call-matching-re-matches-a-whole-union-per-member.md)'s
matching changes. **Later the same day the time after FA went from 172 s
to 25 s** ([below](#after-flow-analysis-2026-11-10)): alone it is 163 s,
and 180 s in sweep `check__default__61d11baa+62bfc8eb`. FA is 136 s of
that, so what is left of this issue is FA. Before that:
othello3 compiles and matches CPython since 2026-10-09,
in 994 s, almost all of it FA. Rewritten 2026-11-03; the fix-by-fix
measurements, the stall-guard deletion and the post-FA profile are in git
(`git show 123ac81:ifa/issues/186-FA-pass-one-confluence-through-shared-builtins.md`).
It also carries [closed/192](closed/192-review-analysis-and-issues-2026-11.md)
C1 and C2.

## Mechanism

Start-minimal means pass 1 has one EntrySet per function, so every shared
builtin contour (`__list_iter__.__next__`, `len`, `join`, `__eq__`,
`append`) carries every caller's values. othello3 (830 `Flip` subclasses,
64 `Put`, 1,848 table stores) forms a ~2,039-CreationSet union there, and
it spreads to tens of thousands of AVars one member at a time. The demand
that would separate the callers is evaluated at quiescence, after the
pass, so the pass must finish first. **Not the cause:** start-merged list
CreationSets (`PYC_CSDCPA1=0` stalls the same way).

## Landed (2026-10-08/09)

Four constant-factor fixes: dispatch was cubic in the receiver union
(`pattern.cc`); the flow worklist defers pushes so a union settles before
readers see it (105 M → 1.85 M out-changes on pass 1); a subset fast path
in `type_union`; one-pass `type_diff_all` in violation collection;
per-index match-cache lookup. Then clone's `cs_member_merge_enlarges`
made linear, the 830-arm dispatch chain collapsed to one slot call when
every arm agrees, and `-g` dropped from the C compile (23 GB → 3.4 GB).
Result: pass 1 184 s, 13 passes, FA ~850 s. The within-pass stall guard
was wrong (it counted minting as progress and was wall-clock) and was
deleted; only the cross-pass guards remain.

What shedskin does: eager CPA (identity, forbidden here), a deferred
connection limit (`CPA_LIMIT`: a call whose candidates × argument
combinations exceed it is not connected until the limit doubles), and
incremental connection. The last two are scheduling, not identity, and
carry over.

## Directions, in order

**2026-11-08:** the mechanism behind these is general, not othello3's.
[207](207-FA-call-matching-re-matches-a-whole-union-per-member.md)
measured it on sunfish and plcfrs. Every growth of an argument union is a
match-cache miss and a full re-match, so the cost is quadratic in union
width. Directions 1 and 2 are now 207's directions 2 and 3, alongside
incremental matching. othello3 stays this issue's witness.

1. **Deferred connection.** Do not connect a call whose receiver union is
   wider than a limit until the pass has otherwise settled, then connect
   it once with the settled union. In pyc the union still pours into one
   minimal contour; the gain is that it arrives once rather than ~2,000
   times. Stop condition: pass 1 under 400 s with the union's holders
   unchanged, else re-profile.
2. **A wide-union representation.** `AType` is a sorted `Vec` with
   hash-consing and four per-instance memo maps never cleared; a wide
   union allocates a new canonical AType per added member and memoises
   every pair; `type_diff`'s `subsumed_by` loop over abstract members is
   O(|a|·|b|). A bitset over CreationSet ids above a width threshold makes
   union, diff and subset O(width/64).
3. **Shrink what each pass walks**: [203](203-shrink-the-graph-fa-walks.md).
4. **Do not walk it again next compile**: [198](198-decision-snapshot-across-compiles.md).

## Verification

othello3 compiles under the sweep's 400 s cap with output matching
CPython; `make test` green; a `check` sweep with no outcome regressions.

## After flow analysis, 2026-11-10

**Measured** with `PYC_DBG_PHASES=1` (new, in the driver: wall seconds per
pipeline stage) and a gdb sampler, since `perf` is blocked on this
machine. othello3 compiled alone, tree `61d11baa`:

| stage | before | after |
| --- | --- | --- |
| flow analysis | 136.7 s | 136.2 s |
| clone | 10.2 s | 10.2 s |
| mark-live-code | 30.3 s | 0.7 s |
| inlining | 66.6 s | under 0.5 s |
| write C | 55.3 s | 3.6 s |
| C compiler | 9.7 s | 9.7 s |
| **wall** | **311 s** | **163 s** |

Three causes, one per stage. Each was quadratic or worse in something
othello3 has a lot of.

- **Inlining: `check_invariants` ran before and after every splice.** It
  walks the whole function and allocates two node sets, so a function
  with *n* inlined sites paid O(*n*) per site, and most of the samples
  were in the collector. It now runs once per function, after its sites
  are done (`inline_check`, `inline.cc`). `IFA_DBG_INLINE_CHECK=1`
  restores the per-splice check, which is what says which splice broke
  the CFG.
- **Write C: `cg_note_blind_cast` expanded the receiver union at every
  slot call.** A `.go()` site notes the 894-class union once per class,
  and each note walked every member's fields. What it records is a
  function of (cast type, actual type, slot), so a triple is now expanded
  once (`codegen_common.cc`).
- **Mark-live-code: `forward_live`** walks three forward steps from a dead
  setter, across every contour's edges, and every dead setter started
  from scratch. AVar liveness does not change inside `mark_live_pnodes`,
  so the answer per (Var, distance) is remembered for the pass
  (`dead.cc`).

**Exact.** othello3's generated C is byte-identical. `make test` is green.
`check__default__61d11baa+62bfc8eb` equals
`check__default__0b55bb75+ae4581cf` in every column on all 77 programs.
In the sweep othello3's compile went 352 s → 180 s and the whole `check`
sweep 593 s → 405 s.

**Not done.**
- Byte-identical C was checked on othello3 only; the other 76 rest on the
  sweep's columns.
- `cg_cast_members` decides whether a union member has the field by
  comparing field NAMES. It was there before and is untouched, but it is
  the kind of test AGENTS.md's "never decide by name" rules out.
- clone's 10 s (`determine_basic_clones`, `sets_by_f`) was sampled only 8
  times and not looked at.
