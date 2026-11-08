# 186 — pass 1 pushes a whole-program union through the shared builtins (othello3: FA is ~850 s against a 400 s cap)

**Status: open.** othello3 compiles and matches CPython since 2026-10-09,
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
