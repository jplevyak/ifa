# Issue 037: pattern matcher enumerates the cartesian product of per-argument CreationSets (exponential in call arity)

**Status: CLOSED** — fixed 2026-07-10, same day as filed. Kept as
the record of a corpus-level performance root cause.
**Affects:** `ifa/if1/pattern.cc` — `Matcher::find_best_matches` /
`find_best_cs_match` / `set_filters`.
**Found:** while root-causing the shedskin "compile timeout"
bucket (stereo, bh, pygasus) — the surviving FA divergences after
issue 035 made everything deterministic.

## Symptom

- stereo (222 lines): FA pass 1 did not complete in 90+ seconds.
  A timed SIGINT backtrace showed 14 stacked recursive frames of
  `Matcher::find_best_matches`, one per argument position.
- pygasus: pass 1 took 66s, 98% in match.
- bh: 60s+ timeout.

`-l d` on stereo: the hot send is `update_msgs` /
`do_sweepsSS2`-class calls — plain Python functions with 13-15
positional parameters, EXACTLY ONE candidate function
(`cover_formals: 1, unsubsumed: 1`), and every numeric argument
carrying a {constant-CS, base-type-CS} union. 40 MILLION dispatch
log lines in 35 seconds.

## Root cause

`find_best_matches` recurses per argument position, iterating that
argument's CreationSets:

```cpp
for (CreationSet *cs : args[iarg]->out->sorted) {
  csargs[iarg] = cs;
  find_best_matches(..., iarg + 1);
}
```

i.e. `find_best_cs_match` (candidate coverage, generic
substitution, promotion/coercion recording, O(candidates^2)
subsumption, ambiguity analysis, filter accumulation) runs once
per element of the cartesian product `prod_i |CS(arg_i)|`. With
2-3 CSs per argument and arity 13-15 that is 10^4..10^7 leaf
evaluations PER pattern_match call — and every send re-matches
whenever any argument type grows during convergence (the
MatchCache keys on exact per-position ATypes, so it misses
throughout convergence). The product is pure waste whenever the
candidates cannot DISTINGUISH the CSs being enumerated — which for
a single-candidate call is every position.

## Fix

Dispatch-equivalence collapsing in `find_best_matches`, applied
when the candidate set is a SINGLE non-generic function and the
position's formal is not a pattern: group the argument's CSs by
the only three per-position tests `find_best_cs_match` applies —

- actual-filter membership (`m->actual_filters.get(acpp)->set_in(cs)`),
- `is_exact_match` verdict against the formal's dispatch type,
- `is_this` nil verdict —

and recurse once per class with a representative. `set_filters`
takes a parallel class vector and expands the representative back
to the full class, so the accumulated formal filters — the only
per-CS OUTPUT of the enumeration — are identical to full
enumeration. Every other consumer (coercion_uses, promotion_uses,
verify_arg) operates on `cs->sym->type`, which class members share
by construction of the filter test; generic candidates and pattern
formals keep full enumeration (their substitutions genuinely bind
per-CS), as do multi-candidate sends (subsumption/ambiguity are
per-CS there).

## Results

- stereo: 90s+ (pass 1 incomplete) -> **4.9s total, compiles**
  (new corpus member; 22 -> 23 compiled, no member lost).
- bh: 60s+ timeout -> **3.9s** (surfaces its pre-existing
  `raise Exception` failure, tracked as pyc issues/028).
- pygasus: pass 1 66s -> reaches pass 13 in 180s; still times out —
  the residue is genuine FA divergence on a 1700-line program plus
  [034](../034-pygasus-update-display-assert.md), not match cost.
- Suites: pyc C 177/0 and pyc LLVM 177/0 under the harness
  determinism gate (byte-identical recompiles), ifa-test all
  phases green.

## Addendum (2026-07-11): viability pruning

The first fix still enumerated REJECTED equivalence classes "for
exactness". That was the residual blowup on pygasus: with mixed
types mid-convergence every position also carries a rejected
class, and the 2^arity rejected combinations produced ~2.4M
`cover_formals: 0` leaves per 40s — all provably effect-free,
since a zero-cover leaf returns before set_filters. The
single-candidate path now recurses ONLY into the viable class
(passes actual-filter membership, is_exact_match, is_this) and
prunes the whole subtree when a position has none. pygasus match
cost fell from 3.5-30s/pass to 0.4-0.6s/pass; combined with the
compar_edge_id reporting fix, pygasus goes from HANGING to
terminating with a full 788-violation diagnosis in ~200s (the
remainder is the extend-phase plateau — issue 033 territory — plus
a 32s first pass dominated by 3-candidate `_exec` method sends
that the single-candidate path deliberately skips).

## Future work

- Multi-candidate collapsing (pygasus's remaining 32s pass-1: 3
  candidates x 4 args): sound with key = per-candidate
  (accept, exact, this) bit-vectors + cs->sym->type, PLUS
  cs->sym itself when the candidates' dispatch types differ at
  the position (subsumes_arg reads cs->sym against isa sets;
  identical dispatch types make its verdict CS-independent).
  Estimated 2-5x on that pass; the extend plateau dominates
  pygasus now, so deferred.
- The MatchCache misses throughout convergence by design (exact
  AType keys). With collapsing the misses are cheap; a
  subset-aware cache would help pygasus-scale programs further.
