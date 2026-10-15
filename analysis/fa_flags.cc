// SPDX-License-Identifier: BSD-3-Clause
//
// See fa_flags.h for what belongs here. Each accessor caches its getenv on
// first call: these are read inside the analysis loops, and a raw getenv
// is a linear scan of environ.

#include "ifadefs.h"

#include <string>

#include "fa.h"
#include "fa_flags.h"

// ifa/074: a creation point in a split-child EntrySet joins the CreationSet
// its split parent's creation point is in, BEFORE the start-merged (dcpa1)
// route sends it to the sym's root. An ES split must not split -- or
// re-merge -- a CS; only a demand on the CS itself does. PYC_CSPARENTFIRST=0
// restores the old order (dcpa1 first), for A/B attribution.
int csparentfirst_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CSPARENTFIRST");
    e = v ? atoi(v) : 1;
  }
  return e;
}
// ifa/133: make the split-parent route survive the pass it was created in.
// PYC_ESLINEAGE=0 restores the old `es->split`-only behaviour, for
// attributing a corpus change to this clause.
int eslineage_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_ESLINEAGE");
    e = v ? atoi(v) : 1;
  }
  return e;
}
// ifa/164: a `{None, T}` union at a primitive ARGUMENT is a nullable
// pointer, not an illegal type.
//
// A pointer-shaped `{T, None}` is representable as one nullable pointer --
// the model shedskin compiles the same programs under, where `None` is
// simply `NULL` inside `str *`. (Until ifa/178, `type_cannonicalize`
// stripped `nil_type` from the `->type` projection in that case, so most
// consumers never saw the None. It now keeps it: nullability is a type,
// and representability is decided where a representation is chosen,
// which is this check.)
//
// This check read the RAW `out`, which made it the ONE consumer that
// rejected a member the rest of the compiler had already agreed to
// represent. The inconsistency showed up as OPPOSITE errors on the two
// operand positions of one operator:
//
//   a[1] + "y"      None in the RECEIVER   -- compiled silently
//   "".join(a)      None in an ARGUMENT    -- fatal, via `r + x`
//
// so `cipher = [None] * len(txt)` followed by a full overwrite -- a
// correct CPython program, and the standard preallocation idiom
// (shedskin_examples/solitaire) -- was rejected. The union is TEMPORAL
// (the list holds None at t0 and str at t1, in one object from one
// creation point), so no contour split separates it and none should be
// asked for; the answer is the representation, which pyc already has.
//
// The `{None, scalar}` case is NOT this: None and 0 share a bit pattern
// unboxed, which is why 060 keeps nil in `->type` beside a num_kind
// scalar. The check tests for that scalar directly. It used to accept only
// when canonicalization had stripped the nil from `->type` -- the same
// rule, read off the projection, and wrong once `->type` keeps nil
// (ifa/178).
// That is `genetic2` (an implicit fall-through `return None` unioned with
// int64) and it stays an error -- see ../../issues/048.
//
// Deliberately NOT suppressed: an argument whose whole type is `None`.
// A nullable pointer needs a pointee, so `"" + None` stays an error --
// only the *mixed* union is sanctioned, and only when the non-nil part
// is itself legal for the primitive.
// PYC_NILARG=0 restores the old raw-`out` check, for attributing a change
// to this rather than guessing at it (same role as PYC_STRICTVIOL).
int nilarg_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_NILARG"); e = v ? atoi(v) : 1; }
  return e;
}
// ifa/158: every type violation except MAYBE_UNBOUND is fatal, in every mode.
// PYC_STRICTVIOL=0 restores the pre-2026-09-17 severity.
int strictviol_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_STRICTVIOL"); e = v ? atoi(v) : 1; }
  return e;
}
// ifa/issues/074 (detach-route growth): on the DETACH route
// (`make_entry_set` with a non-null `split`), offer the edge an existing
// contour when that contour is a HARD match -- `entry_set_compatibility`
// == INT_MAX, i.e. no penalty of any kind: type-compatible, sset-
// compatible, constant-compatible, filters admit it. `split` itself is
// vetoed.
//
// Motivation: skipping the scoring path entirely on this route means a
// detached edge is never offered an existing contour -- it takes the
// pending/lineage route or gets a BRAND-NEW one. With the display out of
// contour identity (issues/100) that is now the dominant growth source:
// sudoku4/genetic2 leak an exactly steady `split-fresh=2` plus ~134/42
// new edges for the fresh contours' bodies every pass, to the pass cap;
// hq2x re-manufactures ~250 edges a pass the same way.
//
// OFF by default: it regresses 6 tests (see issues/074's 2026-08-13
// census). Using the *soft* score here is far worse (59 failures) -- it
// re-merges what the split just separated, 073's match_seq hazard -- so
// only the hard match is worth carrying as an experiment. The 6 include
// recursive_polymorphic and match_map_star, i.e. exact type identity is
// NOT sufficient evidence that a contour is not what the split is
// separating; the flag exists to keep investigating that.
// 0 off (default); 1 = entry_set_compatibility == INT_MAX; 2 = that AND
// a POSITIVE type-level match; 3 = that AND a positive CreationSet-level
// match (see edge_type_identical_to_entry_set); 4 = LOOKUP BY DURABLE
// TYPE KEY -- shedskin's model, where the type tuple names the contour
// rather than being a property compatibility-tested against it (it reads
// EntrySet::type_key, captured each pass since PYC_SELFPROD >= 3).
// Defaults to 5 as of 2026-08-16 (was 0 -- off). Mode 5 is mode 4's
// durable-type-key reuse on the detach route, RESTRICTED to splits whose
// own discriminator was argument types (see cur_split_type_only).
//
// ifa/issues/101: the detach route (`if (!split) find_best_entry_sets`)
// never offers a detached edge an existing contour, so every caller split
// cascades into fresh callee contours. Measured on linalg: half its 1290
// contours share an argument-type tuple with another, and the total
// EXCEEDS full cartesian-product specialization (957) by 35% --
// `__pyc_to_bool__` alone had 14 live contours with one type tuple
// between them.
//
// Mode 4 (types alone) was measured and is NOT safe: it breaks sudoku5's
// convergence outright (26 -> 273 violations) and worsens go and plcfrs,
// because a setter- or mark-driven split can produce two contours with
// identical argument types on purpose. Mode 5 adds exactly that
// condition and every one of those regressions disappears.
//
// Corpus, 77 programs: zero exit-code changes, zero pass_limit_hit
// changes, corpus violations 7435 -> 6399 (-13.9%), ess lower on 41
// programs (-2.3% overall), +1.5% analysis time.
int hard_reuse_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_HARDREUSE");
    e = v ? atoi(v) : 5;
  }
  return e;
}
// ifa/issues/101: shedskin's MOLD FALLBACK. When a contour has no live
// split parent to inherit an allocation instance from, shedskin does not
// mint -- ifa_seed_template falls back to `gx.orig_types[node]`, the
// allocation's instance in the dcpa=0/cpa=0 mold, i.e. the one every
// other contour of that function uses. It therefore keeps ONE container
// instance per allocation SITE, shared across contours, and lets `ifa()`
// split it later when it finds a concrete imprecision -- by which time
// the element types are known.
//
// pyc has no such fallback: `creation_point` mints unconditionally, so a
// site allocates once per (site x contour) from pass 0 onward. That is
// what makes `stereo` create 185 container CreationSets covering 2
// element shapes on pass 0 alone.
//
// 1 = containers only (s->element), the DEFAULT from 2026-08-16.
// 2 = every eligible sym; measured and rejected -- it costs plcfrs
// dearly (violations 2232 -> 4353, ess 850 -> 1213) for a few css on
// other programs. 0 restores the old mint-unconditionally behaviour.
// 3 = containers only AND never for a SPLIT CHILD contour, the DEFAULT
// from 2026-08-28 (ifa/105); see the mode-3 note at the fallback itself.
// Measured against mode 1: pyc suite identical (300 passed / 0 failed /
// 15 known), corpus identical program for program (the same five fail:
// chess, go, linalg, othello3, sudoku5), and the whole bounded
// copy-of-copy family -- four or more nested copy.deepcopy calls --
// goes from a BOXING refusal to compiling and running correctly.
//
// Corpus at mode 1, 77 programs: zero exit-code changes, zero
// pass_limit_hit changes, violations 6399 -> 4276 (-33.2%, plcfrs alone
// 4355 -> 2232), ess and css lower on 3 and 4 programs and HIGHER ON
// NONE, analysis time -2.8%.
int csmold_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CSMOLD");
    e = v ? atoi(v) : 3;
  }
  return e;
}
// ifa/128, and the architecture CLAUDE.md's goal statement describes:
// START MERGED. One CreationSet per sym, program-wide, and it multiplies
// ONLY by demand splitting -- `split_css` peeling off a setter-equivalence
// group. This is shedskin's `Class.dcpa = 1` posture.
//
// It inverts today's dependency. Today `creation_point` mints one CS per
// *(allocation site x contour)*, so an EntrySet split MULTIPLIES
// CreationSets as a side effect and CS identity is decided by structure
// before any demand test runs. Under this flag nothing about the structure
// makes a contour: an ES split separates creation points so that a CS
// split BECOMES POSSIBLE, which is the opposite direction of service.
//
// This is not a keying question. PYC_CSELEM's shape canon (deleted, ifa/146)
// existed to canonicalize per-site contours; with one contour per sym there
// is nothing to canonicalize.
// ifa/129: START MERGED -- one CreationSet per sym -- is the DEFAULT.
//
// CLAUDE.md's premise is that IFA starts from the MINIMUM data contours and
// splits only on demand. Without this, `creation_point` keys identity on
// (allocation site x contour), so data contours start MAXIMALLY split and
// never merge: `multidef=0` corpus-wide means every CreationSet has exactly
// one creation point. Mode 2 is that premise implemented; mode 0 is the old
// maximal start and remains available as `PYC_CSDCPA1=0` for one release,
// for bisecting anything this moves.
//
// Mode 2 rather than 1: 1 includes `tuple`, and a tuple's ARITY and
// POSITION are part of its type, not provenance -- merging them costs ten
// corpus programs (ifa/128). The exclusion is measured, not a concession.
//
// The cost of the flip, and what is still owed for it, is ifa/129.
int csdcpa1_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CSDCPA1");
    e = v ? atoi(v) : 2;
  }
  return e;
}
// DEFAULT 3 -- the GENERAL form (ifa/issues/055). A 2-cycle is not the
// only shape the ledger can hold: A->B->C->A is the same disease with
// three signatures, and route_last's one-step memory cannot see it.
// Mode 3 records the whole route relation (FA::route_adj) and refuses
// any route that would close a cycle of ANY length, so the relation is
// acyclic by construction rather than by pattern-matching one shape.
// Measured strictly >= mode 1: repro 32 -> 31 passes, suite identical
// (298 passed / 0 failed / 13 known), corpus identical program for
// program (67 of 77), plcfrs improves (5353 -> 4993 violations, ess
// 1524 -> 1420). On everything measured only 2-cycles actually occur,
// so the extra reach is insurance, not yet a demonstrated win.
//
// Mode 4 -- "never re-assign to ANY previously routed EntrySet" -- is
// the tempting stronger rule and it is WRONG, measurably: 6 suite
// failures, two of them hard compile failures (test_heapq,
// tuple_compare). Repeating the SAME route every pass is the ledger
// WORKING, a re-derived group landing in its established home; only a
// return to an ABANDONED home is pathological. "Closes a cycle" is
// exactly that distinction, "never revisit" conflates the two. Kept
// only so the difference stays measurable.
int routecycle_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_ROUTECYCLE");
    e = v ? atoi(v) : 3;
  }
  return e;
}
// ifa/issues/055: treat "routed the same group to the same home as last
// pass" as NOT progress. Default 0 while it is measured.
int routestable_enabled() {
  static int e = -1;
  if (e < 0) {
    // ifa/148, DEFAULTED ON 2026-09-11. Re-routing an unchanged group to
    // the same home as last pass is not new information, and claiming it
    // is keeps the first-stage-wins cascade alive and starves every later
    // stage. The field and the test were added with that comment
    // (`SplitDecision::last_route_pass`) and never switched on. Measured,
    // one binary, env toggled:
    //
    //   suite   315/0 both ways
    //   corpus  identical -- 2 cfail (othello3, rdb), 43 warns, 2740 CS
    //   sudoku5 two fewer starved passes
    //
    // Free, so it is the default. Note what it does NOT claim: a recorded
    // decision applied to a NEWLY-APPEARED contour is real work (a ledger
    // entry is keyed on (fun, stage, position, partition), so it can only
    // fire once the types have propagated far enough for a contour to
    // carry that partition). This suppresses only the repeat of last
    // pass's routing of the SAME group to the SAME product.
    cchar *v = getenv("PYC_ROUTESTABLE");
    e = v ? atoi(v) : 1;
  }
  return e;
}
// ifa/issues/074: extend the self-product complement eviction to the
// `v > 0` case (residual violations). See the comment at the eviction
// site for what each mode tests.
//
//   0 = off: eviction only at whole-program convergence (pre-074 shape)
//   1 = evict only the type-disjoint complement          (unsound)
//   2 = keep the group, evict nothing                    (unsound)
//   3,4 = durable key == the recorded partition          (never fires)
//   5 = durable key stable across two passes: per-contour convergence
//
// **5 is the default.** The eviction's real precondition is that THIS
// CONTOUR has stopped moving, which `nviol_this_pass == 0` only ever
// approximated whole-program-wide.
// Defaults to 6 as of 2026-08-16 (was 5): accept a period-2 flip-flop as
// a settled contour, not just a constant one. Measured over the whole
// shedskin corpus as EXACTLY inert -- 77 programs, zero changes to
// rc/violations/ess/css/final_pass/pass_limit_hit, -0.3% time -- while
// making tests/deepcopy_recursive_nested_growth.py converge outright
// (pass 46, pass_limit_hit=0, 0 violations, against mode 5's pass 102
// with 4 violations). See the mode-6 comment at the use site.
int selfprod_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_SELFPROD");
    e = v ? atoi(v) : 6;
  }
  return e;
}
// ifa/133: PYC_NILSTORE=0 restores the pre-fix behaviour, for attributing
// a corpus change to this clause rather than guessing at it.
int nilstore_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_NILSTORE");
    e = v ? atoi(v) : 1;
  }
  return e;
}
// Issue 033 D5: cross-pass identity for a split_css decision. The
// partition split_css applies is "these defs share setter
// equivalence classes" — but Setters/setter_class pointers are
// hash-consed in cannonical_setters, which clear_results() CLEARS,
// so they cannot appear in a cross-pass key (issue 033 D0). The
// stable proxy: the CreationSet's sym, the sorted def-Var sym ids
// of the compatible group (Var/Sym are interned for the life of
// the FA), and the CONTENT of the group's setters — each setter
// AVar's Var sym id paired with its canonical constant-stripped
// value type (canonical ATypes are never cleared). The value types
// keep two same-Var-set groups distinct (an int-writing and a
// float-writing instance of the same creation point must not share
// a key — the ES ledger learned this as the builtins_batch
// int/float poisoning; D5's own note says value types alone are
// too weak and def ids alone can't discriminate either, so the key
// carries both). Same wildcard rule as the ES group_signature: an
// unflowed setter value (empty ->type) means the group has NO
// stable identity this pass — return 0, caller must neither record
// nor count. Setter contributions accumulate commutatively (sum),
// so Vec-set iteration order cannot perturb the hash.
// ifa/issues/066: drop the per-pass term from the CS split signature so
// the ledger can recognise a re-derived split. See the use below.
// Defaults to 3 (the durable setter type: canonical SET of split-chain
// roots) as of 2026-08-16. Measured over the whole shedskin corpus at
// ZERO exit-code changes and zero changes to violations/ess/css/
// final_pass on all 77 programs, +1.1% analysis time, with the full test
// suite unchanged -- while cutting
// tests/deepcopy_recursive_nested_growth.py's runaway contour growth to
// the best figure any mode reaches (ess 272 -> 144 at pass 102, CS mints
// 20 -> 4).
//
// The other modes are kept as controls, and the pair of them is the
// argument for 3: mode 1 (drop the setter type outright) reaches the
// same 144 but pays for it in precision -- linalg 27 -> 74 violations,
// plcfrs losing 173 contours' worth -- while mode 0 keeps the precision
// and none of the convergence. Mode 3 is the first to get both, which is
// what makes it defaultable. Mode 2 is superseded: same idea, but hashed
// over the raw `sorted` sequence, so it was order- and
// multiplicity-sensitive and only got half way (164).
int cskey_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CSKEY");
    e = v ? atoi(v) : 3;
  }
  return e;
}
// ifa/issues/133: partition a CreationSet by its CREATION POINTS.
//
// The gap this closes. A type confluence on a container's element is
// collected (`collect_type_confluences` covers `cs->vars` and the element
// AVar) and then discarded, because `split_ess_for_type` can only split an
// EntrySet and this confluence sits on a CreationSet contour. Measured on
// ifa/133's five-line reproducer: the confluence arrives with everything
// needed -- `cs=983 sym=list defs=6 type= int64 str` -- and nothing
// consumes it. `split_css`, the only other CS splitter, is fed
// `setter_starters` and never sees this CreationSet at all (its element
// has no setters, so no starter's `cs_map` names it).
//
// Why partitioning `defs` is legal and needs no new state.
// `creation_point` writes both halves from the same variable, back to
// back (`v->cs_map->put(s, cs); cs->defs.set_add(v);`), so `cs->defs` IS
// the set of AVars whose `cs_map` names `cs`, and `split_css`'s re-point
// -- `v->cs_map->put(cs->sym, new_cs)` -- applies to them unchanged.
//
// Why WHOLESALE rather than a minimal partition. Which creation point
// contributed which element type is exactly what the merge destroys;
// ifa/133 measured two attempts to recover it and neither works. Wholesale
// does not ask. It gives each creation point its own contour and lets the
// next pass re-derive every element from the writers that actually reach
// it -- sound because `analyze_to_convergence` resets before every pass,
// so derived ATypes come back from bottom and only the DECISION persists.
// Recovering the attribution instead would be provenance, which is never
// the answer (CLAUDE.md).
//
// Why it is the LAST rung. This is shedskin's ladder route 4
// (`infer.py:1576`), and there too it is tried only after no-confusion,
// confluence-partition and path-partition have failed. It is the coarsest
// separation available, so it must not preempt a finer one: the call site
// is gated on quiescence of every stage above, exactly as PER_CS_RECEIVER
// and CSM_ELEMENT_CS are.
//
// Termination is structural, not a cap. After the re-point `cs_map` names
// the new CreationSet, and `creation_point` is memo-first, so the next
// pass routes that site to its own contour and it is no longer among
// `cs->defs`. A CreationSet that has been partitioned down to one creation
// point fails the `defs > 1` test forever after.
int csdefsplit_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CSDEFSPLIT");
    e = v ? atoi(v) : 1;
  }
  return e;
}
// ifa/133 steps 3-6: shedskin's finer ladder rungs, tried BEFORE the
// wholesale partition. Default 0 while being measured. A BITMASK, because
// the pieces had to be attributed separately once they started disagreeing:
//
//   1  route 1, ifa_split_no_confusion  (infer.py:1585)
//   2  route 3, partition csites across paths (infer.py:1571)
//   4  step 6, contour REUSE -- join an existing contour instead of minting
//   8  key the emptycsites group on the BOTTOM AType rather than "no key",
//      which is what makes the reuse lookup reachable for it
//  16  peel EVERY group in route 1, as shedskin does, not just the first
//
// **3 is the measured-good configuration**: suite 9 under PYC_CSDCPA1=2,
// -45 container CreationSets corpus-wide, and every verdict on all 77
// programs identical. 27 and 31 both regress `deepcopy_copy_of_copy_chain`
// -- ifa/105's acceptance test -- to 10. See the issue for why 4/8/16 do
// not pay yet.
int csladder_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CSLADDER");
    e = v ? atoi(v) : 0;
  }
  return e;
}
// ifa/146 C: the CONTENT channel(s) this CreationSet's writers feed.
//
// A container's content is its generic ELEMENT; a plain class's content is
// its MEMBERS (`cs->vars`). ifa/104 calls these the two content channels,
// and this function used to read only the first -- so it returned null for
// every non-container CreationSet, and `split_css_by_defs` had nothing to
// group on and fell back to a per-creation-point FAN.
//
// Measured before this: on `bh`, 16 of 24 route-4 mints were `Vec3` and all
// 16 came through that fan, leaving 18 of 20 `Vec3` contours byte-identical
// across all 29 members. The fan is the arbitrary mechanism ifa/146 exists
// to retire; giving the rung a real grouping key for plain classes is what
// lets it go.
//
// Containers are UNCHANGED: when an element channel exists it is used
// alone, exactly as before, so no container result moves.
// ifa/133: PYC_CSCONTENT=0 restores the element-or-vars form.
// ifa/133's fix for "the rung looks in the wrong channel", ON BY DEFAULT since
// 2026-09-15 (ifa/154). A container has TWO content channels (ifa/104) and an
// arity-N literal leaves the element bottom, so without this the CS flow graph
// is built over an empty AVar and route 4 declines `sets=0` on every such
// CreationSet. Measured with PYC_CSBACKTRACK above. `PYC_CSCONTENT=0` disables.
int cscontent_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_CSCONTENT"); e = v ? atoi(v) : 1; }
  return e;
}
// ifa/133: when route 4's key declines because a SHARED contour hides the
// partition, split that contour so the CreationSet split becomes possible.
// `PYC_ESBLOCK=0` restores the old opt-in behaviour.
//
// ON BY DEFAULT since 2026-09-24, on a measured corpus A/B (`check`, 77
// programs, one binary, env the only difference). It used to be opt-in
// because it "moves the set, it does not shrink it" and cost `softrender`;
// `softrender` no longer compiles at either arm, so that trade is gone,
// and the current one is favourable:
//
//   compile_fail 32 -> 32, run_fail 14 -> 13, container CS/shapes
//   2730/641 = 4.26 -> 2461/662 = 3.72
//
//   gained:  sudoku2 (now compiles, runs, and MATCHES CPython -- the only
//            stdout_match=yes anywhere in the diff), webserver (compiles)
//   lost:    dijkstra2, quameon -- both of which already compiled and then
//            died at RUN (rc=124 / rc=134), so neither was a working program
//   sunfish: compiler timeout (124) -> clean diagnostic (1)
//
// Fewer contours AND better outcomes is ifa/146's non-monotone diagnostic
// pointing the right way, so this is the lever earning its keep rather
// than a preference.
//
// It is what makes the comprehension accumulator's result-move in
// `build_list_comp_inner_pyda` safe to keep: that move round-trips
// through `list.append`'s return, and without this split every
// comprehension in the program shares one `append` EntrySet, so the
// round-trip becomes a program-wide element-union channel (`plcfrs`).
// ifa/174: collect_violation_imprecisions backtracks a violation along its
// value flow to the first load over a union receiver, and offers that
// receiver to the type splitter. PYC_LOADBT=0 turns it off.
int loadbt_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_LOADBT"); e = v ? atoi(v) : 1; }
  return e;
}
int esblock_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_ESBLOCK"); e = v ? atoi(v) : 1; }
  return e;
}
// How many contours does a given function have, and what is each one's
// receiver? IFA_DBG_FUNES=<name> prints it at convergence. Added for ifa/133
// group B: "two clones of one class disagree on a slot" is usually a question
// about how many contours the WRITER got, and nothing printed that.
// IFA_DBG_CSVARS=<sym name>: every CreationSet of that class with its
// member AVars and their types, as FA leaves them. ifa/133 group B needs
// exactly this: a `<placeholder>` member in the emitted C means
// `has[i]->type` is null, and that is set from these AVars -- so this says
// whether the analysis or the cloning lost the field.
// ifa/152: PYC_CSBACKTRACK=1 -- when a demanded CreationSet cannot be
// partitioned because it has ONE creation point, backtrack the demand along
// the value flow to the nearest CreationSet that HAS several, and offer that
// one instead.
//
// This is the step the ladder was missing. The demand is observed where the
// union is USED, and that is generally not where the merge HAPPENED: the
// merged CreationSet is upstream and is usually representable on its own, so
// `cs_elem_irrepresentable` never nominates it and it is not a candidate at
// all. chull measures it exactly: five lists (`Hull.edges` and friends) carry
// element {Vertex, Edge} with defs=1 and decline "single creation point" on
// every pass, while the walk backward from each of the five names ONE
// CreationSet with defs>=2 -- cs=1112, element {Vertex}, nine creation points
// spanning `InitEdges`'s `newedges = []` and `Edge.__init__`'s
// `self.endpts = []`. `extend` fills the second with Vertex; `InitEdges`
// returns the first unwritten; they are the same contour, so `Hull.edges`
// inherits a Vertex.
//
// Under ifa/146's two-question test this is a MECHANISM, not a reason. The
// reason is the demand -- an irrepresentable element with nothing to
// partition. Take it away and nothing is nominated: the walk only ever runs
// from a CreationSet that has already declined. And the handle is not
// provenance: "the offending element flows from here" is a statement about
// value flow and deduced types, not about where a value was born.
// ON BY DEFAULT since 2026-09-15 (ifa/154), with PYC_CSCONTENT which supplies
// the other half. Corpus `-m check`, one binary: compile failures 8 -> 3,
// total warnings 1973 -> 1364 (-31%). `bh` compiles and matches CPython;
// `sudoku3` goes from compile-fail with 121 warnings to clean and running.
// NOTHING THAT WORKED REGRESSED -- all four programs whose stdout matched
// CPython still match. `PYC_CSBACKTRACK=0` disables.
int csbacktrack_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_CSBACKTRACK"); e = v ? atoi(v) : 1; }
  return e;
}
// ifa/157 THE LINK (PYC_RETDEMAND). Stage 1 observes a demand on a value it
// has no actuator for and drops it (`census.tc_skip_rval`, 769 of 923 per pass on
// softrender). Both halves of the answer already exist: a CS-contoured
// confluence is handed to route 4 (`tc_cs_dropped`), and ifa/152 built the
// backtrack that finds the merged CreationSet upstream. Only the link is
// missing.
//
// Return null unless this really is a demand:
//   - the callee returns must DISAGREE -- an unresolved dispatch, something
//     observing a distinction and unable to proceed, not "the type is a union"
//   - the dispatched-on classes must be UNRELATED. Classes sharing a
//     user-defined ancestor are legitimate polymorphism and must be HOISTED,
//     not split; splitting richards' four Task subclasses is what broke it.
int retdemand_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_RETDEMAND"); e = v ? atoi(v) : 0; }
  return e;
}
