// SPDX-License-Identifier: BSD-3-Clause
//
// See fa_flags.h for what belongs here. Each accessor caches its getenv on
// first call: these are read inside the analysis loops, and a raw getenv
// is a linear scan of environ.

#include "ifadefs.h"

#include <string>

#include "fa.h"
#include "fa_flags.h"

// ifa/148: PYC_SPLITEDGES2=1 -- stage 5's per-CreationSet fan becomes a
// two-group split. See the comment at the use.
int splitedges2_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_SPLITEDGES2"); e = v ? atoi(v) : 0; }
  return e;
}
// ifa/148: PYC_FILTEREQ=1 -- filtered-contour reuse requires matching
// filters. See the comment at the use.
int filtereq_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_FILTEREQ"); e = v ? atoi(v) : 0; }
  return e;
}
// ifa/148: PYC_ESPATH=1 -- split the whole EntrySet path to a confluence
// in one pass, instead of one contour per pass. See the comment at the use.
int espath_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_ESPATH"); e = v ? atoi(v) : 0; }
  return e;
}
// ifa/129: PYC_ESDEFS1=1 -- a violation on a single-creation-point
// CreationSet splits the contour that makes its site occur once. See the
// comment at the use.
int esdefs1_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_ESDEFS1"); e = v ? atoi(v) : 0; }
  return e;
}
// ifa/146 E: PYC_ESRECV=1 -- a violation inside a contour makes that
// contour's RECEIVER an imprecision. See the comment at the use.
int esrecv_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_ESRECV"); e = v ? atoi(v) : 0; }
  return e;
}
int confdemand_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_CONFDEMAND"); e = v ? atoi(v) : 0; }
  return e;
}
// ifa/issues/074 (PYC_CPAMARK): swap the cartesian-product name in for the
// mark. different_marked_args already compares two sets of CreationSets --
// the CPA question -- but only over CSs admitted by the distance filter
// `m - offset == x->value`. With this on, the filter is dropped and the CS
// sets are compared directly, so the split rule becomes "which CreationSet
// is here", with no depth term. IFA_DBG_MARKWHY predicts the effect: it is
// exactly the `cs_same` verdicts (98% of hq2x's, 2% of the listcomp
// repro's) that stop firing.
int cpa_mark_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CPAMARK");
    e = v ? atoi(v) : 0;
  }
  return e;
}
int mark_why_enabled() {
  static int e = -1;
  if (e < 0) e = getenv("IFA_DBG_MARKWHY") ? 1 : 0;
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
// ifa/issues/109: record a violation when sizeof_element's receiver spans
// CreationSets that cannot share one concrete container type.
int sizeof_viol_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_SIZEOF_VIOL");
    e = v ? atoi(v) : 0;
  }
  return e;
}
// ifa/164: a `{None, T}` union at a primitive ARGUMENT is a nullable
// pointer, not an illegal type.
//
// `type_cannonicalize` already strips `nil_type` from the `->type`
// projection whenever the rest of the union is pointer-shaped -- that is
// issue 060's settled decision, "Optional[pointer] still single-clone
// (frontend-sanctioned merge preserved)" -- and it is the model shedskin
// compiles the same programs under, where `None` is simply `NULL` inside
// `str *`. Dispatch, narrowing and defaulted parameters all read that
// projection, so none of them ever sees the None.
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
// The `{None, scalar}` case is NOT this, and condition (3) below keeps it
// out: 060 deliberately KEEPS nil in `->type` when the union carries a
// num_kind scalar, because None and 0 share a bit pattern unboxed. That
// is `genetic2` (an implicit fall-through `return None` unioned with
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
int confnil_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_CONFNIL"); e = v ? atoi(v) : 0; }
  return e;
}
int splithomo_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_SPLITHOMO"); e = v ? atoi(v) : 0; }
  return e;
}
// ifa/issues/101: include the return types in group_signature. 1 is the
// historical behaviour and stays the DEFAULT; 0 drops the term.
//
// Dropping it was the obvious repair for the ledger cycle described at
// the use site, and it measured EXACTLY INERT -- byte-identical
// final_pass/violations/ess/css on go, linalg, plcfrs and sudoku5. So the
// two cycling signatures do NOT differ in their return term, and the
// hypothesis behind this flag is wrong: the difference is in the argument
// term, which means linalg's 792/692 pair is most likely two distinct
// groups SWAPPING contours rather than one group cycling. Kept as a
// measured negative result, defaulted to the historical behaviour since
// it buys nothing and has not been swept.
int gsigret_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_GSIGRET");
    e = v ? atoi(v) : 1;
  }
  return e;
}
// ifa/issues/075: element-CS container-method separation (pyc's analog
// of shedskin's func_copy-per-dcpa). 0 off (default, byte-identical to
// baseline), 2 split (fans split_edges per (CS x display), Piece 1; the
// demand-driven stage itself is split_container_methods_per_element_cs
// below). 1 is reserved for a future side-effect-free dump/probe mode
// (see 075 Piece 1) -- not yet built, treated as off.
int csm_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CSM");
    e = v ? atoi(v) : 0;
  }
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
// rather than being a property compatibility-tested against it. Requires
// PYC_TYPEKEY.
// ifa/issues/074: match compatibility against EntrySet::type_key (the
// previous pass's CONVERGED formal types) rather than the contour's
// momentary mid-pass accumulation. The point is durability: shedskin
// binds a contour to a fixed type tuple and keeps that binding across
// passes, so routing is a lookup rather than a race. Off by default.
int typekey_enabled() {
  static int e = -1;
  if (e < 0) e = getenv("PYC_TYPEKEY") ? 1 : 0;
  return e;
}
// ifa/issues/074: canonicalize contour creation on the durable type key
// -- at most one contour per (fun, type tuple), found by lookup and
// created on miss, the way find_or_make_filtered_entry_set already works
// for CS partitions. Durable keys alone (PYC_TYPEKEY) proved necessary
// but not sufficient: they make matching stable in TIME but not unique
// in SPACE, so two same-keyed contours still let the `x != split` veto
// alternate. Canonicalization removes the duplicates, so there is
// nothing to alternate between.
//   1 = canonicalize, but never hand an edge back to the contour the
//       splitter is detaching it FROM (conflicts logged, split honored)
//   2 = full canonicalization: reuse even then, so a split that
//       disagrees with the canonical key becomes a no-op
// IFA_DBG_CANON=1 logs every conflict and prints per-pass stats.
int canon_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CANON");
    e = v ? atoi(v) : 0;
  }
  return e;
}
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
// ifa/issues/074: disable mark-based splitting stages, as an
// investigable option.
//   0 = mark-based splitting on (pre-2026-08-14 behaviour)
//   1 = skip MARK_TYPE (stage 2)  -- THE DEFAULT
//   2 = also skip MARK_SETTER / MARK_SETTER_OF_SETTER (stage 4)
// Marks exist to separate two contours that carry the SAME argument
// types but different value origins (IFA.md §6.2, "recursion-meets-
// polymorphism without k-CFA"). That is by construction a
// distinction no type-tuple contour name can express -- so if
// contours are canonicalized on their type key, mark splits are
// unnameable. IFA_DBG_KEYSPACE=1 measures the gap they open.
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
// ifa/issues/074: gate the ES ledger ROUTE on the recorded product still
// being a compatible home for the group; =2 also refreshes an entry
// proven stale. OFF by default, and kept only as a measured control:
// both modes do stop the churn, but by declining a route you only mint
// instead, so the growth comes straight back (repro ess 144 -> 279 for
// mode 1, 257 for mode 2). PYC_SELFPROD=6 fixes the same oscillation
// from the other end without that cost.
// ifa/issues/101: canonicalize CONTAINER CreationSet identity on the
// durable element type instead of the creation site x contour. Off by
// default. See capture_elem_keys() and creation_point's Lcanon.
//
// 3 = ifa/issues/074: key on the RECEIVER's structural element SHAPE
// instead (`list<list<float64>>`), which is what shedskin gets free from
// `list<T>` being keyed on T. Off by default, and MEASURED:
//
//   deepcopy_recursive_nested_growth.py, guards off, ess/css by pass
//     mode 0   0:77/599  20:175/782  40:280/997  60:385/1212
//              80:490/1427  100:595/1642      -- dead linear, +5.25/pass
//     mode 3   0:77/599  20:159/749  40:157/740  60:211/852
//              80:223/877  100:207/850        -- bounded, band ~210
//
// That is 074's headline defect gone: the growth is UNBOUNDED at the
// default and BOUNDED here (-62% ess, -46% css at pass 101). What is
// left is an oscillation inside the band, which is a different and much
// smaller problem than divergence -- but CONVERGED is still 0, and the
// repro still does not compile.
//
// Not the default because of the cost: pyc suite is identical
// (301/0/14), corpus goes from 5 failing to 9 -- kanoodle, plcfrs and
// rdb time out and quameon fails to compile. The likely reason is in
// cselem_shape_key: the canon map is MONOTONE and global, and the shape
// it keys on is read LIVE on the pass the contour is created, because
// that is the only moment creation_point is ever consulted (`cs_map`
// answers ever after). A merge decided from an incomplete shape can
// never be revisited, and the splitter then has to work around it. That
// is ifa/issues/066 -- the decision is keyed per pass, not per creation
// site -- and making the canonicalization revisitable is the next step,
// not a wider or narrower key.
int cselem_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CSELEM");
    e = v ? atoi(v) : 0;
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
// ifa/129 step 4: drop the PER-SITE component from the mode-3 shape key.
//
// cselem_shape_key composes `v<id>|class#id|shape`. The leading `v<id>` is
// `v->var->id` -- WHERE the value was allocated -- so the canon is keyed per
// allocation site and the fewest contours it can ever name is the number of
// distinct (site, class, shape) triples rather than of (class, shape) pairs.
// Measured on the corpus: 1670 against 660, a 2.53x fragmentation, and the
// 660 lands within 5% of `shapes` (626), the independent content census.
// Contour identity is supposed to key on types and CS partitioning, never on
// provenance, and `v->var->id` is provenance.
//
// OFF by default: it merges contours of UNRELATED allocation sites, which is
// a bigger merge than anything mode 3 does today, and it is only safe with
// cselem_resplit_diverged armed to take the merge back. Turn both on
// together (PYC_CSSITELESS=1 PYC_CSRESPLIT=1) or neither.
int cssiteless_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CSSITELESS");
    e = v ? atoi(v) : 0;
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
// Independent of PYC_CSELEM -- this is not a keying question. The shape
// canon exists to canonicalize per-site contours, and with one contour per
// sym there is nothing for it to canonicalize.
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
int routegate_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_ROUTEGATE");
    e = v ? atoi(v) : 0;
  }
  return e;
}
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
// ifa/148: PYC_TYPEMOVE=1 -- run another pass while the derived types are
// still changing, instead of only while a stage split. See the use.
int typemove_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_TYPEMOVE"); e = v ? atoi(v) : 0; }
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
int elemsetter_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_ELEMSETTER"); e = v ? atoi(v) : 0; }
  return e;
}
// ifa/157: PYC_SETTERMIN -- partition to the COARSEST setter-induced grouping
// that discharges the demand, instead of the finest one setter equivalence
// induces.
//
// A union has no representation exactly when it mixes a basic type with a
// pointer-shaped one, or two distinct basic kinds. So a group is representable
// exactly when its written types are all pointer-shaped or all one basic kind,
// and the unique coarsest valid partition is "one group for every
// pointer-shaped writer, one group per basic kind" -- computable in one pass
// from `s->out` over each starter's setters.
//
// This is the partition CLAUDE.md's premise asks for: the minimum contours
// demand requires, and no more. Where it currently costs programs
// (`plcfrs`, `sudoku5`), the finest partition was HIDING a defect that this
// surfaces -- root cause that, do not accept the over-split.
int settermin_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_SETTERMIN"); e = v ? atoi(v) : 0; }
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
// ifa/129 third clause: split an EntrySet BY CALL SITE, because a container
// it allocates has an irrepresentable element and nothing type-shaped can
// name the contributors.
//
// The reason/mechanism distinction matters here and is the only thing that
// makes this legal under CLAUDE.md's provenance rule. The REASON is demand:
// an element union with no representation, on a CreationSet that CS-side
// partitioning cannot touch (one creation point) and that type-side
// splitting has already declined (`no_groups` -- every formal's callers
// agree). The call site is only the HANDLE that says which caller goes
// where. Take the demand away and nothing splits: this never fires on a
// contour whose containers are representable, so it is not 1-CFA by the
// back door.
//
// Measured on sudoku3 before building (IFA_DBG_THIRD): of 428 single-def
// candidates, 427 decline type-side with `no_groups`, and 361 of those have
// a single in-edge so there is no call site to split on either. This route
// is aimed at the remaining 67 -- `__pyc_getslice__` (1 contour, 7 callers)
// and `__pyc_tolist__` -- where the flag has FEWER contours than the
// default (getslice: 2 -> 1), because dcpa1 merged the receiver
// CreationSets that gave CPA its distinction.
int cscallsite_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CSCALLSITE");
    e = v ? atoi(v) : 0;
  }
  return e;
}
// ifa/133: PYC_CSSLOTDEMAND=0 restores the element-only demand test.
int csslotdemand_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_CSSLOTDEMAND"); e = v ? atoi(v) : 0; }
  return e;
}
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
// ifa/133: the MEMBER partition key.
//
// The content key above (`build_cs_flow_graph` over `cs_content_avars`)
// asks what is IN a container. This asks what the container is IN: the set
// of instance variables / members its creation point can reach. Those are
// different graphs, and on `bh` only the second separates the lists --
// by the time an element union is observable EVERY creation point carries
// the whole union, so the content key collapses them (measured: 2 groups
// from 8 creation points, both still `{str, Body}`), while the members they
// reach are distinct (`Tree.bodies` reached by exactly one of the eight).
//
// This is a TYPE-side key, not provenance: a member is part of a class's
// declared structure (`Sym::has`), so "which field holds this container" is
// a structural fact about the program's types, the same family as arity
// (ifa/132). It is keyed on `Sym::id`, never on the name (CLAUDE.md).
//
// It is also observable BEFORE the union forms, which is what the
// violation-gated experiments in ifa/133 could not manage.
int csmember_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_CSMEMBER"); e = v ? atoi(v) : 0; }
  return e;
}
int settergate_level() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_SETTERGATE"); e = v ? atoi(v) : 0; }
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
// ifa/133: route a CS-contoured violation to route 4. PYC_VIOLCS=0 restores
// the old behaviour (drop it) for attribution.
int violcs_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_VIOLCS"); e = v ? atoi(v) : 0; }
  return e;
}
// ifa/132: PYC_SLOTARITY=1 -- when one AVar must hold two CreationSets of ONE
// sym whose arities DISAGREE, neither can keep a record layout, because the
// value has a single C type and a record and a list are not the same type.
// Drop the static arity on all of them; `make_kind` then seeds the generic
// element from the per-index vars (see its `no_static_arity` clause) and
// clone gives the whole family list layout.
//
// This is the same rule ifa/132 already applies WITHIN a CreationSet ("two
// creation points of different arity means it has no static arity") and that
// `get_sym_tup` applies WITHIN a layout equivalence class (`if (n !=
// cs->vars.n) tup = false`). What was missing is the case where the
// disagreement is ACROSS two CreationSets that identity correctly keeps
// apart: `determine_basic_clones` splits them on `cs1->vars.n != cs2->vars.n`
// before `get_sym_tup` can see it, so one stays a record and the other is a
// list, and the slot holding both gets no type at all -- emitted `_CG_void`,
// read as `_CG_any`, with every access resolved from the FA type instead.
//
// Census, 84 corpus programs (IFA_DBG_SLOTREP): 976 such conflicts in 13
// programs. Every one of the 13 COMPILES and then fails or prints the wrong
// answer, and three of the aborts name the untyped value directly --
// `amaze` "getter not resolved", `linalg` `(_CG_any, _CG_int64)` "list
// element type mismatch", `quameon` `(_CG_ps26965, _CG_any)` "matching
// function not found". Nine have zero warnings.
int slotarity_enabled() {
  static int e = -1;
  if (e < 0) { cchar *v = getenv("PYC_SLOTARITY"); e = v ? atoi(v) : 0; }
  return e;
}
// ifa/129 STEP 4, first increment: RE-DECIDE a CreationSet that was minted
// while the receiver shape was unknown.
//
// This is the first place in pyc where a site->CreationSet decision is taken
// back. Everything it needs already existed:
//
//   - Flow state is not the obstacle. analyze_to_convergence resets BEFORE
//     each pass, not after, so every pass re-derives from bottom already.
//     The one thing that survives and is in the way is the DECISION,
//     av->cs_map (see clear_results's header).
//   - Re-pointing cs_map is a shipped primitive: split_css does exactly
//     `v->cs_map->put(cs->sym, new_cs)`, and its ledger `route` path aims a
//     group at a CreationSet that ALREADY EXISTS. It has simply never had a
//     caller that runs in the joining direction. This is that caller.
//   - The invariant the cs_map pin cites (issue 030 / make_closure_var: a
//     CS's positional vars[i] must be fed by every pass that feeds the CS)
//     is about a CS LOSING a feeder, not about a site being re-aimed. The
//     CS joined here keeps every feeder it had and gains one; the CS
//     abandoned here stops being fed at all, so it leaves `fa->css` and no
//     consumer sees it.
//
// WHEN: only once the pass has otherwise settled (see the call site). The
// question being answered is "would this decision have been reusable had it
// waited", so waiting is the point, and running it against a half-derived
// pass would just re-ask it on worse information. It is also the ifa/055
// lesson -- interleaving a decision change with the splitter re-perturbs
// contours the splitter had just settled.
//
// TERMINATION: each entry is re-decided at most once, because after the
// re-point `cs_map` names the joined CS and the `!= u.cs` test below skips
// it forever. The batch is done in one call, so this costs a bounded few
// extra passes, not one per join.
int cselem_rejoin_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CSREJOIN");
    e = v ? atoi(v) : 1;  // on whenever mode 3 is; mode 3 is itself opt-in
  }
  return e;
}
// ifa/129 STEP 4, the SPLIT-BACK direction.
//
// cselem_rejoin_unknown_mints widens: it moves a site onto a CreationSet
// that already exists. This narrows, and narrowing is the direction a
// growing fixed point does NOT absorb for free -- which is exactly why it
// has to exist before the merge is widened. Without it, a merge that later
// turns out to be wrong is permanent, and that is ifa/105's measured
// failure: a split child handed its parent's container came back with
// element type {list<itself>, int64, int64, list, int64}, self-referential
// and container/scalar mixed.
//
// The test is symmetric with the join's, and it is a DEMAND test: the join
// merges two sites whose shapes are known and EQUAL; this separates the
// defs of one CreationSet whose shapes are known and DIFFER. Divergence is
// an observed distinction, so acting on it is demand splitting rather than
// structural splitting.
//
// A def whose key is not known does not move and does not vote. Separating
// on an unknown is the mint-side error running in the other direction, and
// the whole point of this issue is not to make contours out of ignorance.
int csresplit_enabled() {
  static int e = -1;
  if (e < 0) {
    cchar *v = getenv("PYC_CSRESPLIT");
    e = v ? atoi(v) : 0;  // see cssiteless_enabled: arm the two together
  }
  return e;
}
