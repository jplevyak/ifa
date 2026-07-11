#ifndef _fa_H_
#define _fa_H_

#include <sys/types.h>
#include "code.h"
#include "ifadefs.h"
#include "prim.h"
#include "sym.h"

#define DEFAULT_NUM_CONSTANTS_PER_VARIABLE 1
// Default cap on outer convergence iterations. The runtime value lives
// in `FA::pass_limit` — change that to override per-FA instance. See
// `ifa/notes/`-style discussion at the cleanup entry in
// `ifa/analysis/CLEANUP.md` tier-2 item 4 for why the cap is soft
// (frontends inspect `FA::pass_limit_hit` instead of aborting).
#define IFA_PASS_LIMIT 100

// Stall guard on the splitting/reanalysis loop. The splitter's
// decisions are not idempotent across passes (order-dependent split
// choices, issues 009/021 flavor), so on some inputs the outer loop
// never reaches a fixed point: EntrySet counts and violations
// oscillate while per-pass cost grows superlinearly, and
// IFA_PASS_LIMIT is unreachable in wall time (issue 025 compile
// timeouts). Splitting exists to RESOLVE violations, so if the
// violation count hasn't improved on its best in this many
// consecutive passes, further splitting is treated as divergence and
// the loop stops (same pass_limit_hit semantics as the hard cap).
// Passes with zero violations (pure precision splitting) don't count.
// This is a MITIGATION; the root cause and the real fix (persistent,
// keyed, order-independent split decisions) are documented in
// ifa/issues/033-splitter-non-idempotent-divergence.md.
#define IFA_STALL_LIMIT 8

// The contour of module-level (global) variables' shared AVars.
// Historically a raw sentinel (`(void *)1`), which made every
// `(EntrySet *)av->contour` deref on a global AVar a latent crash
// (issue pyc/005) and required scattered `!= GLOBAL_CONTOUR`
// guards. Now a real, distinguished EntrySet (`FA::global_es`,
// created at `FA::analyze` entry): never registered in `fa->ess`,
// never given edges or pnodes, and with `in_es_worklist`
// permanently 1 so the standard `if (!es->in_es_worklist)`
// enqueue checks naturally skip it. Pointer comparisons against
// GLOBAL_CONTOUR keep their meaning ("is this the global
// contour?"); derefs of its EntrySet fields are now safe no-ops.
// See ifa/issues/031-globals-outside-fa-precision.md step 1.
#define GLOBAL_CONTOUR ((void *)::fa->global_es)

class Prim;
class RegisteredPrim;
class Fun;
class PNode;
class PDB;
class Edge;
class AVar;
class AEdge;
class AType;
class SettersHashFns;
class Setters;
class CreationSet;
class ATypeViolation;
class Patterns;
class MatchCache;
class MPosition;
class EntrySet;
struct FAPassEvent;

typedef Map<PNode *, Vec<AEdge *> *> EdgeMap;
// EdgeHash moved below class AEdge
typedef Vec<CreationSet *> VecCreationSet;
typedef Vec<Vec<CreationSet *> *> CSSS;

// Mix two pointer-sized values into a 32-bit hash. The bespoke
// `(13 * a) + (100003 * b)` pattern previously appeared inline in
// every hash-functions class; centralizing it here keeps the mixer
// consistent and gives future hashable types one obvious helper to
// reach for. Callers that need to mix three values do
// `a + combine_hash(b, c)` (matches the historical shape).
inline uint combine_hash(uintptr_t a, uintptr_t b) {
  return (uint)((13 * a) + (100003 * b));
}

class AType : public Vec<CreationSet *> {
 public:
  uint hash;
  AType *type;  // not including values (constants)
  Vec<CreationSet *> sorted;
  Map<AType *, AType *> union_map;
  Map<AType *, AType *> intersection_map;
  Map<AType *, AType *> diff_map;
  // Cache for type_coerce_numeric_constants (issue 025 numeric
  // unification): target numeric type -> coerced AType.
  Map<Sym *, AType *> coerce_map;

  AType(CreationSet *cs);
  AType(AType &a);
  AType() : hash(0) {}

  AType *constants();
};

class AEdge : public gc {
 public:
  int id;
  EntrySet *from, *to;
  PNode *pnode;
  Map<MPosition *, AVar *> args;
  Map<MPosition *, AVar *> filtered_args;
  Map<MPosition *, AType *> initial_types;
  Vec<AVar *> rets;
  Fun *fun;
  Match *match;
  uint in_edge_worklist : 1;
  uint es_backedge : 1;
  uint es_cs_backedge : 1;
  LINK(AEdge, edge_worklist_link);

  AEdge();
};

class AEdgeHashFns {
 public:
  static uintptr_t hash(AEdge *e) { return e ? e->id : 0; }
  static int equal(AEdge *a, AEdge *b) { return a == b; }
};

typedef BlockHash<AEdge *, AEdgeHashFns> EdgeHash;

class PendingMapHash {
 public:
  // Issue 035: hash the key CONTENT by ids, not raw pointers, so
  // bucket layout (and any iteration over the pending map) is
  // deterministic across runs.
  static uint hash(AEdge *e);
  static int equal(AEdge *a, AEdge *b) { return (a->fun == b->fun) && (a->pnode == b->pnode) && (a->from == b->from); }
};

typedef HashMap<AEdge *, PendingMapHash, Vec<EntrySet *> *> PendingAEdgeEntrySetsMap;
typedef MapElem<AEdge *, Vec<EntrySet *> *> MapElemAEdgeEntrySets;

enum { DFS_white = 0, DFS_grey, DFS_black };

class EntrySet : public gc {
 public:
  Fun *fun;
  int id;
  uint dfs_color : 2;
  uint in_es_worklist : 1;
  Map<MPosition *, AVar *> args;
  Vec<AVar *> rets;
  Map<MPosition *, AType *> filters;
  EdgeHash edges;
  EdgeMap out_edge_map;
  Vec<CreationSet *> creates;
  Vec<EntrySet *> display;
  Vec<AEdge *> out_edges;
  Vec<AEdge *> backedges;
  Vec<AEdge *> es_cs_backedges;
  Vec<CreationSet *> cs_backedges;
  Vec<PNode *> live_pnodes;
  EntrySet *split;
  PendingAEdgeEntrySetsMap pending_es_backedge_map;
  Vec<EntrySet *> *equiv;  // clone.cpp

  // Escape annotation per positional formal arg.  Populated
  // by IFA's escape transfer (Phase 2+).  In Phase 1 the
  // vector stays empty; codegen reads from `Fun::arg_escapes`
  // / `cg_normalize_v2`'s post-IFA pass instead.  When
  // populated it parameterises the EntrySet's spec key, so
  // calls with divergent escape settings clone (Phase 4).
  Vec<uint8_t> arg_escapes;

  LINK(EntrySet, es_worklist_link);

  EntrySet(Fun *af);
};

class CreationSet : public gc {
 public:
  Sym *sym;
  int id;
  uint dfs_color : 2;
  uint clone_for_constants : 1;
  uint added_element_var : 1;
  uint closure_used : 1;
  uint tuple_able : 1;
  Vec<AVar *> defs;
  AType *atype;  // the type that this creation set belongs to
  Vec<AVar *> vars;
  Map<cchar *, AVar *> var_map;
  Vec<EntrySet *> ess;           // entry sets restricted by this creation set
  Vec<EntrySet *> es_backedges;  // entry sets restricted by this creation set
  CreationSet *split;            // creation set this one was split from
  Vec<CreationSet *> *equiv;     // used by clone.cpp & fa.cpp
  Vec<CreationSet *> not_equiv;  // used by clone.cpp
  Sym *type;                     // used by clone.cpp & fa.capp
  Vec<cchar *> unknown_vars;

  CreationSet(Sym *s);
  CreationSet(CreationSet *cs);
};

class Setters : public Vec<AVar *> {
 public:
  uint hash;
  Vec<AVar *> sorted;
  Map<AVar *, Setters *> add_map;

  Setters() : hash(0) {}
};

typedef MapElem<void *, int> MarkElem;
typedef Map<void *, int> MarkMap;
typedef Map<Sym *, CreationSet *> CSMap;
typedef MapElem<Sym *, CreationSet *> CSMapElem;

// Escape integration into IFA — two-point monotonic lattice
// per AVar.  Bottom = NoEscape (the value provably stays
// within its function's frame); Top = Escape (default
// conservative).  See ESCAPE_PLAN.md for the lattice
// definition, transfer functions, and the cloning policy
// that consumes this annotation.
//
// Phase 1: enum defined; AVar carries the field; everyone
// is initialized to Escape; analysis is not yet wired.
enum EscapeStatus {
  ES_NoEscape = 0,
  ES_Escape   = 1,
};

inline EscapeStatus join_escape(EscapeStatus a, EscapeStatus b) {
  return (a == ES_Escape || b == ES_Escape) ? ES_Escape : ES_NoEscape;
}

// Predicate-based restrict kinds for AVar narrowing.
// Issue 026 Bug 5: type-level narrowing predicates that
// re-evaluate as new CSs arrive at v->in.  Replaces the
// CS-snapshot-in-restrict pattern that filtered out
// later-arriving CSs (`is None` narrowing of recursive-
// receiver types in particular).  See
// `ifa/issues/026-recursive-self-mutation-struct-collapse.md`.
enum AVarRestrictPred {
  RP_None = 0,
  RP_IsNilType,      // keep cs if cs->sym->type == sym_nil_type
  RP_IsNotNilType,   // keep cs if cs->sym->type != sym_nil_type
  RP_IsInstanceOf,   // keep cs if restrict_pred_cls->meta_type->implementors.in(cs->sym->type)
  RP_NotInstanceOf,  // keep cs if !restrict_pred_cls->meta_type->implementors.in(cs->sym->type)
};

class AVar : public gc {
 public:
  Var *var;
  int id;
  void *contour;
  Vec<AVar *> forward;
  Vec<AVar *> backward;
  AVar *lvalue;
  AType *gen;
  AType *in;
  AType *out;
  AType *restrict;
  AVarRestrictPred restrict_pred;
  Sym *restrict_pred_cls;
  AVar *container;
  Setters *setters;
  Setters *setter_class;
  MarkMap *mark_map;
  CSMap *cs_map;
  MatchCache *match_cache;
  Sym *type;
  // Numeric-confluence coercion target (issue 025 numeric
  // unification). Set BETWEEN passes (PycCompiler::reanalyze ->
  // fa_coerce_numeric_confluences) on an AVar whose converged `out`
  // mixed a numeric constant with a wider numeric type; during
  // subsequent passes every out-computation maps numeric-constant
  // CSs to this type's constants (0 -> 0.0). Per-AVar == per
  // (Var, contour), so the coercion is flow- and contour-sensitive:
  // an int-only specialization of the same Var keeps int. Persists
  // across clear_avar (like cs_map) by design -- it must hold from
  // the first instant of the next pass so the transform is
  // element-wise monotone (nothing a consumer saw is ever
  // retracted mid-pass).
  Sym *num_coerce;
  int ivar_offset;
  uint in_send_worklist : 1;
  uint contour_is_entry_set : 1;
  uint is_lvalue : 1;
  uint live : 1;
  uint live_arg : 1;
  uint is_if_arg : 1;
  // Escape status (Phase 1+: see ESCAPE_PLAN.md).  Stored as
  // uint:1 to fit alongside the existing bit-fields.
  uint escape : 1;
  // Issue 029 step 1: marks an AVar that participates in a
  // polymorphic confluence — i.e. its `out` contains CSs
  // from multiple distinct non-nil metatypes, OR it
  // backward-flows into such a confluence.  Set by
  // `mark_fat_avars()` after FA convergence; consumed by
  // codegen (in a later step) to choose a fat-pointer
  // representation `{tag, ptr}` for the corresponding Var.
  uint needs_fat : 1;
  Accum<AVar *> arg_of_send;
  LINK(AVar, send_worklist_link);

  AVar(Var *v, void *acontour);
};

// Content-based hashing for `Vec<T*>::set_add` / `set_in` over
// the four id-bearing FA pointer types. Using `c->id` instead of
// `(uintptr_t)c` makes set iteration order and table capacity
// deterministic across runs (see ifa/notes/004).
template <> struct PointerHash<AVar *> {
  static uintptr_t hash(AVar *c) { return c ? (uintptr_t)c->id : 0; }
};
template <> struct PointerHash<AEdge *> {
  static uintptr_t hash(AEdge *c) { return c ? (uintptr_t)c->id : 0; }
};
template <> struct PointerHash<EntrySet *> {
  static uintptr_t hash(EntrySet *c) { return c ? (uintptr_t)c->id : 0; }
};
template <> struct PointerHash<CreationSet *> {
  static uintptr_t hash(CreationSet *c) { return c ? (uintptr_t)c->id : 0; }
};

typedef Map<MPosition *, AVar *> MapMPositionAVar;
typedef MapElem<MPosition *, AVar *> MapMPositionAVarElem;
#define form_MPositionAVar(_p, _v) form_Map(MapMPositionAVarElem, _p, _v)

typedef MapElem<MPosition *, AType *> MapMPositionATypeElem;
#define form_MPositionAType(_p, _v) form_Map(MapMPositionATypeElem, _p, _v)

class ATypeChainHashFns {
 public:
  static uint hash(AType *a) { return a->hash; }
  static int equal(AType *a, AType *b) {
    if (a->sorted.n != b->sorted.n) return 0;
    for (int i = 0; i < a->sorted.n; i++)
      if (a->sorted[i] != b->sorted[i]) return 0;
    return 1;
  }
};

class SettersHashFns {
 public:
  static uint hash(Setters *a) { return a->hash; }
  static int equal(Setters *a, Setters *b) {
    if (a->sorted.n != b->sorted.n) return 0;
    for (int i = 0; i < a->sorted.n; i++)
      if (a->sorted[i] != b->sorted[i]) return 0;
    return 1;
  }
};

enum class ATypeViolation_kind {
  PRIMITIVE_ARGUMENT,
  SEND_ARGUMENT,
  DISPATCH_AMBIGUITY,
  MEMBER,
  MATCH,
  NOTYPE,
  BOXING,
  CLOSURE_RECURSION
};

class ATypeViolation : public gc {
 public:
  ATypeViolation_kind kind;
  AVar *av;
  AVar *send;
  AType *type;
  Vec<Fun *> *funs;

  ATypeViolation(ATypeViolation_kind akind, AVar *aav, AVar *asend)
      : kind(akind), av(aav), send(asend), type(nullptr), funs(nullptr) {}
};

class ATypeViolationHashFuns {
 public:
  static uint hash(ATypeViolation *x) {
    return (uint)x->kind + combine_hash((uintptr_t)x->av, (uintptr_t)x->send);
  }
  static int equal(ATypeViolation *x, ATypeViolation *y) {
    return x->kind == y->kind && x->av == y->av && x->send == y->send;
  }
};

class ATypeFold : public gc {
 public:
  Prim *p;
  AType *a;
  AType *b;
  AType *result;

  ATypeFold(Prim *ap, AType *aa, AType *ab, AType *aresult = nullptr) : p(ap), a(aa), b(ab), result(aresult) {}
};

class ATypeFoldChainHashFns {
 public:
  static uint hash(ATypeFold *x) {
    return (uint)(uintptr_t)x->p + combine_hash((uintptr_t)x->a, (uintptr_t)x->b);
  }
  static int equal(ATypeFold *x, ATypeFold *y) { return x->p == y->p && x->a == y->a && x->b == y->b; }
};

// Issue 033: persistent record of split decisions, so re-running
// the splitter over a re-derived flow state can recognize a split
// it already made in an earlier pass (stage A: record + count
// duplicates only; enforcement lands in later stages). Key
// components are all interned/stable across passes (issue 033 D0):
// the function, the FAPassStage that made the split, the argument
// position driving it, and the canonical AType partition assigned
// to the product contour. Never cleared between passes — only a
// fresh FA starts empty.
struct SplitDecision : public gc {
  Fun *fun = nullptr;
  int stage = 0;                // FAPassStage of the split site
  MPosition *pos = nullptr;     // confluence/dispatch position (interned)
  AType *partition = nullptr;   // canonical (cannonical_atypes) filter type
  int pass_made = 0;            // analysis_pass at record time (diagnostics)
  EntrySet *product = nullptr;  // ES created/selected (nullptr for CS splits)
};

class SplitDecisionHashFns {
 public:
  static uintptr_t hash(SplitDecision *d) {
    return (uintptr_t)d->fun + combine_hash((uintptr_t)d->pos, (uintptr_t)d->partition) +
           combine_hash((uintptr_t)d->stage, (uintptr_t)d->stage);
  }
  static int equal(SplitDecision *a, SplitDecision *b) {
    return a->fun == b->fun && a->stage == b->stage && a->pos == b->pos && a->partition == b->partition;
  }
};

// Per-FA hash-cons world (tier-3 reentrancy step 2). Holds the
// canonical-AType table, the Setters hash-cons, the type-fold cache,
// and the violation-dedup table. AType identity is meaningful only
// *within* one TypeWorld: two FAs with separate TypeWorlds will hand
// out distinct canonical pointers for structurally identical types.
// Today there's one TypeWorld per FA (embedded as a member); future
// work can share a TypeWorld across multiple FAs if cross-instance
// AType identity is wanted.
class TypeWorld : public gc {
 public:
  ChainHash<AType *, ATypeChainHashFns> cannonical_atypes;
  ChainHash<Setters *, SettersHashFns> cannonical_setters;
  ChainHash<ATypeFold *, ATypeFoldChainHashFns> type_fold_cache;
  ChainHash<ATypeViolation *, ATypeViolationHashFuns> type_violation_hash;

  // Canonical AType pointers (tier-3 reentrancy step 3). Populated by
  // `initialize()` at FA::analyze entry; valid for the lifetime of
  // their owning FA. Accessed via `fa->type_world.bottom_type` etc.
  AType *bottom_type = nullptr;
  AType *nil_type = nullptr;
  AType *unknown_type = nullptr;
  AType *void_type = nullptr;
  AType *top_type = nullptr;
  AType *any_type = nullptr;
  AType *bool_type = nullptr;
  AType *true_type = nullptr;
  AType *false_type = nullptr;
  AType *size_type = nullptr;
  AType *anyint_type = nullptr;
  AType *anynum_kind = nullptr;
  AType *symbol_type = nullptr;
  AType *string_type = nullptr;
  AType *tuple_type = nullptr;
  AType *anytype_type = nullptr;
  AType *function_type = nullptr;
};

class FA : public gc {
 public:
  PDB *pdb;
  cchar *fn;
  Patterns *patterns;
  Vec<Fun *> funs;
  AEdge *top_edge;
  Vec<EntrySet *> ess;      // all used entry sets as array
  Vec<EntrySet *> ess_set;  // all used entry sets as set
  Vec<Sym *> basic_types;
  Vec<CreationSet *> css, css_set;
  Vec<AVar *> global_avars;
  // The distinguished global contour (see GLOBAL_CONTOUR above).
  // Created once at FA::analyze entry; fun points at the top-level
  // Fun so incidental `es->fun` derefs are safe.
  EntrySet *global_es = nullptr;

  int print_call_depth;
  bool permit_boxing;
  bool no_unused_instance_variables;
  int tuple_index_base;
  int num_constants_per_variable;
  // ---- Per-instance id counters (tier-3 reentrancy step 4) ----
  // Each new AVar/AEdge/CreationSet/EntrySet gets its id from these.
  // Constructors do `id = fa->X_id++`, so the global `fa` must be
  // set (which it is at `FA::analyze` entry) before any of these
  // objects are constructed.
  int avar_id = 1;
  int aedge_id = 1;
  int creation_set_id = 1;
  int entry_set_id = 1;

  // Cap on outer convergence iterations. Frontends may raise it for
  // pathological inputs or lower it for fail-fast tests.
  int pass_limit;
  // Set to true when `pass_limit` was reached with `analyze_again`
  // still set — i.e. the splitter wanted another pass and was cut
  // off. Frontends inspect this to distinguish a converged
  // `type_violations` from a snapshot mid-iteration.
  bool pass_limit_hit;
  // Divergence guard (see IFA_STALL_LIMIT): consecutive passes the
  // splitter may run without improving on the best violation count
  // seen so far before the loop is force-terminated.
  int stall_limit;
  int best_violations;  // best (lowest) nonzero-pass violation count seen
  int stall_passes;     // consecutive passes at or above best_violations
  // Print FA's accumulated type violations to stderr at the end of
  // `FA::analyze`. Default true (production behavior); the test
  // harness sets it to false because fixtures are designed to
  // *trigger* violations and the stderr noise drowns out the test
  // results. Frontends that want to suppress can also flip this.
  bool show_violation_output = true;

  // ---- Per-instance hash-cons world (tier-3 reentrancy step 2) ----
  // See TypeWorld declaration above.
  TypeWorld type_world;

  // ---- Issue 033 split-decision ledger (stage A: record-only) ----
  // See SplitDecision above. `dup_split_attempts` counts, per pass,
  // splits whose key was already in the ledger from an earlier pass
  // — i.e. work the splitter is redoing on a re-derived flow state.
  // Reported in the -v PASS line; reset in initialize_pass.
  ChainHash<SplitDecision *, SplitDecisionHashFns> split_ledger;
  int dup_split_attempts = 0;
  SplitDecision *ledger_find(Fun *afun, int stage, MPosition *pos, AType *partition);
  SplitDecision *ledger_add(Fun *afun, int stage, MPosition *pos, AType *partition, EntrySet *product);
  // Issue 033 D6: per-Var count of stage-5 (split_for_violations)
  // split attempts. A Var whose violation drove two attempts and
  // still violates is not refinable by contour splitting; stage 5
  // excludes it thereafter instead of manufacturing contours
  // forever. Keyed on Var* (stable across passes), not AVar*.
  // Persistent across passes, like the ledger.
  Map<Var *, int> violation_split_attempts;

  // ---- Per-instance worklists / completion set / violation list ----
  // Sunk into FA from file-static globals June 2026 (tier-3
  // reentrancy step 1). See AUDIT §2.2 and CLEANUP.md.
  Que(AEdge, edge_worklist_link) edge_worklist;
  Que(AVar, send_worklist_link) send_worklist;
  Que(EntrySet, es_worklist_link) es_worklist;
  Vec<EntrySet *> entry_set_done;
  Vec<ATypeViolation *> type_violations;

  // ---- FAPassEvent sidecar (issue 003) ----
  // Test-harness sidecar; populated by `record_fa_event()` during a
  // splitter pass when `fa_events_enabled` is true. Accessed via the
  // `fa_events_*()` free functions which delegate to `pdb->fa`.
  bool fa_events_enabled;
  Vec<FAPassEvent *> fa_events_storage;

  // ---- Issue 033 M0: per-stage measurement (S5 prereq) ----
  // Cumulative wall-clock time (seconds) spent in each of
  // extend_analysis's stages (collect_* + split_* combined), and a
  // count of how many passes that stage was the one to report
  // progress (first-stage-wins truncation point). Always collected
  // (cheap: array increments + a timer lap per stage boundary);
  // printed under -v at convergence. Distinguishes which stage
  // dominates plateau-pass cost and how much of the plateau is
  // first-stage-wins truncation -- a stage other than the winning
  // one may have found work too, on a batched extend (see issue 033
  // S5 M2). Sized to FAPassStage's cardinality (kept as a plain
  // constant since FAPassStage is declared after this class).
  static constexpr int kNumFAPassStages = 7;
  double stage_time[kNumFAPassStages] = {};
  long stage_progress_count[kNumFAPassStages] = {};

  FA(PDB *apdb)
      : pdb(apdb),
        patterns(0),
        top_edge(0),
        print_call_depth(2),
        permit_boxing(0),
        no_unused_instance_variables(0),
        tuple_index_base(0),
        num_constants_per_variable(1),
        pass_limit(IFA_PASS_LIMIT),
        pass_limit_hit(false),
        stall_limit(IFA_STALL_LIMIT),
        best_violations(INT_MAX),
        stall_passes(0),
        fa_events_enabled(false) {}

  int analyze(Fun *f);
  int concretize();

  RegisteredPrim *register_primitive(cchar *name, PrimitiveTransferFunctionPtr ptr);
};

// Clear all module-level analysis state. Called by ifa_reset().
void fa_reset();

// ---------------------------------------------------------------------------
// FA-pass-event sidecar (issue 003)
// ---------------------------------------------------------------------------
// Records which splitter stage fired on which pass, plus before/after
// counts of ess / css / type_violations. Mirrors the InlineEvent
// pattern in ifa/optimize/inline.h — events are only recorded when
// fa_events_enable() has been called; production pays nothing.

enum class FAPassStage {
  TYPE_CONFLUENCE,        // split_ess_for_type
  MARK_TYPE,              // split_ess_for_mark_type
  SETTER,                 // split_for_setters (type-based)
  SETTER_OF_SETTER,       // split_for_setters_of_setters (type-based)
  MARK_SETTER,            // split_for_setters with marks
  MARK_SETTER_OF_SETTER,  // split_for_setters_of_setters with marks
  VIOLATION,              // split_for_violations
};

struct FAPassEvent {
  int pass;                  // 1-indexed analysis_pass at time of record
  FAPassStage stage;
  int splits;                // value returned by the split_* function
  int ess_before, ess_after;
  int css_before, css_after;
  int violations_before, violations_after;
};

void fa_events_enable();
void fa_events_disable();
void fa_events_reset();
const Vec<FAPassEvent *> &fa_events_get();

AVar *make_AVar(Var *, EntrySet *);
AVar *get_element_avar(CreationSet *);
Sym *coerce_num(Sym *, Sym *);
Sym *type_info(IFAAST *a, Sym *s = 0);
void call_info(Fun *f, IFAAST *a, Vec<Fun *> &funs);
int constant_info(IFAAST *a, Vec<Sym *> &constants, Sym *s);
int constant_info(Var *v, Vec<Sym *> &constants);
Sym *get_constant(Var *v);
Sym *get_constant(AVar *av);
int symbol_info(Var *v, Vec<Sym *> &symbols);
AType *make_AType(CreationSet *cs);
AType *make_AType(Vec<CreationSet *> &css);
AType *make_abstract_type(Sym *s);
void fill_tvals(Fun *fn, PNode *p, int n);
void update_gen(AVar *v, AType *t);
void update_in(AVar *v, AType *t);
void flow_vars(AVar *v, AVar *vv);
void flow_var_type_permit(AVar *v, AType *t);
// Issue 025 numeric unification: annotate AVars whose converged
// type mixes a numeric constant with a wider numeric (BOXING
// violations) so the next pass coerces the constant (0 -> 0.0)
// at that flow point. Returns the number of newly annotated AVars
// and, when nonzero, resets per-pass analysis state
// (clear_results) so the re-run re-derives flow with the
// annotations in force. Call from IFACallbacks::reanalyze.
int fa_coerce_numeric_confluences(Vec<ATypeViolation *> &violations);
// Set / install a predicate restrict on `v`.  cls is only
// used for RP_IsInstanceOf / RP_NotInstanceOf.  Idempotent:
// re-installing the same predicate is a no-op; installing a
// different one is currently treated as a no-op (composing
// predicates isn't needed by today's narrowing sites).
void flow_var_permit_pred(AVar *v, AVarRestrictPred pred, Sym *cls = nullptr);
CreationSet *creation_point(AVar *v, Sym *s, int nvars = -1);
void prim_make_constraints(PNode *p, EntrySet *es);
void type_violation(ATypeViolation_kind akind, AVar *av, AType *type, AVar *send, Vec<Fun *> *funs = nullptr);
// Live count of unique (kind, av, send) violation triples currently
// recorded. Use this — not `type_violations.n`, which is the
// underlying open-addressed table capacity (see issue 009).
int type_violations_count();
AType *type_cannonicalize(AType *t);
AType *type_diff(AType *, AType *);
AType *type_intersection(AType *, AType *);
AType *type_union(AType *a, AType *b);
void log_var_types(Var *, Fun *);
void set_container(AVar *av, AVar *container);
AVar *unique_AVar(Var *v, void *contour);
AVar *unique_AVar(Var *v, EntrySet *es);
void qsort_pointers(void **left, void **right);
void initialize_Sym_for_fa(Sym *s);
int function_dispatch(PNode *p, EntrySet *es, AVar *a0, CreationSet *s, Vec<AVar *> &args, Vec<char *> &names,
                      int is_closure, Partial_kind partial, PNode *visibility_point = 0);
void add_var_constraint(AVar *av, Sym *s = 0);
void return_nil_transfer_function(PNode *pn, EntrySet *es);
void return_int_transfer_function(PNode *pn, EntrySet *es);
void return_string_transfer_function(PNode *pn, EntrySet *es);
AType *make_size_constant_type(int n);
void collect_types_and_globals(FA *fa, Vec<Sym *> &typesyms, Vec<Var *> &globalsyms);
void fa_dump_types(FA *fa, FILE *fp);

template <class C>
void qsort_by_id(C **left, C **right) {
Lagain:
  if (right - left < 5) {
    for (C **y = right - 1; y > left; y--) {
      for (C **x = left; x < y; x++) {
        if (x[0]->id > x[1]->id) {
          C *t = x[0];
          x[0] = x[1];
          x[1] = t;
        }
      }
    }
  } else {
    C **i = left + 1, **j = right - 1;
    C *x = *left;
    for (;;) {
      while (x->id < (*j)->id) j--;
      while (i < j && (*i)->id < x->id) i++;
      if (i >= j) break;
      C *t = *i;
      *i = *j;
      *j = t;
      i++;
      j--;
    }
    if (j == right - 1) {
      *left = *(right - 1);
      *(right - 1) = x;
      right--;
      goto Lagain;
    }
    if (left < j) qsort_by_id(left, j + 1);
    if (j + 2 < right) qsort_by_id(j + 1, right);
  }
}

template <class C>
void qsort_by_id(Vec<C *> &v) {
  if (v.n > 1) qsort_by_id(&v[0], v.end());
}

// Return a sorted-by-id snapshot of `v` (live entries only,
// skipping null hash-table holes). Leaves `v` untouched. Use
// when iterating a `set_add`-populated `Vec<C*>` and the loop
// body would observe order — preferred over the in-place
// `qsort_by_id(v); for(...) ...` pattern when `v` should not be
// mutated. See ifa/notes/004.
template <class C>
Vec<C *> sorted_view(const Vec<C *> &v) {
  Vec<C *> out;
  for (int i = 0; i < v.n; i++)
    if (v.v[i]) out.add(v.v[i]);
  if (out.n > 1) qsort_by_id(out);
  return out;
}

extern FA *fa;

// Canonical AType pointers (bottom_type, void_type, etc.) are now
// members of TypeWorld owned by FA. Access via
// `fa->type_world.bottom_type` (the global `fa` is set at
// `FA::analyze` entry). Tier-3 reentrancy step 3.

extern int analysis_pass;

void pp(AVar *);
void pp(AType *);
void pp(CreationSet *);

#endif
