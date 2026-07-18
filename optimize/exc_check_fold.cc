#include "exc_check_fold.h"

#include "ast.h"
#include "builtin.h"
#include "dead.h"
#include "fa.h"
#include "fun.h"
#include "if1.h"
#include "pnode.h"
#include "prim.h"
#include "sym.h"
#include "var.h"

#include <cstring>

// issue 011 (per-callee can-raise gating, Tier 2): true iff `operand`
// is the local temp an exception-check's `isinstance(t, nil_type)`
// reads (i.e. `operand` is defined by a MOVE whose source is the
// `__pyc_exc__` global -- nothing else in a pyc-compiled program ever
// reads that slot) AND the call it immediately follows (found via the
// defining move's ->ast and FA's own Fun::calls, via call_info) is
// PROVEN safe: every resolved candidate Fun has can_raise == false.
// Conservative if unsure either way. Moved here from
// ifa/codegen/codegen_common.cc's cg_exc_check_provably_safe: now
// consumed pre-codegen (mark_exc_checks_constant below) instead of at
// codegen time, so neither backend needs this logic anymore.
static bool exc_check_operand_provably_safe(Var *operand, Fun *f) {
  if (!operand || !f) return false;
  PNode *def = operand->def;
  if (!def || !def->code || def->code->kind != Code_MOVE) return false;
  if (!def->rvals.n || !def->rvals[0]->sym || !def->rvals[0]->sym->name) return false;
  if (strcmp(def->rvals[0]->sym->name, "__pyc_exc__")) return false;
  if (!def->code->ast) return false;
  Vec<Fun *> funs;
  call_info(f, def->code->ast, funs);
  if (!funs.n) return false;  // unresolved (e.g. dead code) -- stay conservative
  for (Fun *callee : funs.values())
    if (!callee || callee->can_raise) return false;
  return true;
}

void mark_exc_checks_constant(FA *fa) {
  for (Fun *f : fa->funs) {
    for (PNode *p : f->fa_all_PNodes) {
      if (!p->code || p->code->kind != Code_SEND || !p->prim) continue;
      if (p->prim->index != P_prim_isinstance) continue;
      // issue 011: mirrors cg.cc's/cg_emit_llvm.cc's is_nil_check
      // detection -- the checked class arg is nil_type either
      // directly (raw prim_isinstance lowering) or via ->meta_type
      // (build_isinstance_call's monomorphic clone form).
      if (p->rvals.n < 4 || !p->lvals.n) continue;
      Sym *cls = p->rvals[3]->sym;
      if (!cls || !(cls == sym_nil_type || cls->meta_type == sym_nil_type)) continue;
      if (!exc_check_operand_provably_safe(p->rvals[2], f)) continue;
      // Same Var object is IF's rvals[0] (single-use result of this
      // SEND, per emit_exc_check's construction) -- both backends'
      // Code_IF handling already special-cases this exact Sym
      // identity for ordinary FA-folded conditions, so this alone
      // makes the dead (propagate) arm unreachable from codegen's
      // entry-driven CFG walk. No CFG edit needed.
      mark_var_constant(p->lvals[0], fa->type_world.true_type->v[0]->sym);
      // The isinstance send itself is now skipped at emission
      // (virtual_cg_is_const_folded_send), but its own inputs --
      // notably the __pyc_exc__ slot-read MOVE -- were marked live
      // by mark_live_code BEFORE this fold ran and are now stale.
      // Reclaim anything that's become genuinely unread.
      reclaim_dead_producer_chain(p->lvals[0]);
    }
  }
}
