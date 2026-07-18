#ifndef _dead_h_
#define _dead_h_

class Var;
class Sym;

int mark_live_code(FA *fa);
void mark_live_types(FA *fa);
void mark_live_funs(FA *fa);

// -------------------------------------------------------------
// Constant-condition dead-branch elimination primitives
// -------------------------------------------------------------
//
// Both backends' Code_IF termination code (cg.cc's write_c_pnode,
// cg_emit_llvm.cc's emit_block_terminator/discover_blocks) already
// special-cases a branch condition Var whose ->sym is EXACTLY FA's
// own canonical `type_world.true_type`/`false_type` constant Sym:
// only the live successor is emitted (or, for LLVM, even allocated a
// BasicBlock) -- the untaken arm is never visited by either backend's
// entry-driven CFG walk. FA's own constant folding (e.g. an
// isinstance() call it can resolve to a single CreationSet) already
// produces this identity naturally for ordinary user code; these two
// functions let any OTHER post-FA pass (today:
// ifa/optimize/exc_check_fold.cc) feed the SAME mechanism with a fact
// FA itself can't derive (e.g. Fun::can_raise's call-graph-precise
// knowledge), instead of each such pass inventing its own bespoke
// codegen-time special case.

// Marks `v` as holding the compile-time constant `constant_sym` for
// codegen's constant-condition dead-branch elision (see above). Safe
// ONLY when the caller has established `v` is effectively single-use
// for this purpose -- e.g. the fresh, single-consumer result Var of
// one specific SEND at one specific call site/contour (as
// exc_check_fold.cc's candidates are, by construction of
// emit_exc_check). This is a raw `Var::sym` write, not a new
// analysis: it does not re-verify the precondition.
void mark_var_constant(Var *v, Sym *constant_sym);

// After `mark_var_constant(v, ...)`, `v`'s producer PNode is
// typically skipped at emission time by codegen's OWN existing
// constant-fold check (`virtual_cg_is_const_folded_send`) -- but
// `mark_live_code`'s liveness (computed earlier, before the fold) is
// now stale for that producer's OWN inputs, which may have become
// genuinely unread. NOT a full `mark_live_code()` re-run (whole-
// program, expensive) -- a small, targeted backward walk from `v`'s
// producer's rvals: for each one, if this producer was its ONLY use
// (`Var::uses.n <= 1`), clear that input's own producer's `->live`
// bit and recurse into ITS rvals the same way. Stops at a Var with
// no local producer (formal argument / global read) or one still
// read by something else. Also clears `v`'s own producer's `->live`
// bit directly, for consistency with anything besides codegen that
// might consult it (harmless either way for codegen itself, which
// already skips it via the constant-fold check independently of
// `->live`).
void reclaim_dead_producer_chain(Var *v);

#endif
