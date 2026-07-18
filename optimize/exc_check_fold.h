#ifndef _exc_check_fold_H_
#define _exc_check_fold_H_

class FA;

// issue 011 (per-callee can-raise gating, Tier 2 continued): rewrite
// every exception-propagation check (emit_exc_check,
// python_ifa_build_if1.cc) that Fun::can_raise proves can never fire
// so its condition Var carries FA's own canonical true_type constant
// Sym -- the SAME identity both backends' Code_IF codegen already
// special-cases for ordinary FA-folded conditions (cg.cc's
// write_c_pnode, cg_emit_llvm.cc's emit_pnode). That existing
// machinery then emits the live arm only and never even visits the
// dead (exception-dispatch) arm, so a folded check costs nothing --
// not just the check's own boolean, but the whole dead handler chain
// it used to guard. Must run after compute_fun_can_raise()
// (python_ifa_main.cc) so Fun::can_raise is final.
void mark_exc_checks_constant(FA *fa);

#endif
