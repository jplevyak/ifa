// SPDX-License-Identifier: BSD-3-Clause
// FA test-harness environment setup. See fa_setup.h.

#include "ifadefs.h"

#include "ast.h"
#include "code.h"
#include "if1.h"
#include "sym.h"
#include "testing/fa_setup.h"

// Build sym___main__ as a stub top closure: takes itself as arg, body
// is a single `(send primitive reply cont ret)` that replies the
// (nil) return value to its continuation. Mirrors build_init() in
// python_ifa_main.cc:34.
//
// Why: make_top_edge() unconditionally does
//   AVar *av = make_AVar(sym___main__->var, e->to);
//   update_gen(av, av->var->sym->abstract_type);
// and abstract_type is only populated by initialize_Sym_for_fa when
// the Sym is is_symbol/is_fun/type_kind. By turning sym___main__ into
// a closure (via if1_closure → is_fun=1) we get an abstract_type and
// FA::analyze can run.
static void build_synthetic_main() {
  if (sym___main__->code) return;  // already built (rerun protection)
  Sym *fn = sym___main__;
  fn->cont = new_Sym();
  fn->ret = sym_nil;
  Code *code = 0;
  if1_send(if1, &code, 4, 0, sym_primitive, sym_reply, fn->cont, fn->ret);
  if1_closure(if1, fn, code, 1, &fn);
}

void fa_setup_environment(IF1 *p) {
  init_default_builtin_types();
  new_builtin_global_variable(sym___main__, "__main__");
  build_synthetic_main();
  // pyc order: finalize_types runs first (it populates meta_type for
  // primitive type Syms), then build_type_hierarchy can use that.
  finalize_types(p);
  build_type_hierarchy();
  // Run if1_finalize sub-phases except set_top — set_top would replace
  // if1->top with sym___main__, which is what we want here, so include
  // it.
  if1_finalize_set_top(p);
  if1_finalize_bind_prims(p);
  if1_finalize_dce(p);
  if1_finalize_flatten_and_fixup_nesting(p);
}
