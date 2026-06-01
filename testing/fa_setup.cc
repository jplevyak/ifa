// SPDX-License-Identifier: BSD-3-Clause
// FA test-harness environment setup. See fa_setup.h.

#include "ifadefs.h"

#include "ast.h"
#include "code.h"
#include "if1.h"
#include "sym.h"
#include "testing/fa_setup.h"

// Build sym___main__ as the top closure. Body layout:
//   [user_entry->code]      ; spliced from the .ir fixture's (entry %x)
//   (send primitive reply cont ret)  ; reply nil to continuation
//
// Without the splice the user code is registered as a closure but
// never executed by FA — sym___main__'s body would just be the reply.
// Splicing makes the fixture's body part of the top closure, which is
// how the V/Python frontends compose module-level code in build_init
// (python_ifa_main.cc:34).
//
// Why is sym___main__ a closure at all? make_top_edge() does:
//   AVar *av = make_AVar(sym___main__->var, e->to);
//   update_gen(av, av->var->sym->abstract_type);
// and abstract_type is only populated by initialize_Sym_for_fa when
// the Sym is is_symbol/is_fun/type_kind. if1_closure sets is_fun=1.
//
// Returns the user-entry Sym if one was spliced in (the caller will
// skip building a separate Fun for it — same Code tree can't be
// shared between two Funs' CFGs), else nullptr.
static Sym *build_synthetic_main() {
  if (sym___main__->code) return 0;  // already built (rerun protection)
  Sym *fn = sym___main__;
  fn->cont = new_Sym();
  fn->ret = sym_nil;

  // Give cont a Var up front — fa.cc:2097 derefs fun->sym->cont->var
  // on every call edge analyzed, so without this the FA crashes the
  // moment any non-top closure is dispatched.
  if (!fn->cont->var) fn->cont->var = new Var(fn->cont);

  Sym *user_entry = if1->top;
  if (user_entry == sym___main__) user_entry = 0;  // no real splice needed

  Code *code = 0;
  if (user_entry && user_entry->code) {
    // Wrap the user entry's body as the first part of sym___main__'s
    // body. Use if1_conc so the existing Code object becomes a child
    // of our new outer Code rather than being shared by reference.
    if1_conc(if1, &code, user_entry->code);
    // Sever the user_entry → code link. The Code tree is now reachable
    // only via sym___main__; if we left it set, if1_finalize_flatten
    // would iterate allclosures and try to re-flatten the same Code,
    // tripping the !c->flattened assert.
    user_entry->code = 0;
  }
  if1_send(if1, &code, 4, 0, sym_primitive, sym_reply, fn->cont, fn->ret);
  if1_closure(if1, fn, code, 1, &fn);
  return user_entry;
}

// Exposed for the printer.
Sym *fa_setup_user_entry = 0;

void fa_setup_environment(IF1 *p) {
  if (getenv("IFA_TEST_FA_DEBUG")) {
    ifa_debug = 1;
    ifa_verbose = 3;
  }
  init_default_builtin_types();
  new_builtin_global_variable(sym___main__, "__main__");
  fa_setup_user_entry = build_synthetic_main();
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
