// SPDX-License-Identifier: BSD-3-Clause
// Helpers that mirror what the V/Python frontend's build_environment +
// build_init do, so the test harness can run FA::analyze without
// linking the frontend. See ifa/testing/phases/05_fa_analyze.md §7.
#pragma once

class IF1;

// One-shot environment setup for fa-level testing:
//   - init_default_builtin_types() — populate sym_any, sym_bool, etc.
//   - synthesize sym___main__ as a no-op closure (a single SEND of the
//     `primitive reply` form), so initialize_Sym_for_fa gives it an
//     abstract_type and make_top_edge has a usable Var.
//   - finalize_types + build_type_hierarchy + if1_finalize sub-steps
//     in the order the pyc frontend uses.
//
// After this returns, pdb->fa->analyze(sym___main__->fun) is safe to
// call as long as Fun(sym___main__, FUN_BUILD_ALL) has been built and
// added to pdb.
void fa_setup_environment(IF1 *p);
