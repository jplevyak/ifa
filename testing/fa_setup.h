// SPDX-License-Identifier: BSD-3-Clause
// Helpers that mirror what the V/Python frontend's build_environment +
// build_init do, so the test harness can run FA::analyze without
// linking the frontend. See ifa/testing/phases/05_fa_analyze.md §7.
#pragma once

class IF1;
class Sym;

// One-shot environment setup for fa-level testing:
//   - init_default_builtin_types() — populate sym_any, sym_bool, etc.
//   - synthesize sym___main__ as a closure whose body is
//       [user_entry->code] (from the .ir's `(entry %x)`)
//       (send primitive reply cont ret)
//     so initialize_Sym_for_fa gives sym___main__ an abstract_type
//     and FA actually traverses the fixture's code.
//   - finalize_types + build_type_hierarchy + if1_finalize sub-steps
//     in the order the pyc frontend uses.
//
// After this returns, pdb->fa->analyze(sym___main__->fun) is safe to
// call as long as Fun(sym___main__, FUN_BUILD_ALL) has been built and
// added to pdb.
void fa_setup_environment(IF1 *p);

// If the .ir fixture supplied an `(entry %x)` form (and that closure
// wasn't already sym___main__), its Sym is stored here after
// fa_setup_environment() runs. The Sym's Code tree is now a sub-tree
// of sym___main__->code, so callers MUST NOT build a separate Fun for
// it — one Code tree can't host two CFGs.
extern Sym *fa_setup_user_entry;
