// SPDX-License-Identifier: BSD-3-Clause
//
// cg_normalize_v2.cc — Phase B of the CG_IR_v2 migration.
//
// Translates IF1 (Sym/Var/Fun/PNode/Code) into a CGv2Program.
// This is the input side of the v2 LLVM emitter — once this
// produces a program with full fidelity to v1's cg_normalize,
// Phase B's pass criterion is met and the pyc-suite can run
// behind IFA_LLVM_V2=1.
//
// Landing cadence: one IF1 concept per commit, with a test
// per landing. Same pattern as the prim layer in Phase A.
//
// Current scope (Phase B.1):
//   - entry point cg_normalize_v2(FA*)
//   - no-op contract: empty FA → empty CGv2Program (just the
//     predefined types from the program constructor)
//   - per-Fun declarations (no bodies yet) — Phase B.2 lands
//     the first body op

#include "ifadefs.h"

#include "codegen/cg_normalize_v2.h"

#include "codegen/cg_ir_v2.h"
#include "fa.h"
#include "fun.h"
#include "sym.h"

CGv2Program *cg_normalize_v2(FA *fa) {
  CGv2Program *p = new CGv2Program();
  if (!fa) return p;

  // Phase B.2+ will lift the type/fun walks in here. For now
  // this is just the empty-contract entry point so the v2
  // emit pipeline has a callable source. Each subsequent
  // landing adds one IF1 → CGv2 mapping.

  return p;
}
