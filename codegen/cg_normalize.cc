// SPDX-License-Identifier: BSD-3-Clause
//
// cg_normalize.cc — the IF1 → CG_IR normalization pass.
//
// Phase 1 of CG_IR_PLAN: header-only scaffolding. This file provides
// the stub `cg_normalize(fa)` so cg_ir.h compiles and downstream PRs
// can start consuming the contract. Phase 2 fills in the actual
// normalization (type table + slot building, per-Fun skeleton,
// per-PNode lowering, phi/phy materialization, construction-flow
// patch).
//
// See [CG_IR_PLAN.md](CG_IR_PLAN.md) for the migration plan and
// [../CODE_GEN_IR.md](../CODE_GEN_IR.md) for the design rationale.

#include "ifadefs.h"

#include "cg_ir.h"
#include "fa.h"

CGProgram *cg_normalize(FA *fa) {
  // Phase 1 stub: return an empty CGProgram for any input. The
  // C and LLVM backends don't consume this yet — they still
  // emit from IF1 directly. Phases 2-4 of CG_IR_PLAN swap them
  // over one at a time.
  (void)fa;
  CGProgram *p = new CGProgram();
  return p;
}
