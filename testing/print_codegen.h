// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for the C backend (c_codegen_print_c).
//
// Runs the full ifa_analyze + ifa_optimize pipeline (clone, dom,
// dce, freq, inline, type liveness) and then captures
// `c_codegen_print_c`'s output for golden compare.
//
// Output is the .c file verbatim with the runtime-include prologue
// stripped. The text the backend produces is already deterministic
// (Sym ids reset per fixture by ifa_reset), so further normalisation
// is left for future work.
#pragma once

#include <stdio.h>
#include "common.h"

class IF1;

void print_codegen_c_normalized(FILE *fp, IF1 *p);

// Phase printer for the LLVM backend (llvm_codegen_print_ir).
// Mirrors the C-side pipeline; captures `llvm_codegen_print_ir`'s
// textual LLVM IR output and applies a line-by-line normalizer
// that strips host-specific module-level lines (target triple /
// datalayout, named metadata, debug-info metadata) so the golden
// can survive across hosts and LLVM versions. See issue 002.
void print_codegen_llvm_normalized(FILE *fp, IF1 *p);

// Phase printer for the CG_IR normalization pass (cg_normalize).
// Runs the same FA + clone + DCE pipeline as the codegen-c /
// codegen-llvm phases, then invokes `cg_normalize(fa)` and dumps
// the resulting CGProgram in a stable textual form: types, globals,
// per-Fun (name, sig, blocks with body insts + terminator).
//
// This phase locks the CGProgram shape during Phase 2 of
// CG_IR_PLAN. Phase 3 (LLVM backend) and Phase 4 (C backend)
// consume CGProgram instead of IF1; the goldens here document the
// intermediate representation each backend will read from.
void print_cg_normalize_normalized(FILE *fp, IF1 *p);
