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
