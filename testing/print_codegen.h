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

// `print_codegen_llvm_normalized` and `print_cg_normalize_normalized`
// retired alongside the v1 LLVM backend (issue 014).  v2 LLVM
// goes through `cg_normalize_v2` + `cg_v2_emit_llvm_module`,
// covered by ifa/testing/cg_ir_v2_test.cc.
