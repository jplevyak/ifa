// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for FA::analyze (flow analysis). See
// ifa/testing/phases/05_fa_analyze.md.
//
// A minimal v1: builds Funs for every closure, registers them with
// the PDB, then runs pdb->fa->analyze(top) and prints summary state
// (entry-set count, creation-set count, AEdge count, type-violation
// count) plus a per-EntrySet line. Detailed per-AVar/AType dumps are
// deferred — even this minimal version locks in the run-twice
// determinism property the plan §7 asks for.
#pragma once

#include <stdio.h>
#include "common.h"

class IF1;

void print_fa_normalized(FILE *fp, IF1 *p);
