// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for `build_arg_positions`. See
// ifa/testing/phases/04_patterns.md §3.1.
//
// For each closure: builds a CFG-only Fun, ensures arg/ret Vars exist
// (build_arg_positions accesses `sym->var` directly), then runs
// build_arg_positions(f). Prints the resulting positional + named arg
// maps.
#pragma once

#include <stdio.h>
#include "common.h"

class IF1;

void print_argpos_normalized(FILE *fp, IF1 *p);
