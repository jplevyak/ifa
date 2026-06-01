// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for `build_patterns`. See
// ifa/testing/phases/04_patterns.md §3.2.
//
// Builds a stub FA, runs build_arg_positions(fa) + build_patterns(fa),
// then prints the MType reverse-dispatch index — per dispatch-type Sym,
// the MPositions and the Funs registered at each position.
#pragma once

#include <stdio.h>
#include "common.h"

class IF1;

void print_patterns_normalized(FILE *fp, IF1 *p);
