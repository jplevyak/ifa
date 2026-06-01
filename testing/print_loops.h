// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for local loop detection. See
// ifa/testing/phases/03_dom_loops.md §3.2.
//
// For each closure: build CFG, build dominators, then call
// find_local_loops. Print the resulting loop tree (per-Fun). Recursive
// loops (call-graph) require FA + clone state and are deferred.
#pragma once

#include <stdio.h>
#include "common.h"

class IF1;

void print_loops_normalized(FILE *fp, IF1 *p);
