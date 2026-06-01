// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for dominator computation. See
// ifa/testing/phases/03_dom_loops.md §3.1.
//
// Builds each closure with FUN_BUILD_CFG_ONLY, runs
// build_cfg_dominators(), then prints idom, idom-rev (post-dominator),
// and the dominance frontier per PNode.
#pragma once

#include <stdio.h>
#include "common.h"

class IF1;

void print_dom_normalized(FILE *fp, IF1 *p);
