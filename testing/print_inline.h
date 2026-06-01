// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for simple_inlining. Enables the InlineEvent sidecar
// in optimize/inline.cc, runs the full ifa pipeline, then dumps the
// recorded events for golden compare.
//
// See ifa/testing/phases/07_dce_optimize.md §3.3.
#pragma once

#include <stdio.h>
#include "common.h"

class IF1;

void print_inline_normalized(FILE *fp, IF1 *p);
