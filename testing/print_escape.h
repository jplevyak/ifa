// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for the in-FA escape analysis (Phases 2-4 of
// ESCAPE_PLAN.md).  Forces `ifa_escape_in_fa = 1`, runs
// ifa_analyze (which calls compute_escape pre-clone), then
// dumps a normalized per-function escape signature suitable
// for golden-file diffing.
//
// See ifa/testing/phases/00_INDEX.md.
#pragma once

#include <stdio.h>
#include "common.h"

class IF1;

void print_escape_normalized(FILE *fp, IF1 *p);
