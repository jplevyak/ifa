// SPDX-License-Identifier: BSD-3-Clause
// Phase printers for the post-clone optimization passes.
//
// `dce` — runs mark_live_code + mark_live_types + mark_live_funs and
//         prints per-Fun live/dead PNode counts, type-liveness, and
//         live-fun summary.
// `freq` — runs frequency_estimation and prints per-Fun and per-PNode
//         execution_frequency.
//
// See ifa/testing/phases/07_dce_optimize.md.
#pragma once

#include <stdio.h>
#include "common.h"

class IF1;

void print_dce_normalized(FILE *fp, IF1 *p);
void print_freq_normalized(FILE *fp, IF1 *p);
