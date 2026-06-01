// SPDX-License-Identifier: BSD-3-Clause
// Normalized output for the `finalize` phase.
// See ifa/testing/phases/01_if1_finalize.md.
#pragma once
#include <stdio.h>
#include "common.h"

class IF1;

// Emit the post-if1_finalize state as a deterministic, diffable text
// dump. Caller has already run if1_finalize(p) (or the equivalent
// sub-step sequence — see if1.h).
void print_finalize_normalized(FILE *fp, IF1 *p);
