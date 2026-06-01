// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for the CFG pass. See ifa/testing/phases/02_cfg_ssu.md.
//
// Builds a CFG-only Fun for every closure (using FUN_BUILD_CFG_ONLY so
// SSU phi/phy nodes don't pollute the diff), then prints a stable
// per-PNode listing.
#pragma once

#include <stdio.h>
#include "common.h"

class IF1;

void print_cfg_normalized(FILE *fp, IF1 *p);
