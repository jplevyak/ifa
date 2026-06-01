// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for the SSU pass. See ifa/testing/phases/02_cfg_ssu.md.
//
// Builds each closure with FUN_BUILD_CFG_SSU and prints:
//   - per-PNode phi/phy lists (with rvals/lvals)
//   - the rename map (Sym → list of distinct Vars)
//   - per-PNode live_vars set
#pragma once

#include <stdio.h>
#include "common.h"

class IF1;

void print_ssu_normalized(FILE *fp, IF1 *p);
