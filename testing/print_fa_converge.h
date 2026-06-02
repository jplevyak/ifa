// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for FA convergence — pass counts + per-pass splitter
// stage history. Uses the FAPassEvent sidecar (ifa/analysis/fa.h).

#ifndef PRINT_FA_CONVERGE_H
#define PRINT_FA_CONVERGE_H

#include <stdio.h>

class IF1;
void print_fa_converge_normalized(FILE *fp, IF1 *p);

#endif
