// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for FA dispatch — for each EntrySet, walk its
// outgoing AEdges and report what each SEND PNode dispatched to.
// Effectively a "call-graph view of pattern_match results".
#pragma once

#include <stdio.h>
#include "common.h"

class IF1;

void print_dispatch_normalized(FILE *fp, IF1 *p);
