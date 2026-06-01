// SPDX-License-Identifier: BSD-3-Clause
// Phase printer for the post-clone state. After clone(fa) runs,
// CSes and ESes have equivalence-class pointers, concrete-type
// Syms are synthesized, and Fun::calls is rebuilt. We print:
//
//   - CS equivalence classes (with members + concrete type).
//   - ES equivalence classes per Fun.
//   - Per-Fun call graph (fun → pnode → [target Funs]).
//
// See ifa/testing/phases/06_clone.md.
#pragma once

#include <stdio.h>
#include "common.h"

class IF1;

void print_clone_normalized(FILE *fp, IF1 *p);
