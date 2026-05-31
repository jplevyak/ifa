// SPDX-License-Identifier: BSD-3-Clause
// Writer for the .ir text format. See ifa/testing/IF1_TEXT_FORMAT.md.
//
// Produces output that the parser (parse_ir.h) can re-read into an
// equivalent in-memory IF1 state. No comments are emitted (only
// `;;` section banners that the lexer skips).
#pragma once
#include <stdio.h>
#include "common.h"

class IF1;
class Sym;

// Write the entire IF1 state as a .ir file.
void write_ir(FILE *fp, IF1 *p);
