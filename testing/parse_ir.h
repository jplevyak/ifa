// SPDX-License-Identifier: BSD-3-Clause
// Parser for the .ir text format. See ifa/testing/IF1_TEXT_FORMAT.md.
//
// Populates the current `if1` singleton. The caller is responsible for
// having already called `ifa_init(callbacks)` and registered enough
// builtin types for the file's `@...` references to resolve.
#pragma once
#include "common.h"

class Sym;

// Parse a .ir file. Returns 0 on success, -1 on errors (printed to stderr).
int parse_ir_file(cchar *filename);

// Parse a .ir string. `fake_filename` is used in error messages.
int parse_ir_string(cchar *source, cchar *fake_filename);

// Look up a `%name` defined in this parse session. NULL if not found.
Sym *parse_ir_lookup(cchar *name);

// Reset the parser's user-sym table. Call between parses if you reuse
// `if1`. (`ifa_reset()` if/when implemented does this for you.)
void parse_ir_reset();
