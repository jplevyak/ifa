// SPDX-License-Identifier: BSD-3-Clause
//
// Layer 3 of the IR builder: whole-program shape generators used
// as synthetic test fixtures. See
// ifa/testing/phases/09d_generator_design.md §2.
//
// Each shape function populates the global `if1` with a complete
// program (closures, types, sym___main__-rooted entry) ready to
// hand off to the phase's `run(if1)` step. Phase printers don't
// distinguish synthetic from `.ir`-loaded fixtures.

#ifndef IFA_TESTING_IR_SHAPES_H
#define IFA_TESTING_IR_SHAPES_H

#include "ifadefs.h"

#include <unordered_map>
#include <string>

namespace IRShape {

using ParamMap = std::unordered_map<std::string, int>;
using ShapeFn = void (*)(const ParamMap &);

// Look up a registered shape by name; returns null if unknown.
ShapeFn lookup(cchar *name);

// Read a param from the map with a default if missing.
int param(const ParamMap &m, cchar *name, int fallback = 0);

// ---------------------------------------------------------------------------
// Registered shapes
// ---------------------------------------------------------------------------
//
// Each shape takes a ParamMap (parsed from the .synth fixture) and
// populates if1. Set if1->top to the user-entry closure; the test
// harness's fa_setup_environment splices it into __main__.

// noop_main: a single closure that does nothing but reply. Used to
// validate the runner end-to-end without exercising any FA paths.
// No parameters.
void noop_main(const ParamMap &);

}  // namespace IRShape

#endif  // IFA_TESTING_IR_SHAPES_H
