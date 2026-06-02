// SPDX-License-Identifier: BSD-3-Clause
// Layer 3 shape generators. See ir_shapes.h.

#include "ifadefs.h"

#include "if1.h"
#include "sym.h"
#include "testing/ir_builder.h"
#include "testing/ir_shapes.h"

#include <string.h>

namespace IRShape {

int param(const ParamMap &m, cchar *name, int fallback) {
  auto it = m.find(name);
  return it == m.end() ? fallback : it->second;
}

// ---------------------------------------------------------------------------
// Shapes
// ---------------------------------------------------------------------------

void noop_main(const ParamMap & /*m*/) {
  // A single closure that does nothing but reply. Verifies the
  // builder + runner round-trip without depending on FA behavior.
  Sym *fn = ClosureBuilder("top").body(
      [&](CodeBuilder &cb, Sym *cont, Sym *ret) { cb.reply(cont, ret); });
  if1->top = fn;
}

// ---------------------------------------------------------------------------
// Registry
// ---------------------------------------------------------------------------

struct Entry {
  cchar *name;
  ShapeFn fn;
};

static Entry kRegistry[] = {
    {"noop_main", noop_main},
    {nullptr, nullptr},
};

ShapeFn lookup(cchar *name) {
  for (Entry *e = kRegistry; e->name; e++)
    if (!strcmp(e->name, name)) return e->fn;
  return nullptr;
}

}  // namespace IRShape
