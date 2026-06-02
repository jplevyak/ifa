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

void polymorphic_formal(const ParamMap &m) {
  int n_types = param(m, "n_types", 2);
  int n_per_type = param(m, "n_per_type", 1);
  if (n_types < 1) n_types = 1;
  if (n_types > 3) n_types = 3;  // const_* helpers cover int32/float64/int64
  if (n_per_type < 1) n_per_type = 1;

  // Build `f(a, b) { return a }` — a trivial polymorphic identity.
  Sym *a = ir::local("a");
  Sym *b = ir::local("b");
  Sym *f = ClosureBuilder("f")
      .arg(a).arg(b)
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        cb.move(a, ret);
        cb.reply(cont, ret);
      });

  // Build the entry. For each of n_types × n_per_type calls,
  // generate a SEND to f with constants of the chosen type. The
  // call shape matches 02_splitter.ir: (send <fn> arg1 arg2 => result)
  // — the closure Sym itself is the dispatched target, no separate
  // method-symbol indirection. f's body returns the first arg, so
  // FA sees a type confluence on the formal and the type-stage
  // splitter must fork f's ES.
  Sym *top = ClosureBuilder("top")
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        for (int t = 0; t < n_types; t++) {
          for (int k = 0; k < n_per_type; k++) {
            Sym *x = nullptr, *y = nullptr;
            switch (t) {
              case 0: x = ir::const_int32(1 + k); y = ir::const_int32(2 + k); break;
              case 1: x = ir::const_float64(1.5 + k); y = ir::const_float64(2.5 + k); break;
              case 2: x = ir::const_int64(10 + k); y = ir::const_int64(20 + k); break;
            }
            // Pass `f` (the closure Sym) as both "method" and the
            // builder's recv-slot positions it as the first arg,
            // yielding (send f x y => result).
            cb.send_method(f, x, {y});
          }
        }
        cb.reply(cont, ret);
      });
  if1->top = top;
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
    {"polymorphic_formal", polymorphic_formal},
    {nullptr, nullptr},
};

ShapeFn lookup(cchar *name) {
  for (Entry *e = kRegistry; e->name; e++)
    if (!strcmp(e->name, name)) return e->fn;
  return nullptr;
}

}  // namespace IRShape
