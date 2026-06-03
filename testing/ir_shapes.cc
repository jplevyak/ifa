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

void same_type_dispatch(const ParamMap &m) {
  int n_allocs = param(m, "n_allocs", 2);
  if (n_allocs < 2) n_allocs = 2;
  if (n_allocs > 3) n_allocs = 3;

  // RECORD T with one field `data`.
  Sym *T = RecordBuilder("T").field("data").build();

  // Reader: peek(t) → t.data. Polymorphic in the value type;
  // monomorphic in the receiver type (all T). This is the
  // dispatch site whose ES does NOT split on type stage (formal
  // is uniformly T), pushing the work to mark-type / setter.
  Sym *t_arg = ir::local("t");
  Sym *peek = ClosureBuilder("peek")
      .arg(t_arg)
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        Sym *v = ir::get_field(cb, t_arg, "data", "v");
        cb.move(v, ret);
        cb.reply(cont, ret);
      });

  // Main: allocate n_allocs instances of T, store a different-
  // typed value in each, then read each via peek.
  Sym *top = ClosureBuilder("top")
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        Sym *insts[3] = {nullptr, nullptr, nullptr};
        Sym *vals[3] = {nullptr, nullptr, nullptr};
        for (int i = 0; i < n_allocs; i++) {
          insts[i] = ir::new_instance(cb, T);
          switch (i) {
            case 0: vals[i] = ir::const_int32(100); break;
            case 1: vals[i] = ir::const_float64(2.5); break;
            case 2: vals[i] = ir::const_int64(42); break;
          }
          ir::set_field(cb, insts[i], "data", vals[i]);
        }
        for (int i = 0; i < n_allocs; i++) {
          cb.send_method(peek, insts[i], {});
        }
        cb.reply(cont, ret);
      });
  if1->top = top;
}

void missing_field_dispatch(const ParamMap & /*m*/) {
  // Two record types with DISJOINT field sets.
  Sym *A = RecordBuilder("A").field("fa").build();
  Sym *B = RecordBuilder("B").field("fb").build();

  // Reader reads field `fa` — exists on A but NOT on B. When
  // called with a B receiver, FA records a type violation.
  Sym *x_arg = ir::local("x");
  Sym *read_fa = ClosureBuilder("read_fa")
      .arg(x_arg)
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        Sym *v = ir::get_field(cb, x_arg, "fa", "v");
        cb.move(v, ret);
        cb.reply(cont, ret);
      });

  // Main: allocate an A and a B; populate fields; call read_fa
  // on each. The B call should produce a missing-field violation.
  Sym *top = ClosureBuilder("top")
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        Sym *a = ir::new_instance(cb, A);
        ir::set_field(cb, a, "fa", ir::const_int32(1));
        Sym *b = ir::new_instance(cb, B);
        ir::set_field(cb, b, "fb", ir::const_int32(2));
        cb.send_method(read_fa, a, {});
        cb.send_method(read_fa, b, {});  // violation: B has no fa
        cb.reply(cont, ret);
      });
  if1->top = top;
}

void setter_chain(const ParamMap &m) {
  int n_types = param(m, "n_types", 2);
  if (n_types < 1) n_types = 1;
  if (n_types > 3) n_types = 3;

  Sym *R1 = RecordBuilder("R1").field("a").build();
  Sym *R2 = RecordBuilder("R2").field("b").build();

  // chain(r1, r2): v = r1.a; r2.b = v
  // The polymorphism lives in the CSes of r1/r2 — r1.a's value
  // type varies by CS. chain's formals are uniformly typed
  // (R1, R2) so type stage doesn't split on them, but the read
  // r1.a sees mixed types at the CS-contour level.
  Sym *r1_arg = ir::local("r1");
  Sym *r2_arg = ir::local("r2");
  Sym *chain = ClosureBuilder("chain")
      .arg(r1_arg).arg(r2_arg)
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        Sym *v = ir::get_field(cb, r1_arg, "a", "v");
        ir::set_field(cb, r2_arg, "b", v);
        cb.reply(cont, ret);
      });

  // Main: per type variant, allocate r1+r2, populate r1.a with
  // a typed value, then call chain(r1, r2). The setter-of-setter
  // target is the cascade r1.a → v → r2.b inside chain, fed by
  // distinct-typed CSes from different call sites.
  Sym *top = ClosureBuilder("top")
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        for (int t = 0; t < n_types; t++) {
          Sym *val = nullptr;
          switch (t) {
            case 0: val = ir::const_int32(1); break;
            case 1: val = ir::const_float64(2.5); break;
            case 2: val = ir::const_int64(99); break;
          }
          Sym *r1 = ir::new_instance(cb, R1);
          Sym *r2 = ir::new_instance(cb, R2);
          ir::set_field(cb, r1, "a", val);
          cb.send_method(chain, r1, {r2_arg ? r2 : r2});  // call chain(r1, r2)
        }
        cb.reply(cont, ret);
      });
  if1->top = top;
}

void stored_fn_dispatch(const ParamMap &m) {
  int n_allocs = param(m, "n_allocs", 2);
  if (n_allocs < 2) n_allocs = 2;
  if (n_allocs > 3) n_allocs = 3;

  Sym *T = RecordBuilder("T").field("fn").build();

  // Build n_allocs leaf closures, each returning a different-
  // typed constant. f0() → int32, f1() → float64, f2() → int64.
  Sym *leaves[3] = {nullptr, nullptr, nullptr};
  for (int i = 0; i < n_allocs; i++) {
    char nm[16];
    snprintf(nm, sizeof(nm), "leaf%d", i);
    leaves[i] = ClosureBuilder(nm).body(
        [&](CodeBuilder &cb, Sym *cont, Sym *ret) {
          Sym *v = nullptr;
          switch (i) {
            case 0: v = ir::const_int32(10); break;
            case 1: v = ir::const_float64(3.14); break;
            case 2: v = ir::const_int64(99); break;
          }
          cb.move(v, ret);
          cb.reply(cont, ret);
        });
  }

  // Dispatcher: call_via(t) does t.fn(). Monomorphic on T but
  // dispatches through a stored function pointer.
  Sym *t_arg = ir::local("t");
  Sym *call_via = ClosureBuilder("call_via")
      .arg(t_arg)
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        Sym *m = ir::get_field(cb, t_arg, "fn", "m");
        Sym *r = ir::call_fn(cb, m, {}, "r");
        cb.move(r, ret);
        cb.reply(cont, ret);
      });

  // Main: allocate n_allocs T instances; store leaves[i] in
  // each fn field; call call_via on each instance.
  Sym *top = ClosureBuilder("top")
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        Sym *insts[3] = {nullptr, nullptr, nullptr};
        for (int i = 0; i < n_allocs; i++) {
          insts[i] = ir::new_instance(cb, T);
          ir::set_field(cb, insts[i], "fn", leaves[i]);
        }
        for (int i = 0; i < n_allocs; i++) {
          cb.send_method(call_via, insts[i], {});
        }
        cb.reply(cont, ret);
      });
  if1->top = top;
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
    {"same_type_dispatch", same_type_dispatch},
    {"stored_fn_dispatch", stored_fn_dispatch},
    {"setter_chain", setter_chain},
    {"missing_field_dispatch", missing_field_dispatch},
    {nullptr, nullptr},
};

ShapeFn lookup(cchar *name) {
  for (Entry *e = kRegistry; e->name; e++)
    if (!strcmp(e->name, name)) return e->fn;
  return nullptr;
}

}  // namespace IRShape
