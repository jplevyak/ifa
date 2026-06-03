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

void iterator_copy(const ParamMap & /*m*/) {
  // Same V + It as vector_iterator; add a `next` method.
  Sym *V = RecordBuilder("V").vector().build();
  Sym *It = RecordBuilder("It").field("vec").field("pos").build();

  // next(self: It) → element via vec_get(self.vec, self.pos).
  ClosureBuilder next_cb("next_tmp");
  Sym *next_self = next_cb.method("next", It);
  Sym *next_fn = next_cb.body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
    Sym *vec = ir::get_field(cb, next_self, "vec", "vec_v");
    Sym *pos = ir::get_field(cb, next_self, "pos", "pos_v");
    Sym *elt = ir::vec_get(cb, vec, pos, "elt");
    cb.move(elt, ret);
    cb.reply(cont, ret);
  });
  Sym *next_sym = if1_make_symbol(if1, "next");

  // copy(src, dst): make an It bound to src, read element via
  // next(it), write to dst[0]. The chain: src CS varies →
  // it.vec → vec_get target (per-src-CS element AVar) → dst's
  // element AVar via set_index_object.
  Sym *src_arg = ir::local("src");
  Sym *dst_arg = ir::local("dst");
  Sym *copy = ClosureBuilder("copy")
      .arg(src_arg).arg(dst_arg)
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        Sym *it = ir::new_instance(cb, It, "it");
        ir::set_field(cb, it, "vec", src_arg);
        ir::set_field(cb, it, "pos", ir::const_int32(0));
        Sym *e = cb.send_method(next_sym, it, {}, ir::local("e"));
        ir::vec_set(cb, dst_arg, ir::const_int32(0), e);
        cb.reply(cont, ret);
      });

  Sym *top = ClosureBuilder("top")
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        Sym *v1 = ir::new_instance(cb, V);
        ir::vec_set(cb, v1, ir::const_int32(0), ir::const_int32(10));
        Sym *v2 = ir::new_instance(cb, V);
        ir::vec_set(cb, v2, ir::const_int32(0), ir::const_float64(2.5));
        Sym *dst = ir::new_instance(cb, V);
        cb.send_method(copy, v1, {dst});
        cb.send_method(copy, v2, {dst});
        cb.reply(cont, ret);
      });
  if1->top = top;
  (void)next_fn;
}

void iterator_missing_field(const ParamMap & /*m*/) {
  // V (vector type) whose elements are records of types A or B
  // with DISJOINT fields. Reader iterates v and reads e.fa —
  // exists on A, missing on B.
  Sym *A = RecordBuilder("A").field("fa").build();
  Sym *B = RecordBuilder("B").field("fb").build();
  Sym *V = RecordBuilder("V").vector().build();
  Sym *It = RecordBuilder("It").field("vec").field("pos").build();

  ClosureBuilder next_cb("next_tmp");
  Sym *next_self = next_cb.method("next", It);
  Sym *next_fn = next_cb.body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
    Sym *vec = ir::get_field(cb, next_self, "vec", "vec_v");
    Sym *pos = ir::get_field(cb, next_self, "pos", "pos_v");
    Sym *elt = ir::vec_get(cb, vec, pos, "elt");
    cb.move(elt, ret);
    cb.reply(cont, ret);
  });
  Sym *next_sym = if1_make_symbol(if1, "next");

  // consume(v): iterate, read .fa on the yielded element.
  // For B-element vectors, fa is missing → violation.
  Sym *v_arg = ir::local("v");
  Sym *consume = ClosureBuilder("consume")
      .arg(v_arg)
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        Sym *it = ir::new_instance(cb, It, "it");
        ir::set_field(cb, it, "vec", v_arg);
        ir::set_field(cb, it, "pos", ir::const_int32(0));
        Sym *e = cb.send_method(next_sym, it, {}, ir::local("e"));
        Sym *v = ir::get_field(cb, e, "fa", "v");
        cb.move(v, ret);
        cb.reply(cont, ret);
      });

  Sym *top = ClosureBuilder("top")
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        Sym *v1 = ir::new_instance(cb, V);
        Sym *a = ir::new_instance(cb, A);
        ir::set_field(cb, a, "fa", ir::const_int32(1));
        ir::vec_set(cb, v1, ir::const_int32(0), a);

        Sym *v2 = ir::new_instance(cb, V);
        Sym *b = ir::new_instance(cb, B);
        ir::set_field(cb, b, "fb", ir::const_int32(2));
        ir::vec_set(cb, v2, ir::const_int32(0), b);

        cb.send_method(consume, v1, {});  // ok
        cb.send_method(consume, v2, {});  // violation: B has no fa
        cb.reply(cont, ret);
      });
  if1->top = top;
  (void)next_fn;
}

void vector_iterator(const ParamMap & /*m*/) {
  // V: vector type; It: iterator with vec + pos fields.
  Sym *V = RecordBuilder("V").vector().build();
  Sym *It = RecordBuilder("It").field("vec").field("pos").build();

  // next(self: It) → element. Method dispatched via the "next"
  // symbol; FA matches based on the symbol Sym at rval[0] and
  // the receiver type at rval[1] (constrained via must_specialize
  // on the formal[1] self Sym).
  Sym *self_arg = nullptr;
  Sym *next_method = ClosureBuilder("next_impl")
      .body([](CodeBuilder &cb, Sym *cont, Sym *ret) { cb.reply(cont, ret); });
  // Build a SEPARATE closure as the actual method, using method().
  ClosureBuilder next_cb("next_tmp");
  self_arg = next_cb.method("next", It);
  Sym *next = next_cb.body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
    Sym *vec = ir::get_field(cb, self_arg, "vec", "vec_v");
    Sym *pos = ir::get_field(cb, self_arg, "pos", "pos_v");
    Sym *elt = ir::vec_get(cb, vec, pos, "elt");
    cb.move(elt, ret);
    cb.reply(cont, ret);
  });
  (void)next_method;  // discard the placeholder

  // Symbol Sym used at every "next(it)" call site.
  Sym *next_sym = if1_make_symbol(if1, "next");

  // consume(v) builds an It bound to v, calls next(it). v is
  // type-uniform V (across CSes) so stage 1 has no formal
  // confluence on consume's args. The polymorphism propagates:
  // v (CS varies) → it.vec (CS varies) → vec_get target (poly
  // element AVar per V's CS) → next's return (poly result).
  Sym *v_arg = ir::local("v");
  Sym *consume = ClosureBuilder("consume")
      .arg(v_arg)
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        Sym *it = ir::new_instance(cb, It, "it");
        ir::set_field(cb, it, "vec", v_arg);
        ir::set_field(cb, it, "pos", ir::const_int32(0));
        Sym *e = cb.send_method(next_sym, it, {}, ir::local("e"));
        cb.move(e, ret);
        cb.reply(cont, ret);
      });

  // Main: two V allocations with distinct-typed element writes,
  // then consume() each. The dispatch chain at consume's call
  // site is polymorphic in V's CS provenance.
  Sym *top = ClosureBuilder("top")
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        Sym *v1 = ir::new_instance(cb, V);
        ir::vec_set(cb, v1, ir::const_int32(0), ir::const_int32(10));
        Sym *v2 = ir::new_instance(cb, V);
        ir::vec_set(cb, v2, ir::const_int32(0), ir::const_float64(2.5));
        cb.send_method(consume, v1, {});
        cb.send_method(consume, v2, {});
        cb.reply(cont, ret);
      });
  if1->top = top;
  (void)next;
}

void vector_element_polymorphism(const ParamMap &m) {
  int n_writes = param(m, "n_writes", 2);
  if (n_writes < 2) n_writes = 2;
  if (n_writes > 3) n_writes = 3;

  // Vector type V — is_vector=1, element=fresh Sym.
  Sym *V = RecordBuilder("V").vector().build();

  // Write n_writes distinct-typed values into the SAME vector CS,
  // then read back. The element AVar for that CS sees a per-CS
  // type confluence (multiple types within one creation set) —
  // pyc's list runtime creates the same shape via list iteration.
  // Stage 1 can't split the CS (single allocation site), so the
  // residual confluence should fall to setter/mark stages.
  Sym *top = ClosureBuilder("top")
      .body([&](CodeBuilder &cb, Sym *cont, Sym *ret) {
        Sym *v = ir::new_instance(cb, V);
        for (int i = 0; i < n_writes; i++) {
          Sym *idx = ir::const_int32(i);
          Sym *val = nullptr;
          switch (i) {
            case 0: val = ir::const_int32(10); break;
            case 1: val = ir::const_float64(2.5); break;
            case 2: val = ir::const_int64(99); break;
          }
          ir::vec_set(cb, v, idx, val);
        }
        // Read element 0 back — sees the per-CS polymorphic element.
        Sym *r = ir::vec_get(cb, v, ir::const_int32(0), "r");
        cb.move(r, ret);
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
    {"vector_element_polymorphism", vector_element_polymorphism},
    {"vector_iterator", vector_iterator},
    {"iterator_copy", iterator_copy},
    {"iterator_missing_field", iterator_missing_field},
    {nullptr, nullptr},
};

ShapeFn lookup(cchar *name) {
  for (Entry *e = kRegistry; e->name; e++)
    if (!strcmp(e->name, name)) return e->fn;
  return nullptr;
}

}  // namespace IRShape
