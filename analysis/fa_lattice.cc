// SPDX-License-Identifier: BSD-3-Clause
//
// ifa/167 step 4: the type-value lattice -- the ALGEBRA over ATypes.
//
// union, difference, intersection, canonicalisation, the numeric
// coercion ladder and the pointer sort they all rest on. This is the part
// of the analysis that has stopped changing: it is declared in fa.h, it is
// the only part of fa.cc with real unit-test cover
// (ifa/testing/lattice_test.cc), and it has no dependency on any contour,
// pass or split state.
//
// What is NOT here, deliberately: `make_AType` and `make_abstract_type`.
// They read like lattice constructors and are not -- they MINT
// CreationSets, which is contour identity, the thing ifa/128 and ifa/129
// are still rewriting. The line is that this file operates on ATypes that
// already exist; creating the CreationSets an AType is made of belongs
// with the rest of contour identity in fa.cc.

#include "ifadefs.h"

#include <algorithm>

#include "fa.h"
#include "fa_census.h"
#include "builtin.h"
#include "fail.h"
#include "if1.h"
#include "prim.h"
#include "sym.h"
#include "var.h"

// Issue 025 numeric unification: map every numeric CS of a type
// other than `w` to `w` -- constants to the coerced constant of `w`
// (0 -> 0.0, value-preserving, free at compile time), non-constant
// numerics to abstract `w` (the runtime value converts at the
// assignment: C's conversion-on-assignment; verified for the LLVM
// path by the regression tests). Applied to an AVar's out when
// av->num_coerce is set (see fa.h). Element-wise, so it is
// monotone: as `t` grows the result only grows -- required for use
// inside the fixpoint. Mapping the abstract narrows too (not just
// constants) matters even for constant-only programs: `in` keeps
// the original int constant, and once the loop's folded constants
// exceed num_constants_per_variable, type_cannonicalize's cap-strip
// rebuilds `in` with every constant's BASE type -- resurrecting an
// abstract int64 from the already-coerced-away constant.
AType *type_coerce_numeric_constants(AType *t, Sym *w) {
  AType *r = t->coerce_map.get(w);
  if (r) return r;
  Vec<CreationSet *> css;
  int changed = 0;
  for (CreationSet *cs : t->sorted) {
    Sym *ct = cs->sym->type;
    if (ct && ct->num_kind && ct != w) {
      if (cs->sym->is_constant) {
        Immediate to;
        to.const_kind = w->num_kind;
        to.num_index = w->num_index;
        Immediate from = cs->sym->imm;
        coerce_immediate(&from, &to);
        css.set_add(make_abstract_type(imm_constant(to, w))->v[0]);
      } else
        css.set_add(make_abstract_type(w)->v[0]);
      changed = 1;
    } else
      css.set_add(cs);
  }
  r = changed ? make_AType(css) : t;
  t->coerce_map.put(w, r);
  return r;
}
//  all float combos become doubles
//  all signed/unsigned combos become signed
//  all int combos below 32 bits become signed 32 bits, above become signed 64
//  bits
Sym *coerce_num(Sym *a, Sym *b) {
  if (a == b) return a;
  if (a == sym_string || b == sym_string) return sym_string;
  if (a->num_kind == b->num_kind) {
    if (a->num_index > b->num_index)
      return a;
    else
      return b;
  }
  if (b->num_kind == IF1_NUM_KIND_FLOAT) {
    Sym *t = b;
    b = a;
    a = t;
  }
  if (b->num_kind == IF1_NUM_KIND_COMPLEX) {
    Sym *t = b;
    b = a;
    a = t;
  }
  // Survey B2: these lookups used to index the precision tables by
  // num_kind (the KIND enum, 0..4) instead of num_index, which made
  // every int operand read a precision of 8 or 16 -- so the
  // "does the int fit the float?" test always said yes, the widening
  // branches were dead, and (had they been reachable) the wide-int
  // case returned the NARROW float. Now: index by num_index, and a
  // >=32-bit int that doesn't fit widens to the 64-bit float/complex.
  if (a->num_kind == IF1_NUM_KIND_COMPLEX) {
    if (b->num_kind == IF1_NUM_KIND_FLOAT) {
      if (a->num_index > b->num_index) return a;
      return if1->complex_types[b->num_index];
    }
    if (int_type_precision[b->num_index] <= float_type_precision[a->num_index]) return a;
    if (int_type_precision[b->num_index] >= 32) return sym_complex64;
    return sym_complex32;
  }
  if (a->num_kind == IF1_NUM_KIND_FLOAT) {
    if (int_type_precision[b->num_index] <= float_type_precision[a->num_index]) return a;
    if (int_type_precision[b->num_index] >= 32) return sym_float64;
    return sym_float32;
  }
  // mixed signed and unsigned
  if (a->num_index >= IF1_INT_TYPE_64 || b->num_index >= IF1_INT_TYPE_64)
    return sym_int64;
  else if (a->num_index >= IF1_INT_TYPE_32 || b->num_index >= IF1_INT_TYPE_32)
    return sym_int32;
  else if (a->num_index >= IF1_INT_TYPE_16 || b->num_index >= IF1_INT_TYPE_16)
    return sym_int16;
  else if (a->num_index >= IF1_INT_TYPE_8 || b->num_index >= IF1_INT_TYPE_8)
    return sym_int8;
  return sym_bool;
}
AType *type_num_fold(Prim *p, AType *a, AType *b) {
  (void)p;
  p = 0;  // for now
  a = type_intersection(a, fa->type_world.anynum_kind);
  b = type_intersection(b, fa->type_world.anynum_kind);
  ATypeFold f(p, a, b), *ff;
  if ((ff = fa->type_world.type_fold_cache.get(&f))) return ff->result;
  AType *r = new AType();
  for (CreationSet *acs : a->sorted) {
    Sym *atype = acs->sym->type;
    for (CreationSet *bcs : b->sorted) {
      Sym *btype = bcs->sym->type;
      r->set_add(coerce_num(atype, btype)->abstract_type->v[0]);
    }
  }
  r = type_cannonicalize(r);
  fa->type_world.type_fold_cache.put(new ATypeFold(p, a, b, r));
  return r;
}
void qsort_pointers(void **left, void **right) {
Lagain:
  if (right - left < 5) {
    for (void **y = right - 1; y > left; y--) {
      for (void **x = left; x < y; x++) {
        if (x[0] > x[1]) {
          void *t = x[0];
          x[0] = x[1];
          x[1] = t;
        }
      }
    }
  } else {
    void **i = left + 1, **j = right - 1, *x = *left;
    for (;;) {
      while (x < *j) j--;
      while (i < j && *i < x) i++;
      if (i >= j) break;
      void *t = *i;
      *i = *j;
      *j = t;
      i++;
      j--;
    }
    if (j == right - 1) {
      *left = *(right - 1);
      *(right - 1) = x;
      right--;
      goto Lagain;
    }
    if (left < j) qsort_pointers(left, j + 1);
    if (j + 2 < right) qsort_pointers(j + 1, right);
  }
}
AType *type_cannonicalize(AType *t) {
  assert(!t->sorted.n);
  assert(!t->union_map.n);
  assert(!t->intersection_map.n);
  int consts = 0, rebuild = 0, nulls = 0;
  Vec<CreationSet *> nonconsts;
  CreationSet *nil_cs = nullptr;  // issue 060 -- decided after the loop
  for (CreationSet *cs : *t) if (cs) {
    // strip out constants if the base type is included
    CreationSet *base_cs = nullptr;
    if (cs->sym->is_constant || (cs->sym->type->num_kind && cs->sym != cs->sym->type))
      base_cs = cs->sym->type->abstract_type->v[0];
    else if (cs->sym->type_kind == Type_TAGGED)
      base_cs = cs->sym->type->specializes[0]->abstract_type->v[0];
    if (base_cs) {
      if (t->set_in(base_cs)) {
        rebuild = 1;
        continue;
      }
      consts++;
      nonconsts.set_add(base_cs);
    } else {
      if (!cs->sym->is_unique_type)  // e.g. nil, void, or unknown
        nonconsts.set_add(cs);
      else if (cs->sym->type == sym_nil_type)
        nil_cs = cs;  // issue 060: keep-or-strip decided after the loop
      else
        nulls = 1;  // void / unknown: always stripped from ->type
    }
    t->sorted.add(cs);
  }
  // issue 060: nil_type (None) is normally stripped from the ->type
  // projection (is_unique_type), so a pointer-shaped `T | None` union
  // stays a single clone -- None is a null pointer there, unambiguous,
  // and a frontend may sanction that merge (pyc does). But IFA's core
  // discipline is to split incompatible types, and None IS
  // incompatible with a raw scalar (int/bool/float): under the unboxed
  // representation they share the zero bit pattern, so a shared clone
  // literally cannot tell `None` from `0`/`False` (issue 060). Keep nil
  // in ->type whenever the union also carries a num_kind scalar, so the
  // type-splitter puts the None value in its own contour instead of
  // coercing it to `(scalar)NULL`.
  if (nil_cs) {
    bool has_scalar = false;
    for (CreationSet *c : nonconsts)
      if (c && c->sym->type && c->sym->type->num_kind) { has_scalar = true; break; }
    if (has_scalar)
      nonconsts.set_add(nil_cs);  // keep nil in ->type (no nulls: it is not stripped)
    else
      nulls = 1;  // pointer / other: strip nil as before
  }
  // PROBE (PYC_CONSTCAP): raise the per-variable constant cap. Default is
  // fa->num_constants_per_variable (1), i.e. an AType holding two constants
  // is rebuilt from their BASE types and the constants are gone -- which is
  // also what stops `type_num_fold`'s constant fold, since that needs each
  // operand to be a SINGLE CreationSet carrying an immediate.
  static int constcap = -2;
  if (constcap == -2) { cchar *v = getenv("PYC_CONSTCAP"); constcap = v ? atoi(v) : -1; }
  const int cap = constcap >= 0 ? constcap : fa->num_constants_per_variable;
  if (consts > cap) {
    rebuild = 1;
    ++census.fa_cap_strips;  // ifa/131 step 1: does the cap-strip fire at all?
  }
  if (rebuild) {
    t->sorted.clear();
    t->sorted.append(nonconsts);
    t->clear();
    t->set_union(t->sorted);
  }
  if (t->sorted.n > 1) qsort_by_id(t->sorted);
  unsigned int h = 0;
  // Accumulate (survey B1): `h =` here discarded all but the last
  // element, collapsing the hash-cons table's distribution to
  // last-element groups. Position sensitivity comes from the
  // per-index prime.
  for (int i = 0; i < t->sorted.n; i++) h += (uint)(intptr_t)t->sorted[i] * open_hash_primes[i % 256];
  t->hash = h ? h : h + 1;  // 0 is empty
  AType *tt = fa->type_world.cannonical_atypes.put(t);
  if (!tt) tt = t;
  // compute "type" (without constants)
  if (nonconsts.n) {
    if (nulls || consts)
      tt->type = make_AType(nonconsts);
    else
      tt->type = tt;
  } else
    tt->type = fa->type_world.bottom_type;
  return tt;
}
AType *type_union(AType *a, AType *b) {
  AType *r;
  if ((r = a->union_map.get(b))) return r;
  if (a == b || b == fa->type_world.bottom_type) {
    r = a;
    goto Ldone;
  }
  if (a == fa->type_world.bottom_type) {
    r = b;
    goto Ldone;
  }
  {
    AType *ab = type_diff(a, b);
    AType *ba = type_diff(b, a);
    r = new AType(*ab);
    for (CreationSet *x : ba->sorted) r->set_add(x);
    for (CreationSet *x : a->sorted) if (b->in(x)) r->set_add(x);
    r = type_cannonicalize(r);
  }
Ldone:
  a->union_map.put(b, r);
  return r;
}
static inline int subsumed_by(Sym *a, Sym *b) {
  return (a == b) || a->type == b || b->specializers.set_in(a->type);
}
AType *type_diff(AType *a, AType *b) {
  AType *r;
  if ((r = a->diff_map.get(b))) return r;
  if (b == fa->type_world.bottom_type) {
    r = a;
    goto Ldone;
  }
  r = new AType();
  for (CreationSet *aa : a->sorted) {
    if (aa->defs.n && b->set_in(aa)) continue;
    for (CreationSet *bb : b->sorted) if (!bb->defs.n) {
      if (subsumed_by(aa->sym, bb->sym)) goto Lnext;
    }
    r->set_add(aa);
  Lnext:;
  }
  r = type_cannonicalize(r);
Ldone:
  a->diff_map.put(b, r);
  return r;
}
AType *type_intersection(AType *a, AType *b) {
  // Issue 033: a null filter (a Map<MPosition*,AType*>::get() miss)
  // means "no constraint" -- analyze_edge already treats a missing
  // formal_filters entry this way (fa.cc, the `if (filter) {...}
  // else filter = es_filter;` / `if (filter && ...)` guards around
  // its own type_intersection calls). Some other callers passed a
  // possibly-null filter straight through without that guard,
  // crashing here on `b->sorted`/`a->sorted` when a position simply
  // has no recorded filter yet. Treat null as the intersection
  // identity (return the other operand) to match the established
  // semantic instead of requiring every caller to null-check first.
  if (!b) return a;
  if (!a) return b;
  AType *r;
  if ((r = a->intersection_map.get(b))) return r;
  if (a == b || a == fa->type_world.bottom_type || b == fa->type_world.top_type) {
    r = a;
    goto Ldone;
  }
  if (a == fa->type_world.top_type || b == fa->type_world.bottom_type) {
    r = b;
    goto Ldone;
  }
  r = new AType();
  for (CreationSet *aa : a->sorted) {
    for (CreationSet *bb : b->sorted) {
      if (aa->defs.n) {
        if (bb->defs.n) {
          if (aa == bb) {
            r->set_add(aa);
            goto Lnexta;
          }
        } else {
          if (subsumed_by(aa->sym, bb->sym)) {
            r->set_add(aa);
            goto Lnexta;
          }
        }
      } else {
        if (bb->defs.n) {
          if (subsumed_by(bb->sym, aa->sym)) r->set_add(bb);
        } else {
          if (subsumed_by(aa->sym, bb->sym)) {
            r->set_add(aa);
            goto Lnexta;
          } else if (subsumed_by(bb->sym, aa->sym))
            r->set_add(bb);
        }
      }
    }
  Lnexta:;
  }
  r = type_cannonicalize(r);
Ldone:
  a->intersection_map.put(b, r);
  return r;
}
