// SPDX-License-Identifier: BSD-3-Clause
//
// ifa/201: the representation oracle.
//
// Representation is the third category beside type and provenance: what
// the target can REPRESENT (width, pointer-ness, None in a union). It has
// one owner, here. The analysis asks it whether a union can be represented
// at all, which is a demand when it cannot; clone and both backends ask it
// what a type is in the emitted program. Nothing else decides either, and
// nothing decides either from a C type name.
//
// Implemented in codegen_common.cc. This header is deliberately small and
// free of backend includes so that `ifa/analysis` can use it.

#ifndef _repr_H_
#define _repr_H_

#include "fa.h"

// What a value of a given type IS in
// the emitted program -- its width, and whether it is an integer, a float
// or a pointer -- answered from the type Sym and nothing else.
struct CGRepr {
  enum Kind : unsigned char {
    UNTYPED,  // pointer-sized, and nothing is known of the pointee: no type, void, any
    INT,      // an integer or bool scalar
    FLOAT,    // a floating-point scalar
    POINTER,  // a typed pointer: an object, a container, a string, a function, None
  };
  Kind kind;
  unsigned char width;  // bytes
  bool scalar() const { return kind == INT || kind == FLOAT; }
  bool is_float() const { return kind == FLOAT; }
  bool pointer_like() const { return kind == POINTER || kind == UNTYPED; }
  bool untyped() const { return kind == UNTYPED; }
};
// Of a TYPE Sym (a Var's `->type`, a member's `->type`). Null is untyped.
CGRepr cg_repr(Sym *type);
static inline CGRepr cg_repr(Var *v) { return cg_repr(v ? v->type : nullptr); }
// Of a VALUE Sym (a member, an element, a constant): its type's.
static inline CGRepr cg_value_repr(Sym *s) { return cg_repr(s ? s->type : nullptr); }
// Untyped, or None: a pointer cell that names no pointee, so a typed pointer
// stored into it or read out of it needs a cast in C.
bool cg_voidish(Sym *type);
// Name the builtin types for codegen (`list` -> `_CG_list`), once, for both
// backends. The oracle reads the set this records, never the names.
void cg_name_builtin_types(FA *fa);

// ---- of a record ----------------------------------------------------------

// ifa/issues/030: does this record type carry the classtag header
// (`__pyc_tag` in C structs / a leading ptr field in LLVM structs)?
// Only class-like records (created via prim_new prototypes +
// clone_dst) qualify. Tuples, list-backed tuples, and vectors are
// laid out as bare element arrays and accessed with raw pointer
// math -- a leading tag member would corrupt them; they are
// identified structurally (only unnamed fields) since tuple clones
// aren't always in sym_tuple->specializers.
bool cg_has_classtag(Sym *s);

// Does a tuple of type `t` have a static arity (every CreationSet it was
// cloned from has one)? Decides the header's fixed-arity bit at
// construction (_CG_tuple_set_fixed), so it matches what FA dispatched on.
bool cg_tuple_type_fixed_arity(Sym *t);

// ---- of a union, as the analysis sees it ---------------------------------

// What the members of a converged AType ARE. A "basic" is a type with a
// value representation of its own (to_basic_type: a number, a string, a
// symbol); an "object" is anything held by pointer that is not basic; the
// unit is None or void, which a pointer represents as null.
struct ATypeRepr {
  int basics = 0;    // distinct basic types
  int numerics = 0;  // ... of which numeric scalars
  int objects = 0;   // members that are neither basic nor the unit
  bool unit = false;

  // Two basic types in one cell: `{int, str}`, `{int, float}`.
  bool mixed_basics() const { return basics > 1; }
  // A number beside an object: the int would be stored as a pointer (ifa/188).
  bool scalar_with_pointer() const { return numerics > 0 && objects > 0; }
  // No representation at all, None aside: a basic beside an object, or two
  // basics. The level question a demand test asks (ifa/157).
  bool irrepresentable() const { return basics > 1 || (basics >= 1 && objects > 0); }
  // The same, of a container's element, where a purely numeric mix is left
  // to the permissive coercion rather than split on (ifa/156).
  bool element_irrepresentable() const {
    return (basics >= 1 && objects > 0) || (basics > 1 && numerics != basics);
  }
  // `{None, T*}`: None is the null pointer. `{None, int}` is not (issues/060).
  bool nullable_pointer() const { return unit && !numerics && (objects > 0 || basics > 0); }
};
ATypeRepr atype_repr(AType *t);
// Is `s` (a CreationSet's sym, or its type) the unit: None or void?
bool repr_is_unit(Sym *s);

#endif
