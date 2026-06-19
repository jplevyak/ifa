// SPDX-License-Identifier: BSD-3-Clause
//
// cg_ir_v2.cc — CG_IR_PLAN Phase 4. Type-system helpers and
// program-builder shared code.
//
// Parser and printer live in cg_ir_v2_parse.cc and
// cg_ir_v2_print.cc respectively. This file holds the
// predefined-type wiring and the CGv2Program constructor.

#include "ifadefs.h"

#include "codegen/cg_ir_v2.h"

#include <string.h>

static CGv2Type *mk_type(int id, cchar *name, CGv2TypeKind kind,
                         int bits) {
  CGv2Type *t = new CGv2Type();
  t->id = id;
  t->name = name;
  t->kind = kind;
  t->bits = bits;
  return t;
}

CGv2Program::CGv2Program() : main_fun(0) {
  // Predefined types. Ids 1..N reserved for builtins; user
  // types get ids starting at 1000.
  t_void   = mk_type(1,  "void",    CG2T_VOID,   0);
  t_bool   = mk_type(2,  "bool",    CG2T_BOOL,   1);
  t_int8   = mk_type(3,  "int8",    CG2T_INT,    8);
  t_int16  = mk_type(4,  "int16",   CG2T_INT,    16);
  t_int32  = mk_type(5,  "int32",   CG2T_INT,    32);
  t_int64  = mk_type(6,  "int64",   CG2T_INT,    64);
  t_uint8  = mk_type(7,  "uint8",   CG2T_UINT,   8);
  t_uint16 = mk_type(8,  "uint16",  CG2T_UINT,   16);
  t_uint32 = mk_type(9,  "uint32",  CG2T_UINT,   32);
  t_uint64 = mk_type(10, "uint64",  CG2T_UINT,   64);
  t_float32 = mk_type(11, "float32", CG2T_FLOAT, 32);
  t_float64 = mk_type(12, "float64", CG2T_FLOAT, 64);
  t_sym     = mk_type(13, "sym",     CG2T_SYMBOL, 0);
  t_nil     = mk_type(14, "nil",     CG2T_OPAQUE,  0);
  t_fun_ptr = mk_type(15, "fun_ptr", CG2T_FUN_PTR, 0);
  // Predefined opaque ptr.  All sites that previously created a
  // CG2T_PTR with `element == nullptr` (FA-unknown ptrs from
  // Type_FUN, Type_REF, Type_PRIMITIVE fallbacks; method-slot
  // fields whose IF1 type is sym_void; the test-roundtrip
  // `:kind ptr` form with no `:element`) now route here.  This
  // makes CG2T_PTR.element a non-null invariant.
  t_ptr     = mk_type(16, "ptr",     CG2T_OPAQUE,  0);
}

CGv2Type *CGv2Program::lookup_type(cchar *name) const {
  if (!name) return nullptr;
  // Predefined first.
  struct { cchar *n; CGv2Type *t; } preset[] = {
    {"void",    t_void},
    {"bool",    t_bool},
    {"int8",    t_int8},
    {"int16",   t_int16},
    {"int32",   t_int32},
    {"int64",   t_int64},
    {"uint8",   t_uint8},
    {"uint16",  t_uint16},
    {"uint32",  t_uint32},
    {"uint64",  t_uint64},
    {"float32", t_float32},
    {"float64", t_float64},
    {"sym",     t_sym},
    {"nil",     t_nil},
    {"fun_ptr", t_fun_ptr},
    {"ptr",     t_ptr},
  };
  for (auto &p : preset) {
    if (p.n && p.t && strcmp(p.n, name) == 0) return p.t;
  }
  for (CGv2Type *t : types) {
    if (t && t->name && strcmp(t->name, name) == 0) return t;
  }
  return nullptr;
}

CGv2Fun *CGv2Program::lookup_fun(cchar *name) const {
  if (!name) return nullptr;
  for (CGv2Fun *f : funs) {
    if (f && f->name && strcmp(f->name, name) == 0) return f;
  }
  return nullptr;
}
