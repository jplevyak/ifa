#include <ctype.h>
#include <set>
#include <string>

#include "ifadefs.h"

#include "cg.h"
#include "builtin.h"
#include "codegen_common.h"
#include "fa.h"
#include "fail.h"
#include "fun.h"
#include "if1.h"
#include "pattern.h"
#include "pdb.h"
#include "pnode.h"
#include "prim.h"
#include "var.h"

// c_type(Var*), c_type(Sym*) — moved to codegen_common.{h,cc}.

// Polymorphic method dispatch map: maps each __new__ Fun to a list of
// (slot_index, target_fun) pairs discovered by tracing FA creation chains
// at polymorphic call sites.  Built once before codegen; consumed by the
// P_prim_clone emitter (to populate method pointer fields) and by
// emit_send_call (to emit indirect calls through those fields).
struct PolymorphicSlot {
  int slot;
  Fun *fun_val;
  int specificity;  // lower = more specific (fewer CSes in the ES sorted list)
};
static Map<Fun *, Vec<PolymorphicSlot> *> new_to_val_map;

static void write_c_fun_proto(FILE *fp, Fun *f, int type = 0) {
  assert(f->rets.n == 1);
  fputs(c_type(f->rets[0]), fp);
  if (type)
    fputs(" (*", fp);
  else
    fputs(" ", fp);
  if (type)
    fputs(cg_get_structural_string(f), fp);
  else
    fputs(cg_get_string(f), fp);
  if (type)
    fputs(")(", fp);
  else
    fputs("(", fp);
  MPosition p;
  p.push(1);
  int wrote_one = 0;
  for (int i = 0; i < f->sym->has.n; i++) {
    MPosition *cp = cannonicalize_mposition(p);
    p.inc();
    Var *v = f->args.get(cp);
    if (!v->live) continue;
    if (wrote_one) fputs(", ", fp);
    wrote_one = 1;
    fputs(c_type(v), fp);
    fprintf(fp, " a%d", i);
  }
  fputs(")", fp);
}

static void write_c_type(FILE *fp, Var *v) { fprintf(fp, "%s", c_type(v)); }

static int application_depth(PNode *n) {
  if (n->rvals[0]->type->fun)
    return 1;
  else
    return 1 + application_depth(n->rvals[0]->def);
}

static void write_c_apply_arg(FILE *fp, cchar *base, int n, int i) {
  fputs(base, fp);
  for (int j = i; j < n - 2; j++) fputs("->e0", fp);
  if (!i)
    fputs("->e0", fp);
  else
    fputs("->e1", fp);
}

static int cg_writeln(FILE *fp, Vec<Var *> &vars, int ln) {
  for (int i = 2; i < vars.n; i++) {
    bool doln = i == vars.n - 1 && ln;
    cchar *sln = doln ? "\\n" : "";
    if (vars[i]->type == sym_int8 || vars[i]->type == sym_int16 || vars[i]->type == sym_int32)
      fprintf(fp, "printf(\"%%d%s\", %s);\n", sln, cg_get_string(vars[i]));
    else if (vars[i]->type == sym_bool || vars[i]->type == sym_uint8 || vars[i]->type == sym_uint16 ||
             vars[i]->type == sym_uint32)
      fprintf(fp, "printf(\"%%u%s\", %s);\n", sln, cg_get_string(vars[i]));
    else if (vars[i]->type == sym_int64)
      fprintf(fp, "printf(\"%%lld%s\", (long long int)%s);\n", sln, cg_get_string(vars[i]));
    else if (vars[i]->type == sym_uint64)
      fprintf(fp, "printf(\"%%llu%s\", (unsigned long long int)%s);\n", sln, cg_get_string(vars[i]));
    else if (vars[i]->type == sym_float32 || vars[i]->type == sym_float64 || vars[i]->type == sym_float128)
      fprintf(fp, "_CG_float_printf(%s,%d);\n", cg_get_string(vars[i]), doln);
    else if (vars[i]->type == sym_string) {
      if (ln || strcmp("_CG_String(\"\")", cg_get_string(vars[i]))) {
        if (doln)
          fprintf(fp, "printf(\"%%s%s\", %s);\n", sln, cg_get_string(vars[i]));
        else
          fprintf(fp, "fputs(%s, stdout);\n", cg_get_string(vars[i]));
      }
    } else
      fprintf(fp, "printf(\"<unsupported type>%s\");\n", sln);
  }
  if (vars.n < 3 && ln) fputs("putchar(\'\\n\');\n", fp);
  return 0;
}

// num_string(Sym*) — moved to codegen_common.{h,cc}.

// Issue 026: dead-field aware slot lookup.
//
// pyc's liveness analysis marks a field's Var dead if no
// downstream code reads it.  The C struct codegen elides
// dead fields entirely (no slot emitted).  For consistency
// across struct emission, setter, getter, and other
// field-access codegen, the slot index `N` in `obj->eN` is
// derived from this helper: live fields get a sequential
// 0-based index in their order within `s->has`; dead
// fields return -1 so callers can elide the access
// altogether.
//
// Without this, the struct definition and the field-access
// codegen drift: the struct skips a dead field at has[k]
// (numbering goes ... e(k-1); e(k+1) ...), while the
// setter / getter emit `obj->eK` referencing the missing
// slot.  Issue 026 documents the symptom.
// Issue 028 Bug A — pick a non-nil component of a union
// receiver for getter/setter codegen.
//
// pyc's `is None` narrowing in IFA only attaches a
// type-predicate restrict to the SSU view of the
// conditional's operand temp.  A fresh re-read of the
// same property in the body (e.g. `self.min.next` after
// `if self.min is None`) creates a NEW period SEND whose
// receiver var has the full union type (None | Node).
//
// At codegen time, getter/setter resolved the field via
// `obj = recv->type; if (obj->type_kind == Type_SUM)
// obj = obj->has[0]` — i.e. blindly took the first union
// component, which for typical orderings is `nil_type`.
// `nil_type` has no fields; the result was a cast to
// `_CG_nil_type` (a `void *` typedef) followed by
// `->eN`, which fails to compile.
//
// Soundness: at runtime, the receiver's value in the
// narrowed branch is non-None (the IF guarded it), so
// emitting code assuming the non-None component is the
// correct semantic.  Picking it here matches what
// `assign_type_cg_strings_pass2` does at the SUM-sym
// level for cg_string assignment (the 2-element
// `None | T` shortcut).
//
// Strategy: search the union for a component whose
// `has` list contains a field named `symbol`.  If none
// found, fall back to the historical `obj->has[0]` so
// the existing "getter not resolved" assert path still
// fires for genuinely-unresolvable cases.
static Sym *resolve_union_receiver(Sym *obj, cchar *symbol) {
  if (!obj || obj->type_kind != Type_SUM) return obj;
  if (symbol) {
    for (Sym *component : obj->has) {
      if (!component || component == sym_nil_type) continue;
      for (Sym *field : component->has) {
        if (field && field->name == symbol) return component;
      }
    }
  }
  if (obj->has.n > 0) {
    for (Sym *component : obj->has) {
      if (component && component != sym_nil_type) return component;
    }
    return obj->has[0];
  }
  return obj;
}

static int cg_field_live(Sym *s, int i) {
  if (!s || i < 0 || i >= s->has.n) return 0;
  if (!s->has[i]->type) return 0;
  if (s->has[i]->var && !s->has[i]->var->live) return 0;
  return 1;
}

static cchar *c_rhs(Var *v) {
  if (!v->sym->is_fun) {
    if (!cg_get_string(v))
      return "0";
    else
      return cg_get_string(v);
  } else {
    char s[100];
    snprintf(s, sizeof(s), "((_CG_function*)%s)", cg_get_string(v));
    return dupstr(s);
  }
}

static void destruct_prim(FILE *fp, Var *l, Var *r) {
  int is_tuple = sym_tuple->specializers.set_in(l->sym) != 0;
  for (int i = 0; i < l->sym->has.n; i++) {
    if (!is_tuple && l->sym->has_name(i)) {
      assert(0);
    } else {
      fprintf(fp, "  %s->e%d = %s->e%d;\n", cg_get_string(l), i, cg_get_string(r), i);
    }
  }
}

static int write_c_prim(FILE *fp, FA *fa, Fun *f, PNode *n) {
  int o = (n->rvals.v[0]->sym == sym_primitive) ? 2 : 1;
  bool listish_tuple = false;
  switch (n->prim->index) {
    default:
      return 0;
    case P_prim_make:
      if (sym_tuple->specializers.set_in(n->rvals[2]->sym)) {
      Ltuple:
        fputs("  ", fp);
        cchar *t = c_type(n->lvals[0]);
        int voidish = n->rvals.n < 4 && n->lvals[0]->type->element->type == sym_void;
        fprintf(fp, "%s = _CG_prim_tuple%s(%s, %d);\n", cg_get_string(n->lvals[0]), listish_tuple ? "_list" : "",
                voidish ? "int*" : t, n->rvals.n - 2);
        for (int i = 3; i < n->rvals.n; i++)
          fprintf(fp, "  %s->e%d = %s;\n", cg_get_string(n->lvals[0]), i - 3, cg_get_string(n->rvals.v[i]));
      } else if (sym_list->specializers.set_in(n->rvals[2]->sym) || n->rvals[2]->sym->is_vector) {
        Sym *t = n->lvals.v[0]->type, *e = t->element->type;
        if (t->type_kind == Type_RECORD) {
          listish_tuple = true;
          goto Ltuple;
        }
        fputs("  ", fp);
        assert(n->lvals.n == 1);
        e = e ? e : sym_void_type;
        assert(cg_get_string(n->lvals[0]));
        fprintf(fp, "%s = ", cg_get_string(n->lvals[0]));
        fprintf(fp, "(_CG_list)_CG_prim_list(%s,%d);\n", cg_get_string(e), n->rvals.n - 3);
        for (int i = 3; i < n->rvals.n; i++) {
          fprintf(fp, "  ((%s*)(_CG_list_ptr(%s)))[%d] = ", cg_get_string(e), cg_get_string(n->lvals[0]), i - 3);
          fputs(cg_get_string(n->rvals[i]), fp);
          fprintf(fp, ";\n");
        }
      }
      break;
    case P_prim_period: {
      cchar *t = c_type(n->lvals[0]);
      cchar *symbol = 0;
      Vec<Sym *> symbols;
      symbol_info(n->rvals[3], symbols);
      if (symbols.n == 1)
        symbol = symbols[0]->name;
      else if (n->rvals[3]->sym->is_symbol)
        symbol = n->rvals[3]->sym->name;
      else {
        assert(!"selector");
      }
      Sym *obj = n->rvals[1]->type;
      obj = resolve_union_receiver(obj, symbol);
      if (n->lvals[0]->type->type_kind == Type_FUN && n->creates) {  // creates a closure
        fprintf(fp, "  %s = _CG_prim_closure(%s);\n", cg_get_string(n->lvals[0]), t);
        if (n->prim && n->prim->index == P_prim_period) {
          fprintf(fp, "  %s->e%d = %s;\n", cg_get_string(n->lvals[0]), 0, cg_get_string(n->rvals.v[3]));
          fprintf(fp, "  %s->e%d = %s;\n", cg_get_string(n->lvals[0]), 1, cg_get_string(n->rvals.v[1]));
        } else {
          for (int i = 0; i < n->rvals.n; i++)
            fprintf(fp, "  %s->e%d = %s;\n", cg_get_string(n->lvals[0]), i, cg_get_string(n->rvals.v[i]));
        }
      } else {
        for (int i = 0; i < obj->has.n; i++) {
          if (symbol == obj->has[i]->name && cg_field_live(obj, i)) {
            assert(cg_get_string(n->lvals[0]));
            fprintf(fp, "  %s = (%s)((%s)%s)->e%d; /* %s */\n", cg_get_string(n->lvals[0]), t, cg_get_string(obj),
                    cg_get_string(n->rvals[1]), i, symbol);
            goto Lgetter_found;
          }
        }
        if (!fruntime_errors)
          fail("getter not resolved");
        else
          fputs("  assert(!\"runtime error: getter not resolved\");\n", fp);
      Lgetter_found:;
      }
      break;
    }
    case P_prim_setter: {
      cchar *symbol = n->rvals[3]->sym->is_symbol ? n->rvals[3]->sym->name : 0;
      if (!symbol) {
        Vec<Sym *> symbols;
        symbol_info(n->rvals[3], symbols);
        assert(symbols.n == 1);
        symbol = symbols[0]->name;
      }
      Sym *obj = n->rvals[1]->type;
      obj = resolve_union_receiver(obj, symbol);
      for (int i = 0; i < obj->has.n; i++) {
        if (symbol == obj->has[i]->name) {
          // Issue 026: elide setter writes to dead fields.
          // The struct has no slot for `e<i>` when has[i]
          // is dead, so emitting `obj->e<i> = ...` would
          // reference a missing member.  Dead fields are
          // dead by definition — no downstream code reads
          // the value — so skipping the write is sound.
          if (cg_field_live(obj, i)) {
            fprintf(fp, "  ((%s)%s)->e%d = (%s)%s;\n",
                    cg_get_string(obj), cg_get_string(n->rvals[1]),
                    i, c_type(obj->has[i]),
                    c_rhs(n->rvals.v[4]));
          }
          // P_prim_setter's analyzer (fa.cc:1781) flows val
          // (rvals[4]) into the lvalue — the lvalue carries
          // val's type, matching Python's chained-assignment
          // semantics where `obj.attr = val` evaluates to val.
          // Emit the matching assignment when the lvalue is
          // live; cast through lvals[0]'s declared type so the
          // C compiler accepts the move. See issue 011.
          if (n->lvals[0]->live)
            fprintf(fp, "  %s = (%s)%s;\n", cg_get_string(n->lvals[0]), c_type(n->lvals[0]),
                    c_rhs(n->rvals.v[4]));
          goto Lsetter_found;
        }
      }
      fail("setter not resolved");
    Lsetter_found:
      break;
    }
    case P_prim_apply: {
      assert(!"unimplemented");
      fputs("  ", fp);
      int incomplete = 0;  // n->lvals.n && n->lvals[0]->type->var &&
                           // n->lvals.v[0]->type->var->def == n;
      if (incomplete) {
        fprintf(fp, "Fmake_apply(%s, ", cg_get_string(n->lvals[0]));
        write_c_type(fp, n->lvals[0]);
        fprintf(fp, ", %s, %s);\n", cg_get_string(n->rvals[0]), cg_get_string(n->rvals.v[2]));
      } else {
        if (n->lvals.n) {
          assert(n->lvals.n == 1);
          fprintf(fp, "%s = ", cg_get_string(n->lvals[0]));
        }
        int depth = application_depth(n);
        write_c_apply_arg(fp, cg_get_string(n->rvals[0]), depth, 0);
        fputs("((T__symbol) 0", fp);
        for (int i = 1; i < depth; i++) {
          fputs(", ", fp);
          write_c_apply_arg(fp, cg_get_string(n->rvals[0]), depth, i);
        }
        fputs(", ", fp);
        fputs(cg_get_string(n->rvals[2]), fp);
        fputs(");\n", fp);
      }
      break;
    }
    case P_prim_index_object: {
      fputs("  ", fp);
      assert(n->lvals.n == 1);
      Sym *t = n->rvals[o]->type;
      Sym *e = n->lvals[0]->type;
      if (t->is_vector) {
        fprintf(fp, "%s = %s->v[%s];\n", cg_get_string(n->lvals[0]), cg_get_string(n->rvals[o]), cg_get_string(n->rvals[o + 1]));
      } else if (t->type_kind != Type_RECORD || !n->rvals[o + 1]->sym->constant) {
        if (n->lvals[0]->live) fprintf(fp, "%s = ", cg_get_string(n->lvals[0]));
        if (sym_string->specializers.set_in(t))
          fprintf(fp, "_CG_char_from_string(%s,%s);\n", cg_get_string(n->rvals[o]), cg_get_string(n->rvals[o + 1]));
        else {
          fprintf(fp, "((%s", cg_get_string(e));
          for (int i = o + 1; i < n->rvals.n; i++) fprintf(fp, "*");
          if (t->type_kind == Type_RECORD)
            fprintf(fp, ")(%s))", cg_get_string(n->rvals[o]));
          else
            fprintf(fp, ")(_CG_list_ptr(%s)))", cg_get_string(n->rvals[o]));
          for (int i = o + 1; i < n->rvals.n; i++) {
            if (i != o + 1) fputs(", ", fp);
            fprintf(fp, "[%s-%d]", cg_get_string(n->rvals[i]), fa->tuple_index_base);
          }
          fprintf(fp, ";\n");
        }
      } else {
        if (fruntime_errors && t->type_kind == Type_RECORD && !t->has.n)
          fputs("assert(!\"runtime error: bad getter\");\n", fp);
        else
          fprintf(fp, "%s = ((%s)%s)->e%s;\n", cg_get_string(n->lvals[0]), cg_get_string(t), cg_get_string(n->rvals[o]),
                  n->rvals[o + 1]->sym->constant);
      }
      break;
    }
    case P_prim_set_index_object: {
      fputs("  ", fp);
      Sym *t = n->rvals[o]->type;
      if (t->is_vector) {
        fprintf(fp, "%s->v[%s] = %s;\n", cg_get_string(n->rvals[o]), cg_get_string(n->rvals[o + 1]),
                cg_get_string(n->rvals[n->rvals.n - 1]));
      } else if (t->type_kind != Type_RECORD || !n->rvals[o + 1]->sym->constant) {
        fprintf(fp, "((%s", cg_get_string(n->lvals[0]->type));
        for (int i = o + 1; i < n->rvals.n - 1; i++) fprintf(fp, "*");
        if (t->type_kind == Type_RECORD)
          fprintf(fp, ")(%s))", cg_get_string(n->rvals[o]));
        else
          fprintf(fp, ")(_CG_list_ptr(%s)))", cg_get_string(n->rvals[o]));
        for (int i = o + 1; i < n->rvals.n - 1; i++) {
          if (!fa->tuple_index_base)
            fprintf(fp, "[%s]", cg_get_string(n->rvals[i]));
          else
            fprintf(fp, "[%s-%d]", cg_get_string(n->rvals[i]), fa->tuple_index_base);
        }
        fprintf(fp, " = %s;\n", cg_get_string(n->rvals[n->rvals.n - 1]));
      } else {
        fprintf(fp, "((%s)%s)->e%s = %s;\n", cg_get_string(t), cg_get_string(n->rvals[o]), n->rvals[o + 1]->sym->constant,
                cg_get_string(n->rvals[n->rvals.n - 1]));
      }
      break;
    }
    case P_prim_new: {
      fputs("  ", fp);
      assert(n->lvals.n == 1);
      fprintf(fp, "%s = ", cg_get_string(n->lvals[0]));
      fprintf(fp, "_CG_prim_new(%s);\n", cg_get_string(n->lvals[0]->type));
      break;
    }
    case P_prim_assign: {
      fputs("  ", fp);
      fprintf(fp, "%s = (%s)", cg_get_string(n->lvals[0]), num_string(n->lvals.v[0]->type));
      fprintf(fp, "%s;\n", cg_get_string(n->rvals[3]));
      break;
    }
    case P_prim_len: {
      fputs("  ", fp);
      if (n->lvals.n && cg_get_string(n->lvals[0])) fprintf(fp, "%s = ", cg_get_string(n->lvals[0]));
      Sym *t = n->rvals[o]->type;
      if (sym_string->specializers.set_in(t))
        fprintf(fp, "_CG_string_len(%s);\n", cg_get_string(n->rvals[o]));
      else {
        assert(cg_get_string(n->rvals[o]));
        fprintf(fp, "_CG_prim_len(%s,%s);\n", cg_get_string(n->rvals[o - 1]), cg_get_string(n->rvals[o]));
      }
      break;
    }
    case P_prim_clone_vector:
    case P_prim_clone: {
      fputs("  ", fp);
      assert(n->lvals.n == 1);
      cchar *dst_t = cg_get_string(n->lvals[0]->type);
      if (cg_get_string(n->lvals[0])) fprintf(fp, "%s = ", cg_get_string(n->lvals[0]));
      if (n->prim->index == P_prim_clone) {
        // Issue 026: use destination-sized clone.  When the
        // source prototype's per-CS struct is smaller than
        // the destination instance's (which happens for
        // classes with >1 self-typed field where the proto
        // never receives field writes), the destination's
        // size must drive the GC_MALLOC.
        fprintf(fp, "(%s)_CG_prim_clone_dst(%s, ", dst_t, dst_t);
      } else
        fprintf(fp, "(%s)_CG_prim_clone_vector(", dst_t);
      for (int i = 2; i < n->rvals.n; i++) {
        if (i > 2) fprintf(fp, ", ");
        fputs(cg_get_string(n->rvals[i]), fp);
      }
      fputs(");\n", fp);
      // After cloning, populate any method pointer slots for polymorphic dispatch.
      if (n->prim->index == P_prim_clone && cg_get_string(n->lvals[0])) {
        Vec<PolymorphicSlot> *pslots = new_to_val_map.get(f);
        if (pslots) {
          cchar *dst_t = cg_get_string(n->lvals[0]->type);
          for (int si = 0; si < pslots->n; si++) {
            int slot = (*pslots)[si].slot;
            Fun *fun_val = (*pslots)[si].fun_val;
            if (!cg_field_live(n->lvals[0]->type, slot)) continue;
            if (!cg_get_string(fun_val)) continue;
            fprintf(fp, "  ((%s)%s)->e%d = (void*)%s;\n",
                    dst_t, cg_get_string(n->lvals[0]), slot, cg_get_string(fun_val));
          }
        }
      }
      break;
    }
    case P_prim_sizeof: {
      if (n->lvals.n) {
        assert(n->lvals.n == 1);
        if (cg_get_string(n->lvals[0]))
          fprintf(fp, "  %s = ", cg_get_string(n->lvals[0]));
        else
          fprintf(fp, "  ");
      } else
        fprintf(fp, "  ");
      Sym *t = n->rvals[o]->type;
      fprintf(fp, "%d;\n", t->size);
      break;
    }
    case P_prim_sizeof_element: {
      if (n->lvals.n) {
        assert(n->lvals.n == 1);
        if (cg_get_string(n->lvals[0]))
          fprintf(fp, "  %s = ", cg_get_string(n->lvals[0]));
        else
          fprintf(fp, "  ");
      } else
        fprintf(fp, "  ");
      Sym *t = n->rvals[o]->type;
      if (!t->element->type->size && t->type_kind == Type_RECORD) {
        if (t->has.n)
          fprintf(fp, "%d;\n", t->has[0]->type->size);
        else
          fprintf(fp, "0;\n");
      } else
        fprintf(fp, "%d;\n", t->element->type->size);
      break;
    }
    case P_prim_primitive: {
      if (n->lvals.n) {
        assert(n->lvals.n == 1);
        if (cg_get_string(n->lvals[0]))
          fprintf(fp, "  %s = ", cg_get_string(n->lvals[0]));
        else
          fprintf(fp, "  ");
      } else
        fprintf(fp, "  ");
      cchar *name = n->rvals[1]->sym->name;
      if (!name) name = n->rvals[1]->sym->constant;
      if (!strcmp("print", name))
        cg_writeln(fp, n->rvals, 0);
      else if (!strcmp("println", name))
        cg_writeln(fp, n->rvals, 1);
      else {
        RegisteredPrim *p = prim_get(name);
        if (p && p->cgfn)
          p->cgfn(fp, n, f);
        else {
          fprintf(fp, "_CG_%s_%s(", n->prim->name, name);
          bool first = true;
          for (int i = 2; i < n->rvals.n; i++) {
            if (cg_get_string(n->rvals[i])) {
              if (!first) fprintf(fp, ", ");
              fputs(cg_get_string(n->rvals[i]), fp);
              first = false;
            }
          }
          fputs(");\n", fp);
        }
      }
      break;
    }
    case P_prim_destruct:
      for (int i = 0; i < n->lvals.n; i++) destruct_prim(fp, n->lvals.v[i], n->rvals.v[o + i]);
      break;
  }
  return 1;
}

// C-backend wrapper around get_target_fun_core (codegen_common.{h,cc}).
// When no single resolution is available, the C backend `fail`s in
// the absence of `fruntime_errors`; the LLVM backend has its own
// (search-by-sym, search-by-name) wrapper instead.
static Fun *get_target_fun(PNode *n, Fun *f) {
  Fun *target = get_target_fun_core(n, f);
  if (!target) {
    if (!fruntime_errors) fail("unable to resolve to a single function at call site");
  }
  return target;
}

static void simple_move(FILE *fp, Var *lhs, Var *rhs) {
  if (!lhs->live) return;
  if (lhs->sym->type_kind || rhs->sym->type_kind) return;
  if (!cg_get_string(lhs) || !cg_get_string(rhs)) return;
  if (rhs->type == sym_void->type || lhs->type == sym_void->type) return;
  // If FA collapsed lhs's type to nil_type (`sym_nil_type`)
  // the global-var emitter in c_codegen_print_c sets its
  // cg_string to the literal "NULL" — no storage, no lvalue.
  // Without this guard, `simple_move` would emit
  // `NULL = NULL;` which is invalid C.  The MOVE itself is
  // semantically a no-op (writing None into a single-None-
  // value Var) so dropping it is sound.  Mirrors
  // `is_const_folded_send` for SEND PNodes.  Same logic
  // covers any other future case where a Var's cg_string
  // becomes a non-lvalue literal — `get_constant` returns
  // non-null for Vars folded to a single constant value.
  if (lhs->type == sym_nil_type || get_constant(lhs)) return;
  if (!rhs->sym->fun) {
    ASSERT(cg_get_string(rhs));
    if (rhs->type != lhs->type)
      fprintf(fp, "  %s = (%s)%s;\n", cg_get_string(lhs), c_type(lhs), cg_get_string(rhs));
    else
      fprintf(fp, "  %s = %s;\n", cg_get_string(lhs), cg_get_string(rhs));
  } else if (cg_get_string(rhs))
    fprintf(fp, "  %s = (_CG_function)&%s;\n", cg_get_string(lhs), cg_get_string(rhs));
  else
    fprintf(fp, "  %s = NULL;\n", cg_get_string(lhs));
}

static int write_c_fun_arg(FILE *fp, Fun *f, char *s, char *e, Sym *sym, int i, int &wrote_one) {
  if (!i && sym->type_kind == Type_FUN && !sym->fun && sym->has.n) {
    assert(0);
    for (int i = 0; i < sym->type->has.n; i++) {
      if (i) fprintf(fp, ", ");
      snprintf(e, 4096 - (e - s), "->e%d", i);
      write_c_fun_arg(fp, f, s, s + strlen(s), sym->has[i], i, wrote_one);
      fputs(s, fp);
    }
  } else {
    if (wrote_one) fprintf(fp, ", ");
    wrote_one = 1;
    Fun *f = sym->type->fun;
    if (f)
      fprintf(fp, "((_CG_function)&%s)", cg_get_string(f));
    else {
      snprintf(e, 4096 - (e - s), "->e%d", i);
      fputs(s, fp);
    }
  }
  return 1;
}

// is_closure_var(Var*) — moved to codegen_common.{h,cc}.

static void write_arg_position(FILE *fp, MPosition *p) {
  for (int i = 0; i < p->pos.n; i++) {
    if (is_intPosition(p->pos[i])) {
      if (!i)
        fprintf(fp, "a%d", (int)Position2int(p->pos[i]) - 1);
      else
        fprintf(fp, "->e%d", (int)Position2int(p->pos[i]) - 1);
    }
  }
}

static void write_send_arg(FILE *fp, Fun *f, PNode *n, MPosition *p, int &wrote_one) {
  int i = Position2int(p->pos[0]) - 1;
  Var *v0 = n->rvals[0];
  if (is_closure_var(v0)) {
    if (i < v0->type->has.n) {
      char ss[4096];
      snprintf(ss, sizeof(ss), "(%s)%s", c_type(f->args.get(p)), cg_get_string(v0));
      char *ee = ss + strlen(ss);
      write_c_fun_arg(fp, f, ss, ee, v0->type->has[i], i, wrote_one);
      return;
    } else
      i -= v0->type->has.n - 1;
  }
  Var *v = n->rvals[i];
  if (p->pos.n <= 1) {
    if (wrote_one) fprintf(fp, ", ");
    wrote_one = 1;
    // Voidish-arg cast: if the callsite arg's C type is
    // `_CG_any` / `_CG_void` / `_CG_nil_type` (because FA
    // inferred a union receiver that resolves to one of
    // those at the cg layer) but the formal expects a
    // typed pointer, emit an explicit cast.  FA's dispatch
    // already proved the runtime value is of formal's type
    // — the C source just lost track of the concrete type
    // through an intermediate move or union-field projection.
    // See issue 028 step 5 / issue 029.
    Var *formal = f->args.get(p);
    cchar *formal_t = formal ? c_type(formal) : nullptr;
    cchar *arg_cg = c_rhs(v);
    cchar *arg_t = c_type(v);
    bool arg_is_voidish = arg_t && (!strcmp(arg_t, "_CG_any") ||
                                    !strcmp(arg_t, "_CG_void") ||
                                    !strcmp(arg_t, "_CG_nil_type"));
    bool formal_is_voidish = formal_t && (!strcmp(formal_t, "_CG_any") ||
                                          !strcmp(formal_t, "_CG_void") ||
                                          !strcmp(formal_t, "_CG_nil_type"));
    if (arg_is_voidish && !formal_is_voidish) {
      fprintf(fp, "(%s)%s", formal_t, arg_cg);
    } else {
      fputs(arg_cg, fp);
    }
  } else {
#if 0
    // These are inside tuples: potentially we could unwrap the tuple and pass the arguments directly.
    fputs(c_rhs(v), fp);
    for (int i = 1; i < p->pos.n; i++) 
      if (is_intPosition(p->pos[i])) {
        fprintf(fp, "->e%d", (int)Position2int(p->pos[i])-1);
      }
#endif
  }
}

class CBackendEmitter : public VirtualCGEmitter {
  FILE *fp;
  FA *fa;
  Fun *f;
 public:
  CBackendEmitter(FILE *fp, FA *fa, Fun *f) : fp(fp), fa(fa), f(f) {}

  void emit_move(PNode *pn) override {
    for (int i = 0; i < pn->lvals.n; i++) simple_move(fp, pn->lvals[i], pn->rvals.v[i]);
  }

  // Route all primitives through write_c_prim's switch in one shot.
  // Primitives not in the switch (is, isinstance) return false here and
  // fall through to emit_send_is below.
  bool emit_send_any_prim(PNode *pn) override { return write_c_prim(fp, fa, f, pn) != 0; }

  bool emit_send_is(PNode *pn) override {
    if (pn->prim->index == P_prim_is && pn->rvals.n >= 4) {
      if (pn->lvals.n && cg_get_string(pn->lvals[0])) {
        cchar *lhs = cg_get_string(pn->rvals[2]);
        if (!lhs) lhs = cg_get_string(pn->rvals[2]->sym);
        cchar *rhs = cg_get_string(pn->rvals[3]);
        if (!rhs) rhs = cg_get_string(pn->rvals[3]->sym);
        fprintf(fp, "  %s = ((void *)%s == (void *)%s);\n", cg_get_string(pn->lvals[0]), lhs ? lhs : "NULL", rhs ? rhs : "NULL");
      } else {
        fputs("  ;\n", fp);
      }
      return true;
    }
    if (pn->prim->index == P_prim_isinstance && pn->rvals.n >= 4 && pn->rvals[3]->sym == sym_nil_type) {
      if (pn->lvals.n && cg_get_string(pn->lvals[0])) {
        cchar *opnd = cg_get_string(pn->rvals[2]);
        if (!opnd) opnd = cg_get_string(pn->rvals[2]->sym);
        fprintf(fp, "  %s = (%s == NULL);\n", cg_get_string(pn->lvals[0]), opnd ? opnd : "NULL");
      } else {
        fputs("  ;\n", fp);
      }
      return true;
    }
    return false;
  }
  bool emit_send_default_prim(PNode *pn) override {
    fputs("  ", fp);
    if (pn->lvals.n && cg_get_string(pn->lvals[0])) {
      fprintf(fp, "%s = ", cg_get_string(pn->lvals[0]));
    }
    fprintf(fp, "_CG_%s(", pn->prim->name);
    int comma = 0;
    int start = 1;
    if (pn->rvals[0]->sym == sym_primitive) start = 2;
    for (int i = start; i < pn->rvals.n; i++) {
      cchar *s = cg_get_string(pn->rvals[i]);
      if (!s) s = cg_get_string(pn->rvals[i]->sym);
      assert(s);
      if (comma) fprintf(fp, ", ");
      comma = 1;
      if (pn->prim->index == P_prim_isinstance && i == 3) {
        fprintf(fp, "&_CG_type_%s", s);
      } else {
        fputs(s, fp);
      }
    }
    fputs(");\n", fp);
    return true;
  }

  void emit_send_call(PNode *pn) override {
    Fun *target = get_target_fun(pn, f);
    if (target) {
      fputs("  ", fp);
      if (pn->lvals.n && cg_get_string(pn->lvals[0])) {
        fprintf(fp, "%s = ", cg_get_string(pn->lvals[0]));
      }
      fputs(cg_get_string(target), fp);
      fputs("(", fp);
      int wrote_one = 0;
      for (MPosition *p : target->positional_arg_positions) {
        Var *av = target->args.get(p);
        if (!av->live) continue;
        write_send_arg(fp, target, pn, p, wrote_one);
      }
      fputs(");\n", fp);
    } else {
      // Polymorphic dispatch through a method pointer slot (vtable).
      //
      // Call PNode rvals layout (for a method call like `right.val()`):
      //   rvals[0] = method symbol (e.g. "val"), sym->name="val", def=nil
      //   rvals[i] = receiver (self) at index i = Position2int(self_pos) - 1
      //
      // Strategy: find the first live positional arg of any callee, derive
      // the rvals index from its position, get the receiver cg-string, find
      // the method slot in that arg's concrete type, and emit an indirect call
      // through `((recv_type)(void*)recv)->eN`.
      Vec<Fun *> *fns = f->calls.get(pn);
      if (fns && fns->n > 1) {
        // Find receiver index, concrete type, slot, and return type from any callee.
        int slot = -1;
        int recv_idx = -1;
        cchar *recv_type_str = nullptr;
        cchar *ret_type_str = nullptr;
        for (int fi = 0; fi < fns->n && slot < 0; fi++) {
          Fun *fun_val = (*fns)[fi];
          if (!fun_val || !fun_val->sym || !fun_val->sym->name) continue;
          cchar *method_name = fun_val->sym->name;
          MPosition argp; argp.push(1);
          for (int pi = 0; pi < fun_val->sym->has.n + 2 && slot < 0; pi++) {
            MPosition *cp = cannonicalize_mposition(argp);
            argp.inc();
            Var *argv = fun_val->args.get(cp);
            if (!argv || !argv->live || !argv->type) continue;
            int ridx = (int)Position2int(cp->pos[0]) - 1;
            if (ridx < 0 || ridx >= pn->rvals.n) continue;
            Var *recv_var = pn->rvals[ridx];
            if (!recv_var || !cg_get_string(recv_var)) continue;
            Sym *csym = argv->type;
            for (int k = 0; k < csym->has.n; k++) {
              if (csym->has[k] && csym->has[k]->name == method_name && cg_field_live(csym, k)) {
                slot = k;
                recv_idx = ridx;
                recv_type_str = cg_get_string(csym);
                if (fun_val->rets.n && fun_val->rets[0]) ret_type_str = c_type(fun_val->rets[0]);
                break;
              }
            }
            if (slot >= 0) break;
          }
        }
        if (slot >= 0 && recv_idx >= 0 && recv_type_str && ret_type_str) {
          cchar *recv_str = cg_get_string(pn->rvals[recv_idx]);
          fputs("  ", fp);
          if (pn->lvals.n && cg_get_string(pn->lvals[0]))
            fprintf(fp, "%s = ", cg_get_string(pn->lvals[0]));
          fprintf(fp, "((%s(*)(void*))((%s)(void*)%s)->e%d)((void*)%s);\n",
                  ret_type_str, recv_type_str, recv_str, slot, recv_str);
          return;
        }
      }
      fputs("  assert(!\"runtime error: matching function not found\");\n", fp);
    }
  }
};

static void do_phy_nodes(FILE *fp, PNode *n, int isucc) {
  for (PNode *p : n->phy) simple_move(fp, p->lvals[isucc], p->rvals.v[0]);
}

static void do_phi_nodes(FILE *fp, PNode *n, int isucc) {
  if (n->cfg_succ.n) {
    PNode *succ = n->cfg_succ[isucc];
    if (succ->phi.n) {
      int i = succ->cfg_pred_index.get(n);
      for (PNode *pp : succ->phi) simple_move(fp, pp->lvals[0], pp->rvals.v[i]);
    }
  }
}

// is_const_folded_send moved to codegen_common.cc

static void write_c_pnode(FILE *fp, FA *fa, Fun *f, PNode *n, Vec<PNode *> &done) {
  if (n->live && n->fa_live) switch (n->code->kind) {
      case Code_LABEL:
        fprintf(fp, " L%d:;\n", n->code->label[0]->id);
        break;
      case Code_MOVE: {
        CBackendEmitter emitter(fp, fa, f);
        emitter.emit_move(n);
        break;
      }
      case Code_SEND: {
        if (n->prim && n->prim->index == P_prim_reply) {
          fprintf(fp, "  return %s;\n", c_rhs(n->rvals[3]));
        } else {
          CBackendEmitter emitter(fp, fa, f);
          virtual_cg_emit_send(&emitter, n);
        }
        break;
      }
      case Code_IF:
      case Code_GOTO:
        break;
      default:
        assert(!"case");
    }
  switch (n->code->kind) {
    case Code_IF:
      if (n->live && n->fa_live) {
        if (n->rvals[0]->sym == fa->type_world.true_type->v[0]->sym) {
          do_phy_nodes(fp, n, 0);
          do_phi_nodes(fp, n, 0);
          if (done.set_add(n->cfg_succ[0])) write_c_pnode(fp, fa, f, n->cfg_succ[0], done);
        } else if (n->rvals[0]->sym == fa->type_world.false_type->v[0]->sym) {
          do_phy_nodes(fp, n, 1);
          do_phi_nodes(fp, n, 1);
          if (done.set_add(n->cfg_succ[1])) write_c_pnode(fp, fa, f, n->cfg_succ[1], done);
        } else {
          fprintf(fp, "  if (%s) {\n", cg_get_string(n->rvals[0]));
          do_phy_nodes(fp, n, 0);
          do_phi_nodes(fp, n, 0);
          if (done.set_add(n->cfg_succ[0]))
            write_c_pnode(fp, fa, f, n->cfg_succ[0], done);
          else
            fprintf(fp, "  goto L%d;\n", n->code->label[0]->id);
          fprintf(fp, "  } else {\n");
          do_phy_nodes(fp, n, 1);
          do_phi_nodes(fp, n, 1);
          if (done.set_add(n->cfg_succ[1]))
            write_c_pnode(fp, fa, f, n->cfg_succ[1], done);
          else
            fprintf(fp, "  goto L%d;\n", n->code->label[1]->id);
          fputs("  }\n", fp);
        }
      } else {
        do_phy_nodes(fp, n, 0);
        do_phi_nodes(fp, n, 0);
      }
      break;
    case Code_GOTO:
      do_phi_nodes(fp, n, 0);
      if (n->live && n->fa_live) fprintf(fp, "  goto L%d;\n", n->code->label[0]->id);
      break;
    case Code_SEND:
      if ((!n->live || !n->fa_live) && n->prim && n->prim->index == P_prim_reply)
        fprintf(fp, "  return 0;\n");
      else
        do_phi_nodes(fp, n, 0);
      break;
    default:
      do_phi_nodes(fp, n, 0);
      break;
  }
  int extra_goto = n->cfg_succ.n == 1 && n->code->kind != Code_GOTO;
  for (PNode *p : n->cfg_succ) if (done.set_add(p)) {
    write_c_pnode(fp, fa, f, p, done);
    extra_goto = 0;
  }
  if (extra_goto && n->cfg_succ[0]->live && n->cfg_succ[0]->fa_live) {
    if (n->cfg_succ[0]->code->kind == Code_LABEL)
      fprintf(fp, "  goto L%d;\n", n->cfg_succ[0]->code->label[0]->id);
  }
}

static void write_c_args(FILE *fp, Fun *f) {
  for (MPosition *p : f->positional_arg_positions) {
    Var *v = f->args.get(p);
    Sym *s = v->sym;
    if (cg_get_string(v) && !s->is_symbol && !s->is_fun && v->live) {
      fprintf(fp, "  %s = ", cg_get_string(v));
      write_arg_position(fp, p);
      fprintf(fp, ";\n");
    }
  }
}

static bool lt_type_id(Var *a, Var *b) {
  int ta = a->type ? a->type->id : -1;
  int tb = b->type ? b->type->id : -1;
  return (ta < tb);
}

static void write_c(FILE *fp, FA *fa, Fun *f, Vec<Var *> *globals = 0) {
  if (!f->live) return;
  fputs("\n", fp);
  write_c_fun_proto(fp, f);
  if (!f->entry) {
    fputs(" { }\n", fp);
    return;
  }
  fputs(" {\n", fp);
  int index = 0;
  Vec<Var *> vars, defs;
  f->collect_Vars(vars, 0, FUN_COLLECT_VARS_NO_TVALS);
  for (Var *v : vars) if (v->sym->is_local || v->sym->is_fake) cg_set_string(v, 0);
  for (Var *v : vars) if (!v->is_internal && !v->sym->is_fake) {
    if (!cg_get_string(v) && v->live && !v->sym->is_symbol && v->type != sym_continuation) {
      char s[100];
      snprintf(s, sizeof(s), "t%d", index++);
      cg_set_string(v, dupstr(s));
      defs.add(v);
    }
  }
  defs.qsort(lt_type_id);
  Sym *last_t = (Sym *)-1;
  for (Var *v : defs) {
    if (v->type != last_t) {
      if (last_t != (Sym *)-1) fprintf(fp, ";\n");
      fputs("  ", fp);
      write_c_type(fp, v);
      fprintf(fp, " %s", cg_get_string(v));
    } else
      fprintf(fp, ", %s", cg_get_string(v));
    last_t = v->type;
  }
  if (defs.n) fprintf(fp, ";\n\n");
  if (globals)
    for (Var *v : *globals) if (!v->sym->is_fun && v->sym->fun && !v->sym->type_kind && cg_get_string(v))
        fprintf(fp, "  %s = %s;\n", cg_get_string(v), cg_get_string(v->sym->fun));
  write_c_args(fp, f);
  rebuild_cfg_pred_index(f);
  Vec<PNode *> done;
  done.set_add(f->entry);
  write_c_pnode(fp, fa, f, f->entry, done);
  fputs("}\n", fp);
}

static inline bool homogeneous_tuple(Sym *s) {
  for (int i = 0; i < s->has.n - 1; i++)
    for (int j = i; j < s->has.n; j++)
      if (s->has[i]->type != s->has[j]->type) return false;
  return true;
}

// Returns nothing; failure paths inside use fail() directly.
// (Historically returned int; the caller's `< 0` check was dead.)
static void build_type_strings(FILE *fp, FA *fa, Vec<Var *> &globals) {
// build builtin map
#define S(_n) cg_set_string(if1_get_builtin(fa->pdb->if1, #_n), "_CG_" #_n);
#include "builtin_symbols.h"
#undef S
  // assign functions a C type string
  // Assign each live Fun a cg_string (with /*name*/ annotation, the
  // C-backend convention) and collect their sym->var into globals.
  // See codegen_common.{h,cc}.
  assign_fun_cg_strings(fa, /*annotate=*/true, &globals);
  Vec<Sym *> allsyms;
  collect_types_and_globals(fa, allsyms, globals);
  // assign creation sets C type strings. Pass1 emits forward decls
  // for Type_RECORD types alongside the cg_string assignment; pass2
  // resolves fun/symbol/sum-type aliases.
  if (allsyms.n) fputs("/*\n Type Declarations\n*/\n\n", fp);
  assign_type_cg_strings_pass1(allsyms, fp);
  assign_type_cg_strings_pass2(allsyms);
  if (allsyms.n) fputs("\n", fp);
  // define function types and prototypes
  Vec<Fun *> live_funs;
  for (Fun *f : fa->funs) {
    if (!f->live) continue;
    live_funs.add(f);
  }
  if (live_funs.n) fputs("/*\n Function Prototypes\n*/\n\n", fp);
  if (live_funs.n) {
    fprintf(fp, "typedef _CG_function %s", cg_get_structural_string(live_funs[0]));
    for (int i = 1; i < live_funs.n; i++) fprintf(fp, ", %s", cg_get_structural_string(live_funs[i]));
    fprintf(fp, ";\n");
  }
  for (Fun *f : live_funs) {
    write_c_fun_proto(fp, f);
    fputs(";\n", fp);
  }
  if (live_funs.n) fputs("\n", fp);
  // define structs
  if (allsyms.n) fputs("/*\n Type Definitions\n*/\n\n", fp);
  for (Sym *s : allsyms) {
    switch (s->type_kind) {
      default:
        break;
      case Type_FUN:
        if (s->fun) break;
      // fall through
      case Type_RECORD: {
        if (s->has.n) {
          fprintf(fp, "struct _CG_s%d {\n", s->id);
          // Issue 026: emit only live fields, keeping the
          // has-index `i` as the eN suffix.  Dead fields
          // leave gaps in the numbering — but the setter,
          // getter, and other field-access sites use the
          // SAME has-index `i` for their eN suffix and
          // elide accesses to dead fields (via
          // cg_field_live).  The struct definition and
          // field-access codegen stay consistent.
          for (int i = 0; i < s->has.n; i++) {
            if (!cg_field_live(s, i)) continue;
            fputs("  ", fp);
            fputs(c_type(s->has[i]), fp);
            fprintf(fp, " e%d;", i);
            if (s->has[i]->name) fprintf(fp, " /* %s */;", s->has[i]->name);
            fputs("\n", fp);
          }
          if (s->is_vector) {
            fputs("  ", fp);
            fputs(c_type(s->element), fp);
            fprintf(fp, " v[0];");
          }
          fprintf(fp, "};\n");
        }
      }
    }
  }
  if (allsyms.n) fputs("\n/*\n Type Objects\n*/\n\n", fp);
  std::set<std::string> emitted_types;
  for (Sym *s : allsyms) {
    if (s->type_kind == Type_RECORD && !s->is_system_type) {
      if (emitted_types.find(s->name) == emitted_types.end()) {
        fprintf(fp, "_CG_TypeObject _CG_type_%s = { PYC_TAG_OBJECT, \"%s\" };\n", s->name, s->name);
        emitted_types.insert(s->name);
      }
    }
  }

  if (allsyms.n) fputs("\n/*\n Builtin Functions\n*/\n\n", fp);
  for (Sym *s : allsyms) {
    if (s->type_kind == Type_RECORD && s->creators.n && s->creators[0]->sym == sym_list && homogeneous_tuple(s) &&
        s->has.n)
      fprintf(fp, "_CG_TUPLE_TO_LIST_FUN(%d, %d);\n", s->id, s->has.n);
  }
}

// Populate new_to_val_map:
//   For each live function that appears at any poly call site (method name
//   with fns->n > 1 anywhere in the program), find its self-arg concrete type,
//   discover the slot for its name in that type, trace the FA creation chain
//   from that arg's AType through cs->defs to the creator function, and register
//   creator → (slot, fun_val).
//
// Unlike the previous approach (only scanning fa_send_PNodes of original
// functions), this scans ALL fun->calls entries (including cloned functions)
// to collect poly method names, then iterates ALL live functions with those
// names. This catches val clones that only appear at monomorphic call sites
// within specialized callers but participate in vtable dispatch overall.
static void build_new_to_val_map(FA *fa) {
  new_to_val_map.clear();

  // Pass 1: collect method names that appear at any poly call site.
  Vec<cchar *> poly_names;
  for (Fun *f : fa->funs) {
    if (!f->live) continue;
    for (int ci = 0; ci < f->calls.n; ci++) {
      if (!f->calls[ci].key) continue;
      Vec<Fun *> *fns = f->calls[ci].value;
      if (!fns || fns->n <= 1) continue;
      for (Fun *fv : *fns)
        if (fv && fv->sym && fv->sym->name) poly_names.set_add(fv->sym->name);
    }
  }

  // Pass 2: for every live function whose name is a poly method, find its
  // self arg, the method slot in that arg's concrete type, and register all
  // creators of self with this function.
  for (Fun *fun_val : fa->funs) {
    if (!fun_val->live || !fun_val->sym || !fun_val->sym->name) continue;
    if (!poly_names.set_in(fun_val->sym->name)) continue;
    cchar *method_name = fun_val->sym->name;

    // Find the self-arg position and slot.
    int slot = -1;
    MPosition *self_cp = nullptr;
    {
      MPosition argp; argp.push(1);
      for (int pi = 0; pi < fun_val->sym->has.n + 2 && slot < 0; pi++) {
        MPosition *cp = cannonicalize_mposition(argp);
        argp.inc();
        Var *v = fun_val->args.get(cp);
        if (!v || !v->live || !v->type) continue;
        Sym *csym = v->type;
        for (int k = 0; k < csym->has.n; k++) {
          if (csym->has[k] && csym->has[k]->name == method_name && cg_field_live(csym, k)) {
            slot = k;
            self_cp = cp;
            break;
          }
        }
        if (slot >= 0) break;
      }
    }
    if (slot < 0 || !self_cp) continue;

    // Walk every EntrySet for fun_val; look only at the self arg's AType.
    // Track specificity = sorted.n of the ES: lower means more specific.
    // When multiple val clones compete for the same (creator, slot), the
    // most-specific one (smallest sorted.n) wins — FA is conservative and
    // may include extra CSes in the self AType of less-specific clones.
    for (EntrySet *es : fun_val->ess) {
      AVar *self_av = nullptr;
      for (int j = 0; j < es->args.n; j++) {
        if (es->args.v[j].key == self_cp) { self_av = es->args.v[j].value; break; }
      }
      if (!self_av || !self_av->out) continue;
      int specificity = self_av->out->sorted.n;  // fewer CSes = more specific
      for (CreationSet *cs : self_av->out->sorted) {
        if (!cs) continue;
        for (AVar *def_av : cs->defs) {
          if (!def_av || !def_av->contour_is_entry_set) continue;
          EntrySet *creator_es = (EntrySet *)def_av->contour;
          Fun *fun_new = creator_es->fun;
          if (!fun_new || !fun_new->live) continue;
          Vec<PolymorphicSlot> *slots = new_to_val_map.get(fun_new);
          if (!slots) { slots = new Vec<PolymorphicSlot>(); new_to_val_map.put(fun_new, slots); }
          // Find existing registration for this (slot) — replace if less specific.
          int existing = -1;
          for (int k = 0; k < slots->n; k++)
            if ((*slots)[k].slot == slot) { existing = k; break; }
          if (existing >= 0) {
            if ((*slots)[existing].fun_val == fun_val) continue;  // exact dup
            if (specificity < (*slots)[existing].specificity) {
              // More specific: replace existing registration.
              (*slots)[existing].fun_val = fun_val;
              (*slots)[existing].specificity = specificity;
            }
            // else: existing is equally or more specific, keep it
            continue;
          }
          PolymorphicSlot ps; ps.slot = slot; ps.fun_val = fun_val; ps.specificity = specificity;
          slots->add(ps);
        }
      }
    }
  }
}

void c_codegen_print_c(FILE *fp, FA *fa, Fun *init) {
  Vec<Var *> globals;
  int index = 0;
  if (!if1->callback->c_codegen_pre_file(fp)) fprintf(fp, "#include \"c_runtime.h\"\n\n");
  build_type_strings(fp, fa, globals);
  build_new_to_val_map(fa);
  if (globals.n) {
    fputs("\n/*\n Global Variables\n*/\n\n", fp);
  }
  for (Var *v : globals) if (v->sym->is_fun) cg_set_string(v, cg_get_string(v->sym->fun));
  for (Var *v : globals) {
    Sym *s = unalias_type(v->sym);
    if (!v->live) continue;
    if (v->type == sym_nil_type) {
      cg_set_string(v, "NULL");
      continue;
    }
    if (s->imm.const_kind != IF1_NUM_KIND_NONE && s->imm.const_kind != IF1_CONST_KIND_STRING) {
      char ss[100];
      sprint_imm(ss, sizeof(ss), s->imm);
      cg_set_string(v, dupstr(ss));
    } else if (s->constant) {
      if (v->type == sym_string) {
        char *x = escape_string(s->constant);
        cg_set_string(v, (char *)MALLOC(strlen(x) + 20));
        STRCPYZ(cg_get_string(v), "_CG_String(");
        STRCAT(cg_get_string(v), x);
        STRCAT(cg_get_string(v), ")");
      } else
        cg_set_string(v, s->constant);
    } else if (s->is_symbol) {
      char ss[100];
      snprintf(ss, sizeof(ss), "_CG_Symbol(%d, \"%s\")", s->id, s->name);
      cg_set_string(v, dupstr(ss));
    } else if (s->is_fun) {
    } else if (!s->type_kind || s->type_kind == Type_RECORD) {
      char ss[100];
      if (s->name)
        snprintf(ss, sizeof(ss), "/* %s %d */ g%d", s->name, s->id, index++);
      else
        snprintf(ss, sizeof(ss), "/* %d */ g%d", s->id, index++);
      cg_set_string(v, dupstrs(ss));
      write_c_type(fp, v);
      fputs(" ", fp);
      fputs(cg_get_string(v), fp);
      fputs(";\n", fp);
    } else {
      index++;
      cg_set_string(v, dupstr(s->name));
    }
  }
  fputs("\n/*\n Functions\n*/\n", fp);
  for (Fun *f : fa->funs) if (f != init && !f->is_external) write_c(fp, fa, f);
  write_c(fp, fa, init, &globals);
  fprintf(fp,
          "\nint main(int argc, char *argv[]) { (void)argc; (void) argv;\n"
          "  MEM_INIT();\n"
          "  %s();\n"
          "  return 0;\n"
          "}\n",
          cg_get_string(init));
}

void c_codegen_write_c(FA *fa, Fun *main, cchar *filename) {
  char fn[FILENAME_MAX];
  int n = snprintf(fn, sizeof(fn), "%s.c", filename);
  if (n < 0 || (size_t)n >= sizeof(fn)) fail("c_codegen_write_c: filename too long: %s", filename);
  FILE *fp = fopen(fn, "w");
  if (!fp) fail("c_codegen_write_c: unable to open %s for writing", fn);
  c_codegen_print_c(fp, fa, main);
  fclose(fp);
}

int c_codegen_compile(cchar *filename) {
  char target[FILENAME_MAX];
  int n = snprintf(target, sizeof(target), "%s", filename);
  if (n < 0 || (size_t)n >= sizeof(target)) fail("c_codegen_compile: filename too long: %s", filename);
  char *dot = strrchr(target, '.');
  if (!dot) fail("c_codegen_compile: filename has no extension: %s", filename);
  *dot = 0;

  // Build argv for posix_spawn (no shell, no quoting concerns).
  char makefile_arg[FILENAME_MAX];
  if (snprintf(makefile_arg, sizeof(makefile_arg), "%s/Makefile.cg", system_dir) >= (int)sizeof(makefile_arg))
    fail("c_codegen_compile: makefile path too long");
  char cg_root_arg[FILENAME_MAX];
  if (snprintf(cg_root_arg, sizeof(cg_root_arg), "CG_ROOT=%s", system_dir) >= (int)sizeof(cg_root_arg))
    fail("c_codegen_compile: CG_ROOT arg too long");
  char cg_target_arg[FILENAME_MAX];
  if (snprintf(cg_target_arg, sizeof(cg_target_arg), "CG_TARGET=%s", target) >= (int)sizeof(cg_target_arg))
    fail("c_codegen_compile: CG_TARGET arg too long");
  char cg_files_arg[FILENAME_MAX];
  if (snprintf(cg_files_arg, sizeof(cg_files_arg), "CG_FILES=%s.c", filename) >= (int)sizeof(cg_files_arg))
    fail("c_codegen_compile: CG_FILES arg too long");

  // posix_spawn's argv signature is `char *const argv[]`; we pass pointers
  // to local buffers, which is OK because spawnp dup()s them before exec.
  char *argv[16];
  int ai = 0;
  argv[ai++] = (char *)"make";
  argv[ai++] = (char *)"--no-print-directory";
  argv[ai++] = (char *)"-f";
  argv[ai++] = makefile_arg;
  argv[ai++] = cg_root_arg;
  argv[ai++] = cg_target_arg;
  argv[ai++] = cg_files_arg;
  if (codegen_optimize) argv[ai++] = (char *)"OPTIMIZE=1";
  if (codegen_debug) argv[ai++] = (char *)"DEBUG=1";
  argv[ai] = nullptr;
  return codegen_spawn("make", argv);
}
