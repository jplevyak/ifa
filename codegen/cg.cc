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
// PolymorphicSlot / cg_new_to_val_map / cg_build_new_to_val_map
// moved to codegen_common.{h,cc} (shared with the LLVM backend).

static void write_c_fun_proto(FILE *fp, Fun *f, int type = 0) {
  assert(f->rets.n == 1);
  if (f->sym->is_async)
    fputs("_CG_Coroutine", fp);
  else if (f->sym->is_generator)
    // issues/014: the real coroutine mechanics live in a local lambda
    // inside this function's body (write_c below); the function
    // itself is ordinary from here out, returning the raw coroutine
    // handle as a plain int64 -- regardless of what FA inferred for
    // f->rets[0] (see gen_fun_pyda's comment on the int64-typed
    // default reply this relies on).
    fputs("_CG_int64", fp);
  else
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

// cg_has_classtag / cg_field_live moved to codegen_common.{h,cc}
// (shared with the LLVM backend).

static cchar *c_rhs(Var *v) {
  if (!v->sym->is_fun) {
    if (!cg_get_string(v)) {
      // A constant-folded Var has no backing C variable -- its
      // consumers are expected to inline the literal. That
      // includes `return <result>` in a function called
      // INDIRECTLY through a stored method pointer
      // (ifa/issues/030 dispatch): the caller can't inline a
      // per-clone constant it can't see, so the callee must
      // return the literal rather than a bare "0". Mirror the
      // constant formatting used for global initializers.
      Sym *s = v->constant;
      if (s && s != sym_nil && v->type != sym_nil_type) {
        if (s->imm.const_kind != IF1_NUM_KIND_NONE && s->imm.const_kind != IF1_CONST_KIND_STRING) {
          char ss[100];
          sprint_imm(ss, sizeof(ss), s->imm);
          return dupstr(ss);
        }
        if (s->constant && v->type == sym_string) {
          char *x = escape_string(s->constant);
          char *r = (char *)MALLOC(strlen(x) + 20);
          STRCPYZ(r, "_CG_String(");
          STRCAT(r, x);
          STRCAT(r, ")");
          return r;
        }
      }
      return "0";
    } else
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
        Sym *elem = n->lvals[0]->type->element;
        // An always-empty tuple (n->rvals.n < 4, i.e. zero elements passed
        // to this "make" call) may never get its element type resolved by
        // FA at all -- `element` itself can be null here, not just
        // `element->type`. Treat "no element type" the same as "void
        // element type" rather than dereferencing a null Sym*.
        int voidish = n->rvals.n < 4 && (!elem || elem->type == sym_void);
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
    case P_prim_await: {
      if (virtual_cg_is_const_folded_send(n)) return 1;
      fputs("  ", fp);
      if (n->lvals.n && cg_get_string(n->lvals[0]) && strcmp(cg_get_string(n->lvals[0]), "(null)") != 0) {
        assert(n->lvals.n == 1);
        fprintf(fp, "%s = co_await %s;\n", cg_get_string(n->lvals[0]), cg_get_string(n->rvals[o]));
      } else {
        fprintf(fp, "co_await %s;\n", cg_get_string(n->rvals[o]));
      }
      return 1;
    }
    case P_prim_yield: {
      // issues/014: `co_yield expr` is itself a C++ expression whose
      // value is whatever the *next* resume delivers (pyc_c_runtime.h's
      // _CG_Generator::promise_type::yield_awaiter -- None for a bare
      // advance, or .send(v)'s v). Mirrors P_prim_await immediately
      // above: assign into the lval when `x = yield foo` has one
      // (PY_yield_expr), otherwise emit a bare statement (PY_yield_stmt,
      // `yield foo` alone -- the delivered value has no consumer).
      // Unlike P_prim_await, an explicit cast to the lval's own type is
      // required: `co_yield`'s value is `void*`, and assigning a
      // void* into an integer-typed local (the common case -- pyc's
      // `int` is `_CG_int64`) is not an implicit conversion in C++
      // (confirmed: the analogous uncast `t = co_await ...;` pattern
      // fails to compile for a non-constant int-typed await result;
      // P_prim_await has this same latent gap, just never hit by an
      // existing test). `co_yield` also binds looser than a C-style
      // cast -- `(T)(uintptr_t)co_yield x` is a syntax error (the
      // cast operator wants a unary-expression operand, and
      // `co_yield x` isn't one) -- so the whole `co_yield` expression
      // must be parenthesized before casting it.
      if (virtual_cg_is_const_folded_send(n)) return 1;
      fputs("  ", fp);
      if (n->lvals.n && cg_get_string(n->lvals[0]) && strcmp(cg_get_string(n->lvals[0]), "(null)") != 0) {
        assert(n->lvals.n == 1);
        fprintf(fp, "%s = (%s)(uintptr_t)(co_yield (void*)(uintptr_t)%s);\n", cg_get_string(n->lvals[0]),
                c_type(n->lvals[0]), cg_get_string(n->rvals[o]));
      } else {
        fprintf(fp, "co_yield (void*)(uintptr_t)%s;\n", cg_get_string(n->rvals[o]));
      }
      return 1;
    }
    case P_prim_new: {
      fputs("  ", fp);
      assert(n->lvals.n == 1);
      fprintf(fp, "%s = ", cg_get_string(n->lvals[0]));
      fprintf(fp, "_CG_prim_new(%s);\n", cg_get_string(n->lvals[0]->type));
      // ifa/issues/030: stamp the classtag into the prototype.
      // Instances are made via _CG_prim_clone_dst(prototype), whose
      // memcpy copies the tag along -- so this single store per
      // class prototype tags every instance.
      {
        Sym *t = n->lvals[0]->type;
        if (cg_has_classtag(t) && cg_get_string(n->lvals[0]))
          fprintf(fp, "  ((%s)%s)->__pyc_tag = &_CG_type_%s;\n", cg_get_string(t), cg_get_string(n->lvals[0]),
                  t->name);
      }
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
    case P_prim_copy: {
      fputs("  ", fp);
      assert(n->lvals.n == 1);
      cchar *dst_t = cg_get_string(n->lvals[0]->type);
      if (cg_get_string(n->lvals[0])) fprintf(fp, "%s = ", cg_get_string(n->lvals[0]));
      fprintf(fp, "(%s)_CG_prim_copy_dst(%s, ", dst_t, dst_t);
      for (int i = 2; i < n->rvals.n; i++) {
        if (i > 2) fprintf(fp, ", ");
        fputs(cg_get_string(n->rvals[i]), fp);
      }
      fputs(");\n", fp);
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
        Vec<PolymorphicSlot> *pslots = cg_new_to_val_map.get(f);
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
      // A non-container operand type (no `element`) means FA
      // specialized a container method against a scalar receiver or
      // argument -- an upstream precision failure (issue 025's
      // type-resolution family; observed: score4's list.__add__
      // specialized with an int64 right operand). The generated code
      // would be wrong regardless; fail with a location instead of
      // dereferencing null.
      if (!t->element)
        fail("%s:%d: internal: sizeof_element of non-container type '%s' (in %s) -- FA specialized a container "
             "method against a scalar (see issues/025 type-resolution)",
             n->code->ast ? n->code->ast->pathname() : "?", n->code->ast ? n->code->ast->line() : 0,
             t->name ? t->name : "<anonymous>", f->sym->name ? f->sym->name : "<anonymous>");
      int sz = t->element->type->size;
      if (!sz && t->type_kind == Type_RECORD && t->has.n) sz = t->has[0]->type->size;
      // A generic list's element type is the program-wide union of
      // element types. With 2+ distinct record element types that is
      // a Type_SUM with no compile-time size -- but every member is
      // a boxed reference, so the stored element IS a pointer. This
      // used to emit 0, so list::append resized with element size 0,
      // the storage never grew, and reads returned null at runtime
      // with a clean compile (issue 025 tuple-list soundness bug:
      // two functions append-building lists of different tuple
      // types).
      if (!sz) {
        Sym *et = t->element->type;
        if (et && et->type_kind == Type_SUM && et->has.n) {
          bool all_ref = true;
          for (Sym *m : et->has) if (m) {
            Sym *mt = m->type ? m->type : m;
            if (mt->type_kind != Type_RECORD) { all_ref = false; break; }
          }
          if (all_ref) sz = if1->pointer_size;
        }
      }
      fprintf(fp, "%d;\n", sz);
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
  Sym *clo = closure_fun_type(v0);
  if (clo) {
    if (i < clo->has.n) {
      char ss[4096];
      snprintf(ss, sizeof(ss), "(%s)%s", c_type(f->args.get(p)), cg_get_string(v0));
      char *ee = ss + strlen(ss);
      write_c_fun_arg(fp, f, ss, ee, clo->has[i], i, wrote_one);
      return;
    } else
      i -= clo->has.n - 1;
  }
  if (i < 0 || i >= n->rvals.n)
    codegen_fail(n, "call argument %d out of range (call site has %d values; callee '%s' expects more)", i, n->rvals.n,
                 f->sym->name ? f->sym->name : "<anonymous>");
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
      if (fns && fns->n > 1 && pn->rvals.n) {
        // ifa/issues/030 dispatch. Candidates are partitioned into
        // two kinds and dispatched in ONE if/else chain over a
        // single operand:
        //  - CLASSTAG candidates (method calls / carrier-class
        //    __call__ clones): the receiver instance carries
        //    `__pyc_tag` at offset 0; each class branch casts to
        //    THAT class's layout and calls through its own stored
        //    method slot (per-creation-site clone selection rides
        //    the stored pointer).
        //  - PLAIN-FUNCTION candidates (bare callable values,
        //    issue 007's shape): raw function values are function
        //    addresses; dispatch by value identity with a direct
        //    call. (Address compares run after tag compares;
        //    dereferencing a code address for the tag read is
        //    harmless -- mapped readable, never equal to a tag
        //    object's address.)
        auto scalar_ct = [](cchar *t) {
          return t && (!strncmp(t, "_CG_int", 7) || !strncmp(t, "_CG_uint", 8) || !strncmp(t, "_CG_float", 9) ||
                       !strcmp(t, "_CG_bool"));
        };
        cchar *recv_str = nullptr;  // shared dispatch operand
        if (!pn->rvals[0]->sym->is_symbol && cg_get_string(pn->rvals[0])) recv_str = cg_get_string(pn->rvals[0]);
        Vec<Sym *> classes;  // classtag partition, grouped by class
        Vec<int> slots;      //   ... that class's method-slot index
        Vec<Fun *> plains;   // plain-function partition
        Fun *nil_fn = nullptr;  // nil-receiver candidate (None method on a nil|record union)
        // issue 026: pre-pass -- which classes are DIRECTLY, singularly
        // owned by one of THIS call's candidates (a candidate whose
        // self formal is a single concrete Type_RECORD, not a
        // Type_SUM)? A class that's directly owned here has its OWN
        // override among `fns` and must keep using it -- the Type_SUM
        // unpacking below (for a DIFFERENT candidate whose self type
        // is a union covering that same class, e.g. because FA can't
        // fully monomorphize a recursive structure) must not steal it.
        // Confirmed empirically: without this guard, a recursive tree
        // dispatch where EVERY concrete class overrides the method
        // (poly_dispatch_low.py's Leaf/Branch, each with its own
        // val()) segfaulted -- one class's union-typed self formal
        // caused the OTHER class's OWN override to be silently
        // replaced by a mismatched one (calling a field the receiver
        // doesn't have).
        Vec<cchar *> directly_owned;
        for (int fi = 0; fi < fns->n; fi++) {
          Fun *fv = (*fns)[fi];
          if (!fv || !fv->sym || !fv->sym->name) continue;
          cchar *mname = fv->sym->name;
          MPosition dap;
          dap.push(1);
          for (int pi = 0; pi < fv->sym->has.n + 2; pi++) {
            MPosition *cp = cannonicalize_mposition(dap);
            dap.inc();
            Var *argv = fv->args.get(cp);
            if (!argv || !argv->type) continue;
            Sym *csym = argv->type;
            if (csym->type_kind == Type_SUM) continue;  // ambiguous -- not direct ownership
            for (int k = 0; k < csym->has.n; k++) {
              if (csym->has[k] && csym->has[k]->name == mname && cg_field_live(csym, k)) {
                directly_owned.set_add(csym->name);
                break;
              }
            }
            break;
          }
        }
        bool ok = true;
        for (int fi = 0; fi < fns->n && ok; fi++) {
          Fun *fun_val = (*fns)[fi];
          if (!fun_val || !fun_val->sym) { ok = false; break; }
          // Unnamed candidates (lambdas) can't take the classtag
          // route (slot lookup is by method name) -- plain route only.
          cchar *method_name = fun_val->sym->name;
          // Classtag route: find the candidate's receiver type --
          // the first formal whose concrete type carries a live
          // field named like the method. Deliberately does NOT
          // require the formal to be live: leaf methods that ignore
          // self still need a dispatch branch (the CHOICE depends
          // on the receiver).
          //
          // issue 026: when a class INHERITS a method rather than
          // overriding it, FA gives that method's Fun a self formal
          // typed as a Type_SUM (union) Sym covering every concrete
          // class that reaches it through inheritance -- e.g.
          // `Shape.describe`, also called for `Square` instances
          // (Square has no override of its own), gets a self type
          // `Square | Shape`, NOT a single concrete class. A
          // Type_SUM's `.has` holds its MEMBER TYPES (here: the
          // Sqaure and Shape class Syms themselves), not fields --
          // completely different from a Type_RECORD's `.has` (its
          // OWN fields/methods), even though both reuse the same
          // Vec<Sym*> slot. The rt-search below has to branch on
          // which shape it's looking at: for a Type_RECORD, search
          // its own .has for method_name (the original, single-class
          // case); for a Type_SUM, recurse into EACH member and
          // search THAT member's own .has instead -- one Fun
          // candidate can and does resolve to MULTIPLE concrete
          // receiver classes this way, each needing its own classtag
          // branch (all calling through the SAME Fun, since that's
          // what "doesn't override, just inherits" means).
          Vec<Sym *> rts;    // every concrete receiver class found for this candidate
          Vec<int> rt_slots; //   ... and its slot index (layouts can differ per class)
          MPosition argp;
          argp.push(1);
          for (int pi = 0; method_name && pi < fun_val->sym->has.n + 2 && !rts.n; pi++) {
            MPosition *cp = cannonicalize_mposition(argp);
            argp.inc();
            Var *argv = fun_val->args.get(cp);
            if (!argv || !argv->type) continue;
            Sym *csym = argv->type;
            Vec<Sym *> candidates;  // classes to search: the type itself, or its union members
            bool from_union = csym->type_kind == Type_SUM;
            if (from_union) {
              for (Sym *member : csym->has)
                if (member) candidates.add(member);
            } else {
              candidates.add(csym);
            }
            for (Sym *ccls : candidates) {
              // A union member that's DIRECTLY, singularly owned by
              // ANOTHER candidate in `fns` keeps using that candidate's
              // own override -- this candidate's (looser, unioned)
              // match must not steal it. See the directly_owned
              // pre-pass comment above for why this matters.
              if (from_union && ccls->name && directly_owned.set_in(ccls->name)) continue;
              for (int k = 0; k < ccls->has.n; k++) {
                if (ccls->has[k] && ccls->has[k]->name == method_name && cg_field_live(ccls, k)) {
                  rts.add(ccls);
                  rt_slots.add(k);
                  // The receiver value lives at this formal's
                  // call-site position (used when rvals[0] is the
                  // method symbol rather than the callable value).
                  int ridx = (int)Position2int(cp->pos[0]) - 1;
                  if (!recv_str && ridx >= 0 && ridx < pn->rvals.n && pn->rvals[ridx] &&
                      cg_get_string(pn->rvals[ridx]))
                    recv_str = cg_get_string(pn->rvals[ridx]);
                  break;
                }
              }
            }
          }
          if (!rts.n) {
            // Nil-receiver candidate: a None-class method reached
            // through a nil|record union (`if not self.field:` where
            // the field starts as None). None is a NULL pointer at
            // runtime -- no classtag to read, no value identity to
            // compare -- so it gets its own partition, dispatched by
            // a null test emitted BEFORE any tag dereference (which
            // also makes the tag reads null-safe).
            MPosition np;
            np.push(1);
            bool is_nil_recv = false;
            for (int pi = 0; pi < fun_val->sym->has.n + 2 && !is_nil_recv; pi++) {
              MPosition *cp = cannonicalize_mposition(np);
              np.inc();
              Var *argv = fun_val->args.get(cp);
              if (!argv || !argv->type) continue;
              if (argv->type == sym_nil_type) {
                is_nil_recv = true;
                int ridx = (int)Position2int(cp->pos[0]) - 1;
                if (!recv_str && ridx >= 0 && ridx < pn->rvals.n && pn->rvals[ridx] &&
                    cg_get_string(pn->rvals[ridx]))
                  recv_str = cg_get_string(pn->rvals[ridx]);
              }
            }
            if (is_nil_recv) {
              // Only one nil-receiver branch is distinguishable, and
              // its live formals must map to call-site rvals.
              bool compat = !nil_fn && cg_get_string(fun_val);
              if (compat)
                for (MPosition *p : fun_val->positional_arg_positions) {
                  Var *av = fun_val->args.get(p);
                  if (!av->live) continue;
                  int i = (int)Position2int(p->pos[0]) - 1;
                  if (i < 0 || i >= pn->rvals.n || !cg_get_string(pn->rvals[i])) { compat = false; break; }
                }
              if (!compat) { ok = false; break; }
              nil_fn = fun_val;
              continue;
            }
          }
          if (rts.n) {
            // Merge each found receiver class into a per-class-name
            // branch (clones of one class share a tag; the stored
            // slot pointer disambiguates). A single Fun candidate can
            // contribute several classes here (see the Type_SUM
            // comment above) -- add every one that carries a real
            // classtag. Matches the original single-class code's
            // fallback behavior: if NONE of the found classes are
            // classtag-eligible, fall through to the plain-function
            // route below instead of silently dropping the candidate.
            bool added_any = false;
            for (int ri = 0; ri < rts.n; ri++) {
              Sym *rt = rts[ri];
              if (!cg_has_classtag(rt) || !cg_get_string(rt)) continue;
              added_any = true;
              bool found = false;
              for (int ci = 0; ci < classes.n; ci++)
                if (!strcmp(classes[ci]->name, rt->name)) { found = true; break; }
              if (!found) {
                classes.add(rt);
                slots.add(rt_slots[ri]);
              }
            }
            if (added_any) continue;
          }
          // Plain-function route: needs the fun's address and every
          // live formal mapping to an in-range call-site rval of a
          // castable C type.
          if (cg_get_string(fun_val) && !pn->rvals[0]->sym->is_symbol) {
            bool compat = true;
            for (MPosition *p : fun_val->positional_arg_positions) {
              Var *av = fun_val->args.get(p);
              if (!av->live) continue;
              int i = (int)Position2int(p->pos[0]) - 1;
              if (i < 0 || i >= pn->rvals.n || !cg_get_string(pn->rvals[i])) { compat = false; break; }
              cchar *ft = c_type(av), *at = c_type(pn->rvals[i]);
              if (strcmp(ft, at) && scalar_ct(ft) != scalar_ct(at)) { compat = false; break; }
            }
            if (compat) {
              plains.add(fun_val);
              continue;
            }
          }
          ok = false;
        }
        if (ok && recv_str && (classes.n || plains.n || nil_fn)) {
          cchar *lhs = (pn->lvals.n && cg_get_string(pn->lvals[0])) ? cg_get_string(pn->lvals[0]) : nullptr;
          cchar *ret_type_str = (pn->lvals.n && pn->lvals[0]->type) ? c_type(pn->lvals[0]) : "void*";
          int nb = 0;
          if (nil_fn) {
            // Null test first: it both selects the None method and
            // keeps the classtag dereferences below null-safe.
            fprintf(fp, "  if (!%s) {\n", recv_str);
            fputs("    ", fp);
            if (lhs) fprintf(fp, "%s = ", lhs);
            fprintf(fp, "%s(", cg_get_string(nil_fn));
            int wrote_one = 0;
            for (MPosition *p : nil_fn->positional_arg_positions) {
              Var *av = nil_fn->args.get(p);
              if (!av->live) continue;
              int i = (int)Position2int(p->pos[0]) - 1;
              cchar *ft = c_type(av), *at = c_type(pn->rvals[i]);
              if (wrote_one) fputs(", ", fp);
              wrote_one = 1;
              if (!strcmp(ft, at))
                fputs(cg_get_string(pn->rvals[i]), fp);
              else if (scalar_ct(ft))
                fprintf(fp, "(%s)%s", ft, cg_get_string(pn->rvals[i]));
              else
                fprintf(fp, "(%s)(void*)%s", ft, cg_get_string(pn->rvals[i]));
            }
            fputs(");\n  }\n", fp);
            nb++;
          }
          for (int ci = 0; ci < classes.n; ci++, nb++) {
            fprintf(fp, "  %sif ((*(_CG_TypeObject**)(void*)%s) == &_CG_type_%s) {\n", nb ? "else " : "", recv_str,
                    classes[ci]->name);
            fputs("    ", fp);
            if (lhs) fprintf(fp, "%s = ", lhs);
            fprintf(fp, "((%s(*)(void*))((%s)(void*)%s)->e%d)((void*)%s);\n", ret_type_str,
                    cg_get_string(classes[ci]), recv_str, slots[ci], recv_str);
            fputs("  }\n", fp);
          }
          for (int fi = 0; fi < plains.n; fi++, nb++) {
            Fun *fv = plains[fi];
            fprintf(fp, "  %sif ((void*)%s == (void*)&%s) {\n", nb ? "else " : "", recv_str, cg_get_string(fv));
            fputs("    ", fp);
            if (lhs) fprintf(fp, "%s = ", lhs);
            fprintf(fp, "%s(", cg_get_string(fv));
            int wrote_one = 0;
            for (MPosition *p : fv->positional_arg_positions) {
              Var *av = fv->args.get(p);
              if (!av->live) continue;
              int i = (int)Position2int(p->pos[0]) - 1;
              cchar *ft = c_type(av), *at = c_type(pn->rvals[i]);
              if (wrote_one) fputs(", ", fp);
              wrote_one = 1;
              if (!strcmp(ft, at))
                fputs(cg_get_string(pn->rvals[i]), fp);
              else if (scalar_ct(ft))
                fprintf(fp, "(%s)%s", ft, cg_get_string(pn->rvals[i]));
              else
                fprintf(fp, "(%s)(void*)%s", ft, cg_get_string(pn->rvals[i]));
            }
            fputs(");\n  }\n", fp);
          }
          fputs("  else { assert(!\"runtime error: polymorphic dispatch: no branch matched\"); }\n", fp);
          return;
        }
      }
      if (getenv("PYC_DBG_DISPATCH")) {
        fprintf(stderr, "DISPATCH FAIL in %s: fns=%d rvals=%d |", f->sym->name ? f->sym->name : "?",
                fns ? fns->n : -1, pn->rvals.n);
        if (fns)
          for (Fun *fv : *fns)
            fprintf(stderr, " cand=%s", fv && fv->sym && fv->sym->name ? fv->sym->name : "?");
        for (int i = 0; i < pn->rvals.n; i++)
          fprintf(stderr, " r%d=%s:%s", i, pn->rvals[i]->sym->name ? pn->rvals[i]->sym->name : "_",
                  pn->rvals[i]->type && pn->rvals[i]->type->name ? pn->rvals[i]->type->name : "?");
        fprintf(stderr, "\n");
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
          if (f->sym->is_async) {
            fprintf(fp, "  co_return %s;\n", c_rhs(n->rvals[3]));
          } else if (f->sym->is_generator) {
            // issues/014: the coroutine's real return value (whatever
            // FA computed for fn->ret, see gen_fun_pyda) is a
            // synthetic int64 placeholder, not meaningful to the
            // _CG_Generator promise type's return_void() -- and not
            // yet observable to Python code anyway (no `.send()`/
            // StopIteration.value in v1 scope).
            fputs("  co_return;\n", fp);
          } else {
            fprintf(fp, "  return %s;\n", c_rhs(n->rvals[3]));
          }
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
        // A dead reply usually means the result is unused -- but a
        // constant-folded result also deadens the reply while
        // callers reached through a stored method pointer
        // (ifa/issues/030 dispatch) still consume the return value.
        // c_rhs returns the constant literal in that case ("0" as
        // before when there is genuinely no value).
        if (f->sym->is_async)
          fprintf(fp, "  co_return %s;\n", c_rhs(n->rvals[3]));
        else
          fprintf(fp, "  return %s;\n", c_rhs(n->rvals[3]));
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

  Vec<Var*> coro_vars;
  Vec<PNode*> nodes;
  f->collect_PNodes(nodes);
  for (PNode *n : nodes) {
    if (n->code->kind == Code_SEND && n->lvals.n == 1) {
      if (n->prim && n->prim->index == P_prim_await) continue;
      Fun *target = get_target_fun_core(n, f);
      if (target && target->sym->is_async) {
         coro_vars.set_add(n->lvals[0]);
      }
    }
  }

  // issues/014: is_generator's real coroutine body is wrapped in an
  // immediately-invoked local lambda -- the ONLY thing in this
  // function that actually contains co_yield/co_return, so it's the
  // only thing the C++ compiler treats as a coroutine. This keeps the
  // outer function itself an ordinary function returning a plain
  // int64 handle (write_c_fun_proto forces that return type above),
  // exactly what the synthesized wrapper Fun (build_if1_pyda's
  // PY_funcdef) expects to call -- no call-site-typing surgery needed
  // anywhere else.
  //
  // CRITICAL: the coroutine's locals (defs below) must be declared
  // INSIDE the lambda, not captured by reference from the outer
  // function. initial_suspend() (_CG_Generator's promise_type)
  // returns std::suspend_always, so the outer function constructs the
  // suspended coroutine and returns *immediately* -- its stack frame
  // is gone by the time anything resumes the coroutine from a later,
  // unrelated call. A `[&]`-captured local declared outside the
  // lambda would dangle the moment that happens (confirmed:
  // segfaulted on the first resume, from __pyc_more__, called well
  // after gen()'s own C stack frame had returned). Locals declared
  // *inside* the lambda are correctly promoted to the coroutine's own
  // heap-allocated frame by the C++ compiler, so they survive
  // suspension like any other local in a real coroutine.
  //
  // CRITICAL #2 (found investigating a list-argument corruption bug):
  // when a lambda's operator() is itself a coroutine, [=]-captured
  // members live in the CLOSURE OBJECT, not the coroutine frame --
  // the frame only stores an implicit pointer back to the closure.
  // A stack-local closure (`auto __coro_1014 = [=]...`) is destroyed
  // the moment this outer function returns (same "gone by the time
  // anything resumes" problem as CRITICAL above), so any *captured
  // parameter* (a0, a1, ...; scalars happened to often "work" by
  // stack-reuse luck, struct/list pointers reliably didn't) read
  // garbage after the first resume. Fix: heap-allocate the closure
  // itself (`new auto(...)`) so the object the frame's implicit
  // pointer refers to survives indefinitely, exactly like the
  // coroutine frame does. Confirmed via a minimal pyc-independent
  // repro (dangling closure reproduced with a plain stack `auto`,
  // fixed with `new auto`).
  if (f->sym->is_generator) {
    fputs("  auto *__coro_1014 = new auto([=]() -> _CG_Generator {\n", fp);
  }
  for (Var *v : defs) {
    fputs("  ", fp);
    if (coro_vars.set_in(v)) {
      fputs("_CG_Coroutine", fp);
    } else {
      write_c_type(fp, v);
    }
    fprintf(fp, " %s;\n", cg_get_string(v));
  }
  if (defs.n) fprintf(fp, "\n");
  if (globals)
    for (Var *v : *globals) if (!v->sym->is_fun && v->sym->fun && !v->sym->type_kind && cg_get_string(v))
        fprintf(fp, "  %s = %s;\n", cg_get_string(v), cg_get_string(v->sym->fun));
  // issues/014: for a generator, formal parameters (a0, a1, ...) are
  // the OUTER function's parameters; [=] above captures them by value
  // into the lambda, so this assignment (which reads a0/a1 by name)
  // resolves to the captured copies here, not the outer ones -- safe
  // for the same reason the locals above are. Positional generator
  // arguments are forwarded from the synthesized wrapper (see the
  // PY_funcdef case in python_ifa_build_if1.cc); this ordering (args
  // assigned inside the lambda, not before it) is what makes that
  // safe.
  write_c_args(fp, f);
  rebuild_cfg_pred_index(f);
  Vec<PNode *> done;
  done.set_add(f->entry);
  write_c_pnode(fp, fa, f, f->entry, done);
  if (f->sym->is_generator) {
    fputs("  });\n", fp);
    fputs("  _CG_Generator __g_1014 = (*__coro_1014)();\n", fp);
    fputs("  return (_CG_int64)(uintptr_t)__g_1014.handle.address();\n", fp);
  }
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
          // ifa/issues/030: user-class records carry a classtag
          // header at offset 0 (a pointer to the class's
          // _CG_TypeObject, written once into the class prototype
          // at prim_new and inherited by every instance via
          // clone_dst's memcpy). Polymorphic dispatch reads it to
          // select the per-class branch (each class's method slot
          // index differs), then calls through that class's own
          // stored method pointer (which is per-creation-site, so
          // FA clones keep working). All other field accesses are
          // by eN member NAME, so the extra leading member is
          // layout-transparent to them.
          if (cg_has_classtag(s)) fputs("  _CG_TypeObject *__pyc_tag;\n", fp);
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

void c_codegen_print_c(FILE *fp, FA *fa, Fun *init) {
  Vec<Var *> globals;
  int index = 0;
  if (!if1->callback->c_codegen_pre_file(fp)) fprintf(fp, "#include \"c_runtime.h\"\n\n");
  build_type_strings(fp, fa, globals);
  cg_build_new_to_val_map(fa);
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
