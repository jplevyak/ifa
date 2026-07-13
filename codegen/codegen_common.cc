// SPDX-License-Identifier: BSD-3-Clause
//
// See `codegen_common.h` for the header-level contract.

#include "ifadefs.h"

#include "builtin.h"
#include "codegen_common.h"
#include "fa.h"
#include "fail.h"
#include "fun.h"
#include "pattern.h"
#include "pnode.h"
#include "sym.h"
#include "var.h"

#include <spawn.h>
#include <sys/wait.h>
extern char **environ;

#include <cstdarg>

// -------------------------------------------------------------
// Type-name strings
// -------------------------------------------------------------

cchar *c_type(Var *v) {
  if (!v->type || !cg_get_string(v->type)) return "_CG_void";
  return cg_get_string(v->type);
}

cchar *c_type(Sym *s) {
  if (!s->type || !cg_get_string(s->type)) return "_CG_void";
  return cg_get_string(s->type);
}

cchar *num_string(Sym *s) {
  switch (s->num_kind) {
    default:
      assert(!"case");
    case IF1_NUM_KIND_UINT:
      switch (s->num_index) {
        case IF1_INT_TYPE_1:
          return "_CG_bool";
        case IF1_INT_TYPE_8:
          return "_CG_uint8";
        case IF1_INT_TYPE_16:
          return "_CG_uint16";
        case IF1_INT_TYPE_32:
          return "_CG_uint32";
        case IF1_INT_TYPE_64:
          return "_CG_uint64";
        default:
          assert(!"case");
      }
      break;
    case IF1_NUM_KIND_INT:
      switch (s->num_index) {
        case IF1_INT_TYPE_1:
          return "_CG_bool";
        case IF1_INT_TYPE_8:
          return "_CG_int8";
        case IF1_INT_TYPE_16:
          return "_CG_int16";
        case IF1_INT_TYPE_32:
          return "_CG_int32";
        case IF1_INT_TYPE_64:
          return "_CG_int64";
        default:
          assert(!"case");
      }
      break;
    case IF1_NUM_KIND_FLOAT:
      switch (s->num_index) {
        case IF1_FLOAT_TYPE_32:
          return "_CG_float32";
        case IF1_FLOAT_TYPE_64:
          return "_CG_float64";
        case IF1_FLOAT_TYPE_128:
          return "_CG_float128";
        default:
          assert(!"case");
          break;
      }
      break;
  }
  fail("num_string: unhandled num_kind %d num_index %d", s->num_kind, s->num_index);
  return 0;
}

// -------------------------------------------------------------
// Closure / dispatch helpers
// -------------------------------------------------------------

bool cg_has_classtag(Sym *s) {
  if (!(s && s->type_kind == Type_RECORD && !s->is_system_type && s->name && s->has.n && !s->is_vector &&
        !sym_tuple->specializers.set_in(s) && !(s->creators.n && s->creators[0]->sym == sym_list)))
    return false;
  // Tuple clones aren't always in sym_tuple->specializers; identify
  // them structurally: element records have only UNNAMED fields,
  // while class records always carry named fields/methods.
  for (Sym *h : s->has) if (h && h->name) return true;
  return false;
}

int cg_field_live(Sym *s, int i) {
  if (!s || i < 0 || i >= s->has.n) return 0;
  if (!s->has[i]->type) return 0;
  if (s->has[i]->var && !s->has[i]->var->live) return 0;
  return 1;
}

Map<Fun *, Vec<PolymorphicSlot> *> cg_new_to_val_map;

// See codegen_common.h. For each live function that appears at any
// poly call site (method name with fns->n > 1 anywhere), find its
// self-arg concrete type, discover the slot for its name in that
// type, trace the FA creation chain from that arg's AType through
// cs->defs to the creator function, and register
// creator -> (slot, fun_val). Scans ALL fun->calls entries
// (including cloned functions), catching val clones that only
// appear at monomorphic call sites within specialized callers but
// participate in vtable dispatch overall.
void cg_build_new_to_val_map(FA *fa) {
  cg_new_to_val_map.clear();

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

  // Pass 1.5 (issue 026): for each poly method name, which classes are
  // DIRECTLY, singularly owned by one of ITS candidates (a candidate
  // whose self formal is a single concrete Type_RECORD, not a
  // Type_SUM)? Mirrors cg.cc's/cg_emit_llvm.cc's identical pre-pass at
  // the dispatch site, for the same reason: a class with its own
  // override among the poly candidates must keep using it, even when
  // ANOTHER candidate's self formal is a union that happens to cover
  // that same class (FA imprecision on a recursive structure, not
  // genuine inheritance-sharing) -- confirmed empirically without this
  // guard (poly_dispatch_low.py, every concrete class overrides the
  // dispatched method, segfaulted: one class's union-typed self formal
  // silently stole another class's own, distinct override).
  Map<cchar *, Vec<cchar *> *> directly_owned_by_name;
  for (Fun *fv : fa->funs) {
    if (!fv->live || !fv->sym || !fv->sym->name) continue;
    if (!poly_names.set_in(fv->sym->name)) continue;
    cchar *mname = fv->sym->name;
    MPosition dap;
    dap.push(1);
    for (int pi = 0; pi < fv->sym->has.n + 2; pi++) {
      MPosition *cp = cannonicalize_mposition(dap);
      dap.inc();
      Var *argv = fv->args.get(cp);
      if (!argv || !argv->type) continue;
      Sym *csym = argv->type;
      if (csym->type_kind == Type_SUM) break;  // ambiguous -- not direct ownership
      for (int k = 0; k < csym->has.n; k++) {
        if (csym->has[k] && csym->has[k]->name == mname && cg_field_live(csym, k)) {
          Vec<cchar *> *owned = directly_owned_by_name.get(mname);
          if (!owned) {
            owned = new Vec<cchar *>();
            directly_owned_by_name.put(mname, owned);
          }
          owned->set_add(csym->name);
          break;
        }
      }
      break;
    }
  }

  // Pass 2: for every live function whose name is a poly method, find its
  // self arg, the method slot in that arg's concrete type, and register all
  // creators of self with this function.
  for (Fun *fun_val : fa->funs) {
    if (!fun_val->live || !fun_val->sym || !fun_val->sym->name) continue;
    if (!poly_names.set_in(fun_val->sym->name)) continue;
    cchar *method_name = fun_val->sym->name;
    Vec<cchar *> *owned_by_others = directly_owned_by_name.get(method_name);

    // Find the self-arg position. issue 026: a method a class INHERITS
    // rather than overrides gets a self formal typed as a Type_SUM
    // (union) Sym covering every concrete class reaching it through
    // inheritance (e.g. `Square | Shape` for `Shape.describe`, also
    // called for `Square` instances) -- its `.has` holds those MEMBER
    // TYPES, not fields, so "does this position look like self" has
    // to check either the type's own .has (single concrete class) OR
    // recurse into a Type_SUM's members (mirrors cg.cc's identical
    // fix in emit_send_call's classtag construction). The slot index
    // itself is NOT resolved here anymore -- a union's member classes
    // can have different dead-field-elision layouts (issue 026's own
    // earlier text: "leaf structs carried val at e1, Inner at e2"),
    // so it has to be looked up per concrete class below, keyed by
    // each CreationSet's own `cs->sym`, not once for the whole
    // (union-typed) self arg.
    MPosition *self_cp = nullptr;
    bool self_is_union = false;
    int direct_slot = -1;  // slot found when self ISN'T a union -- old, single-slot behavior
    {
      MPosition argp;
      argp.push(1);
      for (int pi = 0; pi < fun_val->sym->has.n + 2 && !self_cp; pi++) {
        MPosition *cp = cannonicalize_mposition(argp);
        argp.inc();
        Var *v = fun_val->args.get(cp);
        if (!v || !v->live || !v->type) continue;
        Sym *csym = v->type;
        bool from_union = csym->type_kind == Type_SUM;
        Vec<Sym *> candidates;
        if (from_union) {
          for (Sym *member : csym->has)
            if (member) candidates.add(member);
        } else {
          candidates.add(csym);
        }
        for (Sym *ccls : candidates) {
          int found_k = -1;
          for (int k = 0; k < ccls->has.n; k++)
            if (ccls->has[k] && ccls->has[k]->name == method_name && cg_field_live(ccls, k)) { found_k = k; break; }
          if (found_k >= 0) {
            self_cp = cp;
            self_is_union = from_union;
            if (!from_union) direct_slot = found_k;
            break;
          }
        }
      }
    }
    if (!self_cp) continue;

    // Walk every EntrySet for fun_val; look only at the self arg's AType.
    // Track specificity = sorted.n of the ES: lower means more specific.
    // When multiple val clones compete for the same (creator, slot), the
    // most-specific one (smallest sorted.n) wins — FA is conservative and
    // may include extra CSes in the self AType of less-specific clones.
    for (EntrySet *es : fun_val->ess) {
      AVar *self_av = nullptr;
      for (int j = 0; j < es->args.n; j++) {
        if (es->args.v[j].key == self_cp) {
          self_av = es->args.v[j].value;
          break;
        }
      }
      if (!self_av || !self_av->out) continue;
      int specificity = self_av->out->sorted.n;  // fewer CSes = more specific
      for (CreationSet *cs : self_av->out->sorted) {
        if (!cs || !cs->sym) continue;
        // A class DIRECTLY, singularly owned by a DIFFERENT candidate
        // (this fun_val's OWN self type was a union, so it doesn't
        // directly own anything itself) keeps using that candidate's
        // registration instead -- see the directly_owned_by_name
        // pre-pass comment above for why.
        if (self_is_union && owned_by_others && owned_by_others->set_in(cs->sym->name)) continue;
        // Resolve the slot for THIS concrete class. When the self arg
        // wasn't a union, keep the ORIGINAL behavior exactly (reuse
        // direct_slot, found once above) -- only a union-typed self
        // arg needs re-resolving per cs->sym, since only THEN can
        // different member classes have different dead-field-elision
        // layouts (see the comment above self_cp's search). Recomputing
        // this unconditionally regressed poly_dispatch_low.py/high.py
        // (non-union, single-class self args where every concrete
        // class already overrides the method) -- confirmed via
        // PYC_DBG_DISPATCH that neither candidate there is Type_SUM at
        // all, so the bug was purely in this slot re-resolution, not
        // the union-unpacking it was meant to fix.
        int slot = direct_slot;
        if (self_is_union) {
          slot = -1;
          for (int k = 0; k < cs->sym->has.n; k++)
            if (cs->sym->has[k] && cs->sym->has[k]->name == method_name && cg_field_live(cs->sym, k)) { slot = k; break; }
        }
        if (slot < 0) continue;
        for (AVar *def_av : cs->defs) {
          if (!def_av || !def_av->contour_is_entry_set) continue;
          EntrySet *creator_es = (EntrySet *)def_av->contour;
          Fun *fun_new = creator_es->fun;
          if (!fun_new || !fun_new->live) continue;
          Vec<PolymorphicSlot> *slots = cg_new_to_val_map.get(fun_new);
          if (!slots) {
            slots = new Vec<PolymorphicSlot>();
            cg_new_to_val_map.put(fun_new, slots);
          }
          // Find existing registration for this (slot) — replace if less specific.
          int existing = -1;
          for (int k = 0; k < slots->n; k++)
            if ((*slots)[k].slot == slot) {
              existing = k;
              break;
            }
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
          PolymorphicSlot ps;
          ps.slot = slot;
          ps.fun_val = fun_val;
          ps.specificity = specificity;
          slots->add(ps);
        }
      }
    }
  }
}

Sym *closure_fun_type(Var *v) {
  Sym *t = v->type;
  if (!t) return nullptr;
  // Nullable closure: SUM{nil_type, closure}. pass2 already
  // types this SUM as the closure struct pointer in C, so the
  // unpacking path can treat it as the closure component.
  if (t->type_kind == Type_SUM && t->has.n == 2) {
    if (t->has[0] == sym_nil_type)
      t = t->has[1];
    else if (t->has[1] == sym_nil_type)
      t = t->has[0];
  }
  if (t->type_kind == Type_FUN && !t->fun && t->has.n) return t;
  return nullptr;
}

int is_closure_var(Var *v) { return closure_fun_type(v) != nullptr; }

Fun *get_target_fun_core(PNode *n, Fun *f) {
  Vec<Fun *> *fns = f->calls.get(n);
  if (!fns || fns->n != 1) return nullptr;
  return fns->v[0];
}

// -------------------------------------------------------------
// Process invocation
// -------------------------------------------------------------

int codegen_spawn(const char *file, char *const argv[]) {
  pid_t pid = 0;
  int rc = posix_spawnp(&pid, file, nullptr, nullptr, argv, environ);
  if (rc != 0) {
    fail("codegen_spawn: posix_spawnp failed for %s: errno=%d", file, rc);
    return -1;
  }
  int status = 0;
  if (waitpid(pid, &status, 0) < 0) {
    fail("codegen_spawn: waitpid failed for %s pid %d", file, (int)pid);
    return -1;
  }
  if (WIFEXITED(status)) return WEXITSTATUS(status);
  return -1;
}

// -------------------------------------------------------------
// Type-string assignment pass
// -------------------------------------------------------------

void assign_fun_cg_strings(FA *fa, bool annotate, Vec<Var *> *globals) {
  int f_index = 0;
  for (Fun *f : fa->funs) {
    if (!f->live) continue;
    char s[100];
    if (annotate && f->sym->name) {
      if (f->sym->has.n > 1 && f->sym->has[1]->must_specialize)
        snprintf(s, sizeof(s), "_CG_f_%d_%d/*%s::%s*/", f->sym->id, f_index,
                 f->sym->has[1]->must_specialize->name, f->sym->name);
      else
        snprintf(s, sizeof(s), "_CG_f_%d_%d/*%s*/", f->sym->id, f_index, f->sym->name);
    } else {
      snprintf(s, sizeof(s), "_CG_f_%d_%d", f->sym->id, f_index);
    }
    cg_set_string(f, dupstr(s));
    snprintf(s, sizeof(s), "_CG_pf%d", f_index);
    cg_set_structural_string(f, dupstr(s));
    cg_set_string(f->sym, cg_get_structural_string(f));
    f_index++;
    if (globals && f->sym->var) globals->set_add(f->sym->var);
  }
}

void assign_type_cg_strings_pass1(Vec<Sym *> &allsyms, FILE *fp) {
  for (Sym *s : allsyms) {
    if (s->num_kind) {
      cg_set_string(s, num_string(s));
    } else if (s->is_symbol) {
      cg_set_string(s, "_CG_symbol");
    } else if (!cg_get_string(s)) {
      switch (s->type_kind) {
        default:
          cg_set_string(s, dupstr("_CG_any"));
          break;
        case Type_FUN:
          if (s->fun) break;
        // fall through
        case Type_RECORD: {
          if (s->has.n) {
            char ss[100];
            if (fp) {
              fprintf(fp, "/* %s */ struct _CG_s%d; ", s->name ? s->name : "", s->id);
              fprintf(fp, "typedef struct _CG_s%d *_CG_ps%d;\n", s->id, s->id);
            }
            snprintf(ss, sizeof(ss), "_CG_ps%d", s->id);
            cg_set_string(s, dupstr(ss));
          } else {
            cg_set_string(s, "_CG_void");
          }
          break;
        }
      }
    }
  }
}

// -------------------------------------------------------------
// Failure reporting with PNode / Var context (phase 5.3)
// -------------------------------------------------------------

void codegen_fail(PNode *n, cchar *fmt, ...) {
  fflush(stdout);
  fflush(stderr);

  char msg[1024];
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(msg, sizeof(msg), fmt, ap);
  va_end(ap);

  if (n && n->code && n->code->ast)
    fprintf(stderr, "%s:%d: codegen: %s\n", n->code->pathname(), n->code->line(), msg);
  else
    fprintf(stderr, "fail: codegen: %s\n", msg);
  exit(1);
}

void codegen_fail(Var *v, cchar *fmt, ...) {
  fflush(stdout);
  fflush(stderr);

  char msg[1024];
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(msg, sizeof(msg), fmt, ap);
  va_end(ap);

  if (v && v->sym && v->sym->ast)
    fprintf(stderr, "%s:%d: codegen: %s\n", v->sym->pathname(), v->sym->line(), msg);
  else if (v && v->def && v->def->code && v->def->code->ast)
    fprintf(stderr, "%s:%d: codegen: %s\n", v->def->code->pathname(), v->def->code->line(), msg);
  else
    fprintf(stderr, "fail: codegen: %s\n", msg);
  exit(1);
}

void assign_type_cg_strings_pass2(Vec<Sym *> &allsyms) {
  for (Sym *s : allsyms) {
    if (s->fun) {
      cg_set_string(s, cg_get_structural_string(s->fun));
    } else if (s->is_symbol) {
      cg_set_string(s, cg_get_string(sym_symbol));
    }
    if (s->type_kind == Type_SUM && s->has.n == 2) {
      if (s->has[0] == sym_nil_type)
        cg_set_string(s, cg_get_string(s->has[1]));
      else if (s->has[1] == sym_nil_type)
        cg_set_string(s, cg_get_string(s->has[0]));
    }
  }
}

bool virtual_cg_is_const_folded_send(PNode *pn) {
  if (!pn || !pn->code || pn->code->kind != Code_SEND) return false;
  if (!pn->prim || (pn->prim->nonfunctional && pn->prim->index != P_prim_await)) return false;
  if (pn->lvals.n != 1) return false;
  Var *lv = pn->lvals.v[0];
  if (!lv) return false;
  return get_constant(lv) != nullptr;
}

void virtual_cg_emit_send(VirtualCGEmitter *emitter, PNode *pn) {
  if (!pn) return;
  if (virtual_cg_is_const_folded_send(pn)) return;
  if (pn->prim) {
    int idx = pn->prim->index;
    if (idx == P_prim_reply) return;
    if (emitter->emit_send_any_prim(pn)) return;
    if (emitter->emit_send_unaryop(pn)) return;
    if (emitter->emit_send_binop(pn)) return;
    if (emitter->emit_send_period(pn)) return;
    if (emitter->emit_send_setter(pn)) return;
    if (emitter->emit_send_new(pn)) return;
    if (emitter->emit_send_clone(pn)) return;
    if (emitter->emit_send_len(pn)) return;
    if (emitter->emit_send_strcat(pn)) return;
    if (emitter->emit_send_is(pn)) return;
    if (emitter->emit_send_coerce(pn)) return;
    if (emitter->emit_send_make(pn)) return;
    if (emitter->emit_send_index_load(pn)) return;
    if (emitter->emit_send_index_store(pn)) return;
    if (emitter->emit_send_sizeof(pn)) return;
    if (emitter->emit_send_primitive(pn)) return;
    if (emitter->emit_send_default_prim(pn)) return;
    return;
  }
  emitter->emit_send_call(pn);
}
