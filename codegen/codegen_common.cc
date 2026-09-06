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
  // Callers pass null deliberately: write_c_prim's index-store path
  // computes `Sym *e = t && t->element ? t->element->type : nullptr` and
  // hands the result straight here, so null means "this container has no
  // element type" -- which is exactly the `_CG_void` the `!s->type` case
  // below already returns. Without this the pairing segfaults on any
  // untyped element, which is how ifa/135 turned an FA imprecision on
  // sudoku3 into a compiler crash (c_type(s=0x0), cg.cc:1083) instead of
  // a diagnostic.
  if (!s || !s->type || !cg_get_string(s->type)) return "_CG_void";
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

// See codegen_common.h.
void cg_fail_unrepresentable_container_union(Sym *outer, Sym *fn_sym, cchar *path, int line) {
  char un[512];
  un[0] = 0;
  if (outer && outer->type_kind == Type_SUM && outer->has.n) {
    int off = 0;
    off += snprintf(un + off, sizeof(un) - off, "{");
    for (int i = 0; i < outer->has.n && off < (int)sizeof(un) - 8; i++) {
      Sym *m = outer->has[i];
      off += snprintf(un + off, sizeof(un) - off, "%s%s", i ? ", " : "", m && m->name ? m->name : "?");
    }
    snprintf(un + off, sizeof(un) - off, "}");
  } else
    snprintf(un, sizeof(un), "'%s'", outer && outer->name ? outer->name : "?");

  // Deliberately no file:line when the only location available is the
  // BUILTIN's -- `__pyc__.py:1155`, where `__add__` is defined. Printing
  // it reads as "the bug is in the library", which sent at least one
  // investigation the wrong way. The user's own line is already named by
  // the FA warning that precedes this ("illegal primitive argument type
  // 'x'"), so no location beats a misleading one.
  bool in_builtin = path && strstr(path, "__pyc__");
  cchar *fname = fn_sym && fn_sym->name ? fn_sym->name : "?";
  if (in_builtin || !path)
    fail("a variable holding %s has no representation: '%s' resolved to the CONTAINER method, whose receiver "
         "may be a scalar. pyc does not box, so a {container, scalar} union cannot be represented "
         "(issues/018)",
         un, fname);
  else
    fail("%s:%d: a variable holding %s has no representation: '%s' resolved to the CONTAINER method, whose "
         "receiver may be a scalar. pyc does not box, so a {container, scalar} union cannot be represented "
         "(issues/018)",
         path, line, un, fname);
}

int cg_field_live(Sym *s, int i) {
  if (!s || i < 0 || i >= s->has.n) return 0;
  if (!s->has[i]->type) return 0;
  if (s->has[i]->var && !s->has[i]->var->live) return 0;
  return 1;
}

Map<Fun *, Vec<PolymorphicSlot> *> cg_new_to_val_map;
Vec<cchar *> poly_names;  // ifa/123: filled by cg_build_new_to_val_map, read by cg_report_slot_use
extern Vec<Sym *> cg_slot_use_cls;
extern Vec<int> cg_slot_use_idx;
extern Vec<int> cg_slot_use_rd;

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
  poly_names.clear();
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
    Sym *self_type = nullptr;  // the concrete self formal, when it isn't a union
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
            if (!from_union) {
              direct_slot = found_k;
              self_type = ccls;
            }
            break;
          }
        }
      }
    }
    static int dbg_slot = getenv("IFA_DBG_POLYSLOT") ? 1 : 0;
    if (!self_cp) {
      if (dbg_slot) fprintf(stderr, "[polyslot] %s: NO SELF ARG found (fun %d)\n", method_name, fun_val->id);
      continue;
    }

    // Walk every EntrySet for fun_val; look only at the self arg's AType.
    // Track specificity = sorted.n of the ES: lower means more specific.
    // When multiple val clones compete for the same (creator, slot), the
    // most-specific one (smallest sorted.n) wins — FA is conservative and
    // may include extra CSes in the self AType of less-specific clones.
    // NB `if (!es) continue`: Fun::ess is a hash-set Vec (null
    // holes) -- every sibling loop guards; this one crashed on
    // chess once the dup-aware stall guard's extra passes left
    // holes here.
    for (EntrySet *es : fun_val->ess) {
      if (!es) continue;
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
        // issues/118: a method clone may only implement a class's slot if
        // the class it was DECLARED on is that class or an ancestor of
        // it. FA's self AType for a clone carries every CreationSet that
        // reached it, and a degenerate union puts foreign classes in
        // there -- so on shedskin_examples/loop with a hashed dict,
        // __pyc_None_type__::__eq__ (self typed {nil, Basic_block}) won
        // Basic_block's __eq__ slot. That method is None's identity
        // compare: it answers False for every non-None argument, so a
        // dict lookup never matched its own key and the program silently
        // computed the wrong answer.
        //
        // The declared owner is the method Sym's self formal's
        // must_specialize -- the same thing assign_fun_cg_strings prints
        // as the `Class::` half of a clone's name. `object::__eq__` still
        // registers for every class, since object is everyone's ancestor;
        // an inherited method whose self is a Type_SUM still registers
        // for the members that specialize its owner (issue 026's case).
        {
          Sym *owner = (fun_val->sym->has.n > 1) ? fun_val->sym->has[1]->must_specialize : nullptr;
          if (owner && cs->sym != owner && !owner->specializers.set_in(cs->sym)) {
            if (dbg_slot)
              fprintf(stderr, "[polyslot] %s: %s is not an ancestor of %s -- not registering\n", method_name,
                      owner->name ? owner->name : "?", cs->sym->name ? cs->sym->name : "?");
            continue;
          }
        }
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
        // ifa/issues/123: resolve the slot in `cs->sym` -- the class whose
        // struct is actually stored into -- rather than reusing
        // `direct_slot`, which was found in the self FORMAL's class and
        // is equal only by luck. Under PYC_PREFIX_LAYOUT the two are
        // deliberately reordered and the luck runs out: the stale index
        // wrote a method pointer into a slot the struct declared
        // `_CG_string`, as a raw clang error.
        //
        // Falling back to `direct_slot` when the name is not found keeps
        // the behaviour the note above records -- recomputing
        // UNCONDITIONALLY (leaving slot = -1) regressed
        // poly_dispatch_low/high, where this lookup misses.
        int slot = -1;
        for (int k = 0; k < cs->sym->has.n; k++)
          if (cs->sym->has[k] && cs->sym->has[k]->name == method_name && cg_field_live(cs->sym, k)) { slot = k; break; }
        if (slot < 0 && !self_is_union) slot = direct_slot;
        if (slot < 0) {
          if (dbg_slot)
            fprintf(stderr, "[polyslot] %s: cs %s has NO LIVE SLOT for it\n", method_name,
                    cs->sym->name ? cs->sym->name : "?");
          continue;
        }
        int n_defs = 0, n_es_defs = 0, n_live = 0;
        for (AVar *def_av : cs->defs) {
          ++n_defs;
          if (!def_av || !def_av->contour_is_entry_set) continue;
          ++n_es_defs;
          EntrySet *creator_es = (EntrySet *)def_av->contour;
          Fun *fun_new = creator_es->fun;
          if (!fun_new || !fun_new->live) continue;
          ++n_live;
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
        if (dbg_slot)
          fprintf(stderr, "[polyslot] %s fun=%d selfunion=%d selftype=%s : cs %s slot %d defs=%d es_defs=%d live=%d\n",
                  method_name, fun_val->id, self_is_union ? 1 : 0,
                  (self_type && self_type->name) ? self_type->name : "-", cs->sym->name ? cs->sym->name : "?", slot,
                  n_defs, n_es_defs, n_live);
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

// issues/112: two clones of one source function can reach a call site
// with IDENTICAL C signatures -- same Sym, and every argument the same
// `type` POINTER, element type included. They differ only in FA contour
// identity, which C has no way to discriminate and no reason to: either
// one is a correct callee for these argument types. Measured on a
// copy-of-copy of a tuple, where `self[k]` inside tuple.__deepcopy__
// drew two clones of tuple::__getitem__ whose receivers were both
// `tuple` with element `T` at the same addresses.
//
// Deliberately strict: same Sym, same arity, same type pointer at every
// position, same return type. Candidates that differ anywhere are still
// a genuine polymorphic call and still fall through to the dispatch
// machinery (and, failing that, the "matching function not found"
// assert), so this cannot paper over a real ambiguity.
static bool identical_c_signature(Fun *a, Fun *b) {
  if (!a || !b || a->sym != b->sym) return false;
  if (a->args.n != b->args.n) return false;
  if (a->sym->ret != b->sym->ret) return false;
  if (a->rets.n != b->rets.n) return false;
  for (int i = 0; i < a->rets.n; i++)
    if ((a->rets[i] ? a->rets[i]->type : nullptr) != (b->rets[i] ? b->rets[i]->type : nullptr)) return false;
  for (int i = 0; i < a->args.n; i++) {
    if (!a->args[i].key) continue;
    Var *va = a->args[i].value;
    Var *vb = b->args.get(a->args[i].key);
    if (!va || !vb) return false;
    if (va->type != vb->type) return false;
  }
  return true;
}

// ifa/129: how many call sites resolve to ONE target (a direct call) and
// how many stay polymorphic (a call through a method-pointer slot).
//
// The `splitter_*.py` tests used to assert which splitter STAGES fired.
// That pins the ROUTE the analysis took, not the property that matters, so
// it breaks under any architecture change even when the emitted code is
// identical or better -- measured under PYC_CSDCPA1, which changes all four
// stage lists while three of the four emit the same calls. This is the
// property itself: "we should get direct calls if at all possible."
//
// Counted HERE, from `get_target_fun_core`, and not in either emitter,
// because the two backends decide with different predicates -- cg.cc asks
// `get_target_fun`, cg_emit_llvm.cc asks `callees->n > 1` -- and a test
// golden is shared by both. The resolver is what both consult, and
// resolvability is a fact about the analysis result rather than about who
// prints it.
void report_call_resolution(FA *fa) {
  if (!getenv("PYC_DBG_CALLS")) return;
  long direct = 0, dynamic = 0;
  for (Fun *f : fa->pdb->funs) {
    if (!f->live) continue;
    Vec<PNode *> nodes;
    f->collect_PNodes(nodes);
    for (PNode *n : nodes) {
      if (n->code->kind != Code_SEND) continue;
      Vec<Fun *> *fns = f->calls.get(n);
      if (!fns || !fns->n) continue;  // no candidates: a primitive, not a call
      if (get_target_fun_core(n, f))
        ++direct;
      else
        ++dynamic;
    }
  }
  fprintf(stderr, "CALLS: direct=%ld dynamic=%ld\n", direct, dynamic);
}

Fun *get_target_fun_core(PNode *n, Fun *f) {
  Vec<Fun *> *fns = f->calls.get(n);
  if (!fns || !fns->n) return nullptr;
  if (fns->n == 1) return fns->v[0];
  for (int i = 1; i < fns->n; i++)
    if (!identical_c_signature(fns->v[0], fns->v[i])) return nullptr;
  if (getenv("PYC_DBG_DISPATCH"))
    fprintf(stderr, "[dispatch] %d indistinguishable clones of %s collapsed\n", fns->n,
            fns->v[0]->sym->name ? fns->v[0]->sym->name : "?");
  return fns->v[0];
}

// -------------------------------------------------------------
// Polymorphic call-site classtag/nil-receiver resolution -- see
// codegen_common.h's declarations for the full rationale.
// -------------------------------------------------------------

void poly_dispatch_directly_owned(Vec<Fun *> *candidates, Vec<cchar *> &out) {
  if (!candidates) return;
  for (int fi = 0; fi < candidates->n; fi++) {
    Fun *fv = (*candidates)[fi];
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
          out.set_add(csym->name);
          break;
        }
      }
      break;
    }
  }
}

void poly_dispatch_classtag_targets(Fun *candidate, PNode *pn, Vec<cchar *> &directly_owned, Vec<Sym *> &classes,
                                     Vec<int> &slots, Vec<int> &rval_idxs) {
  if (!candidate || !candidate->sym || !candidate->sym->name || !pn) return;
  cchar *method_name = candidate->sym->name;
  MPosition argp;
  argp.push(1);
  bool found = false;
  for (int pi = 0; pi < candidate->sym->has.n + 2 && !found; pi++) {
    MPosition *cp = cannonicalize_mposition(argp);
    argp.inc();
    Var *argv = candidate->args.get(cp);
    if (!argv || !argv->type) continue;
    Sym *csym = argv->type;
    Vec<Sym *> members;  // classes to search: the type itself, or its union members
    bool from_union = csym->type_kind == Type_SUM;
    if (from_union) {
      for (Sym *member : csym->has) if (member) members.add(member);
    } else {
      members.add(csym);
    }
    for (Sym *ccls : members) {
      // A union member that's DIRECTLY, singularly owned by ANOTHER
      // candidate keeps using that candidate's own override -- this
      // candidate's (looser, unioned) match must not steal it.
      if (from_union && ccls->name && directly_owned.set_in(ccls->name)) continue;
      for (int k = 0; k < ccls->has.n; k++) {
        if (ccls->has[k] && ccls->has[k]->name == method_name && cg_field_live(ccls, k)) {
          classes.add(ccls);
          slots.add(k);
          rval_idxs.add((int)Position2int(cp->pos[0]) - 1);
          found = true;
          break;
        }
      }
    }
  }
}

bool poly_dispatch_is_nil_receiver(Fun *candidate, PNode *pn, int *rval_idx) {
  if (!candidate || !candidate->sym || !pn) return false;
  MPosition np;
  np.push(1);
  for (int pi = 0; pi < candidate->sym->has.n + 2; pi++) {
    MPosition *cp = cannonicalize_mposition(np);
    np.inc();
    Var *argv = candidate->args.get(cp);
    if (!argv || !argv->type) continue;
    if (argv->type == sym_nil_type) {
      if (rval_idx) *rval_idx = (int)Position2int(cp->pos[0]) - 1;
      return true;
    }
  }
  return false;
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
  if (pn->prim->index == P_prim_await) {
    // issues/022: `await`'s RESULT can be a provable compile-time
    // constant (FA constant-propagates through the awaited function's
    // body) even when the OPERAND is a genuine, dynamically
    // constructed coroutine whose body still has to actually run --
    // suspending/resuming it isn't optional just because its return
    // value is known ahead of time. Checking the operand's OWN
    // constant-ness doesn't distinguish the two cases (FA's abstract
    // value for a real async call's result is exactly as constant as
    // a literal's -- that's the whole reason the result folds at
    // all); have to look at *how* the operand was produced instead:
    // if it's the result of a real call (Code_SEND with no prim --
    // `emit_send_call`'s case, per virtual_cg_emit_send below), it's
    // a genuine coroutine construction and must not be skipped. Only
    // a non-call operand (e.g. `await 42`, a literal -- not really
    // awaitable in real Python either, `co_await`ing one wouldn't
    // even compile; this is the sole reason P_prim_await is exempted
    // from the `nonfunctional` early-out above at all) is safe to
    // fold away. This check only sees the real call's Var (with a
    // proper `.def`) rather than a disconnected constant stand-in
    // because ifa/optimize/inline.cc's sub_constants leaves an
    // await's own operand alone -- and ifa/optimize/dead.cc's
    // mark_live_avars keeps it genuinely live even though it's
    // constant -- both carrying the identical exemption rationale.
    int o = (pn->rvals.n && pn->rvals.v[0]->sym == sym_primitive) ? 2 : 1;
    if (o >= pn->rvals.n) return false;
    Var *operand = pn->rvals.v[o];
    if (operand && operand->def && operand->def->code && operand->def->code->kind == Code_SEND &&
        !operand->def->prim)
      return false;
  }
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

// ifa/issues/123: run AFTER emission -- cg_slot_use_* is populated by the
// emitters, so reporting from inside cg_build_new_to_val_map (which runs
// BEFORE them) saw an empty set and called every slot unused.
void cg_report_slot_use(FA *fa) {
  // ifa/issues/123: WHICH method slots are actually needed?
  //
  // A member only has to occupy a slot if something dispatches through
  // it. Two facts here already decide that: `poly_names` is every method
  // name appearing at a POLYMORPHIC call site (fns->n > 1) anywhere, and
  // `cg_new_to_val_map` is every (creator, slot) the registry will
  // actually store into. A member that is in neither is reached only by
  // direct calls, and its slot is pure overhead -- 8 bytes per instance.
  //
  // Report-only. Eliminating them is NOT simply cg_field_live returning
  // 0: dropping a member changes the BYTE offsets of the members after
  // it (the `eN` suffix keeps the has-index, so the numbering does not
  // shift, but the C struct layout does), so two classes reached through
  // one union receiver must agree on the live SET -- the ifa/122 layout
  // contract. Elimination therefore has to be decided per prefix GROUP,
  // not per class.
  if (!getenv("IFA_DBG_SLOTUSE")) return;
  {
    Vec<int> stored_slots;  // flattened, per creator -- membership test only
    for (int i = 0; i < cg_new_to_val_map.n; i++)
      if (cg_new_to_val_map.v[i].key && cg_new_to_val_map.v[i].value)
        for (PolymorphicSlot &ps : *cg_new_to_val_map.v[i].value) stored_slots.set_add(ps.slot);
    // A member is only a METHOD slot if some function in the program
    // bears that name. Without this filter every DATA field counts as
    // "never dispatched" too -- mass/pos are not in poly_names and are
    // not stored by the registry either -- which overstated the figure
    // badly (bh read 65% before this line existed).
    Vec<cchar *> method_names;
    for (Fun *f : fa->funs)
      if (f && f->sym && f->sym->name) method_names.set_add(f->sym->name);
    // THE READ SIDE. A slot is also used when the member is read as an
    // ATTRIBUTE -- `f = obj.method`, or any attribute access whose
    // selector names a method -- which goes through cg.cc's generic
    // P_prim_period getter and never appears in `poly_names` (built from
    // polymorphic CALL sites) nor in the store registry. Eliminating a
    // slot that is only read this way turns the access into the
    // "getter not resolved" runtime assert.
    // Tracked PER CLASS, not per name: a name-global set says `foo` read
    // on ANY class marks `foo` used on EVERY class, which is far too
    // coarse to decide elimination.
    Vec<Sym *> read_cls;
    Vec<cchar *> read_nm;
    Vec<cchar *> read_names;  // the coarse set, kept only to contrast
    for (Fun *f : fa->funs)
      for (PNode *n : f->fa_all_PNodes) {
        if (!n || !n->prim || n->prim->index != P_prim_period) continue;
        if (n->rvals.n < 4 || !n->rvals[3]->sym || !n->rvals[3]->sym->is_symbol || !n->rvals[3]->sym->name) continue;
        cchar *nm = n->rvals[3]->sym->name;
        read_names.set_add(nm);
        for (EntrySet *es : f->ess) {
          if (!es) continue;
          AVar *obj = make_AVar(n->rvals[1], es);
          if (!obj || !obj->out) continue;
          for (CreationSet *rcs : *obj->out) if (rcs && rcs->type) { read_cls.add(rcs->type); read_nm.add(nm); }
        }
      }
    auto read_on = [&](Sym *cls, cchar *nm) {
      for (int k = 0; k < read_cls.n; k++)
        if (read_cls.v[k] == cls && read_nm.v[k] && !strcmp(read_nm.v[k], nm)) return true;
      return false;
    };
    long members = 0, dead = 0, dead_bytes = 0, methods = 0, coarse_only = 0, name_only = 0, unemitted = 0, unread = 0;
    Vec<Sym *> seen;
    for (CreationSet *cs : fa->css) {
      if (!cs || !cs->type || !seen.set_add(cs->type)) continue;
      Sym *t = cs->type;
      long cdead = 0;
      for (int i = 0; i < t->has.n; i++) {
        Sym *m = t->has[i];
        if (!m || !m->name || !cg_field_live(t, i)) continue;
        members++;
        if (!method_names.set_in(m->name)) continue;  // data field, not a slot
        methods++;
        bool is_poly = poly_names.set_in(m->name) != 0;
        bool is_stored = stored_slots.set_in(i) != 0;
        // Emission truth, not names: was an access to THIS slot of THIS
        // class actually emitted anywhere?
        bool emitted = false, emitted_read = false;
        for (int k = 0; k < cg_slot_use_cls.n; k++)
          if (cg_slot_use_cls.v[k] == t && cg_slot_use_idx.v[k] == i) {
            emitted = true;
            emitted_read = cg_slot_use_rd.v[k] != 0;
            break;
          }
        if (!emitted_read) unread++;
        bool is_read = read_on(t, m->name);
        if (is_read && !emitted) name_only++;
        if (read_names.set_in(m->name) && !is_read) coarse_only++;
        if (!emitted) unemitted++;
        if (!is_poly && !is_stored && !is_read) { dead++; cdead++; dead_bytes += 8; }
      }
      if (cdead && t->name)
        fprintf(stderr, "[slotuse] %-24s live=%d never-dispatched=%ld\n", t->name, t->has.n, cdead);
    }
    fprintf(stderr,
            "[slotuse] TOTAL live members=%ld of which method slots=%ld; never-dispatched=%ld "
            "(%ld%% of slots, %ld%% of members) ~%ld bytes\n",
            members, methods, dead, methods ? 100 * dead / methods : 0, members ? 100 * dead / members : 0,
            dead_bytes);
    fprintf(stderr, "[slotuse] (name-global read set would have spared %ld more)\n", coarse_only);
    fprintf(stderr, "[slotuse] BY EMISSION: method slots with no emitted access = %ld of %ld (%ld%%); "
                    "name-matching alone would have kept %ld of those alive\n",
            unemitted, methods, methods ? 100 * unemitted / methods : 0, name_only);
    // Name the slots that ARE read: with a precise call graph these
    // should be only the genuinely polymorphic dispatches.
    for (int k = 0; k < cg_slot_use_cls.n; k++)
      if (cg_slot_use_rd.v[k]) {
        Sym *t = cg_slot_use_cls.v[k];
        int i = cg_slot_use_idx.v[k];
        cchar *nm = (t && i >= 0 && i < t->has.n && t->has[i] && t->has[i]->name) ? t->has[i]->name : "?";
        if (!method_names.set_in(nm)) continue;  // data field read, not a slot
        fprintf(stderr, "[slotuse] READ-METHOD %s.e%d /* %s */\n", t && t->name ? t->name : "?", i, nm);
      }
    fprintf(stderr, "[slotuse] NEVER READ (only written, or untouched) = %ld of %ld method slots (%ld%%)\n",
            unread, methods, methods ? 100 * unread / methods : 0);
  }
}
