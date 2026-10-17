#include <time.h>
#include <map>
#include <string>
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

// See codegen_common.h.
Sym *cg_tag_family(Sym *t) {
  if (t && t->creators.n && t->creators[0] && t->creators[0]->sym) return t->creators[0]->sym;
  return t;
}

cchar *cg_tag_name(Sym *t) {
  Sym *f = cg_tag_family(t);
  char buf[256];
  snprintf(buf, sizeof(buf), "_CG_type_%s_%d", t && t->name ? t->name : "anon", f ? f->id : 0);
  return dupstr(buf);
}

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
int cg_isinstance_tag_targets(PNode *pn, Sym *cls_type, Vec<Sym *> &concrete) {
  // A builtin class reaches here as its meta Sym (`list` the value); the
  // type whose implementors are the instances is its meta_type.
  if (cls_type->is_meta_type && cls_type->meta_type) cls_type = cls_type->meta_type;
  for (Sym *impl : cls_type->implementors)
    if (impl && cg_has_classtag(impl)) concrete.add(impl);
  Sym *t = pn->rvals.n >= 3 ? pn->rvals[2]->type : nullptr;
  Vec<Sym *> members;
  if (t && t->type_kind == Type_SUM) {
    for (Sym *m : t->has) if (m) members.add(m);
  } else if (t)
    members.add(t);
  if (!members.n) return -1;
  // A member is an instance when it, or the class it was cloned from, is
  // an implementor -- clones are not in the implementor set themselves.
  auto is_instance = [&](Sym *m) {
    return cls_type->implementors.in(m) || cls_type->implementors.in(cg_tag_family(m));
  };
  int yes = 0, untagged_yes = 0;
  Sym *culprit = nullptr;
  for (Sym *m : members)
    if (is_instance(m)) {
      ++yes;
      if (!cg_has_classtag(m)) { ++untagged_yes; culprit = m; }
    }
  if (yes == members.n) return 1;
  if (!yes) return 0;
  if (untagged_yes) {
    std::string un;
    for (Sym *m : members) {
      if (!un.empty()) un += ", ";
      un += m->name ? m->name : "?";
    }
    codegen_fail(pn,
                 "isinstance(x, %s) cannot be decided at run time: x is {%s}, and '%s' carries no class tag to test. "
                 "pyc refuses it rather than fold the test to False (issues/173)",
                 cls_type->name ? cls_type->name : "?", un.c_str(), culprit->name ? culprit->name : "?");
  }
  return -1;
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
  // The condition here is `!t->element`: a receiver with NO element channel
  // reached a container method's `sizeof_element`. That is not necessarily a
  // {container, scalar} union, and saying so misdirects the reader --
  // measured on pystone, where the receiver is a bare `int64` and the old
  // text asserted a union that does not exist ("a variable holding 'int64'
  // has no representation", which is self-contradicting: one type always has
  // a representation). Report what is actually known, and separate the two
  // cases, because they have different causes:
  //
  //   union of container and scalar -> genuinely unrepresentable (issues/018)
  //   single non-container          -> a RESOLUTION defect; the receiver is
  //                                    not a container and should never have
  //                                    reached this method
  bool is_union = outer && outer->type_kind == Type_SUM && outer->has.n;
  cchar *loc_pfx = (in_builtin || !path) ? "" : "%s:%d: ";
  (void)loc_pfx;
  if (is_union) {
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
  } else {
    if (in_builtin || !path)
      fail("receiver %s is not a container but '%s' resolved to the CONTAINER method (it has no element "
           "channel, so sizeof_element has nothing to read). This is a call-resolution defect, not a "
           "representation gap -- see ifa/issues/137",
           un, fname);
    else
      fail("%s:%d: receiver %s is not a container but '%s' resolved to the CONTAINER method (it has no "
           "element channel, so sizeof_element has nothing to read). This is a call-resolution defect, not a "
           "representation gap -- see ifa/issues/137",
           path, line, un, fname);
  }
}

int cg_field_live(Sym *s, int i) {
  if (!s || i < 0 || i >= s->has.n) return 0;
  if (!s->has[i]->type) return 0;
  if (s->has[i]->var && !s->has[i]->var->live) return 0;
  return 1;
}

static bool clones_same_class(Fun *a, Fun *b);  // ifa/121, below
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
        //
        // ifa/187: and after clone() that struct is the CONCRETE type,
        // `cs->type`, not `cs->sym`. `cs->sym` can be the unspecialized
        // class, whose method members are never emitted and so are never
        // live: softrender's Vector4 CreationSets have `cs->sym` =
        // Vector4 (no live `add`) and `cs->type` = the clone that holds
        // `add` at e14, so they registered nothing, and their
        // constructors stored no `add`. The C backend hid it by also
        // storing the method into the prototype that every instance
        // copies; the LLVM backend installs slots only from this map, and
        // the dispatch called through NULL. The ancestry test above stays
        // on `cs->sym`: it is about the class, not its layout.
        Sym *ct = cs->type ? cs->type : cs->sym;
        int slot = -1;
        for (int k = 0; k < ct->has.n; k++)
          if (ct->has[k] && ct->has[k]->name == method_name && cg_field_live(ct, k)) { slot = k; break; }
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
            // ifa/121: two clones compete for one object's slot; the loser
            // is never called through it. Sound only if they are
            // interchangeable (or one overrides the other).
            if (getenv("IFA_DBG_CLONEEQ")) {
              Fun *prev = (*slots)[existing].fun_val;
              bool same_decl = prev && prev->sym == fun_val->sym;
              if (getenv("IFA_DBG_CLONEEQ")[0] == '2' && same_decl && !clones_same_class(prev, fun_val)) {
                extern void clone_eq_dump(Fun *, Fun *);
                clone_eq_dump(prev, fun_val);
              }
              fprintf(stderr, "CLONEEQ slot-compete %s ctor=%s slot=%d %s\n", method_name,
                      fun_new->sym && fun_new->sym->name ? fun_new->sym->name : "?", slot,
                      !same_decl ? "different-method(override)" : clones_same_class(prev, fun_val) ? "interchangeable"
                                                                                                   : "NOT-INTERCHANGEABLE");
            }
            // An OVERRIDE beats what it overrides, whatever order the two
            // register in. Both candidates passed the ancestor filter above,
            // so each is declared on `cs->sym` or an ancestor of it; Python
            // resolves the slot to the MOST-DERIVED of those. Before this, a
            // tie on specificity kept whichever registered first -- measured
            // on pygmy: `everythingshader.shade` calls `shader.shade(self, ..)`,
            // so a `shader.shade` clone has self = everythingshader too, and
            // it took everythingshader's slot at HEAD for spotshader and,
            // after ifa/151 reordered the clones, for everythingshader --
            // rendering one class's objects with the base shader either way.
            // Structural: declared owners and `specializers`, never names.
            {
              Fun *prev = (*slots)[existing].fun_val;
              Sym *po = (prev && prev->sym && prev->sym->has.n > 1) ? prev->sym->has[1]->must_specialize : nullptr;
              Sym *no = (fun_val->sym->has.n > 1) ? fun_val->sym->has[1]->must_specialize : nullptr;
              if (po && no && po != no) {
                if (po->specializers.set_in(no)) {  // new owner is more derived
                  (*slots)[existing].fun_val = fun_val;
                  (*slots)[existing].specificity = specificity;
                  continue;
                }
                if (no->specializers.set_in(po)) continue;  // existing is more derived
              }
            }
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

// ifa/121: THE DISPATCH-ARM PLAN. A class-tag arm used to call through the
// receiver's method slot, and cg_build_new_to_val_map fills each
// (constructor, slot) with ONE clone. Where two clones of a method compete
// for that slot, the loser is never called -- even at a site whose FA edge
// chose it. So each arm is decided from what the SITE chose:
//
//   ARM_SLOT     every clone installed in that class's slot is
//                interchangeable with the site's clone: the slot call is
//                right (and ifa/186 can make the whole chain one call);
//   ARM_DIRECT   the site has one clone for the class and the slot may hold
//                another: call the site's clone directly after the tag test;
//   ARM_AMBIGUOUS the site itself has two non-interchangeable clones for one
//                class, which no tag test separates.
//
// The objects reaching an arm are those whose tag is that class's
// (cg_tag_name, shared by all clones of a class), so the installed
// clones are read from every CreationSet whose concrete type carries it.
struct ArmPlanRec {
  int plan;
  Fun *fun;  // ARM_DIRECT: the clone the site chose for the class
};
static Map<PNode *, Map<Sym *, ArmPlanRec *> *> dispatch_arm_plans;  // keyed by tag family
static int arm_plan_counts[3];

// The clones the constructors of `cs` install in its slot for `method_name`.
static void installed_slot_clones(CreationSet *cs, cchar *method_name, Vec<Fun *> &out) {
  Sym *ct = cs->type ? cs->type : cs->sym;
  int slot = -1;
  for (int k = 0; k < ct->has.n; k++)
    if (ct->has[k] && ct->has[k]->name == method_name && cg_field_live(ct, k)) { slot = k; break; }
  static int dbg3 = -1;
  if (dbg3 < 0) { cchar *e = getenv("IFA_DBG_DISPATCHARMS"); dbg3 = e && e[0] == '3'; }
  if (dbg3) {
    fprintf(stderr, "  inst cs=%d ct=%s#%d slot=%d defs=%d:", cs->id, ct->name ? ct->name : "?", ct->id, slot, cs->defs.n);
    for (AVar *d : cs->defs) if (d) {
      Fun *fn = d->contour_is_entry_set ? ((EntrySet *)d->contour)->fun : nullptr;
      Vec<PolymorphicSlot> *ps = fn ? cg_new_to_val_map.get(fn) : nullptr;
      fprintf(stderr, " [es=%d fun=%d live=%d map=%d", d->contour_is_entry_set, fn ? fn->id : -1, fn ? fn->live : -1, ps ? ps->n : -1);
      if (ps) for (PolymorphicSlot &q : *ps) fprintf(stderr, " s%d", q.slot);
      fprintf(stderr, "]");
    }
    fprintf(stderr, "\n");
  }
  if (slot < 0) return;
  for (AVar *def_av : cs->defs) {
    if (!def_av || !def_av->contour_is_entry_set) continue;
    Fun *fun_new = ((EntrySet *)def_av->contour)->fun;
    Vec<PolymorphicSlot> *slots = fun_new ? cg_new_to_val_map.get(fun_new) : nullptr;
    if (slots)
      for (PolymorphicSlot &ps : *slots)
        if (ps.slot == slot && ps.fun_val) out.set_add(ps.fun_val);
  }
}

static bool all_interchangeable(Vec<Fun *> &v, Fun *with) {
  for (Fun *g : v)
    if (g && !clones_interchangeable(with, g)) return false;
  return true;
}

void cg_build_dispatch_arm_plans(FA *fa) {
  dispatch_arm_plans.clear();
  arm_plan_counts[0] = arm_plan_counts[1] = arm_plan_counts[2] = 0;
  const bool dbg = getenv("IFA_DBG_DISPATCHARMS") != nullptr;
  for (Fun *f : fa->funs) {
    if (!f || !f->live) continue;
    for (int ci = 0; ci < f->calls.n; ci++) {
      PNode *pn = f->calls[ci].key;
      Vec<Fun *> *fns = f->calls[ci].value;
      if (!pn || !fns || fns->n < 2) continue;
      Vec<cchar *> directly_owned;
      poly_dispatch_directly_owned(fns, directly_owned);
      // Per receiver CreationSet, the site's candidates for it -- read from
      // each candidate's EntrySets (its receiver formal's CreationSets), so a
      // class with two clones split by creation point is resolved per object.
      Map<CreationSet *, Vec<Fun *> *> by_cs;
      Map<Sym *, Vec<Sym *> *> types_of;  // tag family -> its concrete types
      Vec<Sym *> names;
      for (Fun *c : *fns) {
        if (!c || !c->sym || !c->sym->name) continue;
        Vec<Sym *> rts;
        Vec<int> sl, ri;
        poly_dispatch_classtag_targets(c, pn, directly_owned, rts, sl, ri);
        for (int k = 0; k < rts.n; k++) {
          Sym *rt = rts[k];
          if (!rt || !rt->name || rt->is_system_type || !cg_has_classtag(rt)) continue;
          Sym *fam = cg_tag_family(rt);
          Vec<Sym *> *ts = types_of.get(fam);
          if (!ts) { ts = new Vec<Sym *>; types_of.put(fam, ts); names.add(fam); }
          ts->set_add(rt);
          MPosition mp;
          mp.push(ri[k] + 1);
          MPosition *cp = cannonicalize_mposition(mp);
          auto note = [&](AVar *a) {
            if (!a || !a->out) return;
            for (CreationSet *cs : a->out->sorted) {
              if (!cs || (cs->type ? cs->type : cs->sym) != rt) continue;
              Vec<Fun *> *v = by_cs.get(cs);
              if (!v) { v = new Vec<Fun *>; by_cs.put(cs, v); }
              v->set_add(c);
            }
          };
          // What THIS site passes: the candidate's in-edges from this
          // caller's contours at this call (codegen's PNode is the clone's
          // copy; it shares the original's `code`), filtered per edge. An
          // EntrySet's own receiver set is the union over all its callers,
          // so it is only the fallback when no edge is matched.
          bool matched = false;
          for (EntrySet *es : c->ess) {
            if (!es) continue;
            for (AEdge *e : es->edges) {
              if (!e || !e->from || !e->pnode || e->pnode->code != pn->code || !f->ess.in(e->from)) continue;
              AVar *a = e->filtered_args.get(cp);
              if (!a) a = e->args.get(cp);
              if (a) { matched = true; note(a); }
            }
          }
          if (!matched)
            for (EntrySet *es : c->ess)
              if (es) note(es->args.get(cp));
        }
      }
      if (!names.n) continue;
      Map<Sym *, ArmPlanRec *> *plans = new Map<Sym *, ArmPlanRec *>;
      for (Sym *fam : names) {
        cchar *nm = fam->name ? fam->name : "?";
        Vec<Sym *> *ts = types_of.get(fam);
        ts->set_to_vec();
        bool cs_ambiguous = false, one_for_tag = true, slot_ok = true;
        Fun *tag_fun = nullptr;
        int reached = 0;
        for (Sym *t : *ts)
          for (CreationSet *cs : t->creators) {
            if (!cs) continue;
            Vec<Fun *> *cands = by_cs.get(cs);
            if (!cands || !cands->n) continue;  // never reaches this site
            reached++;
            cands->set_to_vec();
            // One object, two non-interchangeable clones: chosen by something
            // other than the receiver, which neither a tag nor a slot carries.
            if (!all_interchangeable(*cands, cands->v[0])) {
              cs_ambiguous = true;
              if (dbg && getenv("IFA_DBG_DISPATCHARMS")[0] == '2') {
                fprintf(stderr, "  ambiguous cs=%d ct=%d defs=%d:", cs->id, t->id, cs->defs.n);
                for (Fun *g : *cands) fprintf(stderr, " fun%d", g->id);
                fprintf(stderr, "\n");
              }
            }
            if (!tag_fun) tag_fun = cands->v[0];
            else if (!clones_interchangeable(tag_fun, cands->v[0])) one_for_tag = false;
            Vec<Fun *> inst;
            installed_slot_clones(cs, cands->v[0]->sym->name, inst);
            if (!inst.n || !all_interchangeable(inst, cands->v[0])) {
              slot_ok = false;
              if (dbg && getenv("IFA_DBG_DISPATCHARMS")[0] == '2')
                fprintf(stderr, "  why %s.%s cs=%d: %s (installed %d)\n", nm, cands->v[0]->sym->name, cs->id,
                        !inst.n ? "nothing installed" : "installed clone differs", inst.n);
            }
          }
        // No receiver CreationSet traced to this tag (a candidate reached by
        // a union formal the walk above does not see): the slot is unproven,
        // and the emitter's own candidate is the one to call.
        if (!reached) slot_ok = false;
        int plan = cs_ambiguous ? ARM_AMBIGUOUS : slot_ok ? ARM_SLOT : one_for_tag ? ARM_DIRECT : ARM_AMBIGUOUS;
        ArmPlanRec *rec = new ArmPlanRec;
        rec->plan = plan;
        rec->fun = tag_fun;
        plans->put(fam, rec);
        arm_plan_counts[plan]++;
        if (dbg && plan != ARM_SLOT)
          fprintf(stderr, "DISPATCHARM %s %s:%d %s.%s types=%d reached=%d%s\n", plan == ARM_DIRECT ? "direct" : "AMBIGUOUS",
                  pn->code ? pn->code->pathname() : "?", pn->code ? pn->code->line() : 0, nm,
                  tag_fun && tag_fun->sym->name ? tag_fun->sym->name : "?", ts->n, reached,
                  cs_ambiguous ? " (one object, two clones)" : "");
      }
      dispatch_arm_plans.put(pn, plans);
    }
  }
  if (dbg)
    fprintf(stderr, "DISPATCHARM arms slot=%d direct=%d ambiguous=%d\n", arm_plan_counts[0], arm_plan_counts[1],
            arm_plan_counts[2]);
}

static ArmPlanRec *dispatch_arm_rec(PNode *pn, Sym *cls) {
  Map<Sym *, ArmPlanRec *> *plans = pn ? dispatch_arm_plans.get(pn) : nullptr;
  if (!plans || !cls) return nullptr;
  return plans->get(cg_tag_family(cls));
}

int dispatch_arm_plan(PNode *pn, Sym *cls) {
  ArmPlanRec *r = dispatch_arm_rec(pn, cls);
  return r ? r->plan : ARM_SLOT;
}

// See codegen_common.h.
void apply_dispatch_arm_plans(PNode *pn, Vec<Fun *> *fns, Vec<Sym *> &classes, Vec<int> &slots, Vec<Fun *> &class_funs) {
  static int on = -1;
  if (on < 0) { cchar *e = getenv("PYC_DISPATCH_ARM_PLANS"); on = e ? atoi(e) : 1; }  // 0: the old slot-only arms, for A/B
  if (!on) return;
  for (int ci = 0; ci < classes.n; ci++) {
    if (slots[ci] == -1) continue;  // already a direct call (a closure carrier)
    ArmPlanRec *r = dispatch_arm_rec(pn, classes[ci]);
    if (!r || r->plan == ARM_SLOT) continue;
    if (r->plan == ARM_AMBIGUOUS) {
      // One receiver object reaches two clones that differ in behaviour,
      // chosen by a non-receiver argument (softrender's Vector4.add over
      // int- and float-field Vector4s). Both share one class tag, so no
      // test separates them, and the slot holds the one its constructor
      // installed. Calling it is the old behaviour, sound only if the
      // argument's concrete type is that clone's at run time: an
      // accommodation, so permissive mode keeps it and says so, and
      // --strict refuses (issues/171).
      if (!fruntime_errors)
        codegen_unresolved_send(pn, fns,
                                "one receiver object reaches two clones that are not interchangeable, chosen by an "
                                "argument that neither its class tag nor its method slot carries (ifa/121)");
      static Vec<PNode *> warned;
      if (warned.set_add(pn))
        fprintf(stderr,
                "%s:%d: warning: the call to '%s' reaches two versions of it for one receiver object, chosen by an "
                "argument's type; the object's method slot decides, which is right only if that argument has the type "
                "its version expects. Permissive, refused under --strict (ifa/121)\n",
                pn->code ? pn->code->pathname() : "?", pn->code ? pn->code->line() : 0,
                r->fun && r->fun->sym && r->fun->sym->name ? r->fun->sym->name : "?");
      continue;
    }
    if (r->fun) {
      slots[ci] = -1;
      class_funs[ci] = r->fun;
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
// The test is on the EMITTED C SIGNATURE, not on `type` pointers.
// 983ac5a4 widened it and this comment kept describing the old rule, so
// state what it actually does now:
//
//   - same Sym and same arity, as before;
//   - a parameter or return whose `type` POINTER differs still matches
//     when `c_type()` gives the same string, because two Syms that print
//     as one C type ARE one C type to the caller -- that is the whole
//     question being asked here;
//   - a DEAD parameter is skipped, and both sides must agree on which
//     ones are dead. `write_c_fun_proto` (cg.cc) emits no parameter for
//     a `!live` formal, so a dead argument is not part of the signature
//     it would be compared against.
//
// What it therefore does NOT establish is that the two funs BEHAVE the
// same -- only that C cannot tell their signatures apart, so a caller
// can be emitted against either. That is the right question for
// `get_target_fun_core` and for cg.cc's untagged-direct route, which use
// it to collapse indistinguishable clones of ONE source function;
// it would be the wrong question for anything choosing between different
// functions. Candidates that differ anywhere are still a genuine
// polymorphic call and still fall through to the dispatch machinery
// (and, failing that, the "matching function not found" assert).
bool identical_c_signature(Fun *a, Fun *b) {
  if (!a || !b) return false;
  if (a->sym != b->sym) {
    if (getenv("PYC_DBG_DISPATCH")) fprintf(stderr, "IDENT: sym mismatch %s vs %s\n", a->sym->name, b->sym->name);
    return false;
  }
  if (a->args.n != b->args.n) {
    if (getenv("PYC_DBG_DISPATCH")) fprintf(stderr, "IDENT %s: args.n %d vs %d\n", a->sym->name, a->args.n, b->args.n);
    return false;
  }
  if (a->sym->ret != b->sym->ret) {
    if (getenv("PYC_DBG_DISPATCH")) fprintf(stderr, "IDENT %s: sym->ret mismatch\n", a->sym->name);
    return false;
  }
  if (a->rets.n != b->rets.n) {
    if (getenv("PYC_DBG_DISPATCH")) fprintf(stderr, "IDENT %s: rets.n %d vs %d\n", a->sym->name, a->rets.n, b->rets.n);
    return false;
  }
  for (int i = 0; i < a->rets.n; i++) {
    Sym *ta = a->rets[i] ? a->rets[i]->type : nullptr;
    Sym *tb = b->rets[i] ? b->rets[i]->type : nullptr;
    if (ta != tb) {
      cchar *ca = a->rets[i] ? c_type(a->rets[i]) : "_CG_void";
      cchar *cb = b->rets[i] ? c_type(b->rets[i]) : "_CG_void";
      if (strcmp(ca, cb) != 0) {
        if (getenv("PYC_DBG_DISPATCH")) fprintf(stderr, "IDENT %s: ret[%d] type %s vs %s\n", a->sym->name, i, ca, cb);
        return false;
      }
    }
  }
  for (int i = 0; i < a->args.n; i++) {
    if (!a->args[i].key) continue;
    Var *va = a->args[i].value;
    Var *vb = b->args.get(a->args[i].key);
    if (!va || !vb) {
      if (getenv("PYC_DBG_DISPATCH")) fprintf(stderr, "IDENT %s: arg[%d] null va=%p vb=%p\n", a->sym->name, i, va, vb);
      return false;
    }
    if (va->live != vb->live) {
      if (getenv("PYC_DBG_DISPATCH")) fprintf(stderr, "IDENT %s: arg[%d] live %d vs %d\n", a->sym->name, i, va->live, vb->live);
      return false;
    }
    if (!va->live) continue;
    if (va->type != vb->type) {
      cchar *ca = c_type(va);
      cchar *cb = c_type(vb);
      if (strcmp(ca, cb) != 0) {
        if (getenv("PYC_DBG_DISPATCH")) fprintf(stderr, "IDENT %s: arg[%d] type %s vs %s\n", a->sym->name, i, ca, cb);
        return false;
      }
    }
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
static Sym *method_receiver_type(Fun *cand, PNode *pn);
void report_call_resolution(FA *fa) {
  if (!getenv("PYC_DBG_CALLS")) return;
  long direct = 0, dynamic = 0, niltest = 0;
  for (Fun *f : fa->pdb->funs) {
    if (!f->live) continue;
    Vec<PNode *> nodes;
    f->collect_PNodes(nodes);
    for (PNode *n : nodes) {
      if (n->code->kind != Code_SEND) continue;
      Vec<Fun *> *fns = f->calls.get(n);
      if (!fns || !fns->n) continue;  // no candidates: a primitive, not a call
      if (Fun *t = get_target_fun_core(n, f)) {
        ++direct;
        // PYC_DBG_CALLS=2 names each direct call, to attribute a count change.
        if (atoi(getenv("PYC_DBG_CALLS")) >= 2)
          fprintf(stderr, "CALL %s -> %s\n", f->sym->name ? f->sym->name : "?", t->sym->name ? t->sym->name : "?");
      } else if (nil_tested_target(n, f, nullptr)) {
        ++niltest;
      } else {
        ++dynamic;
        // PYC_DBG_CALLS=3 names each dynamic call and its candidates.
        if (atoi(getenv("PYC_DBG_CALLS")) >= 3) {
          fprintf(stderr, "DYNCALL %s:%d in %s ->", n->code->pathname(), n->code->line(),
                  f->sym->name ? f->sym->name : "?");
          for (Fun *c : *fns)
            if (Sym *rt = method_receiver_type(c, n))
              fprintf(stderr, " %s#%d(%s:%d)[%s]", c->sym->name ? c->sym->name : "?", c->id, c->sym->pathname(),
                      c->sym->line(), rt->name ? rt->name : (rt->type_kind == Type_SUM ? "union" : "?"));
            else
              fprintf(stderr, " %s#%d(%s:%d)", c->sym->name ? c->sym->name : "?", c->id, c->sym->pathname(), c->sym->line());
          // The receivers' common user-defined ancestor, if any:
          // shared-ancestor polymorphism is hoisted, an unrelated union is
          // a precision failure (AGENTS.md).
          Vec<Sym *> recvs;
          for (Fun *c : *fns)
            if (Sym *rt = method_receiver_type(c, n)) {
              if (rt->type_kind == Type_SUM) {
                for (Sym *m : rt->has) if (m) recvs.set_add(m);
              } else
                recvs.set_add(rt);
            }
          Vec<Sym *> common;
          bool first = true;
          for (Sym *r : recvs) {
            if (!r || r == sym_nil_type) continue;
            // A clone's `specializes` names the ORIGINAL parents; compare
            // originals, which a clone's creators carry.
            if (r->creators.n && r->creators[0] && r->creators[0]->sym) r = r->creators[0]->sym;
            Vec<Sym *> anc, work;
            anc.set_add(r);
            work.add(r);
            for (int i = 0; i < work.n; i++)
              for (Sym *p : work[i]->specializes)
                if (p && anc.set_add(p)) work.add(p);
            if (first) {
              for (Sym *a : anc) if (a) common.set_add(a);
              first = false;
            } else {
              Vec<Sym *> keep;
              for (Sym *a : common) if (a && anc.set_in(a)) keep.set_add(a);
              common.move(keep);
            }
          }
          fprintf(stderr, " ancestors:");
          for (Sym *a : common)
            if (a && !a->is_builtin && !a->is_system_type) fprintf(stderr, " %s", a->name ? a->name : "?");
          fprintf(stderr, "\n");
        }
      }
    }
  }
  // A nullable receiver decided by a null test (nil_tested_target) is not a
  // dispatch; it is reported apart, and only when there is one, so a program
  // without one keeps its line.
  if (niltest)
    fprintf(stderr, "CALLS: direct=%ld dynamic=%ld niltest=%ld\n", direct, dynamic, niltest);
  else
    fprintf(stderr, "CALLS: direct=%ld dynamic=%ld\n", direct, dynamic);
}

// ifa/132: identical C signatures do not make two clones interchangeable
// when their RECORD arguments' members hold different contents. Two
// CreationSets of one class can share a struct (each member has the same
// C type) while a member holds an int-list in one and a float-list in the
// other -- `_CG_list` does not say which -- so each clone's body reads its
// own receiver's elements differently. quameon: after the per-receiver
// split of `coulomb_pot.compute_en_value`, the collapse called one clone
// for both receivers and read the int-list `charges` as float64.
// Refusing the collapse sends the call through the record's own method
// slot, which each instance stores per creation point. Only RECORD
// arguments are compared: those carry a tag and a slot to dispatch on.
static void record_member_contents(Fun *f, MPosition *p, Map<cchar *, Vec<CreationSet *> *> &out) {
  for (EntrySet *es : f->ess) {
    if (!es) continue;
    AVar *av = es->args.get(p);
    if (!av || !av->out || !av->out->type) continue;
    for (CreationSet *cs : av->out->type->sorted) {
      if (!cs || !cs->sym || cs->sym->type_kind != Type_RECORD) continue;
      for (AVar *iv : cs->vars) {
        if (!iv || !iv->var || !iv->var->sym || !iv->var->sym->name || !iv->out || !iv->out->type) continue;
        Vec<CreationSet *> *v = out.get(iv->var->sym->name);
        if (!v) out.put(iv->var->sym->name, (v = new Vec<CreationSet *>));
        for (CreationSet *c : iv->out->type->sorted) if (c) v->set_add(c);
      }
    }
  }
}

static bool clones_see_same_record_contents(Fun *a, Fun *b) {
  for (MPosition *p : a->positional_arg_positions) {
    Map<cchar *, Vec<CreationSet *> *> ma, mb;
    record_member_contents(a, p, ma);
    record_member_contents(b, p, mb);
    for (int i = 0; i < ma.n; i++) {
      if (!ma.v[i].key) continue;
      Vec<CreationSet *> *va = ma.v[i].value, *vb = mb.get(ma.v[i].key);
      if (!vb) continue;  // a member only one side reads is not a disagreement
      if (va->set_count() != vb->set_count()) return false;
      for (CreationSet *c : *va) if (c && !vb->set_in(c)) return false;
    }
  }
  return true;
}

// The type of a method candidate's RECEIVER formal: on a method send, the
// first formal after the selector.
static Sym *method_receiver_type(Fun *cand, PNode *pn) {
  if (!cand || !cand->sym || !pn || !pn->rvals.n || !pn->rvals[0] || !pn->rvals[0]->sym ||
      !pn->rvals[0]->sym->is_symbol)
    return nullptr;
  MPosition p;
  p.push(1);
  for (int pi = 0; pi < cand->sym->has.n + 2; pi++) {
    MPosition *cp = cannonicalize_mposition(p);
    p.inc();
    Var *av = cand->args.get(cp);
    if (!av || !av->type) continue;
    if (av->type == sym_symbol) continue;
    return av->type;
  }
  return nullptr;
}

// A method send on a NULLABLE receiver -- pointer types sharing one method, or
// None -- is not a
// dispatch: the null test decides it, and each arm calls one known function.
// Returns the non-None target (and the None class's method in `*nil_fn`) when
// the candidates are exactly that pair. Shared by the C emitter and the call
// census so the two agree.
Fun *nil_tested_target(PNode *n, Fun *f, Fun **nil_fn) {
  Vec<Fun *> *fns = f ? f->calls.get(n) : nullptr;
  if (!fns || fns->n != 2) return nullptr;
  Fun *nil = nullptr, *other = nullptr;
  for (Fun *c : *fns) {
    Sym *rt = method_receiver_type(c, n);
    if (!rt) return nullptr;
    if (rt == sym_nil_type) {
      if (nil) return nullptr;
      nil = c;
    } else {
      // A union receiver is several types sharing ONE method, which is what
      // a single candidate with a union `self` already is. Every member must
      // be a pointer, so that NULL means None and nothing else: a scalar
      // shares 0 with None. And the members must be all tagged (the emitter's
      // classtag arms, all calling this one candidate) or all untagged (its
      // untagged-direct arm), not a mix.
      if (other) return nullptr;
      int tagged = 0, untagged = 0;
      auto member = [&](Sym *m) {
        if (!m || m == sym_nil_type || m->num_kind) return false;
        if (cg_has_classtag(m)) ++tagged;
        else ++untagged;
        return true;
      };
      if (rt->type_kind == Type_SUM) {
        for (Sym *m : rt->has)
          if (!member(m)) return nullptr;
      } else if (!member(rt))
        return nullptr;
      if (tagged && untagged) return nullptr;
      other = c;
    }
  }
  if (!nil || !other) return nullptr;
  if (nil_fn) *nil_fn = nil;
  return other;
}

// ifa/121: CLONE EQUIVALENCE, before codegen. Two clones of one source
// function are INTERCHANGEABLE when their bodies are the same program:
// the same nodes in the same order, the same primitives, variables of the
// same types (numbered locally, in walk order), the same constants and
// globals, and calls to interchangeable targets. That last clause is
// recursive, so it is a partition refinement: start from the body with
// callees left out, then split classes by their callees' classes until
// nothing moves. Types are compared as Syms (union Syms are interned,
// ifa/120); a C type string is not needed.
//
// IFA_DBG_CLONEEQ=1 reports the partition, and get_target_fun_core reports
// each call site it collapses onto one clone whose candidates are NOT all
// in one class -- the narrowing ifa/121 says assumes an equivalence it
// never checks.
static Map<Fun *, int> clone_class_map;
static int clone_classes_ready = 0;
static std::string clone_body_key(Fun *f, bool with_callees) {
  std::string k;
  Map<Var *, int> vnum;
  auto var = [&](Var *v) {
    if (!v) { k += "_;"; return; }
    int id = vnum.get(v);
    if (!id) { id = vnum.n + 1; vnum.put(v, id); }
    k += "v" + std::to_string(id) + ":" + std::to_string(v->type ? v->type->id : 0) + (v->live ? "L" : "d");
    Sym *c = v->constant ? v->constant : (v->sym && v->sym->is_constant ? v->sym : nullptr);
    if (c) k += std::string("=") + (c->constant ? c->constant : "?") + "#" + std::to_string(c->type ? c->type->id : 0);
    else if (v->sym && !v->sym->is_local) k += "g" + std::to_string(v->sym->id);
    k += ";";
  };
  for (int i = 0; i < f->args.n; i++)
    if (f->args[i].key) { k += "a" + std::to_string(Position2int(f->args[i].key->pos[0])); var(f->args[i].value); }
  for (Var *r : f->rets) { k += "r"; var(r); }
  Map<PNode *, int> pnum;
  Vec<PNode *> order;
  if (f->entry) { pnum.put(f->entry, 1); order.add(f->entry); }
  for (int i = 0; i < order.n; i++)
    for (PNode *q : order[i]->cfg_succ)
      if (q && !pnum.get(q)) { pnum.put(q, order.n + 1); order.add(q); }
  for (PNode *n : order) {
    k += "|" + std::to_string(n->code ? (int)n->code->kind : -1) + "p" + std::to_string(n->prim ? n->prim->index : -1) +
         (n->live ? "L" : "d") + (n->fa_live ? "F" : "f") + ":";
    for (Var *v : n->lvals) var(v);
    k += "<";
    for (Var *v : n->rvals) var(v);
    for (PNode *m : n->phi) { k += "phi"; for (Var *v : m->lvals) var(v); for (Var *v : m->rvals) var(v); }
    for (PNode *m : n->phy) { k += "phy"; for (Var *v : m->lvals) var(v); for (Var *v : m->rvals) var(v); }
    k += ">";
    for (PNode *q : n->cfg_succ) k += std::to_string(q ? pnum.get(q) : 0) + ",";
    if (with_callees)
      if (Vec<Fun *> *fs = f->calls.get(n)) {
        Vec<int> cls;
        for (Fun *t : *fs) cls.set_add(t ? clone_class_map.get(t) : 0);
        cls.set_to_vec();
        if (cls.n > 1) qsort(cls.v, cls.n, sizeof(int), [](const void *a, const void *b) { return *(const int *)a - *(const int *)b; });
        k += "c";
        for (int c : cls) k += std::to_string(c) + ",";
      }
  }
  return std::to_string(f->sym ? f->sym->id : 0) + "#" + k;
}

void compute_clone_equivalence(FA *fa) {
  struct timespec t0, t1;
  clock_gettime(CLOCK_MONOTONIC, &t0);
  clone_class_map.clear();
  Vec<Fun *> funs;
  for (Fun *f : fa->funs) if (f && f->live) funs.add(f);
  std::map<std::string, int> ids;
  for (Fun *f : funs) {
    std::string key = clone_body_key(f, false);
    auto it = ids.find(key);
    int c = it != ids.end() ? it->second : (ids[key] = (int)ids.size() + 1);
    clone_class_map.put(f, c);
  }
  int nclasses = (int)ids.size(), rounds = 0;
  for (;;) {
    rounds++;
    std::map<std::string, int> next;
    Map<Fun *, int> nm;
    for (Fun *f : funs) {
      std::string key = std::to_string(clone_class_map.get(f)) + "@" + clone_body_key(f, true);
      auto it = next.find(key);
      int c = it != next.end() ? it->second : (next[key] = (int)next.size() + 1);
      nm.put(f, c);
    }
    for (Fun *f : funs) clone_class_map.put(f, nm.get(f));
    if ((int)next.size() == nclasses) break;
    nclasses = (int)next.size();
  }
  clone_classes_ready = 1;
  if (getenv("IFA_DBG_CLONEEQ")) {
    std::map<int, int> sz;
    for (Fun *f : funs) sz[clone_class_map.get(f)]++;
    int dup = 0;
    for (auto &kv : sz) dup += kv.second - 1;
    clock_gettime(CLOCK_MONOTONIC, &t1);
    fprintf(stderr, "CLONEEQ live_funs=%d classes=%d interchangeable_duplicates=%d rounds=%d ms=%.1f\n", funs.n, nclasses,
            dup, rounds, (t1.tv_sec - t0.tv_sec) * 1e3 + (t1.tv_nsec - t0.tv_nsec) / 1e6);
  }
}

void clone_eq_dump(Fun *a, Fun *b) {
  std::string ka = clone_body_key(a, false), kb = clone_body_key(b, false);
  // first differing token, and the arg/ret types of both
  size_t i = 0;
  while (i < ka.size() && i < kb.size() && ka[i] == kb[i]) i++;
  size_t st = i > 120 ? i - 120 : 0;
  fprintf(stderr, "CLONEEQ-diff fun %d vs %d (len %zu vs %zu) at %zu:\n  A ...%s\n  B ...%s\n", a->id, b->id, ka.size(),
          kb.size(), i, ka.substr(st, 260).c_str(), kb.substr(st, 260).c_str());
  for (Fun *f : {a, b}) {
    fprintf(stderr, "  fun %d args:", f->id);
    for (int j = 0; j < f->args.n; j++)
      if (f->args[j].key && f->args[j].value)
        fprintf(stderr, " %s:%s%s", f->args[j].value->sym && f->args[j].value->sym->name ? f->args[j].value->sym->name : "_",
                f->args[j].value->type && f->args[j].value->type->name ? f->args[j].value->type->name : "?",
                f->args[j].value->live ? "" : "(dead)");
    fprintf(stderr, "\n");
  }
}

static bool clones_same_class(Fun *a, Fun *b) {
  if (!clone_classes_ready) return false;
  int c = clone_class_map.get(a);
  return c && c == clone_class_map.get(b);
}

// See codegen_common.h.
bool clones_interchangeable(Fun *a, Fun *b) {
  if (a == b) return true;
  if (!clone_classes_ready) return true;  // a caller that never computed them (the V frontend): no claim
  bool same = clones_same_class(a, b);
  if (!same && getenv("IFA_DBG_CLONEEQ"))
    fprintf(stderr, "CLONEEQ refuse-collapse %s: fun %d vs %d are not interchangeable\n",
            a->sym && a->sym->name ? a->sym->name : "?", a->id, b->id);
  return same;
}

static bool candidates_interchangeable(Vec<Fun *> *fns) {
  if (!clone_classes_ready) return true;  // not computed: no claim
  int c = clone_class_map.get(fns->v[0]);
  for (int i = 1; i < fns->n; i++)
    if (!c || clone_class_map.get(fns->v[i]) != c) return false;
  return true;
}

Fun *get_target_fun_core(PNode *n, Fun *f) {
  Vec<Fun *> *fns = f->calls.get(n);
  if (!fns || !fns->n) return nullptr;
  if (fns->n == 1) return fns->v[0];
  // ifa/121: agreeing on the C signature lets a caller be EMITTED against
  // either clone; it does not make them the same function. Collapsing
  // onto fns->v[0] also needs them interchangeable (compute_clone_equivalence).
  for (int i = 1; i < fns->n; i++)
    if (!identical_c_signature(fns->v[0], fns->v[i]) || !clones_see_same_record_contents(fns->v[0], fns->v[i]) ||
        !clones_interchangeable(fns->v[0], fns->v[i]))
      return nullptr;
  if (getenv("PYC_DBG_DISPATCH"))
    fprintf(stderr, "[dispatch] %d indistinguishable clones of %s collapsed\n", fns->n,
            fns->v[0]->sym->name ? fns->v[0]->sym->name : "?");
  if (getenv("IFA_DBG_CLONEEQ")) {
    static Vec<PNode *> reported;
    if (reported.set_add(n))
      fprintf(stderr, "CLONEEQ collapse%s %s:%d %s candidates=%d\n", candidates_interchangeable(fns) ? "" : "-not-equivalent",
              n->code ? n->code->pathname() : "?", n->code ? n->code->line() : 0,
              fns->v[0]->sym->name ? fns->v[0]->sym->name : "?", fns->n);
  }
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
  // On a METHOD send the receiver is the first formal after the selector,
  // and only it is asked. Scanning on past it found `__pyc_None_type__
  // .__eq__(self, x)`'s `x : V` when `self` (None) carries no `__eq__`, so
  // the None method was taken for a V-tag candidate, its nil branch was
  // never emitted, and `None == v` on a {V, None} receiver read a tag
  // through NULL (chull's `fv.vertex[i] != e.endpts[0]`).
  const bool method_send = pn->rvals.n && pn->rvals[0] && pn->rvals[0]->sym && pn->rvals[0]->sym->is_symbol;
  for (int pi = 0; pi < candidate->sym->has.n + 2 && !found; pi++) {
    MPosition *cp = cannonicalize_mposition(argp);
    argp.inc();
    Var *argv = candidate->args.get(cp);
    if (!argv || !argv->type) continue;
    Sym *csym = argv->type;
    if (method_send && csym == sym_symbol) continue;  // the selector
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
    if (method_send) break;
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

// ifa/185: is `t` a tuple type, record or list layout? Tuples have no
// classtag (cg_has_classtag), so they are recognised by their creators,
// the way cg_has_classtag recognises a list.
static bool cg_is_tuple_type(Sym *t) {
  if (!t || !t->creators.n) return false;
  Sym *k = t->creators[0]->sym;
  return k == sym_tuple || (k && k->type == sym_tuple);
}

// See codegen_common.h.
bool cg_tuple_type_fixed_arity(Sym *t) {
  if (!cg_is_tuple_type(t)) return false;
  for (CreationSet *cs : t->creators)
    if (cs && (cs->no_static_arity || cs->static_arity < 0)) return false;
  return true;
}

// Can a runtime length test discriminate `v`'s values? Every member of its
// type must be a tuple or None.
static bool arity_testable_operand(Var *v) {
  Sym *t = v ? v->type : nullptr;
  if (!t) return false;
  if (t->type_kind != Type_SUM) return cg_is_tuple_type(t);
  for (Sym *m : t->has)
    if (m && m != sym_nil_type && !cg_is_tuple_type(m)) return false;
  return true;
}

static bool operand_may_be_nil(Var *v) {
  Sym *t = v ? v->type : nullptr;
  if (!t) return false;
  if (t == sym_nil_type) return true;
  if (t->type_kind != Type_SUM) return false;
  for (Sym *m : t->has)
    if (m == sym_nil_type) return true;
  return false;
}

// A list's layout: the shared header a tuple also has (pyc_c_runtime.h).
static bool cg_is_list_type(Sym *t) { return t && t->creators.n && t->creators[0]->sym == sym_list; }

// Every member of `v`'s type a list, a tuple or None: its header can be
// tested (ifa/102).
static bool layout_testable_operand(Var *v) {
  Sym *t = v ? v->type : nullptr;
  if (!t) return false;
  if (t->type_kind != Type_SUM) return cg_is_tuple_type(t) || cg_is_list_type(t);
  for (Sym *m : t->has)
    if (!m || (m != sym_nil_type && !cg_is_tuple_type(m) && !cg_is_list_type(m))) return false;
  return true;
}

// The header kinds a formal covers (LAYOUT_* bits), or -1 when it covers
// something that is not a list or a tuple.
static int formal_layout_mask(Var *av) {
  if (!av || !av->type) return -1;
  Vec<Sym *> ms;
  if (av->type->type_kind == Type_SUM) for (Sym *m : av->type->has) ms.add(m);
  else ms.add(av->type);
  int mask = 0;
  for (Sym *m : ms) {
    if (!m || m == sym_nil_type) return -1;
    if (cg_tuple_type_fixed_arity(m)) mask |= LAYOUT_FIXED_TUPLE;
    else if (cg_is_tuple_type(m)) mask |= LAYOUT_DYNAMIC_TUPLE;
    else if (cg_is_list_type(m)) mask |= LAYOUT_LIST;
    else return -1;
  }
  return mask;
}

// ifa/102: the layout plan (see codegen_common.h). Each candidate's
// receiver formal decides its case: a None method, or every member a
// set of the three header kinds (fixed-arity tuple, dynamic tuple, list).
// Every case is tested positively, so a value whose header carries no
// expected mark falls to the "no branch matched" assert instead of a
// wrong arm. Refuses
// when the receiver can hold anything else, when the candidates disagree
// on which operand is the receiver, or when two cases share a test.
static bool poly_dispatch_layout_plan(PNode *pn, Vec<Fun *> *fns, Vec<ArityDispatchCase *> &cases) {
  const bool dbg = getenv("PYC_DBG_ARITYDISPATCH") != nullptr;
  auto refuse = [&](cchar *why) {
    if (dbg) fprintf(stderr, "ARITYDISPATCH layout refuse pn=%d: %s\n", pn->id, why);
    return false;
  };
  int recv = -1, covered = 0;
  bool nil_case = false;
  Vec<ArityDispatchCase *> out;
  for (Fun *f : *fns) {
    if (!f) return false;
    Var *av = nullptr;
    int ridx = -1;
    for (MPosition *p : f->positional_arg_positions) {
      Var *a = f->args.get(p);
      if (!a || !a->sym || p->pos.n != 1) continue;
      int i = (int)Position2int(p->pos[0]) - 1;
      if (i < 0 || i >= pn->rvals.n || !pn->rvals[i]) return false;
      if (pn->rvals[i]->sym->is_symbol) continue;  // the selector
      av = a;
      ridx = i;
      break;
    }
    if (!av || !av->type) return false;
    if (recv < 0) {
      recv = ridx;
      if (!layout_testable_operand(pn->rvals[recv])) return refuse("receiver is not lists, tuples and None");
    } else if (ridx != recv)
      return false;
    int test;
    if (av->type == sym_nil_type) {
      if (nil_case) return false;
      nil_case = true;
      test = -3;
    } else {
      // The kinds this candidate's receiver formal covers.
      int mask = formal_layout_mask(av);
      if (mask < 0) return refuse("formal member is not a list or tuple");
      if (!mask || (mask & covered)) return refuse("two candidates cover one kind");  // two candidates for one kind: no test separates them
      covered |= mask;
      test = layout_test(mask);
    }
    ArityDispatchCase *c = new ArityDispatchCase;
    c->fun = f;
    c->tests.add({ridx, test});
    out.add(c);
  }
  // One non-None candidate needs no header test: a null test and a direct
  // call decide it (the classtag machinery's nil-tested route).
  if (out.n - (nil_case ? 1 : 0) < 2) return refuse("fewer than two non-None candidates");
  // The null test first, so the header reads after it are null-safe.
  for (ArityDispatchCase *c : out) if (c->tests[0].arity == -3) cases.add(c);
  for (ArityDispatchCase *c : out) if (c->tests[0].arity != -3) cases.add(c);
  if (getenv("PYC_DBG_ARITYDISPATCH")) fprintf(stderr, "ARITYDISPATCH layout plan pn=%d cases=%d\n", pn->id, cases.n);
  return true;
}

// See codegen_common.h.
bool poly_dispatch_arity_plan(PNode *pn, Vec<Fun *> *fns, Vec<ArityDispatchCase *> &cases) {
  if (!pn || !fns || fns->n < 2) return false;
  bool any = false;
  for (Fun *f : *fns) {
    if (!f) return false;
    for (MPosition *p : f->positional_arg_positions) {
      Var *av = f->args.get(p);
      if (av && av->sym && av->sym->dispatch_arity != -1) any = true;
    }
  }
  if (getenv("PYC_DBG_ARITYDISPATCH")) {
    fprintf(stderr, "ARITYDISPATCH pn=%d fns=%d any=%d:", pn->id, fns->n, any);
    for (Fun *f : *fns) {
      fprintf(stderr, " [%s", f->sym->name ? f->sym->name : "?");
      for (MPosition *p : f->positional_arg_positions) {
        Var *av = f->args.get(p);
        fprintf(stderr, " %d", av && av->sym ? av->sym->dispatch_arity : -9);
      }
      fprintf(stderr, "]");
    }
    fprintf(stderr, "\n");
  }
  if (!any) return poly_dispatch_layout_plan(pn, fns, cases);
  Vec<int> nlen, ndyn;
  for (Fun *f : *fns) {
    ArityDispatchCase *c = new ArityDispatchCase;
    c->fun = f;
    int len_tests = 0, dyn_tests = 0;
    for (MPosition *p : f->positional_arg_positions) {
      Var *av = f->args.get(p);
      if (!av || !av->sym || p->pos.n != 1) continue;
      int ridx = (int)Position2int(p->pos[0]) - 1;
      if (ridx < 0 || ridx >= pn->rvals.n || !pn->rvals[ridx]) return false;
      if (pn->rvals[ridx]->sym->is_symbol) continue;  // the selector
      Sym *fs = av->sym;
      int a = fs->dispatch_arity;
      bool typed = fs->must_specialize && !fs->must_specialize->is_symbol;
      bool nil_formal = av->type == sym_nil_type;  // a None method (poly_dispatch_is_nil_receiver)
      if (a == -1 && !typed && !nil_formal) continue;
      // ifa/102: an operand that may also hold a LIST. A length test still
      // separates a fixed-arity tuple (it requires the fixed bit, which a
      // list never has), but "non-null", "no fixed arity" and an untested
      // case would each take a list too, so those become layout tests on
      // what this candidate's formal covers.
      bool lists = false;
      if (!arity_testable_operand(pn->rvals[ridx]) && layout_testable_operand(pn->rvals[ridx])) lists = true;
      else if (!arity_testable_operand(pn->rvals[ridx])) {
        if (getenv("PYC_DBG_ARITYDISPATCH")) {
          Sym *t = pn->rvals[ridx]->type;
          fprintf(stderr, "ARITYDISPATCH refuse: operand %d not tuple|None:", ridx);
          if (t && t->type_kind == Type_SUM) for (Sym *m : t->has) fprintf(stderr, " %s(kind=%d,cr=%d)", m && m->name ? m->name : "?", m ? m->type_kind : -1, m ? m->creators.n : -1);
          else fprintf(stderr, " %s(kind=%d,cr=%d)", t && t->name ? t->name : "?", t ? t->type_kind : -1, t ? t->creators.n : -1);
          fprintf(stderr, "\n");
        }
        return false;
      }
      if (nil_formal) {
        c->tests.add({ridx, -3});
        len_tests++;  // as discriminating as a length
      } else if (a >= 0) {
        c->tests.add({ridx, a});
        len_tests++;
      } else if (lists) {
        int m = formal_layout_mask(av);
        if (m <= 0) return false;
        c->tests.add({ridx, layout_test(m)});
        dyn_tests++;
      } else if (a == DISPATCH_ARITY_DYNAMIC) {
        c->tests.add({ridx, -2});
        dyn_tests++;
      } else if (operand_may_be_nil(pn->rvals[ridx]))
        c->tests.add({ridx, -1});  // a non-null test is vacuous on an operand that cannot be None
    }
    // Most constrained first: by length tests, then by "no fixed arity"
    // tests, then by test count.
    int at = cases.n;
    while (at > 0 && (nlen[at - 1] < len_tests ||
                      (nlen[at - 1] == len_tests && (ndyn[at - 1] < dyn_tests ||
                                                     (ndyn[at - 1] == dyn_tests &&
                                                      cases[at - 1]->tests.n < c->tests.n)))))
      at--;
    cases.insert(at, c);
    nlen.insert(at, len_tests);
    ndyn.insert(at, dyn_tests);
  }
  // A case with no tests takes every value that reaches it, so one before
  // the last would hide the rest: refuse rather than guess. (Taking the
  // general case for every tuple was tried before the fixed-arity bit and
  // is unsound -- its body re-dispatches on values FA never typed it for.)
  for (int i = 0; i + 1 < cases.n; i++)
    if (!cases[i]->tests.n) {
      if (getenv("PYC_DBG_ARITYDISPATCH")) fprintf(stderr, "ARITYDISPATCH refuse: untested case %d is not last\n", i);
      return false;
    }
  // Two cases whose tests are identical cannot be told apart at run time.
  for (int i = 0; i < cases.n; i++)
    for (int j = i + 1; j < cases.n; j++) {
      if (cases[i]->tests.n != cases[j]->tests.n) continue;
      bool same = true;
      for (int k = 0; k < cases[i]->tests.n && same; k++) {
        bool found = false;
        for (int l = 0; l < cases[j]->tests.n; l++)
          if (cases[i]->tests[k].ridx == cases[j]->tests[l].ridx && cases[i]->tests[k].arity == cases[j]->tests[l].arity)
            found = true;
        same = found;
      }
      if (same) {
        if (getenv("PYC_DBG_ARITYDISPATCH")) fprintf(stderr, "ARITYDISPATCH refuse: cases %d and %d indistinguishable\n", i, j);
        return false;
      }
    }
  return true;
}

// issues/048, issues/171: a send whose receiver is a {None, scalar} union.
// None is a null pointer and a scalar reaches the same slot bit-for-bit,
// so a null test cannot tell None from 0/0.0/False: the dispatch has no
// representation. Returns the scalar member (and the selector name via
// *sel_out) when a nil-receiver candidate of `pn` meets such a receiver,
// nullptr otherwise. Truthiness selectors are exempt: there None and zero
// give the same answer, so the conflation is invisible (genetic2's
// `if f():` on an implicit-None return).
//
// Shared by both backends so they refuse with the same diagnostic; the C
// backend used to fall through to a generic "unable to resolve" here.
Sym *poly_dispatch_nil_scalar_receiver(PNode *pn, Vec<Fun *> *cands, Var *recv_var, cchar **sel_out) {
  if (!pn) return nullptr;
  if (!recv_var && cands) {
    for (Fun *c : *cands) {
      int idx = -1;
      if (c && poly_dispatch_is_nil_receiver(c, pn, &idx) && idx >= 0 && idx < pn->rvals.n) {
        recv_var = pn->rvals[idx];
        break;
      }
    }
  }
  if (!recv_var || !recv_var->type || recv_var->type->type_kind != Type_SUM) return nullptr;
  bool has_nil = false;
  Sym *scalar_member = nullptr;
  for (Sym *m : recv_var->type->has) {
    if (!m) continue;
    if (m == sym_nil_type) has_nil = true;
    else if (m->num_kind) scalar_member = m;
  }
  if (!has_nil || !scalar_member) return nullptr;
  cchar *sel = (pn->rvals.n && pn->rvals[0]->sym->is_symbol) ? pn->rvals[0]->sym->name : nullptr;
  if (sel && (!strcmp(sel, "__pyc_to_bool__") || !strcmp(sel, "__bool__") || !strcmp(sel, "__not__")))
    return nullptr;
  if (sel_out) *sel_out = sel;
  return scalar_member;
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
  // ifa/124: clone.cc gives a container group with an element its own copy
  // of the builtin's type sym (`list` -> a new `list`), and it does so before
  // codegen names the builtins (`_CG_list`), so the copy has no C name and
  // fell to `_CG_any` below: `xs = [1, 2]; xs.append(3)` declared xs
  // `_CG_any`, and --refuse-imprecise reported it as untyped. A copy keeps
  // its builtin's meta type, so a primitive type with no name of its own
  // takes the name of the named primitive of the same meta type -- the same
  // class, hence the same representation.
  Map<Sym *, cchar *> primitive_name;
  for (Sym *s : allsyms)
    if (s->type_kind == Type_PRIMITIVE && !s->num_kind && s->meta_type && cg_get_string(s))
      primitive_name.put(s->meta_type, cg_get_string(s));
  for (Sym *s : allsyms) {
    if (s->num_kind) {
      cg_set_string(s, num_string(s));
    } else if (s->is_symbol) {
      cg_set_string(s, "_CG_symbol");
    } else if (!cg_get_string(s)) {
      switch (s->type_kind) {
        default:
          if (cchar *n = (s->type_kind == Type_PRIMITIVE && s->meta_type) ? primitive_name.get(s->meta_type) : nullptr)
            cg_set_string(s, n);
          else
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

void codegen_unresolved_send(PNode *pn, Vec<Fun *> *candidates, cchar *why) {
  static int stub = -1;
  if (stub < 0) {
    cchar *e = getenv("PYC_DISPATCH_STUB");
    stub = e && atoi(e);
  }
  if (stub) return;
  // Names are for the human reading the diagnostic only.
  cchar *sel = "?";
  if (pn && pn->rvals.n && pn->rvals[0] && pn->rvals[0]->sym && pn->rvals[0]->sym->is_symbol && pn->rvals[0]->sym->name)
    sel = pn->rvals[0]->sym->name;
  std::string cands;
  if (candidates)
    for (Fun *f : *candidates) {
      if (!f || !f->sym) continue;
      Sym *cls = f->sym->has.n > 1 && f->sym->has[1] ? f->sym->has[1]->must_specialize : nullptr;
      if (!cands.empty()) cands += ", ";
      if (cls && cls->name) cands += std::string(cls->name) + ".";
      cands += f->sym->name ? f->sym->name : "?";
    }
  if (cands.empty()) cands = "none";
  codegen_fail(pn, "cannot resolve the call to '%s' (candidates: %s): %s. pyc refuses it rather than emit a run-time "
               "stub (ifa/102)", sel, cands.c_str(), why);
}

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
  // ifa/issues/184: before the constant-fold early-out -- a read whose
  // value is a known constant still raises when its receiver is None.
  cchar *nil_sel = nullptr;
  if (Var *recv = nil_period_receiver(pn, &nil_sel)) emitter->emit_none_check(recv, nil_sel);
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
    // ifa/issues/181: no emitter claimed this prim. It must not silently
    // produce no code -- a live result then reads as zero, which is how
    // `**` printed 0 on the LLVM backend (ifa/180). The backend may handle
    // it deliberately; otherwise this is a compile error. IFA_DBG_NOEMIT
    // lists every prim that reaches here.
    if (getenv("IFA_DBG_NOEMIT"))
      fprintf(stderr, "NOEMIT prim=%s lvals=%d live=%d at %s:%d\n", pn->prim->name, pn->lvals.n,
              pn->lvals.n && pn->lvals.v[0] ? (int)pn->lvals.v[0]->live : -1,
              pn->code ? pn->code->pathname() : "?",
              pn->code ? pn->code->line() : -1);
    if (emitter->emit_send_unhandled(pn)) return;
    fail("codegen: no code generated for primitive '%s' at %s:%d (ifa/issues/181)", pn->prim->name,
         pn->code ? pn->code->pathname() : "?", pn->code ? pn->code->line() : -1);
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
