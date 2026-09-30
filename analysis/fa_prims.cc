// SPDX-License-Identifier: BSD-3-Clause
//
// ifa/167 step 5: what each PRIMITIVE does to types.
//
// The transfer function for every `P_prim_*`: what `merge_in` does to two
// CreationSets, what `len` folds to, what `index_object` reads out of a
// container's element channel, what `coerce` and `isinstance` decide. It
// was the back half of `add_send_edges_pnode`, sharing a function with
// that routine's OTHER job -- building the call edges for a non-primitive
// send. Those two are unrelated, and only one of them is still changing.
//
// What stayed in fa.cc is the call-graph half: `all_applications`,
// the closure construction, and the argument/return constraint loop that
// runs before this switch. Primitive semantics are a table; call-graph
// construction is the analysis.
//
// CONTRACT (survey S2), and it is why `result` is a parameter rather than
// recomputed here: every snapshot-style transfer below -- isinstance, len,
// merge, index_object, destruct, period, anything iterating an operand's
// ->out->sorted at execution time -- relies on the caller's blanket
// `arg_of_send` registration to be re-run when operand types arrive later.
// That registration hangs off the result AVar, so a prim send without
// lvals has no resume path, and such a prim must not read operand->out.

#include "ifadefs.h"

#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "fa.h"
#include "fa_census.h"
#include "fa_flags.h"
#include "fa_internal.h"
#include "ast.h"
#include "builtin.h"
#include "clone.h"
#include "fail.h"
#include "fun.h"
#include "if1.h"
#include "log.h"
#include "pattern.h"
#include "pdb.h"
#include "pnode.h"
#include "prim.h"
#include "var.h"

// `o`: the index of the first real operand -- 2 when the send carries the
// `primitive` marker symbol, 1 otherwise. Computed by the caller, which
// already had to know it.
void add_prim_send_constraints(PNode *p, EntrySet *es, AVar *result, int o) {
switch (p->prim->index) {
      default:
        break;
      case P_prim_await: {
        AVar *a = make_AVar(p->rvals[o], es);
        flow_vars(a, result);
        break;
      }
      case P_prim_yield: {
        // issues/014: unlike P_prim_await just above (whose result
        // flows from a real, call-graph-visible callee return value),
        // a yield expression's result (`x` in `x = yield foo`) is
        // whatever a *later*, call-graph-invisible `.send(v)` call
        // delivers -- no IF1 edge connects __pyc_generator__.send()'s
        // `value` formal to this primitive; the transfer happens only
        // through the C++ promise's `sent` field at runtime (see
        // pyc_c_runtime.h's yield_awaiter). Flowing the yielded
        // value's own type into the result here (the original,
        // `.send()`-less design, matching P_prim_await's shape) is
        // unsound whenever a generator's yielded expression depends
        // on its own previously-received values (`total += x; yield
        // total`): with FA seeing no other source, the fixed point
        // legitimately-but-uselessly collapses the whole loop to a
        // compile-time constant seeded by the first yield, which
        // then gets constant-folded away entirely -- silently
        // breaking .send() (observed: co_yield of a hardcoded literal
        // instead of the real running value). Anchor to the generic
        // int64 type instead, the same "opaque, non-constant" trick
        // as the coroutine-handle placeholder
        // (_CG_generator_placeholder_return, see gen_fun_pyda) --
        // correct for today's only supported payload shape (v1
        // scope: int64 smuggled through void*, same as the yielded-
        // out value itself). `a` (the yielded value) is already
        // registered as reachable/used by the generic per-rval
        // make_AVar/arg_of_send loop above this switch -- no need to
        // touch it again here, unlike P_prim_await's case, since it
        // does not feed the result's type.
        update_gen(result, sym_int64->abstract_type);
        break;
      }
      case P_prim_id: {
        // id(x): the operand's address (or value bits for unboxed
        // scalars) as a plain int64 -- the result's type never
        // depends on the operand's.
        update_gen(result, sym_int64->abstract_type);
        break;
      }
      case P_prim_primitive: {
        cchar *name = p->rvals[1]->sym->name;
        RegisteredPrim *rp = prim_get(name);
        if (!rp) fail("undefined primitive transfer function '%s'", name);
        rp->tfn(p, es);
        break;
      }
      case P_prim_meta_apply: {
        cchar *file = p->code && p->code->filename() ? p->code->filename() : "<unknown>";
        int line = p->code ? p->code->line() : 0;
        fail("P_prim_meta_apply transfer function not implemented at %s:%d; "
             "no live frontend emits this prim — see ifa/notes/003-cast-and-meta-apply-prims.md",
             file, line);
        break;
      }
      case P_prim_destruct: {
        assert(p->rvals.n - o == 2);
        int tvars = 0;
        destruct(p->lvals.v, p->lvals.n, make_AVar(p->rvals.v[o], es), p->rvals[o + 1]->sym, result, tvars);
        break;
      }
      case P_prim_vector:
        prim_make_vector_constraints(p, es);
        break;
      case P_prim_index_object: {
        AVar *vec = make_AVar(p->rvals[o], es);
        AVar *index = make_AVar(p->rvals[o + 1], es);
        set_container(result, vec);
        for (CreationSet *cs : vec->out->sorted) {
          if (sym_string->specializers.set_in(cs->sym))
            update_gen(result, sym_char->abstract_type);
          else if (sym_bytes->specializers.set_in(cs->sym))
            // bytes shares str's exact char* buffer layout (see
            // sym_bytes registration) but indexes/iterates to plain int,
            // matching CPython's `bytes[i]` -- unlike sym_char, which is
            // aliased to sym_string (python_ifa_sym.cc), sym_int is
            // aliased to sym_int64, a genuine scalar.
            update_gen(result, sym_int->abstract_type);
          else {
            int i;
            bool is_const = get_obj_index(index, &i, cs->vars.n);
            if (cs->sym->element) flow_vars(get_element_avar(cs), result);
            if (!cs->sym->is_vector) {
              if (is_const)
                flow_vars(cs->vars[i], result);
              else
                for (AVar *av : cs->vars) flow_vars(av, result);
            }
          }
        }
        break;
      }
      case P_prim_set_index_object: {
        AVar *vec = make_AVar(p->rvals[o], es);
        AVar *index = make_AVar(p->rvals[o + 1], es);
        AVar *val = make_AVar(p->rvals[o + 2], es);
        fill_tvals(es->fun, p, 1);
        AVar *tval = make_AVar(p->tvals[0], es);
        flow_vars(val, tval);
        set_container(tval, vec);
        for (CreationSet *cs : vec->out->sorted) {
          if (sym_string->specializers.set_in(cs->sym)) {
            AType *d = type_diff(sym_char->abstract_type, val->out);
            if (d != fa->type_world.bottom_type) type_violation(ATypeViolation_kind::MATCH, val, d, result);
          } else {
            int i;
            bool is_const = get_obj_index(index, &i, cs->vars.n);
            if (cs->sym->is_vector) {
              if (cs->sym->element) flow_vars(tval, get_element_avar(cs));
            } else if (is_const)
              flow_vars(tval, cs->vars[i]);
            else {
              if (cs->sym->element) flow_vars(tval, get_element_avar(cs));
              for (int i = 0; i < cs->vars.n; i++) flow_vars(tval, cs->vars[i]);
            }
          }
        }
        flow_vars(val, result);
        break;
      }
      case P_prim_apply: {
        assert(p->lvals.n == 1);
        Vec<AVar *> args;
        Vec<cchar *> names;
        names.add(0);
        names.add(0);
        AVar *fun = make_AVar(p->rvals[1], es);
        AVar *a1 = make_AVar(p->rvals[3], es);
        args.add(a1);
        if (all_applications(p, es, fun, args, names, 0, (Partial_kind)p->code->partial) > 0) make_closure(result);
        break;
      }
      case P_prim_period: {
        AVar *obj = make_AVar(p->rvals[1], es);
        AVar *selector = make_AVar(p->rvals[3], es);
        Vec<AVar *> methods;
        set_container(result, obj);
        bool partial = p->code->partial != Partial_NEVER;
        for (CreationSet *sel : selector->out->sorted) {
          cchar *symbol = sel->sym->name;
          if (!symbol) symbol = sel->sym->constant;
          if (!symbol) symbol = sel->sym->imm.v_string;
          assert(symbol);
          for (CreationSet *cs : obj->out->sorted) {
            AVar *iv = cs->var_map.get(symbol);
            if (iv) {
              iv->arg_of_send.add(result);
              if (partial) {
                // Function-valued fields split two ways, following
                // Python's actual rule -- a function found on the
                // CLASS binds as a method, a function stored as an
                // INSTANCE attribute does not (issue 025
                // first-class-function-in-field):
                //  - METHOD-like values keep the historical behavior
                //    (filtered out of the direct flow, re-routed
                //    through a method-binding partial application):
                //    real methods and capturing-def carriers
                //    (Fun->sym->self set), and class-body lambdas /
                //    defs (Sym::in is a class, i.e. non-fun) -- pyc
                //    stores class attributes as prototype fields, so
                //    definition scope is the FA-visible equivalent of
                //    "found on the class".
                //  - BARE function values (a module- or
                //    function-level def stored in an instance
                //    attribute: fun set, no self, in absent or a
                //    function) flow through UNBOUND --
                //    `self.cf(3, 1)` calls cf(3, 1), not
                //    cf(self, 3, 1). Previously they were bound too,
                //    so any call through such a field dispatched with
                //    the object inserted as the first argument and
                //    matched nothing (timsort's self.comparefn).
                // Mixed fields (both kinds) conservatively flow both
                // forms; permits and bindings only ever grow, so the
                // fixpoint stays monotone.
                AType *fnpart = type_intersection(iv->out, fa->type_world.function_type);
                bool all_bare = fnpart != fa->type_world.bottom_type;
                for (CreationSet *fcs : fnpart->sorted) {
                  Fun *ff = fcs->sym->fun;
                  if (!ff) { all_bare = false; break; }
                  // issue 027 feature: @staticmethod lives in a class
                  // scope like a method but takes NO receiver -- reads
                  // through an instance must flow the raw function
                  // value unbound, overriding the class-scope test.
                  if (ff->sym->is_static_method) continue;
                  if (ff->sym->self || (ff->sym->in && !ff->sym->in->is_fun)) { all_bare = false; break; }
                }
                if (all_bare) {
                  flow_var_type_permit(result, iv->out);
                  flow_vars(iv, result);
                } else {
                  flow_var_type_permit(result, type_diff(iv->out, fa->type_world.function_type));
                  flow_vars(iv, result);
                  if (fnpart != fa->type_world.bottom_type) methods.add(iv);
                }
              } else
                flow_vars(iv, result);
            }
          }
        }
        for (AVar *x : methods) {
          Vec<AVar *> args;
          Vec<cchar *> names;
          names.add(0);
          names.add(0);
          args.add(obj);
          if (all_applications(p, es, x, args, names, 0, (Partial_kind)p->code->partial) > 0)
            make_period_closure(result, x, args);
        }
        {
          Vec<AVar *> args;
          Vec<cchar *> names;
          names.add(0);
          names.add(0);
          args.add(obj);
          if (all_applications(p, es, selector, args, names, 0, (Partial_kind)p->code->partial) > 0)
            make_period_closure(result, selector, args);
        }
        break;
      }
      case P_prim_setter: {
        AVar *obj = make_AVar(p->rvals[1], es);
        AVar *selector = make_AVar(p->rvals[3], es);
        AVar *val = make_AVar(p->rvals[4], es);
        fill_tvals(es->fun, p, 1);
        AVar *tval = make_AVar(p->tvals[0], es);
        flow_vars(val, tval);
        set_container(tval, obj);
        for (CreationSet *sel : selector->out->sorted) {
          cchar *symbol = sel->sym->name;
          if (!symbol) symbol = sel->sym->constant;
          if (!symbol) symbol = sel->sym->imm.v_string;
          assert(symbol);
          // issues/128: is a union-receiver write SEPARABLE? Count members
          // that already have the field against those that do not. MIXED is a
          // demand whose partition is exactly 2 and is named by the demand
          // itself; ALL-MISS is not separable this way. Measured first.
          // IFA_DBG_FIELDSPLIT=2 prints EVERY record write with its pass and
          // receiver classes, single-class ones too, so a class that holds a
          // field only because a union write reached it can be told apart
          // from one that has its own writer (issues/128 step 3).
          static int dbg_fs = getenv("IFA_DBG_FIELDSPLIT") ? atoi(getenv("IFA_DBG_FIELDSPLIT")) : 0;
          if (dbg_fs && (obj->out->sorted.n > 1 || dbg_fs >= 2)) {
            int have = 0, miss = 0;
            for (CreationSet *c2 : obj->out->sorted) { if (c2->var_map.get(symbol)) have++; else miss++; }
            fprintf(stderr, "[fieldsplit] p=%d %s n=%d have=%d miss=%d '%s' fun=%s:", analysis_pass,
                    (have && miss) ? "MIXED" : (have ? "ALL-HAVE" : "ALL-MISS"),
                    obj->out->sorted.n, have, miss, symbol,
                    (es->fun && es->fun->sym && es->fun->sym->name) ? es->fun->sym->name : "?");
            for (CreationSet *c2 : obj->out->sorted)
              fprintf(stderr, " %s", (c2->sym && c2->sym->name) ? c2->sym->name : "?");
            fprintf(stderr, "\n");
          }
          // issues/128: MIXED (some members have the field, some do not) was
          // TRIED as "do not record, it is a demand not evidence". It fixes
          // `chull` completely -- each class ends with exactly its own five
          // fields, matching shedskin -- and STILL BREAKS `richards` with
          // `no matching function for call`. So a MIXED write can be the
          // legitimate first write of a field onto a class that really has
          // it, and dropping it is never sound.
          //
          // That is the measurement that makes the SPLIT mandatory rather
          // than an optimisation: the receiver has to be separated so the
          // write lands on the right class. Reverted; the classification
          // survives only as the IFA_DBG_FIELDSPLIT diagnostic above.
          if (obj->out->sorted.n > 1 && obj->contour_is_entry_set) {
            int fh = 0, fm = 0;
            for (CreationSet *c2 : obj->out->sorted) { if (c2->var_map.get(symbol)) fh++; else fm++; }
            if (fh && fm) fieldsplit_demands.set_add(obj);
          }
          for (CreationSet *cs : obj->out->sorted) {
            AVar *iv = cs->var_map.get(symbol);
            if (iv)
              flow_vars(tval, iv);
            else {
              // ifa/135: WHICH write promotes a field onto WHICH classes.
              // `obj->out` holding more than one class here is the whole
              // cross-class-promotion problem: every class in the union
              // acquires every other's fields, at whatever slot each has
              // reached, and a later union read blind-casts across the
              // mismatched layouts. This names the write, so the union can
              // be traced to its source instead of guessed at.
              if (getenv("IFA_DBG_PROMOTE") && obj->out->sorted.n > 1) {
                fprintf(stderr, "[promote] p=%d fun=%s write '%s' onto %d classes:", analysis_pass,
                        (es->fun && es->fun->sym && es->fun->sym->name) ? es->fun->sym->name : "?", symbol,
                        obj->out->sorted.n);
                for (CreationSet *c2 : obj->out->sorted)
                  if (c2 && c2->sym) fprintf(stderr, " %s#%d", c2->sym->name ? c2->sym->name : "?", c2->id);
                fprintf(stderr, "\n");
              }
              if (if1->callback->discovers_fields_by_write())
                cs->unknown_vars.add(symbol);
              else
                type_violation(ATypeViolation_kind::MEMBER, selector, make_AType(cs), result);
            }
          }
        }
        flow_vars(val, result);
        break;
      }
      case P_prim_assign: {
        AVar *lhs = make_AVar(p->rvals[1], es);
        AVar *rhs = make_AVar(p->rvals[3], es);
        for (CreationSet *cs : lhs->out->sorted) {
          if (cs->sym == sym_ref) {
            assert(cs->vars.n);
            AVar *av = cs->vars[0];
            flow_vars(rhs, av);
            flow_vars(rhs, result);
          } else {
            if (sym_anynum->specializers.set_in(cs->sym->type))
              update_in(result, cs->sym->type->abstract_type);
            else
              type_violation(ATypeViolation_kind::MATCH, lhs, make_AType(cs), result);
          }
        }
        break;
      }
      case P_prim_deref: {
        AVar *ref = make_AVar(p->rvals[2], es);
        set_container(result, ref);
        for (CreationSet *cs : ref->out->sorted) {
          AVar *av = cs->vars[0];
          flow_vars(av, result);
        }
        break;
      }
      case P_prim_new: {
        AVar *thing = make_AVar(p->rvals[p->rvals.n - 1], es);
        for (CreationSet *cs : thing->out->sorted) creation_point(result, cs->sym->meta_type);  // recover original type
        break;
      }
      // NB P_prim_copy result CSs must stay FRESH (creation_point),
      // not shared with the source: an experiment sharing them
      // (update_gen(result, thing->out)) created a within-pass
      // divergence for self-referential deepcopy -- each copy
      // contour's result list unioned back into the SOURCE CS's
      // field, which re-widened the copier's own input and spawned
      // another contour, unboundedly (genetic2's TreeNode). The
      // same-class layout agreement the sharing was after is
      // guaranteed by determine_layouts' canonical field ordering
      // instead (clone.cc).
      case P_prim_copy:
      case P_prim_clone_vector:
      case P_prim_clone: {
        AVar *thing = make_AVar(p->rvals[o], es);
        // issue 078 (Option D): the literal Sym referenced at the
        // clone-source operand -- for the __new__-synthesized
        // clone(proto, t), this is always cls->self, the class's own
        // prototype; for any other clone() call it's whatever Sym the
        // source expression resolves to (never a class prototype, per
        // structural_assignment's comment above). Passed through so
        // the per-field copy can consult its clone_elides_fields.
        Sym *clone_source_sym = p->rvals[o]->sym;
        for (CreationSet *cs : thing->out->sorted) {
          CreationSet *new_cs = creation_point(result, cs->sym);
          structural_assignment(new_cs, cs, p, es, false, false, clone_source_sym);
        }
        break;
      }
      case P_prim_is: {
        // Real identity comparison.  Lattice: if the two
        // operand AVars' CS-sets are disjoint, the result
        // is statically False.  Otherwise it's polymorphic
        // bool — we can't prove True or False at compile
        // time (two AVars sharing a CS might or might not
        // hold the same instance at runtime).
        //
        // A CONSTANT is not a contour of its own: the constant `True` and
        // the runtime `bool` are different CreationSets, but a runtime bool
        // can be True. So a constant overlaps the non-constant contour of
        // its own type, and only two DIFFERENT constants are disjoint.
        // Testing CreationSet identity alone folded `r is True` to False
        // whenever r's constants had been stripped (a function returning
        // True on one path and False on another) -- silently, and
        // shedskin_examples/sat's `assert r is True` failed on a True.
        AVar *thing1 = make_AVar(p->rvals[p->rvals.n - 2], es);
        AVar *thing2 = make_AVar(p->rvals[p->rvals.n - 1], es);
        auto may_be_same = [](CreationSet *a, CreationSet *b) {
          if (a == b) return true;
          if (!a->sym || !b->sym) return false;
          bool ka = a->sym->constant != nullptr, kb = b->sym->constant != nullptr;
          if (ka == kb) return false;  // two distinct constants, or two contours
          Sym *ta = a->sym->type ? a->sym->type : a->sym;
          Sym *tb = b->sym->type ? b->sym->type : b->sym;
          return ta == tb;
        };
        bool overlap = false;
        for (CreationSet *cs1 : thing1->out->sorted) {
          for (CreationSet *cs2 : thing2->out->sorted) {
            if (may_be_same(cs1, cs2)) { overlap = true; break; }
          }
          if (overlap) break;
        }
        AType *rtype = overlap ? fa->type_world.bool_type : fa->type_world.false_type;
        update_gen(result, rtype);
        break;
      }
      case P_prim_isinstance: {
        AVar *thing1 = make_AVar(p->rvals[p->rvals.n - 2], es);  // instance
        AVar *thing2 = make_AVar(p->rvals[p->rvals.n - 1], es);  // type
        // Give the frontend first refusal: it may recognize this
        // specific check as foldable via language/runtime-specific
        // knowledge FA structurally can't derive on its own (see
        // IFACallbacks::provably_constant_isinstance, ifa.h, for the
        // full rationale and the conservatism contract). Default
        // (nullptr) falls straight through to the normal
        // CreationSet-intersection logic below, unchanged.
        if (AType *forced = if1->callback->provably_constant_isinstance(thing1, es, p)) {
          update_gen(result, forced);
          break;
        }
        AType *rtype = fa->type_world.bottom_type;
        for (CreationSet *cs1 : thing1->out->sorted) {
          for (CreationSet *cs2 : thing2->out->sorted) {
            if (cs2->sym->meta_type && cs2->sym->meta_type->implementors.in(cs1->sym->type))
              rtype = type_union(rtype, fa->type_world.true_type);
            else
              rtype = type_union(rtype, fa->type_world.false_type);
          }
        }
        update_gen(result, rtype);
        break;
      }
      case P_prim_issubclass: {
        AVar *thing1 = make_AVar(p->rvals[p->rvals.n - 2], es);
        AVar *thing2 = make_AVar(p->rvals[p->rvals.n - 1], es);
        AType *rtype = fa->type_world.bottom_type;
        for (CreationSet *cs1 : thing1->out->sorted) {
          for (CreationSet *cs2 : thing2->out->sorted) {
            if (cs2->sym->type->implementors.in(cs1->sym->type))
              rtype = type_union(rtype, fa->type_world.true_type);
            else
              rtype = type_union(rtype, fa->type_world.false_type);
          }
        }
        update_gen(result, rtype);
        break;
      }
      case P_prim_merge: {
        AVar *thing1 = make_AVar(p->rvals[p->rvals.n - 2], es);
        AVar *thing2 = make_AVar(p->rvals[p->rvals.n - 1], es);
        for (CreationSet *cs : thing1->out->sorted) {
          CreationSet *new_cs = creation_point(result, cs->sym);
          structural_assignment(new_cs, cs, p, es, true);
          for (CreationSet *cs2 : thing2->out->sorted) {
            if (cs->sym == cs2->sym) structural_assignment(new_cs, cs2, p, es, true);
          }
        }
        break;
      }
      case P_prim_merge_in: {
        AVar *thing1 = make_AVar(p->rvals[p->rvals.n - 2], es);
        AVar *thing2 = make_AVar(p->rvals[p->rvals.n - 1], es);
        for (CreationSet *cs : thing1->out->sorted) {
          for (CreationSet *cs2 : thing2->out->sorted) {
            if (cs->sym == cs2->sym) structural_assignment(cs, cs2, p, es, true, true);
          }
        }
        flow_vars(thing1, result);
        break;
      }
      case P_prim_coerce: {
        Sym *s = unalias_type(p->rvals[p->rvals.n - 2]->sym);
        assert(s->abstract_type);
        AVar *rhs = make_AVar(p->rvals[p->rvals.n - 1], es);
        Vec<CreationSet *> css;
        // Compare against the type operand at its positional slot
        // (n-2), not rvals[1] -- in the @primitive-prefixed form
        // rvals[1] is the prim-name symbol and the filter could
        // never match (survey S5).
        for (CreationSet *cs : rhs->out->sorted) if (cs->sym->type == p->rvals[p->rvals.n - 2]->sym) css.set_add(cs);
        if (css.n)
          update_gen(result, make_AType(css));
        else if (s->type->num_kind || s->type == sym_string || s->type->is_symbol)
          update_gen(result, s->abstract_type);
        break;
      }
      case P_prim_len: {
        AVar *t = make_AVar(p->rvals[2], es);
        AType *rtype = fa->type_world.bottom_type;
        for (CreationSet *cs : t->out->sorted) {
          AVar *elem = get_element_avar(cs);
          if (elem) elem->arg_of_send.add(result);
          // issues/114: a CreationSet with NO DEFS was not built by any
          // creation site in this program -- it is abstract, or
          // synthesised for the result of an opaque `__pyc_c_call__`
          // (a generator's value channel is exactly that). Its
          // `vars.n` is 0 because nothing ever filled it, NOT because
          // the container is empty, so folding len() to 0 is simply
          // wrong. Measured: a tuple arriving through a generator had
          // correct contents -- `len(x)` and `x[0]` were right at the
          // use site -- but inside tuple::__eq__, whose formal carries
          // the synthesised CS, `len(self)` folded to 0, so the very
          // first `n != len(t)` check returned False and `x == (1, 2)`
          // was silently False for a tuple that WAS (1, 2).
          if (cs->no_static_arity || !cs->defs.n || (elem && elem->out != fa->type_world.bottom_type) ||
              sym_string->specializers.set_in(cs->sym) || sym_bytes->specializers.set_in(cs->sym))
            rtype = type_union(rtype, fa->type_world.size_type);
          else
            rtype = type_union(rtype, make_size_constant_type(cs->vars.n));
        }
        update_gen(result, rtype);
        break;
      }
      case P_prim_sizeof: {
        AVar *t = make_AVar(p->rvals[2], es);
        AType *rtype = fa->type_world.bottom_type;
        for (CreationSet *cs : t->out->sorted) {
          if (cs->sym->size)
            rtype = type_union(rtype, make_size_constant_type(cs->sym->size));
          else
            rtype = type_union(rtype, fa->type_world.size_type);
        }
        update_gen(result, rtype);
        break;
      }
      case P_prim_sizeof_element: {
        AVar *t = make_AVar(p->rvals[2], es);
        AType *rtype = fa->type_world.bottom_type;
        // ifa/issues/109: sizeof_element needs ONE container layout. If
        // the receiver spans CreationSets that will become distinct
        // concrete types -- different sym, or different arity, which for
        // a record-shaped tuple means a different type -- their sum has
        // no `element` and codegen fails with "sizeof_element of
        // non-container type". FA used to just union the sizes and say
        // nothing, so the splitter had no reason to separate them.
        //
        // Recording a violation here is what makes the EXISTING backward
        // machinery do the work: split_for_violations (stage 5) splits
        // the offending AVar, and that split propagates back to the
        // caller's contour automatically. No annotation, no special case
        // in codegen -- FA simply has to know the constraint.
        if (sizeof_viol_enabled() && t->out->sorted.n > 1) {
          Sym *sym0 = nullptr;
          int arity0 = -1;
          bool uniform = true;
          for (CreationSet *cs : t->out->sorted) {
            if (!cs->sym) continue;
            if (!sym0) { sym0 = cs->sym; arity0 = cs->vars.n; }
            else if (sym0 != cs->sym || arity0 != cs->vars.n) { uniform = false; break; }
          }
          if (!uniform) type_violation(ATypeViolation_kind::BOXING, t, t->out, nullptr, nullptr);
        }
        for (CreationSet *cs : t->out->sorted) {
          AVar *elem = get_element_avar(cs);
          if (elem) {
            for (CreationSet *cs2 : elem->out->sorted) {
              if (cs2->sym->size)
                rtype = type_union(rtype, make_size_constant_type(cs2->sym->size));
              else
                rtype = type_union(rtype, fa->type_world.size_type);
            }
          }
        }
        update_gen(result, rtype);
        break;
      }
      case P_prim_typeof: {
        AVar *t = make_AVar(p->rvals[2], es);
        AType *rtype = fa->type_world.bottom_type;
        for (CreationSet *cs : t->out->sorted) rtype = type_union(rtype, make_abstract_type(cs->sym->meta_type));
        update_gen(result, rtype);
        break;
      }
      case P_prim_typeof_element: {
        AVar *t = make_AVar(p->rvals[2], es);
        AType *rtype = fa->type_world.bottom_type;
        for (CreationSet *cs : t->out->sorted) {
          AVar *elem = get_element_avar(cs);
          if (elem)
            for (CreationSet *cs2 : elem->out->sorted) rtype = type_union(rtype, make_abstract_type(cs2->sym->meta_type));
        }
        update_gen(result, rtype);
        break;
      }
      case P_prim_cast: {
        cchar *file = p->code && p->code->filename() ? p->code->filename() : "<unknown>";
        int line = p->code ? p->code->line() : 0;
        fail("P_prim_cast transfer function not implemented at %s:%d; "
             "no live frontend emits this prim — see ifa/notes/003-cast-and-meta-apply-prims.md",
             file, line);
        break;
      }
    }
}
