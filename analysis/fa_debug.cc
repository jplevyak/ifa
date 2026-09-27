// SPDX-License-Identifier: BSD-3-Clause
//
// ifa/167 step 2: the FA diagnostics -- everything that only FORMATS what
// the analysis decided, moved out of fa.cc so the algorithm can be read
// without them.
//
// The split was measured, not eyeballed: a function moves when it does not
// call into the analysis. `dbg_es_per_fun` is the one that stayed, and it
// is worth saying why -- it calls `clear_splits`, `build_joint_type_marks`,
// `collect_type_confluences` and `clear_marks`, i.e. it RE-RUNS stage
// machinery to report on it. That is an instrumented analysis pass wearing
// a diagnostic's name, and moving it would have dragged the splitter here.
//
// Everything in this file may read analysis state. Nothing in it may
// change what the analysis DOES; the counters it prints live in
// fa_census.h for the same reason.
#include "ifadefs.h"

#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "fa.h"
#include "fa_flags.h"
#include "fa_census.h"
#include "fa_internal.h"
#include "ast.h"
#include "builtin.h"
#include "clone.h"
#include "dead.h"
#include "fail.h"
#include "fun.h"
#include "graph.h"
#include "if1.h"
#include "inline.h"
#include "log.h"
#include "pattern.h"
#include "pdb.h"
#include "pnode.h"
#include "prim.h"
#include "timer.h"
#include "var.h"


// ifa/issues/112: hash FA's whole computed state -- every AVar's `out`
// CreationSet set, in a canonical walk -- at a named point. Called per
// pass and at the FA/clone boundaries, so two runs' traces can be
// diffed and the FIRST differing label names the interval that
// introduced the divergence.
void dbg_trace_fa_state(cchar *where) {
  static int on = -1;
  if (on < 0) on = getenv("IFA_DBG_FATRACE") ? 1 : 0;
  if (!on) return;
  Vec<AVar *> avs;
  for (Fun *f : fa->funs) if (f)
    for (Var *v : f->fa_all_Vars) if (v)
      form_AVarMapElem(x, v->avars) if (x->value) avs.add(x->value);
  for (CreationSet *cs : fa->css) if (cs)
    for (AVar *iv : cs->vars) if (iv) avs.add(iv);
  if (avs.n > 1) qsort_by_id(avs);
  unsigned long h = 1469598103934665603UL;
#define FAMIX2(x) (h = (h ^ (unsigned long)(x)) * 1099511628211UL)
  for (AVar *av : avs) {
    FAMIX2(av->id);
    if (av->out) {
      Vec<int> ids;
      for (CreationSet *c : *av->out) if (c) ids.add(c->id);
      if (ids.n > 1) qsort(ids.v, ids.n, sizeof(ids[0]), [](const void *a, const void *b) {
        int x = *(const int *)a, y = *(const int *)b; return (x > y) - (x < y);
      });
      for (int i : ids) FAMIX2(i);
    }
    FAMIX2(0x9e3779b9UL);
  }
#undef FAMIX2
  fprintf(stderr, "FASTATE %s navars=%d h=%lx\n", where, avs.n, h);
}

AEdge::AEdge() : from(nullptr), to(nullptr), pnode(nullptr), fun(nullptr), match(nullptr), in_edge_worklist(0) {
  id = fa->aedge_id++;
  fa->all_aedges.add(this);  // ifa/issues/098: authoritative list for clear_results
}

uint PendingMapHash::hash(AEdge *e) {
  return (uint)(uintptr_t)(e->fun ? e->fun->id : 0) +
         combine_hash((uintptr_t)(e->pnode ? e->pnode->id : 0), (uintptr_t)(e->from ? e->from->id : 0));
}

AVar::AVar(Var *v, void *acontour)
    : var(v),
      contour(acontour),
      lvalue(nullptr),
      gen(nullptr),
      in(fa->type_world.bottom_type),
      out(fa->type_world.bottom_type),
      restrict(nullptr),
      restrict_pred(RP_None),
      restrict_pred_cls(nullptr),
      container(nullptr),
      setters(nullptr),
      setter_class(nullptr),
      mark_map(nullptr),
      cs_map(nullptr),
      match_cache(nullptr),
      type(nullptr),
      num_coerce(nullptr),
      ivar_offset(0),
      in_send_worklist(0),
      contour_is_entry_set(0),
      is_lvalue(0),
      live(0),
      live_arg(0),
      is_if_arg(0),
      escape(ES_Escape),  // Phase 1: conservative top
      needs_fat(0) {
  id = fa->avar_id++;
}

AType::AType(AType &a) {
  hash = 0;
  this->copy(a);
}

AType::AType(CreationSet *cs) {
  hash = 0;
  set_add(cs);
}

// ifa/issues/055: PYC_DBG_BIND=<fun name> logs every edge->EntrySet
// binding for that function -- MINT vs REUSE, the chosen ES, and the
// edge's ACTUAL argument types. set_entry_set is the one chokepoint
// both routes go through, so nothing can bind behind its back. This is
// what identifies the caller that re-admits a second receiver
// CreationSet into an already-monomorphic contour.
void dbg_bind(AEdge *e, EntrySet *new_es, bool mint) {
  static cchar *want = nullptr;
  static int checked = 0;
  if (!checked) { want = getenv("PYC_DBG_BIND"); checked = 1; }
  if (!want || !e || !e->match || !e->match->fun || !e->match->fun->sym) return;
  cchar *nm = e->match->fun->sym->name;
  if (!nm || strcmp(nm, want)) return;
  char args[512];
  args[0] = 0;
  int used = 0;
  for (MPosition *p : e->match->fun->positional_arg_positions) {
    AVar *av = e->args.get(p);
    char t[160];
    dbg_atype_str(av ? av->out : nullptr, t, (int)sizeof t, 0);
    used += snprintf(args + used, (int)sizeof args - used, "%s%s", used ? " " : "", t);
    if (used >= (int)sizeof args - 1) break;
  }
  fprintf(stderr, "BIND pass=%d %s e=%d %s es=%d from_es=%d line=%d actuals=[%s]\n", analysis_pass, want, e->id,
          mint ? "MINT " : "REUSE", new_es ? new_es->id : -1, e->from ? e->from->id : -1,
          e->pnode && e->pnode->code ? e->pnode->code->line() : -1, args);
}

void show_sym_name(Sym *s, FILE *fp) {
  if (s->name)
    fprintf(fp, "%s", s->name);
  else if (s->constant)
    fprintf(fp, "\"%s\"", s->constant);
  else if (s->is_constant) {
    fputs("\"", fp);
    fprint_imm(fp, s->imm);
    fputs("\"", fp);
  } else
    fprintf(fp, "%d", s->id);
}

void show_type(Vec<CreationSet *> &t, FILE *fp, int verbose) {
  if (verbose < 3) {
    Vec<Sym *> type;
    for (CreationSet *cs : t) if (cs) {
      Sym *s = cs->sym;
      if (!ifa_verbose) s = s->type;
      type.set_add(s);
    }
    type.set_to_vec();
    qsort_by_id(type);
    if (type.n > 1) fprintf(fp, "( ");
    for (Sym *s : type) if (s) {
      show_sym_name(s, fp);
      fprintf(fp, " ");
    }
    if (type.n > 1) fprintf(fp, ") ");
  } else {
    fprintf(fp, "( ");
    for (CreationSet *cs : t) if (cs) {
      show_sym_name(cs->sym, fp);
      fprintf(fp, " ");
      if (cs->vars.n) fprintf(fp, "[ ");
      for (AVar *av : cs->vars) {
        show_sym_name(av->var->sym, fp);
        fprintf(fp, ":");
        show_type(*av->out, fp, verbose - 1);
        fprintf(fp, " ");
      }
      if (cs->added_element_var && get_element_avar(cs)->out) {
        fprintf(fp, " *elements*:");
        show_type(*get_element_avar(cs)->out, fp, verbose - 1);
      }
      if (cs->vars.n) fprintf(fp, " ] ");
    }
    fprintf(fp, ") ");
  }
}

void show_sym(Sym *s, FILE *fp) {
  if (s->is_pattern) {
    fprintf(fp, "( ");
    for (Sym *ss : s->has) {
      if (ss != s->has[0]) fprintf(fp, ", ");
      show_sym(ss, fp);
    }
    fprintf(fp, ")");
  } else if (s->name)
    fprintf(fp, "%s", s->name);
  else if (s->constant)
    fprintf(fp, "\"%s\"", s->constant);
  else
    fprintf(fp, "_");
  if (s->type && s->type->name)
    fprintf(fp, " = %s", s->type->name);
  else if (s->must_implement && s->must_implement == s->must_specialize) {
    fprintf(fp, " : ");
    show_sym_name(s->must_implement, fp);
  } else if (s->must_implement) {
    fprintf(fp, " < ");
    show_sym_name(s->must_implement, fp);
  } else if (s->must_specialize && !s->must_specialize->is_symbol) {
    fprintf(fp, " @ ");
    show_sym_name(s->must_specialize, fp);
  }
}

void show_fun(Fun *f, FILE *fp) {
  if (f->line() > 0) fprintf(fp, "%s:%d: ", f->filename(), f->source_line());
  for (Sym *s : f->sym->has) {
    show_sym(s, fp);
    if (s != f->sym->has[f->sym->has.n - 1]) fprintf(fp, ", ");
  }
  if (ifa_verbose) fprintf(fp, " id:%d", f->sym->id);
}

void show_atype(AType &t, FILE *fp, int level) {
  fprintf(fp, "( ");
  for (CreationSet *cs : t.sorted) if (cs) {
    show_sym_name(cs->sym, fp);
    fprintf(fp, " id:%d ", cs->id);
    if (level > 0) {
      for (AVar *av : cs->vars) {
        show_sym_name(av->var->sym, fp);
        show_atype(*av->out, fp, level - 1);
      }
    }
  }
  fprintf(fp, ") ");
}

void show_name(FILE *fp, AVar *av) {
  if (av->var->sym->name) {
    if (ifa_verbose)
      fprintf(fp, "'%s':%d ", av->var->sym->name, av->var->sym->id);
    else
      fprintf(fp, "'%s' ", av->var->sym->name);
  } else if (ifa_verbose)
    fprintf(fp, "expr:%d ", av->var->sym->id);
  else
    fprintf(fp, "expression ");
}

void show_illegal_type(FILE *fp, ATypeViolation *v) {
  AVar *av = v->av;
  show_name(fp, av);
  if (ifa_verbose) {
    fprintf(fp, "id:%d ", av->var->sym->id);
    if (av->out->n) {
      fprintf(fp, ": ");
      show_type(*av->out, fp);
    }
  }
  // ifa/149: say "no type" instead of printing nothing.
  //
  // 416 of the corpus's 747 `illegal call argument type expression`
  // warnings -- 55%, across 14+ programs (tarsalzp 51, msp_ss 49, rubik 42,
  // doom 38, plcfrs 31) -- render as `illegal: ` with nothing after the
  // colon, because the offending type is BOTTOM: the analysis never typed
  // the argument at all. That is a real and specific condition, and the
  // single largest diagnostic class in the corpus was communicating it as
  // a blank.
  //
  // Printing the unprojected type first, since `AType::type` also drops
  // constants and a constants-only type projects to bottom for a different
  // reason; only when BOTH are empty is it genuinely untyped.
  fprintf(fp, "illegal: ");
  if (v->type->type && v->type->type->sorted.n)
    show_type(*v->type->type, fp);
  else if (v->type->sorted.n)
    show_type(*v->type, fp);
  else
    fprintf(fp, "(no type)");
  fprintf(fp, "\n");
}

void show_call_tree(FILE *fp, PNode *p, EntrySet *es, int depth) {
  depth++;
  if (depth > fa->print_call_depth || !p->code) return;
  if (depth > 1 && p->code->filename() && p->code->line() > 0) {
    for (int x = 0; x < depth; x++) fprintf(fp, " ");
    fprintf(fp, "called from %s:%d", p->code->filename(), p->code->line());
    if (ifa_verbose && p->lvals.n) fprintf(fp, " send:%d", p->lvals[0]->sym->id);
    fprintf(fp, "\n");
  }
  // `es` may be the distinguished global contour (fa->global_es),
  // whose edges vec is always empty — the loop below no-ops.
  Vec<AEdge *> edges;
  for (AEdge *e : es->edges) if (e) edges.add(e);
  qsort(edges.v, edges.n, sizeof(edges[0]), compar_edge_id);
  for (AEdge *e : edges) show_call_tree(fp, e->pnode, e->from, depth);
}

void show_avar_call_tree(FILE *fp, AVar *av) {
  EntrySet *es = (EntrySet *)av->contour;
  Vec<AEdge *> edges;
  for (AEdge *e : es->edges) if (e) edges.add(e);
  qsort(edges.v, edges.n, sizeof(edges[0]), compar_edge_id);
  for (AEdge *e : edges) show_call_tree(fp, e->pnode, e->from, 1);
}

void show_candidates(FILE *fp, PNode *pn, Sym *arg0) {
  Vec<Fun *> *pfuns = pn->code->ast->visible_functions(arg0);
  if (!pfuns) return;
  Vec<Fun *> funs(*pfuns);
  funs.set_to_vec();
  qsort_by_id(funs);
  fprintf(fp, "note: candidates are:\n");
  for (Fun *f : funs) {
    show_fun(f, fp);
    fprintf(fp, "\n");
  }
}

void show_violations(FA *fa, FILE *fp) {
  Vec<ATypeViolation *> vv;
  for (ATypeViolation *v : fa->type_violations) if (v) vv.add(v);
  qsort(vv.v, vv.n, sizeof(vv[0]), compar_tv);
  Vec<cchar *> printed;
  for (ATypeViolation *v : vv) if (v) {
    if (is_uninformative_violation(v)) continue;  // ifa/149
    char *buf = nullptr;
    size_t size = 0;
    FILE *memfp = open_memstream(&buf, &size);
    if (!memfp) memfp = fp;

    cchar *filename = nullptr;
    int line = 0;
    int col = 0;

    find_violation_user_loc(v, &filename, &line, &col);

    if (!filename || line <= 0) {
      if (v->send && v->send->var && v->send->var->def && v->send->var->def->code) {
        if (!filename) filename = v->send->var->def->code->filename();
        if (line <= 0) line = v->send->var->def->code->line();
      } else if (v->av && v->av->var && v->av->var->sym && v->av->var->sym->ast) {
        if (!filename) filename = v->av->var->sym->filename();
        if (line <= 0) line = v->av->var->sym->line();
      } else if (v->av && !v->av->contour_is_entry_set && v->av->contour != GLOBAL_CONTOUR) {
        CreationSet *cs = (CreationSet *)v->av->contour;
        if (cs && cs->sym) {
          if (!filename) filename = cs->sym->filename();
          if (line <= 0) line = cs->sym->line();
        }
      }
    }
    if ((!filename || !filename[0]) && fa->funs.n && fa->funs[0]->sym) filename = fa->funs[0]->sym->filename();

    // issues/018: a BOXING violation is an ERROR even in permissive
    // mode. Permissive mode's bargain is "warn, and insert a runtime
    // check" -- but a variable whose type mixes basic types (int64 and
    // str, say) has NO RUNTIME REPRESENTATION at all, so there is no
    // check to insert and nothing downstream can recover. Reporting it
    // as a warning produced the worst available outcome: 8 warnings,
    // exit 0, and a binary that aborts with "matching function not
    // found" the moment the value is used. shedskin reaches the same
    // wall and at least fails at build time (its generated C++ gets
    // `invalid conversion from '__ss_int' to 'pyobj*'`).
    // issues/018: BOXING has no representation, so it is an error in
    // every environment. ifa/issues/039: DEFINITELY_UNBOUND likewise --
    // no execution of the program is correct, so refusing costs
    // nothing. MAYBE_UNBOUND is deliberately NOT here: it is a strict
    // warning and otherwise a runtime check, because a "possibly
    // unbound" read can be perfectly valid (a short-circuit guard --
    // see the issue), and erroring on it would reject correct programs.
    // ifa/158, author 2026-09-17: EVERY TYPE VIOLATION IS FATAL, not only
    // BOXING and DEFINITELY_UNBOUND.
    //
    // A violation means the analysis could not type something. Emitting a
    // binary anyway is CLAUDE.md's "the alternative to failing here is emitting
    // a program that lies", and `sudoku5` is the worked example: it compiles
    // with 19 warnings, `solution` is inferred as `{tuple, int64}` where
    // shedskin gives `list<tuple<__ss_int> *>`, and NOTHING catches it -- the
    // program's only output is a `TIME %.2f` line, so the corpus stdout check
    // compares two wall clocks and reports a match.
    //
    // MAYBE_UNBOUND stays advisory, for the reason below: a possibly-unbound
    // read can be perfectly correct, so erroring on it rejects valid programs.
    //
    // PYC_STRICTVIOL=0 restores the old severity, for attributing a change to
    // this rather than guessing at it.
    bool always_fatal = strictviol_enabled()
                            ? v->kind != ATypeViolation_kind::MAYBE_UNBOUND
                            : (v->kind == ATypeViolation_kind::BOXING ||
                               v->kind == ATypeViolation_kind::DEFINITELY_UNBOUND);
    // ifa/issues/039: MAYBE_UNBOUND is a WARNING even under --strict,
    // and deliberately so. A "possibly unbound" read can be perfectly
    // correct -- `if first or d < bd:` short-circuits the read away
    // (tests/scope_read_before_write.py) -- so erroring on it rejects
    // valid programs. Its enforcement is a RUNTIME check, or an
    // auto-initialisation under `safe`; the compile-time message is
    // advisory in every environment.
    bool always_warning = v->kind == ATypeViolation_kind::MAYBE_UNBOUND;
    cchar *severity = always_fatal ? "error" : (always_warning || fruntime_errors) ? "warning" : "error";

    if (filename && line > 0) {
      if (col > 0)
        fprintf(memfp, "%s:%d:%d: %s: ", filename, line, col, severity);
      else
        fprintf(memfp, "%s:%d: %s: ", filename, line, severity);
    } else {
      fprintf(memfp, "%s: ", severity);
    }

    switch (v->kind) {
      default:
        assert(0);
      case ATypeViolation_kind::PRIMITIVE_ARGUMENT:
        fprintf(memfp, "illegal primitive argument type ");
        show_illegal_type(memfp, v);
        break;
      case ATypeViolation_kind::SEND_ARGUMENT:
        if (v->av->var->sym->is_symbol && v->send->var->def->rvals[0] == v->av->var) {
          fprintf(memfp, "unresolved call '%s'", v->av->var->sym->name);
          if (ifa_verbose) fprintf(memfp, " send:%d", v->send->var->sym->id);
          fprintf(memfp, "\n");
          show_candidates(memfp, v->send->var->def, v->av->var->sym);
        } else {
          fprintf(memfp, "illegal call argument type ");
          show_illegal_type(memfp, v);
        }
        break;
      case ATypeViolation_kind::DISPATCH_AMBIGUITY:
        fprintf(memfp, "ambiguous call '%s'", v->av->var->sym->name);
        if (ifa_verbose) fprintf(memfp, " send:%d", v->send->var->sym->id);
        fprintf(memfp, "\n");
        fprintf(memfp, "note: candidates are:\n");
        for (Fun *f : *v->funs) if (f) {
          show_fun(f, memfp);
          fprintf(memfp, "\n");
        }
        break;
      case ATypeViolation_kind::MEMBER:
        if (v->av->out->n == 1)
          fprintf(memfp, "unresolved member '%s'", v->av->out->v[0]->sym->name);
        else {
          fprintf(memfp, "unresolved member\n");
          for (CreationSet *selector : v->av->out->sorted) fprintf(memfp, "  selector '%s'\n", selector->sym->name);
        }
        // A constant's CreationSet has an UNNAMED sym (the literal) whose
        // `type` is the class: `b"".join(...)` used to report "of class
        // '<anonymous>'" for plain bytes. Name the class, not the literal.
        {
          auto cls_name = [](CreationSet *cs) -> cchar * {
            Sym *s = cs ? cs->sym : nullptr;
            if (!s) return "<anonymous>";
            if (s->name) return s->name;
            if (s->type && s->type->name) return s->type->name;
            return "<anonymous>";
          };
          if (v->type->n == 1)
            fprintf(memfp, " of class '%s'\n", cls_name(v->type->v[0]));
          else {
            fprintf(memfp, " of classes\n");
            for (CreationSet *cs : v->type->sorted) if (cs) fprintf(memfp, "  class '%s'\n", cls_name(cs));
          }
        }
        break;
      case ATypeViolation_kind::MATCH:
        if (v->av->var->sym->name)
          fprintf(memfp, "near '%s' unmatched type: ", v->av->var->sym->name);
        else
          fprintf(memfp, "unmatched type: ");
        show_type(*v->type, memfp);
        fprintf(memfp, "\n");
        break;
      case ATypeViolation_kind::NOTYPE:
        show_name(memfp, v->av);
        fprintf(memfp, "has no type\n");
        break;
      case ATypeViolation_kind::BOXING:
        show_name(memfp, v->av);
        fprintf(memfp, "has mixed basic types:");
        show_type(*v->type, memfp);
        fprintf(memfp, "\n");
        if (getenv("PYC_DBG_BOXWHY")) {
          AVar *bv = v->av;
          fprintf(stderr, "[boxwhy] av#%d '%s' contour=%s num_coerce=%s members:", bv->id,
                  (bv->var && bv->var->sym && bv->var->sym->name) ? bv->var->sym->name : "?",
                  bv->contour_is_entry_set ? "ES" : "CS",
                  bv->num_coerce ? (bv->num_coerce->name ? bv->num_coerce->name : "?") : "(none)");
          for (CreationSet *c : bv->out->sorted)
            fprintf(stderr, " %s%s", c->sym->name ? c->sym->name : "?",
                    c->sym->constant ? "[const]" : "[runtime]");
          if (!bv->contour_is_entry_set && bv->contour != GLOBAL_CONTOUR) {
            CreationSet *oc = (CreationSet *)bv->contour;
            fprintf(stderr, "  owner=%s#%d kind=%d", (oc && oc->sym && oc->sym->name) ? oc->sym->name : "?",
                    oc ? oc->id : -1, (oc && oc->sym && oc->sym->type) ? (int)oc->sym->type->type_kind : -1);
          }
          fprintf(stderr, "\n");
          // ifa/146: walk the WHOLE backward closure and report the nearest
          // AVars that are still PURE in one numeric basic -- the points
          // where the int and the float have not yet met. If none exist the
          // mix is born at a single site and no split can separate it; if
          // they do, a coercion-fed demand could be backtracked to them.
          if (getenv("PYC_DBG_BOXPURE")) {
            Vec<AVar *> seen, work;
            int npure_int = 0, npure_flt = 0, nmixed = 0;
            seen.set_add(bv);
            work.add(bv);
            for (int i = 0; i < work.n && i < 20000; i++)
              for (AVar *x : work.v[i]->backward)
                if (x && seen.set_add(x)) {
                  work.add(x);
                  if (!x->out) continue;
                  int ni = 0, nf = 0;
                  for (CreationSet *c : x->out->sorted)
                    if (Sym *bt = to_basic_type(c->sym->type)) {
                      if (bt == sym_float64) ++nf;
                      else if (bt->num_kind) ++ni;
                    }
                  if (ni && nf) { ++nmixed; continue; }
                  if (ni && !nf) {
                    if (++npure_int <= 3)
                      fprintf(stderr, "   PURE-INT av#%d '%s' in %s\n", x->id,
                              (x->var && x->var->sym && x->var->sym->name) ? x->var->sym->name : "?",
                              (x->contour_is_entry_set && ((EntrySet *)x->contour)->fun &&
                               ((EntrySet *)x->contour)->fun->sym->name)
                                  ? ((EntrySet *)x->contour)->fun->sym->name : "(cs)");
                  } else if (nf && !ni)
                    ++npure_flt;
                }
            fprintf(stderr, "   BOXPURE av#%d: upstream pure-int=%d pure-float=%d mixed=%d\n", bv->id, npure_int,
                    npure_flt, nmixed);
          }
          for (AVar *b : bv->backward) {
            if (!b || !b->out) continue;
            bool has_int = false;
            for (CreationSet *c : b->out->sorted)
              if (c->sym->type && to_basic_type(c->sym->type) && to_basic_type(c->sym->type)->num_kind &&
                  to_basic_type(c->sym->type) != sym_float64)
                has_int = true;
            if (!has_int) continue;
            EntrySet *be = b->contour_is_entry_set ? (EntrySet *)b->contour : nullptr;
            fprintf(stderr, "   <- av#%d '%s' in %s coerce=%s :", b->id,
                    (b->var && b->var->sym && b->var->sym->name) ? b->var->sym->name : "?",
                    (be && be->fun && be->fun->sym && be->fun->sym->name) ? be->fun->sym->name : "(cs)",
                    b->num_coerce ? "yes" : "no");
            for (CreationSet *c : b->out->sorted)
              fprintf(stderr, " %s%s", c->sym->name ? c->sym->name : "?",
                      c->sym->constant ? "[const]" : "[runtime]");
            fprintf(stderr, "\n");
          }
        }
        break;
      case ATypeViolation_kind::MAYBE_UNBOUND:
        show_name(memfp, v->av);
        fprintf(memfp, "may be used before assignment on some path; type is:");
        show_type(*v->type, memfp);
        fprintf(memfp, "\n");
        break;
      case ATypeViolation_kind::DEFINITELY_UNBOUND:
        show_name(memfp, v->av);
        fprintf(memfp, "is used before assignment on every path; type is:");
        show_type(*v->type, memfp);
        fprintf(memfp, "\n");
        break;
      case ATypeViolation_kind::CLOSURE_RECURSION:
        show_name(memfp, v->av);
        fprintf(memfp, "is recursive closure\n");
        break;
    }

    if (filename && line > 0) {
      show_source_caret(memfp, filename, line, col);
    }

    if (v->send)
      show_call_tree(memfp, v->send->var->def, (EntrySet *)v->send->contour);
    else if (v->av->contour_is_entry_set)
      show_avar_call_tree(memfp, v->av);
    else if (v->av->contour != GLOBAL_CONTOUR) {
      // cs->defs is a SET (`cs->defs.set_add(v)`), and a Vec in set mode
      // is an open hash table: past SET_LINEAR_SIZE (4) `n` is the table
      // CAPACITY and empty slots are NULL.  `.first()` is a raw `v[0]`,
      // so it reads a hole as soon as a CreationSet has 4+ creation
      // points -- sudoku5's merged `list` CS has 13, and this segfaulted
      // the compiler from a diagnostic path.  Every other reader of
      // `defs` already guards (`for (AVar *av : cs->defs) if (av)`) or
      // uses first_in_set(); this was the one that did not.
      AVar *d = ((CreationSet *)v->av->contour)->defs.first_in_set();
      if (d && d->var && d->var->def && d->contour_is_entry_set)
        show_call_tree(memfp, d->var->def, (EntrySet *)d->contour, 1);
    }

    if (memfp != fp) {
      fclose(memfp);
      if (buf) {
        bool dup = false;
        for (cchar *p : printed) {
          if (p && strcmp(p, buf) == 0) {
            dup = true;
            break;
          }
        }
        if (!dup) {
          printed.add(buf);
          fputs(buf, fp);
        } else {
          free(buf);
        }
      }
    }
  }
}

// ifa/issues/124 probe: is a named function's FORMAL seen as a type
// confluence at all? `append(self, x)` is called with an int from one
// comprehension and None from another, so arg3 ought to be one.
void dbg_confluence_probe(AVar *av, bool added) {
  static cchar *want = nullptr;
  static int checked = 0;
  if (!checked) { want = getenv("IFA_DBG_CONFLUENCE"); checked = 1; }
  if (!want || !av->contour_is_entry_set) return;
  EntrySet *es = (EntrySet *)av->contour;
  if (!es->fun || !es->fun->sym || !es->fun->sym->name || strcmp(es->fun->sym->name, want)) return;
  if (!av->var || !av->var->is_formal) return;
  fprintf(stderr, "CONFL p=%d es=%d formal=%s added=%d in:", analysis_pass, es->id,
          av->var->sym->name ? av->var->sym->name : "?", added ? 1 : 0);
  for (CreationSet *c : av->in->type->sorted) fprintf(stderr, " %s#%d", c->sym->name ? c->sym->name : "?", c->id);
  fprintf(stderr, " out:");
  for (CreationSet *c : av->out->type->sorted) fprintf(stderr, " %s#%d", c->sym->name ? c->sym->name : "?", c->id);
  // EVERY backward writer, including empty-typed ones the earlier probe
  // filtered out -- the question is where `None` enters when no writer
  // seems to carry it.
  fprintf(stderr, " | writers(%d):", av->backward.n);
  for (AVar *x : av->backward) if (x) {
    EntrySet *xes = x->contour_is_entry_set ? (EntrySet *)x->contour : nullptr;
    fprintf(stderr, " {av=%d %s/es%d:", x->id,
            xes && xes->fun && xes->fun->sym && xes->fun->sym->name ? xes->fun->sym->name : "?",
            xes ? xes->id : -1);
    for (CreationSet *c : x->out->type->sorted) fprintf(stderr, " %s#%d", c->sym->name ? c->sym->name : "?", c->id);
    fprintf(stderr, " RAW:");
    for (CreationSet *c : x->out->sorted)
      fprintf(stderr, " %s#%d%s", c->sym->name ? c->sym->name : "?", c->id, c->sym->is_constant ? "(const)" : "");
    fprintf(stderr, "}");
  }
  fprintf(stderr, "\n");
}

// ifa/133 probe: IFA_DBG_AV=<id> dumps one AVar's identity and every
// backward writer, with the writer's function and the type it contributes.

// ifa/133 probe: IFA_DBG_AV=<id> dumps one AVar's identity and every
// backward writer, with the writer's function and the type it contributes.
void dbg_dump_av(AVar *av) {
  static int want = -2;
  if (want == -2) { cchar *v = getenv("IFA_DBG_AV"); want = v ? atoi(v) : -1; }
  if (want < 0 || !av || av->id != want) return;
  fprintf(stderr, "[av] p=%d %d var=%s in=%s type=", analysis_pass, av->id,
          (av->var && av->var->sym && av->var->sym->name) ? av->var->sym->name : "(anon)",
          (av->contour_is_entry_set && ((EntrySet *)av->contour)->fun &&
           ((EntrySet *)av->contour)->fun->sym && ((EntrySet *)av->contour)->fun->sym->name)
              ? ((EntrySet *)av->contour)->fun->sym->name : "(cs)");
  if (av->in && av->in->type)
    for (CreationSet *c : av->in->type->sorted) if (c && c->sym)
      fprintf(stderr, " %s#%d", c->sym->name ? c->sym->name : "?", c->id);
  fprintf(stderr, "\n");
  for (AVar *b : av->backward) if (b) {
    fprintf(stderr, "    <- av=%-6d var=%-14s in=%-18s contributes:", b->id,
            (b->var && b->var->sym && b->var->sym->name) ? b->var->sym->name : "(anon)",
            (b->contour_is_entry_set && ((EntrySet *)b->contour)->fun &&
             ((EntrySet *)b->contour)->fun->sym && ((EntrySet *)b->contour)->fun->sym->name)
                ? ((EntrySet *)b->contour)->fun->sym->name : "(cs)");
    if (b->out && b->out->type)
      for (CreationSet *c : b->out->type->sorted) if (c && c->sym)
        fprintf(stderr, " %s#%d", c->sym->name ? c->sym->name : "?", c->id);
    fprintf(stderr, "\n");
  }
}

// ifa/133: the same conflation the `nilstore` fix closed in
// compute_setters, at the site this issue's audit measured as "inert". It
// is NOT inert on `bh`: cs=1191 is the arity-1 literal contour holding both
// `__slots__ = ["seed"]` and four `[None]` literals, its var[0] is
// {str, nil}, and the NIL writer is skipped here -- so no confluence is
// detected, the CreationSet never reaches `tc_cs_dropped`, and route 4
// never sees the merge at all. `->type` projects a lone nil to bottom, so
// skip only when the RAW type is empty too, i.e. genuinely not analyzed.

// issues/128 step 1: the ELEMENT CONFLUENCE census.
//
// The demand is not the MIXED field write -- that is three steps downstream,
// and acting on it failed because its receiver is a loop local with nothing
// to filter on. The demand is an element channel that receives two DIFFERENT
// CLASSES, which is what makes the loop variable a union in the first place.
// On `chull`: Hull.edges' element is `Vertex Edge Edge Edge Edge`, while
// vertices and faces are clean.
//
// The census classifies each such channel by whether its WRITERS are already
// separated, because that decides whether anything can act:
//
//   SEPARABLE    two writers carry disjoint class sets -- e.g. chull's
//                `es=680 __setitem__ Vertex` against `es=497 __setitem__
//                Edge`. The value path is already split and only the
//                RECEIVER is shared, so splitting the contour that shares it
//                gives the site two contours, defs becomes 2, and route 4
//                can partition. This is the actionable population.
//   FUSED        every writer already carries the whole union. Nothing
//                distinguishes them, so an ES split has no key and this
//                needs a different answer.
//
// `defs` is reported with each because route 4 declines at defs=1, which is
// why this family survives today: chull's are defs=1.
// issues/128 step 3 precondition: the RECEIVER CARDINALITY measurement.
//
// The proposed split is on the receiver formal of a shared container-method
// contour. The risk is ifa/144's fan: if a contour's receiver holds N
// containers, splitting by receiver can hand back N groups, and
// extend/append/__setitem__ are the most-shared functions in the program --
// the most expensive place to get that wrong.
//
// So measure it FIRST. For every EntrySet, how many distinct CreationSets
// does its receiver (positional argument 1) hold? A distribution dominated
// by 1 means a receiver split is cheap and precise; a long tail means it
// fans and must peel one group at a time.
void report_recv_cardinality() {
  if (!getenv("IFA_DBG_RECVCARD")) return;
  int hist[9] = {0};  // index 8 = "8 or more"
  int total = 0, over1 = 0, maxn = 0;
  cchar *maxfun = "?";
  for (EntrySet *es : fa->ess) {
    if (!es || !es->fun || !es->fun->sym) continue;
    // positional_arg_positions is the ordered list; [0] is the SELECTOR and
    // [1] is the receiver -- the FUNES dump shows the same shape,
    // `args= [__setitem__#44] [list#1848 list#1887] [int64#6] [Edge#1896]`.
    // (Selecting by comparing MPosition POINTERS, as a first cut did, picks
    // an arbitrary formal and reported every receiver as cardinality 1.)
    Vec<MPosition *> &pp = es->fun->positional_arg_positions;
    if (pp.n < 2) continue;
    AVar *recv = es->args.get(pp.v[1]);
    if (!recv || !recv->out || !recv->out->type) continue;
    Vec<CreationSet *> cs;
    for (CreationSet *c : recv->out->type->sorted) if (c) cs.set_add(c);
    int n = cs.set_count();
    if (!n) continue;
    ++total;
    if (n > 1) ++over1;
    hist[n < 8 ? n : 8]++;
    if (n > maxn) { maxn = n; maxfun = es->fun->sym->name ? es->fun->sym->name : "?"; }
  }
  fprintf(stderr, "RECVCARD total=%d over1=%d max=%d(%s) hist:", total, over1, maxn, maxfun);
  for (int i = 1; i < 9; i++) fprintf(stderr, " %d=%d", i, hist[i]);
  fprintf(stderr, "\n");
}

// SPLIT vs HOIST, shedskin's `lowest_common_parents` test. Extracted from
// report_elem_confluence (ifa/152) so the splitter can apply it too, per
// CLAUDE.md: "Classify the confluence before splitting it."
//
//   RELATED   richards' DeviceTask/HandlerTask/IdleTask/WorkTask all derive
//             from Task. Legitimate polymorphism -- the field is HOISTED to
//             the common ancestor (shedskin's virtualvars), never split.
//             Splitting these is what broke richards.
//   UNRELATED chull's Vertex/Edge/Face have no bases at all. A precision
//             failure, and what should be split away.
//
// Every pyc class specializes `object` and `__pyc_any_type__`, so "shares an
// ancestor" is trivially true; the shared ancestor must be USER code.
// Structural, not by name.

void report_elem_confluence() {
  if (!getenv("IFA_DBG_ELEMCONF")) return;
  int n_conf = 0, n_sep = 0, n_fused = 0, n_sep_rel = 0, n_sep_unrel = 0;
  // how many distinct classes exist at all -- the yardstick for "universal root"
  Vec<Sym *> all_classes;
  for (CreationSet *c : fa->css) if (c && c->sym) all_classes.set_add(c->sym);
  (void)all_classes;
  for (CreationSet *cs : fa->css) {
    if (!cs || !cs->sym || !cs->sym->element || !cs->sym->element->var || !cs->added_element_var) continue;
    AVar *e = unique_AVar(cs->sym->element->var, cs);
    if (!e || !e->out || !e->out->type) continue;
    // distinct CLASSES in the element, not distinct CreationSets: four Edge
    // contours are one class and are not a confluence.
    Vec<Sym *> classes;
    for (CreationSet *c : e->out->type->sorted) if (c && c->sym) classes.set_add(c->sym);
    if (classes.set_count() < 2) continue;
    ++n_conf;
    // are two writers' class sets disjoint?
    bool separable = false;
    for (AVar *b1 : e->backward) {
      if (!b1 || !b1->out || !b1->out->type) continue;
      Vec<Sym *> s1;
      for (CreationSet *c : b1->out->type->sorted) if (c && c->sym) s1.set_add(c->sym);
      if (!s1.set_count()) continue;
      for (AVar *b2 : e->backward) {
        if (!b2 || b2 == b1 || !b2->out || !b2->out->type) continue;
        bool overlap = false; int n2 = 0;
        for (CreationSet *c : b2->out->type->sorted)
          if (c && c->sym) { ++n2; if (s1.set_in(c->sym)) { overlap = true; break; } }
        if (n2 && !overlap) { separable = true; break; }
      }
      if (separable) break;
    }
    // Do the classes share an ancestor? This is shedskin's
    // `lowest_common_parents` test and it decides SPLIT vs HOIST:
    //
    //   RELATED   richards' DeviceTask/HandlerTask/IdleTask/WorkTask all
    //             derive from Task. The union is legitimate polymorphism and
    //             must NOT be split -- dropping such a write is what broke
    //             richards. shedskin hoists the shared field to the common
    //             ancestor (virtual.py's virtualvars) so one slot serves all.
    //   UNRELATED chull's Vertex/Edge/Face have no bases at all. The union is
    //             a precision failure and is what should be split away.
    // RELATED = the classes share an ancestor that is not a UNIVERSAL root.
    //
    // Every pyc class specializes `object` and `__pyc_any_type__`, so "shares
    // an ancestor" is trivially true and useless. The informative test is
    // structural and needs no names: an ancestor shared by EVERY class in the
    // program tells you nothing, so require one whose implementor count is
    // smaller than the program's class count.
    //
    //   chull:    Vertex -> object __pyc_any_type__
    //             Edge   -> object __pyc_any_type__      shared: roots only
    //   richards: WorkTask -> Task __pyc_any_type__
    //             IdleTask -> Task __pyc_any_type__      shared: Task
    bool related = classes_are_related(classes);
    separable ? ++n_sep : ++n_fused;
    if (separable) (related ? ++n_sep_rel : ++n_sep_unrel);
    fprintf(stderr, "ELEMCONF %s%s cs=%d sym=%s defs=%d classes=%d:", separable ? "SEPARABLE" : "FUSED",
            separable ? (related ? "-RELATED" : "-UNRELATED") : "", cs->id,
            cs->sym->name ? cs->sym->name : "?", cs->defs.set_count(), classes.set_count());
    for (Sym *sy : classes) if (sy) fprintf(stderr, " %s", sy->name ? sy->name : "?");
    fprintf(stderr, "\n");
  }
  fprintf(stderr, "ELEMCONF-TOTAL confluences=%d separable=%d (related=%d unrelated=%d) fused=%d\n",
          n_conf, n_sep, n_sep_rel, n_sep_unrel, n_fused);
}

// ifa/157: IFA_DBG_CSDEFS=<cs id> -- where a CreationSet's creation points
// actually are. "Which contour do several creation points share, and does one
// of them supply the offending type" (CLAUDE.md) is unanswerable without this.

// ifa/157: IFA_DBG_CSDEFS=<cs id> -- where a CreationSet's creation points
// actually are. "Which contour do several creation points share, and does one
// of them supply the offending type" (CLAUDE.md) is unanswerable without this.
void report_cs_defs() {
  cchar *want = getenv("IFA_DBG_CSDEFS");
  if (!want) return;
  int id = atoi(want);
  // id < 0: scan for every CreationSet with an irrepresentable POSITIONAL slot.
  for (CreationSet *cs : fa->css) {
    if (!cs) continue;
    if (id >= 0) {
      if (cs->id != id) continue;
    } else {
      bool bad = false;
      for (AVar *v : cs->vars)
        if (v && v->out && atype_irrepresentable(v->out->type)) { bad = true; break; }
      if (!bad) continue;
    }
    fprintf(stderr, "CSDEFS p=%d cs=%d sym=%s defs=%d\n", analysis_pass, cs->id,
            (cs->sym && cs->sym->name) ? cs->sym->name : "?", cs->defs.set_count());
    for (AVar *d : cs->defs) if (d) {
      EntrySet *de = d->contour_is_entry_set ? (EntrySet *)d->contour : nullptr;
      fprintf(stderr, "  def av=%d var=%s in=%s es=%d line=%d elem=", d->id,
              (d->var && d->var->sym && d->var->sym->name) ? d->var->sym->name : "(anon)",
              (de && de->fun && de->fun->sym && de->fun->sym->name) ? de->fun->sym->name : "(cs)",
              de ? de->id : -1,
              (d->var && d->var->sym && d->var->sym->ast) ? d->var->sym->ast->line() : -1);
      if (d->out && d->out->type)
        for (CreationSet *c : d->out->type->sorted) if (c && c->sym)
          fprintf(stderr, " %s#%d", c->sym->name ? c->sym->name : "?", c->id);
      fprintf(stderr, "\n");
    }
    // ifa/157: the POSITIONAL channel too (ifa/104's second content channel).
    // A tuple's content lives here, not in the element, so a merged tuple is
    // invisible without it.
    for (int i = 0; i < cs->vars.n; i++) {
      AVar *v = cs->vars.v[i];
      if (!v || !v->out) continue;
      fprintf(stderr, "  slot[%d] =", i);
      for (CreationSet *c : v->out->type->sorted) if (c && c->sym)
        fprintf(stderr, " %s#%d", c->sym->name ? c->sym->name : "?", c->id);
      fprintf(stderr, "\n");
      for (AVar *w : v->backward) if (w && w->out && w->out->type->n) {
        EntrySet *we = w->contour_is_entry_set ? (EntrySet *)w->contour : nullptr;
        fprintf(stderr, "      <- av=%d var=%s in=%s es=%d :", w->id,
                (w->var && w->var->sym && w->var->sym->name) ? w->var->sym->name : "(anon)",
                (we && we->fun && we->fun->sym && we->fun->sym->name) ? we->fun->sym->name : "(cs)",
                we ? we->id : -1);
        for (CreationSet *c : w->out->type->sorted) if (c && c->sym)
          fprintf(stderr, " %s#%d", c->sym->name ? c->sym->name : "?", c->id);
        fprintf(stderr, "\n");
      }
    }
    // And who WRITES the element channel -- the union's actual contributors,
    // which are generally not the creation points.
    if (cs->sym && cs->sym->element && cs->sym->element->var && cs->added_element_var) {
      AVar *e = unique_AVar(cs->sym->element->var, cs);
      if (e) {
        fprintf(stderr, "  element writers:\n");
        for (AVar *w : e->backward) if (w && w->out && w->out->type->n) {
          EntrySet *we = w->contour_is_entry_set ? (EntrySet *)w->contour : nullptr;
          fprintf(stderr, "    <- av=%d var=%s in=%s es=%d :", w->id,
                  (w->var && w->var->sym && w->var->sym->name) ? w->var->sym->name : "(anon)",
                  (we && we->fun && we->fun->sym && we->fun->sym->name) ? we->fun->sym->name : "(cs)",
                  we ? we->id : -1);
          for (CreationSet *c : w->out->type->sorted) if (c && c->sym)
            fprintf(stderr, " %s#%d", c->sym->name ? c->sym->name : "?", c->id);
          fprintf(stderr, "\n");
        }
      }
    }
  }
}

void report_cs_vars() {
  cchar *want = getenv("IFA_DBG_CSVARS");
  if (!want) return;
  for (CreationSet *cs : fa->css) {
    if (!cs || !cs->sym || !cs->sym->name || strcmp(cs->sym->name, want)) continue;
    fprintf(stderr, "CSVARS cs=%d sym=%s vars=%d defs=%d arity=%d no_arity=%d elem=", cs->id, cs->sym->name,
            cs->vars.n, cs->defs.set_count(), cs->static_arity, cs->no_static_arity ? 1 : 0);
    // The ELEMENT channel, which is where a list-layout container's content
    // lives -- `vars` is the positional/record channel and is empty for it.
    // Printed read-only: only when the element AVar already exists, so this
    // never calls the accessor that creates one.
    if (cs->sym->element && cs->sym->element->var && cs->added_element_var) {
      AVar *e = unique_AVar(cs->sym->element->var, cs);
      if (e && e->out && e->out->type)
        for (CreationSet *c : e->out->type->sorted)
          if (c && c->sym) fprintf(stderr, " %s", c->sym->name ? c->sym->name : "?");
    } else
      fprintf(stderr, "(none)");
    fprintf(stderr, "\n");
    // Who WRITES into the element channel, and from which contour. For a
    // list-layout container this is the only place its content arrives, so
    // an unexpected type here names the function that put it there.
    if (cs->sym->element && cs->sym->element->var && cs->added_element_var) {
      AVar *e = unique_AVar(cs->sym->element->var, cs);
      if (e)
        for (AVar *b : e->backward) {
          if (!b || !b->out || !b->out->type) continue;
          EntrySet *bes = b->contour_is_entry_set ? (EntrySet *)b->contour : nullptr;
          fprintf(stderr, "  ELEMWRITER es=%d fun=%s var=%s type=", bes ? bes->id : -1,
                  (bes && bes->fun && bes->fun->sym && bes->fun->sym->name) ? bes->fun->sym->name : "(cs)",
                  (b->var && b->var->sym && b->var->sym->name)
                      ? b->var->sym->name
                      : ((b->var && b->var->sym && b->var->sym->constant) ? b->var->sym->constant : "(anon)"));
          for (CreationSet *c : b->out->type->sorted)
            if (c && c->sym) fprintf(stderr, " %s#%d", c->sym->name ? c->sym->name : "?", c->id);
          fprintf(stderr, "\n");
        }
    }
    for (AVar *d : cs->defs) {
      if (!d) continue;
      EntrySet *des = d->contour_is_entry_set ? (EntrySet *)d->contour : nullptr;
      fprintf(stderr, "  DEF av=%d es=%d fun=%s\n", d->id, des ? des->id : -1,
              (des && des->fun && des->fun->sym && des->fun->sym->name) ? des->fun->sym->name : "(cs)");
    }
    for (AVar *v : cs->vars) {
      if (!v) continue;
      fprintf(stderr, "  var=%s av=%d type=", (v->var && v->var->sym && v->var->sym->name) ? v->var->sym->name : "?", v->id);
      if (v->out && v->out->type)
        for (CreationSet *c : v->out->type->sorted)
          if (c && c->sym) fprintf(stderr, " %s#%d", c->sym->name ? c->sym->name : "?", c->id);
      fprintf(stderr, "\n");
    }
  }
}


void report_fun_entry_sets() {
  // IFA_DBG_ESHIST: contours per Fun at the end of the analysis, against
  // how many DISTINCT formal signatures those contours have, at three
  // levels -- the unnecessary-contour metric of ifa/169 and ifa/170:
  //   exact:   CreationSet ids (constants stripped via ->type)
  //   bysym:   classes only
  //   byshape: class + element classes (containers), + per-position
  //            classes (tuples) -- the minimum a type-shaped identity needs
  // Keys are Sym / CreationSet ids, never names (names are not unique).
  // One line per Fun: `ESHIST <contours> <name> fid=<id> <file>:<line>
  // exact=<n> bysym=<n> byshape=<n>`. IFA_DBG_ESHIST_SIGS adds each
  // contour's raw formal types for Funs with more than one.
  if (getenv("IFA_DBG_ESHIST")) {
    auto sym_key = [](CreationSet *c) { return std::to_string(c->sym ? c->sym->id : -1); };
    auto shape = [&](CreationSet *c) -> std::string {
      std::string r = sym_key(c);
      std::set<std::string> el;
      if (c->sym && c->sym->element && c->added_element_var) {
        AVar *ea = unique_AVar(c->sym->element->var, c);
        if (ea && ea->out)
          for (CreationSet *q : ea->out->type->sorted) if (q) el.insert(sym_key(q));
      }
      if (!el.empty()) {
        r += "[";
        for (auto &x : el) r += x + ";";
        r += "]";
      }
      if (c->sym == sym_tuple)
        for (AVar *v : c->vars) if (v && v->out) {
          std::set<std::string> one;
          for (CreationSet *q : v->out->type->sorted) if (q) one.insert(sym_key(q));
          r += "(";
          for (auto &x : one) r += x + ";";
          r += ")";
        }
      return r;
    };
    Map<Fun *, int> per;
    for (EntrySet *x : fa->ess) if (x && x->fun) per.put(x->fun, per.get(x->fun) + 1);
    form_Map(MapElemFunPint, e, per) {
      Fun *fn = e->key;
      std::set<std::string> ex, bysym, byshape;
      for (EntrySet *x : fa->ess) if (x && x->fun == fn) {
        std::string a, b, c3;
        for (MPosition *p : fn->positional_arg_positions) {
          AVar *av = x->args.get(p);
          a += "|"; b += "|"; c3 += "|";
          if (!av || !av->out) continue;
          std::set<std::string> bs, ss;
          for (CreationSet *c : av->out->type->sorted) if (c) {
            a += std::to_string(c->id) + ",";
            bs.insert(sym_key(c));
            ss.insert(shape(c));
          }
          for (auto &q : bs) b += q + ",";
          for (auto &q : ss) c3 += q + ",";
        }
        ex.insert(a); bysym.insert(b); byshape.insert(c3);
      }
      fprintf(stderr, "ESHIST %d %s fid=%d %s:%d exact=%zu bysym=%zu byshape=%zu\n", e->value,
              fn->sym && fn->sym->name ? fn->sym->name : "?", fn->sym ? fn->sym->id : -1,
              fn->sym ? fn->sym->filename() : "?", fn->sym ? fn->sym->line() : 0, ex.size(), bysym.size(),
              byshape.size());
      if (getenv("IFA_DBG_ESHIST_SIGS") && e->value > 1)
        for (EntrySet *x : fa->ess) if (x && x->fun == fn) {
          fprintf(stderr, "  SIG %s es=%d", fn->sym && fn->sym->name ? fn->sym->name : "?", x->id);
          for (MPosition *p : fn->positional_arg_positions) {
            AVar *av = x->args.get(p);
            fprintf(stderr, " [");
            if (av && av->out)
              for (CreationSet *c : av->out->sorted) if (c)
                fprintf(stderr, " %s#%d%s", c->sym && c->sym->name ? c->sym->name : "?", c->id,
                        c->sym && c->sym->constant ? "c" : "");
            fprintf(stderr, " ]");
          }
          fprintf(stderr, "\n");
        }
    }
  }
  cchar *want = getenv("IFA_DBG_FUNES");
  if (!want) return;
  for (Fun *f : fa->funs) {
    if (!f || !f->sym || !f->sym->name || strcmp(f->sym->name, want)) continue;
    int n = 0;
    for (EntrySet *es : fa->ess) if (es && es->fun == f) ++n;
    fprintf(stderr, "FUNES fun=%s contours=%d\n", f->sym->name, n);
    for (EntrySet *es : fa->ess) {
      if (!es || es->fun != f) continue;
      fprintf(stderr, "  es=%d args=", es->id);
      form_Map(MapElemMPositionVarPair, mp, f->args) {
        Var *v = mp->value;
        if (!v || !v->sym) continue;
        AVar *av = make_AVar(v, es);
        if (!av || !av->out || !av->out->type) continue;
        fprintf(stderr, " [");
        for (CreationSet *c : av->out->type->sorted)
          if (c && c->sym) fprintf(stderr, "%s#%d ", c->sym->name ? c->sym->name : "?", c->id);
        fprintf(stderr, "]");
      }
      fprintf(stderr, "\n");
      // ifa/133: per IN-EDGE, the caller and the ACTUAL TYPES at each
      // position. "Do these call sites differ in anything type-shaped?" is
      // the first question a demand-driven ES split has to answer, and it
      // must be answered with data, not assumed.
      for (AEdge *ee : es->edges) {
        if (!ee || !ee->args.n) continue;
        fprintf(stderr, "    <- edge=%d from=%s es=%d args=", ee->id,
                (ee->from && ee->from->fun && ee->from->fun->sym && ee->from->fun->sym->name)
                    ? ee->from->fun->sym->name : "(root)",
                ee->from ? ee->from->id : -1);
        form_Map(MapElemMPositionAVarPair, mq, ee->args) {
          AVar *aa = mq->value;
          fprintf(stderr, " [");
          if (aa && aa->out && aa->out->type)
            for (CreationSet *c : aa->out->type->sorted)
              if (c && c->sym) fprintf(stderr, "%s#%d ", c->sym->name ? c->sym->name : "?", c->id);
          fprintf(stderr, "]");
        }
        fprintf(stderr, "\n");
      }
    }
  }
}

void report_cs_flow_graphs() {
  if (!getenv("IFA_DBG_CSFLOW")) return;
  for (CreationSet *cs : fa->css) {
    if (!cs || cs->defs.set_count() < 2) continue;
    CSFlowGraph *g = build_cs_flow_graph(cs);
    if (!g) continue;
    int csites_in_defs = 0;
    for (AVar *c : g->csites) if (c && cs->defs.set_in(c)) ++csites_in_defs;
    // Sizes via set_count(), NOT .n: csites/emptycsites/paths/creation_points
    // and targets are all built with set_add, so .n is the open-addressed
    // table capacity rather than the element count -- the same trap the
    // type_violations comment names. Reading .n here printed `empty=7` for a
    // CreationSet with 6 defs, which is what caught it.
    fprintf(stderr, "CSFLOW p=%d cs=%d sym=%s defs=%d sets=%d csites=%d (in_defs=%d) empty=%d\n", analysis_pass,
            cs->id, cs->sym->name ? cs->sym->name : "?", cs->defs.set_count(), g->keys.n, g->csites.set_count(),
            csites_in_defs, g->emptycsites.set_count());
    for (int i = 0; i < g->keys.n; i++) {
      fprintf(stderr, "  set[%d] type=", i);
      for (CreationSet *c : g->keys.v[i]->sorted)
        if (c && c->sym) fprintf(stderr, " %s", c->sym->name ? c->sym->name : "?");
      fprintf(stderr, "  targets=%d path=%d cps=%d\n", g->targets.v[i]->set_count(),
              g->paths.v[i]->set_count(), g->creation_points.v[i]->set_count());
    }
    // Route 1's predicate: a creation point on exactly one assign set is
    // UNCONFUSED and can be split off cleanly.
    int unconfused = 0, confused = 0;
    for (AVar *c : g->csites)
      if (c) { if (g->site_set_count(c) == 1) ++unconfused; else ++confused; }
    fprintf(stderr, "  unconfused=%d confused=%d\n", unconfused, confused);
  }
}

// ifa/143: the demand for a CreationSet split lives ON THE CREATION SET.
//
// Route 4's candidate list used to be `tc_cs_dropped` and nothing else --
// and that Vec is populated in exactly one place, the `else` branch of
// stage 1's TYPE_CONFLUENCE handler, where a confluence turns out to sit
// on a CreationSet contour rather than an EntrySet. So the coarsest rung
// was reachable ONLY through a finer rung's detection.
//
// ifa/142 proved that is precisely backwards for the case route 4 exists
// to answer. Once an element union forms, every writer carries the whole
// union, so `etype == stype` on every edge and TYPE_CONFLUENCE HAS
// NOTHING TO SEE -- the CreationSet is never dropped, never becomes a
// candidate, and the one mechanism able to separate it is never told.
// Measured on `bh`: cs=1180 holds 8 creation points and an element union
// of {Body, str} (three `__slots__` string literals merged with the node
// lists), and appears as a candidate in only 4 of 30 passes -- in each of
// which a finer ladder rung had already fired that pass.
//
// So detect the demand where it lives. An element channel whose types
// cannot share a representation is a demand by itself, whether or not any
// confluence fired. This is not provenance and not structure: it asks what
// the deduced types ARE.
//
// Two unions are NOT a demand and must not be flagged:
//   - anything with `nil_type` in it -- {None, Cell} is representable, and
//     `bh`'s `subp` legitimately holds it. Nil is skipped entirely.
//   - a pure-numeric mix -- {int64, float64} is resolved by
//     `coerce_annotate`, so flagging it would split what coercion fixes.
//
// `mixed_basics` is NOT this test and cannot stand in for it: it counts
// only BASIC types, so a {Body, str} union has one basic and reads as
// uniform. Measured on `bh`, that is exactly why the third clause below
// declined on cs=1606 -- the union it exists to separate was invisible to
// the predicate guarding it.

void report_retconf() {
  if (!getenv("IFA_DBG_RETCONF")) return;
  fprintf(stderr,
          "RETCONF p=%d skipped_rvals: ret_differ=%ld (1fun=%ld nfun=%ld) ret_same=%ld ret_one=%ld local=%ld other=%ld"
          " | receiver: FORMAL=%ld local=%ld CS=%ld none=%ld noedge=%ld"
          " | walk from local: ->FORMAL=%ld (avg hops %.1f) ->CS=%ld (avg hops %.1f) ->join=%ld capped=%ld"
          " | ROUTED formal=%ld cs=%ld (1def=%ld ndef=%ld) declined_related=%ld none=%ld"
          " | KIND static=%ld funvar=%ld (avg %.1f fns) class/recv=%ld (avg %.1f classes) related=%ld distinct_unions=%d"
          " class/arg=%ld nonarrow=%ld noedge=%ld\n",
          analysis_pass, census.rc_ret_differ, census.rc_differ_1fun, census.rc_differ_nfun, census.rc_ret_same, census.rc_ret_one, census.rc_local, census.rc_other,
          census.rv_formal, census.rv_local, census.rv_cs, census.rv_none, census.rv_noedge,
          census.wk_formal, census.wk_formal ? (double)census.wk_hops_formal / census.wk_formal : 0.0,
          census.wk_cs, census.wk_cs ? (double)census.wk_hops_cs / census.wk_cs : 0.0, census.wk_join, census.wk_cap,
          census.rd_formal, census.rd_cs, census.rd_cs_1def, census.rd_cs_ndef, census.rd_declined_related, census.rd_none,
          census.dk_static, census.dk_funvar, census.dk_funvar ? (double)census.dk_funvar_syms / census.dk_funvar : 0.0,
          census.dk_recv, census.dk_recv ? (double)census.dk_recv_syms / census.dk_recv : 0.0, census.dk_recv_related, dk_recv_types.set_count(), census.dk_arg, census.dk_nonarrow, census.dk_noedge);
  census.rc_ret_differ = census.rc_ret_same = census.rc_ret_one = census.rc_local = census.rc_other = 0;
  census.rc_differ_1fun = census.rc_differ_nfun = 0;
  census.rv_formal = census.rv_local = census.rv_cs = census.rv_none = census.rv_noedge = 0;
  census.wk_formal = census.wk_cs = census.wk_join = census.wk_cap = census.wk_hops_formal = census.wk_hops_cs = 0;
  census.rd_formal = census.rd_cs = census.rd_declined_related = census.rd_none = 0;
  census.rd_cs_1def = census.rd_cs_ndef = 0;
  census.dk_static = census.dk_funvar = census.dk_recv = census.dk_arg = census.dk_nonarrow = census.dk_noedge = 0;
  census.dk_funvar_syms = census.dk_recv_syms = census.dk_recv_related = 0;
  dk_recv_types.clear();
}

// ifa/157 step 1 (PYC_ESDEMAND): NOMINATE THE DEMAND INSTEAD OF DROPPING IT.
//
// Stage 1 drops 83-87% of its confluences at `census.tc_skip_rval` -- an ES-contoured
// value that is neither a formal nor a return, so its one actuator ("split an
// EntrySet on a formal") does not apply. But the value did not come from
// nowhere: if it is irrepresentable and it derives from a FORMAL of the same
// contour, then splitting that contour on that formal is the actuator, and the
// demand is what says so.
//
// Measured target, `sudoku5` at p=30: `__getitem__ es=379` returns
// `{int64, str}` because its receiver formal unions tuple CreationSets with
// different slot types (one contributes a `str` slot, another an `int64`). The
// demanded value is one hop from `self`.
//
// The walk stays INSIDE the contour on purpose. Crossing out of it lands on a
// caller's local or a CreationSet, which are ifa/152's backtrack and route 4's
// territory -- both already measured here -- and mixing them in would make this
// step's stop condition unreadable.
//
// The demand is an IRREPRESENTABLE CONVERGED TYPE and nothing weaker. "This
// formal's type is a union" is the FACT that made PYC_CPA arbitrary.

// ifa/issues/101: stamp each container CreationSet with its converged
// element type, then roll those up per creation site. Runs in
// complete_pass, AFTER the flow fixpoint, so the value is the
// whole-pass-invariant element type rather than a mid-pass accumulation
// -- the same discipline EntrySet::type_key needs (066).
//
// READ-ONLY with respect to element AVars: get_element_avar() would
// CREATE one and set added_element_var (which gates numeric coercion),
// so only CSs that already have one are considered.
// ifa/issues/105: find the ORIGIN of type degeneration. Report AVars
// whose type spans more than N distinct SYMS (not CreationSets) --
// a variable holding bool+int+str+float+list+dict+user-classes at once.
// Reported with the defining function and source line so the smallest,
// earliest one can be read in the source. Probe-only.
void report_degenerate_avars() {
  const char *v = getenv("IFA_DBG_DEGEN");
  if (!v) return;
  int thresh = atoi(v) > 0 ? atoi(v) : 6;
  std::map<std::string, int> by_site;
  for (EntrySet *es : fa->entry_set_done) if (es && es->fun) {
    for (Var *var : es->fun->fa_all_Vars) {
      AVar *av = make_AVar(var, es);
      if (!av->out || !av->out->type) continue;
      std::set<Sym *> syms;
      for (CreationSet *c : av->out->type->sorted) if (c->sym) syms.insert(c->sym);
      if ((int)syms.size() < thresh) continue;
      char buf[512];
      snprintf(buf, sizeof buf, "fun=%s var=%s line=%d nsyms=%d",
               es->fun->sym->name ? es->fun->sym->name : "?",
               var->sym->name ? var->sym->name : "_",
               var->sym->ast ? var->sym->ast->line() : -1, (int)syms.size());
      by_site[buf]++;
    }
  }
  for (auto &kv : by_site) fprintf(stderr, "[degen] %s x%d\n", kv.first.c_str(), kv.second);
}

// ifa/issues/074: the structural shape of a type, spelled `list<list<int64>>`.
//
// Reads the DURABLE elem_key (captured at the end of the previous pass),
// never the live element AVar: every container starts out empty and
// acquires its elements later, so a live read would call two containers
// equal merely because neither has filled in yet -- the same trap the
// mode-1 key comments already record. The depth is capped: an unbounded
// structural walk is the very non-termination this is meant to fix, and the
// cap alone bounds it -- a container that holds itself simply reaches
// `<...>` like any other 6-deep nest.

// A sub-shape is emitted as an ID, never as its own text (ifa/issues/130).
//
// Unfolding the whole structure into one flat string is exponential in
// (union width)^(depth) on any type whose members repeat, and the repeats
// are not exotic -- they are ifa/128's over-discrimination arriving here as
// type width. Measured on `kanoodle` under PYC_CSELEM=3: a 93-member union
// at EVERY depth 1..6 (77 of the 93 are CreationSets of one class, `list`),
// 1131 bytes of output per call, 1.13 GB of string at 10^6 calls, on a walk
// needing 93^5 ~ 7e9 calls -- about 8 TB. It does not finish slowly, it does
// not finish: one `pyc` reached 47 GB RSS and the kernel OOM-killer took the
// whole corpus sweep with it, nine times over one afternoon. `plcfrs`,
// `quameon` and `rdb` are the same failure.
//
// Ids are handed out by CONTENT, so two structurally equal sub-shapes always
// get the same id and two different ones never do. The key therefore compares
// exactly as the unfolded string did -- same merges, same splits -- at
// O(width) per level instead of O(width^depth). Content-keyed and not
// pointer-keyed on purpose: the shape deliberately erases CreationSet
// identity (it emits the CLASS, `name#id`), so two distinct CSs of one class
// with equal element shapes must land on one id. That is the whole point of
// the mode, and a pointer-keyed table would miss exactly those.
//
// `cselem_shape_ids` is global and monotone ON PURPOSE, like the
// `cselem_shape_canon` that consumes its output. Clearing it per pass would
// let id 7 name one structure in pass 3 and a different one in pass 5, and
// since the canon map never revisits an entry, that is a permanent mis-merge
// -- the same hazard ifa/130 A2 fixed for `Sym::name`. The (AType, depth)
// memo below is the opposite: it MUST be cleared per pass, because elem_key
// moves.

void report_incompat() {
  if (!getenv("IFA_DBG_INCOMPAT")) return;
  fprintf(stderr,
          "INCOMPAT p=%d arg=%ld ret=%ld retn=%ld | stage1 seen=%ld skip(rval=%ld lval=%ld cs=%ld) dec=%ld "
          "defer=%ld split(formal=%ld return=%ld)\n",
          analysis_pass, census.ic_arg, census.ic_ret, census.ic_retn, census.tc_seen, census.tc_skip_rval, census.tc_skip_lval, census.tc_skip_cs, census.tc_dec, census.tc_defer,
          census.tc_formal, census.tc_return);
  fprintf(stderr, "LEDGER p=%d dup_es=%d dup_cs=%d churn=%d\n", analysis_pass, census.ld_dup_es, census.ld_dup_cs, census.ld_churn);
  census.ic_arg = census.ic_ret = census.ic_retn = census.tc_formal = census.tc_return = 0;
  census.tc_seen = census.tc_skip_rval = census.tc_skip_lval = census.tc_skip_cs = census.tc_dec = census.tc_defer = 0;
}

void report_markwhy() {
  if (!mark_why_enabled()) return;
  fprintf(stderr, "MARKWHY p=%d cs_differ=%ld cs_same=%ld\n", analysis_pass, census.mark_cs_differ, census.mark_cs_same);
  census.mark_cs_differ = census.mark_cs_same = 0;
}

void report_keydrift() {
  if (!getenv("IFA_DBG_KEYDRIFT")) return;
  fprintf(stderr, "KEYDRIFT p=%d stable=%ld grew=%ld shrank=%ld flip=%ld new=%ld", analysis_pass, census.kd_stable, census.kd_grew,
          census.kd_shrank, census.kd_flip, census.kd_new);
  if (census.kd_flip) {
    kd_flip_funs.set_to_vec();  // set_add leaves null holes
    qsort_by_id(kd_flip_funs);
    fprintf(stderr, " flip_funs=");
    for (Fun *f : kd_flip_funs)
      if (f && f->sym) fprintf(stderr, "%s#%d,", f->sym->name ? f->sym->name : "?", f->sym->id);
  }
  fprintf(stderr, "\n");
  census.kd_stable = census.kd_grew = census.kd_shrank = census.kd_new = census.kd_flip = 0;
  kd_flip_funs.clear();
}

void report_canon_stats() {
  if (!canon_enabled() || !getenv("IFA_DBG_CANON")) return;
  fprintf(stderr, "CANON p=%d hit=%ld miss=%ld conflict=%ld conflict_honored=%ld\n", analysis_pass, census.canon_hit,
          census.canon_miss, census.canon_conflict, census.canon_conflict_honored);
  census.canon_hit = census.canon_miss = census.canon_conflict = census.canon_conflict_honored = 0;
}

// ifa/issues/074: how many contours would each NAMING scheme give this
// function, versus how many IFA actually built?
//
//   ess    -- contours IFA built (splitters included)
//   setkey -- distinct tuples of argument type SETS (what PYC_CANON
//             names by, and what entry_set_compatibility compares)
//   cpakey -- distinct tuples of SINGLE CreationSets over all the
//             function's call edges: the cartesian-product naming
//             shedskin's dcpa uses
//
// The question this answers: is mark-based splitting recovering a
// distinction that cartesian-product naming already makes structurally
// (cpakey ~ ess >> setkey), or one that no type-tuple naming can make
// (ess >> cpakey)?

void report_keyspace() {
  if (!getenv("IFA_DBG_KEYSPACE")) return;
  const char *only = getenv("IFA_DBG_KEYSPACE_FUN");
  for (Fun *f : fa->pdb->funs) {
    if (f->ess.n < 2) continue;
    if (only && !(f->sym->name && strstr(f->sym->name, only))) continue;
    std::set<std::string> setkeys, cpakeys;
    Vec<MPosition *> pos;
    for (MPosition *p : f->positional_arg_positions) pos.add(p);
    int budget = 20000;
    for (EntrySet *es : f->ess) if (es) {
      std::string k;
      for (MPosition *p : pos) {
        AVar *a = es->args.get(p);
        k += std::to_string((uintptr_t)(a ? (void *)a->out->type : nullptr)) + "|";
      }
      setkeys.insert(k);
      if (only && getenv("IFA_DBG_KEYSPACE_DUMP")) {
        int nedges = 0;
        for (AEdge *ee : es->edges) if (ee) ++nedges;
        fprintf(stderr, "  [key] es=%d edges=%d filters=%d split=%d", es->id, nedges, es->filters.n,
                es->split ? es->split->id : -1);
        for (MPosition *p : pos) {
          AVar *a = es->args.get(p);
          fprintf(stderr, " |");
          if (a && a->out && a->out->type)
            for (CreationSet *cs : a->out->type->sorted)
              fprintf(stderr, " %s#%d", cs->sym && cs->sym->name ? cs->sym->name : "?", cs->id);
        }
        fprintf(stderr, "\n");
      }
      for (AEdge *e : es->edges) if (e && e->args.n && e->match) {
        std::string tup;
        keyspace_cpa(e, pos, 0, tup, cpakeys, budget);
      }
    }
    fprintf(stderr, "KEYSPACE p=%d fun=%s#%d ess=%d setkey=%d cpakey=%d%s\n", analysis_pass,
            f->sym->name ? f->sym->name : "?", f->sym->id, f->ess.n, (int)setkeys.size(), (int)cpakeys.size(),
            budget <= 0 ? " (cpa truncated)" : "");
  }
}


// TEMP probe: which splitter STAGE is producing the per-pass churn.
// ifa/issues/101: per-pass CreationSet population, grouped by the SYM of
// the allocation site. Tests whether contour growth is feeding CS growth
// -- creation_point mints one CS per (site x contour), so a self-
// amplifying loop would show a handful of syms with CS counts tracking
// the contour count. Probe-only.

// TEMP probe: which splitter STAGE is producing the per-pass churn.
// ifa/issues/101: per-pass CreationSet population, grouped by the SYM of
// the allocation site. Tests whether contour growth is feeding CS growth
// -- creation_point mints one CS per (site x contour), so a self-
// amplifying loop would show a handful of syms with CS counts tracking
// the contour count. Probe-only.
void report_cs_population() {
  if (!getenv("IFA_DBG_CSPOP")) return;
  std::map<std::string, int> by_sym;
  for (CreationSet *cs : fa->css) if (cs)
    by_sym[cs->sym && cs->sym->name ? cs->sym->name : "(anon)"]++;
  std::vector<std::pair<std::string, int>> v(by_sym.begin(), by_sym.end());
  std::sort(v.begin(), v.end(), [](auto &a, auto &b) { return a.second > b.second; });
  fprintf(stderr, "CSPOP p=%d css=%d syms=%d |", analysis_pass, fa->css.n, (int)v.size());
  for (int i = 0; i < 8 && i < (int)v.size(); i++) fprintf(stderr, " %s:%d", v[i].first.c_str(), v[i].second);
  fprintf(stderr, "\n");
}

// ifa/issues/101: for each CONTAINER CreationSet, its ELEMENT type. The
// question this answers: CreationSets exist to carry container
// parameterization (shedskin's `list<T>`), so how many DISTINCT element
// types are there, against how many CreationSets? A large gap means CS
// identity is over-discriminating -- keyed on allocation site x contour
// rather than on the parameter it is supposed to capture.
// ifa/issues/124: dump every EntrySet of a named function with the
// CreationSets its positional formals hold. Answers "should ES
// splitting have split this?" directly -- if one contour's receiver
// formal holds two different container CSs, it should have.

// ifa/issues/101: for each CONTAINER CreationSet, its ELEMENT type. The
// question this answers: CreationSets exist to carry container
// parameterization (shedskin's `list<T>`), so how many DISTINCT element
// types are there, against how many CreationSets? A large gap means CS
// identity is over-discriminating -- keyed on allocation site x contour
// rather than on the parameter it is supposed to capture.
// ifa/issues/124: dump every EntrySet of a named function with the
// CreationSets its positional formals hold. Answers "should ES
// splitting have split this?" directly -- if one contour's receiver
// formal holds two different container CSs, it should have.
void report_fun_contours() {
  cchar *want = getenv("IFA_DBG_FUNCONTOURS");
  if (!want) return;
  for (EntrySet *es : fa->ess) if (es && es->fun && es->fun->sym && es->fun->sym->name) {
    if (strcmp(es->fun->sym->name, want)) continue;
    fprintf(stderr, "FUNC %s es=%d", want, es->id);
    for (MPosition *p : es->fun->positional_arg_positions) {
      AVar *a = es->args.get(p);
      if (!a) continue;
      fprintf(stderr, " | arg%d:", (int)Position2int(p->pos[0]));
      for (CreationSet *c : a->out->sorted)
        fprintf(stderr, " %s#%d", c->sym && c->sym->name ? c->sym->name : "?", c->id);
    }
    fprintf(stderr, "\n");
  }
}

// ifa/issues/129 step 1: the element-shape census, shared by the per-pass
// ELEMTYPE probe and the end-of-analysis DEMAND ratio.
//
// The question it answers: CreationSets exist to carry container
// parameterization (shedskin's `list<T>`), so how many DISTINCT element
// types are there, against how many CreationSets? shedskin's data-contour
// identity IS that element-class tuple (`ifa_class_types` / `classes_nr`,
// audited in ifa/issues/129), so `shapes` is the contour count pyc would
// have if CS identity were demand-driven, and `cs / shapes` is the factor
// by which it over-discriminates today -- ifa/issues/128 measured 95 list
// CSs standing for 6 element types on chess.
//
// It is READ-ONLY; see the note at the AVar read in element_census().
//
// Keyed on the container SYM POINTER, and the element shape on the element
// CreationSets' SYM IDS -- never on names. Two classes can share a name
// (CLAUDE.md), and a name-keyed census silently merges them; names here are
// for display only.

// A container's content lives in TWO channels, and a census that reads only
// one is wrong. make_kind fills `cs->vars` per position and deliberately
// does NOT flow it into the generic element (fa.cc, ifa/issues/104: a
// heterogeneous tuple read by constant indices keeps precise per-field
// types only because its element stays bottom, which is what tuple_able()
// tests for). So `[1,2,3]` and `["x","y"]` BOTH present an empty element
// AVar -- measured -- and an element-only census calls them one shape and
// reports a 2x over-discrimination that is not there.
//
// Hence two denominators, and the honest answer is between them:
//   shapes   content classes MERGED across the element and every position.
//            This is shedskin's identity for a one-tvar container: two
//            int lists of different lengths are ONE contour.
//   pshapes  the element set plus the ORDERED per-position class-sets.
//            This is record identity: arity and field order count.
// A demand-driven splitter lands between `shapes` (list-like) and
// `pshapes` (record-like), so cs/shapes bounds the over-discrimination
// from above and cs/pshapes from below.

void report_element_setters() {
  if (!getenv("IFA_DBG_ELEMSETTER")) return;
  for (CreationSet *cs : fa->css) if (cs && cs->sym && cs->sym->element) {
    if (!cs->added_element_var) continue;  // read-only, see element_census()
    AVar *e = unique_AVar(cs->sym->element->var, cs);
    if (!e) continue;
    fprintf(stderr, "ELEM cs=%d sym=%s elem_av=%d ntypes=%d nback=%d\n", cs->id,
            cs->sym->name ? cs->sym->name : "?", e->id, e->out->type->n, e->backward.n);
    for (AVar *b : e->backward) if (b) {
      EntrySet *bes = b->contour_is_entry_set ? (EntrySet *)b->contour : nullptr;
      cchar *fn = bes && bes->fun && bes->fun->sym && bes->fun->sym->name ? bes->fun->sym->name : "?";
      fprintf(stderr, "   <- av=%d in_fun=%s es=%d container=%d setter_class=%d\n", b->id, fn,
              bes ? bes->id : -1, b->container ? b->container->id : -1, b->setter_class ? 1 : 0);
    }
    for (AVar *d : cs->defs) if (d)
      fprintf(stderr, "   def av=%d setters=%d cs_map=%d\n", d->id, d->setters ? d->setters->n : -1,
              d->cs_map ? 1 : 0);
  }
}

// ifa/issues/101: per container sym, its CS count against the element
// types and element class-shapes those CSs stand for.
// ifa/issues/124: dump every EntrySet of a named function with the
// CreationSets its positional formals hold (report_fun_contours, above).

// ifa/issues/101: per container sym, its CS count against the element
// types and element class-shapes those CSs stand for.
// ifa/issues/124: dump every EntrySet of a named function with the
// CreationSets its positional formals hold (report_fun_contours, above).
void report_element_types() {
  if (!getenv("IFA_DBG_ELEMTYPE")) return;
  ElemCensus c;
  element_census(c);
  report_element_setters();
  if (getenv("IFA_DBG_ELEMTYPE_DUMP")) {
    // Print each container CS with its element type spelled out as the
    // SYMS of the element CreationSets, plus their ids. If the distinct
    // element types collapse to a handful of shapes, the extra
    // discrimination is inner-CS identity, not a real type difference.
    std::map<std::string, int> shape;
    for (CreationSet *cs : fa->css) if (cs && cs->sym && cs->sym->element) {
      if (!cs->added_element_var) continue;  // read-only, see element_census()
      AVar *e = unique_AVar(cs->sym->element->var, cs);
      if (!e) continue;
      std::string byid, bysym;
      for (CreationSet *x : e->out->type->sorted) {
        bysym += std::string(" ") + (x->sym && x->sym->name ? x->sym->name : "?");
        byid += " " + std::to_string(x->id);
      }
      fprintf(stderr, "  [elem] cs=%d %s elem_syms=[%s ] elem_ids=[%s ]\n", cs->id,
              cs->sym->name ? cs->sym->name : "?", bysym.c_str(), byid.c_str());
      shape[bysym]++;
    }
    fprintf(stderr, "  [elem] distinct SYM-shapes: %d\n", (int)shape.size());
    for (auto &kv : shape) fprintf(stderr, "  [elem]   %-40s x%d\n", kv.first.c_str(), kv.second);
  }
  // Sorted by name, then id: std::map over Sym* is POINTER order, which
  // reorders the line run to run and makes two logs undiffable.
  std::vector<Sym *> syms;
  for (auto &kv : c.cs_count) syms.push_back(kv.first);
  std::sort(syms.begin(), syms.end(), [](Sym *a, Sym *b) {
    int r = strcmp(a->name ? a->name : "", b->name ? b->name : "");
    return r ? r < 0 : a->id < b->id;
  });
  fprintf(stderr, "ELEMTYPE p=%d |", analysis_pass);
  for (Sym *s : syms)
    fprintf(stderr, " %s: %d CS / %d elemtypes / %d shapes / %d pshapes;", s->name ? s->name : "(anon)",
            c.cs_count[s], (int)c.types[s].size(), (int)c.shapes[s].size(), (int)c.pshapes[s].size());
  fprintf(stderr, " || empty=%d mixed=%d novar=%d\n", c.n_empty, c.n_mixed, c.n_novar);
}

void report_stage_churn() {
  if (!getenv("IFA_DBG_STAGE")) return;
  fprintf(stderr, "STAGE p=%d", analysis_pass);
  for (int i = 0; i < FA::kNumFAPassStages; i++)
    if (fa->dbg_stage_detach[i] || fa->dbg_stage_mint[i] || fa->dbg_stage_reuse[i] || fa->dbg_stage_csmint[i])
      fprintf(stderr, " %s(det=%ld mint=%ld reuse=%ld csmint=%ld)", kStageName[i], fa->dbg_stage_detach[i],
              fa->dbg_stage_mint[i], fa->dbg_stage_reuse[i], fa->dbg_stage_csmint[i]);
  // NOTE deliberately no violation count here: type_violations is
  // collected by collect_var_type_violations() inside extend_analysis(),
  // which runs AFTER complete_pass, so reading it at this point reports
  // 0 (or a stale tally) rather than this pass's. See the VIOL line
  // emitted at the collection site instead.
  fprintf(stderr, " | ess=%d css=%d\n", fa->ess.n, fa->css.n);
  for (int i = 0; i < FA::kNumFAPassStages; i++)
    fa->dbg_stage_detach[i] = fa->dbg_stage_mint[i] = fa->dbg_stage_reuse[i] = fa->dbg_stage_csmint[i] = 0;
}

// ifa/issues/055: per-pass contour trace. PYC_DBG_CONTOURS=<fun name>
// prints, at the end of every pass, one line per EntrySet of that
// function (its formals' and return's types) and one line per
// CreationSet of the named container, with its element type. That is
// exactly the information needed to see WHERE the ideal monomorphic
// contours fail to be derived: the repro's ideal is two difference
// contours, set<int64> and set<str>, each with its own `r`.
AVar *dbg_element_avar(CreationSet *cs) {
  if (!cs || !cs->sym || !cs->sym->element) return nullptr;
  // Scan rather than get_element_avar(): that one CREATES the AVar and
  // sets added_element_var, which a debug path must not do.
  for (AVar *av : cs->vars)
    if (av && av->var && av->var->sym == cs->sym->element) return av;
  return nullptr;
}

void dbg_atype_str(AType *t, char *buf, int n, int depth) {
  if (n <= 0) return;
  buf[0] = 0;
  if (!t) { snprintf(buf, n, "<null>"); return; }
  if (t == fa->type_world.bottom_type) { snprintf(buf, n, "bottom"); return; }
  int used = 0, count = 0;
  for (CreationSet *cs : t->sorted) {
    if (!cs || !cs->sym) continue;
    if (used >= n - 1) break;
    if (count++) used += snprintf(buf + used, n - used, "|");
    if (used >= n - 1) break;
    Sym *ty = cs->sym->is_constant && cs->sym->type ? cs->sym->type : cs->sym;
    cchar *nm = ty->name ? ty->name : "?";
    used += snprintf(buf + used, n - used, "%s#%d", nm, cs->id);
    if (depth > 0 && used < n - 1) {
      AVar *e = dbg_element_avar(cs);
      if (e) {
        char sub[128];
        dbg_atype_str(e->out, sub, (int)sizeof sub, depth - 1);
        used += snprintf(buf + used, n - used, "<%s>", sub);
      }
    }
  }
  if (!count) snprintf(buf, n, "{}");
}

// ifa/issues/055: bounded backward walk from a CreationSet field, to
// find WHERE a union is formed. Prints each AVar with its contour and
// its out type; the first AVar whose out already holds the whole union
// is the merge point, and the edge below it is where a split would have
// to happen.

// ifa/issues/055: bounded backward walk from a CreationSet field, to
// find WHERE a union is formed. Prints each AVar with its contour and
// its out type; the first AVar whose out already holds the whole union
// is the merge point, and the edge below it is where a split would have
// to happen.
void dbg_backwalk(AVar *av, int depth, int maxdepth, Vec<AVar *> &seen) {
  if (!av || depth > maxdepth || !seen.set_add(av)) return;
  char t[224];
  dbg_atype_str(av->out, t, (int)sizeof t, 0);
  char where[96];
  if (av->contour_is_entry_set) {
    EntrySet *e = (EntrySet *)av->contour;
    snprintf(where, sizeof where, "es%d:%s", e ? e->id : -1,
             e && e->fun && e->fun->sym && e->fun->sym->name ? e->fun->sym->name : "?");
  } else {
    CreationSet *c = (CreationSet *)av->contour;
    snprintf(where, sizeof where, "cs%d:%s", c ? c->id : -1,
             c && c->sym && c->sym->name ? c->sym->name : "?");
  }
  int nback = 0;
  for (AVar *b : av->backward) if (b) ++nback;
  fprintf(stderr, "%*sBACK av=%d %s@%s nback=%d out=%s\n", depth * 2, "", av->id,
          av->var && av->var->sym && av->var->sym->name ? av->var->sym->name : "?", where, nback, t);
  for (AVar *b : av->backward) if (b) dbg_backwalk(b, depth + 1, maxdepth, seen);
}

void dbg_dump_contours(int pass) {
  static cchar *want = nullptr;
  static int checked = 0;
  if (!checked) { want = getenv("PYC_DBG_CONTOURS"); checked = 1; }
  if (!want) return;
  int n_es = 0;
  for (EntrySet *es : fa->ess) {
    if (!es || !es->fun || !es->fun->sym || !es->fun->sym->name) continue;
    if (strcmp(es->fun->sym->name, want)) continue;
    ++n_es;
    char args[768];
    args[0] = 0;
    int used = 0;
    for (int i = 0; i < es->args.n; i++) {
      if (!es->args.v[i].key) continue;
      AVar *av = es->args.v[i].value;
      char t[192];
      dbg_atype_str(av ? av->out : nullptr, t, (int)sizeof t, 1);
      used += snprintf(args + used, (int)sizeof args - used, "%s%s", used ? " " : "", t);
      if (used >= (int)sizeof args - 1) break;
    }
    char ret[192];
    dbg_atype_str(es->rets.n && es->rets.v[0] ? es->rets.v[0]->out : nullptr, ret, (int)sizeof ret, 1);
    // ifa/issues/055: setters on the RETURN AVar -- that is the object
    // AVar that split_css needs as a "setter starter" (setters are
    // registered on x->container, i.e. the object, not on the field).
    AVar *rv = es->rets.n ? es->rets.v[0] : nullptr;
    fprintf(stderr, "CONTOUR pass=%d %s es=%d args=[%s] ret=%s ret_setters=%d ret_class=%d ret_cs_map=%d\n", pass,
            want, es->id, args, ret, (rv && rv->setters) ? rv->setters->set_count() : -1,
            (rv && rv->setter_class) ? rv->setter_class->set_count() : -1, (rv && rv->cs_map) ? 1 : 0);
  }
  // PYC_DBG_CONTOURS=* : which functions own the contour growth.
  if (!strcmp(want, "*")) {
    Vec<Fun *> funs;
    Vec<int> counts;
    for (EntrySet *es : fa->ess) {
      if (!es || !es->fun) continue;
      int at = -1;
      for (int i = 0; i < funs.n; i++) if (funs.v[i] == es->fun) { at = i; break; }
      if (at < 0) { funs.add(es->fun); counts.add(1); }
      else counts.v[at]++;
    }
    for (int round = 0; round < 8; round++) {
      int best = -1;
      for (int i = 0; i < counts.n; i++) if (counts.v[i] > 0 && (best < 0 || counts.v[i] > counts.v[best])) best = i;
      if (best < 0 || counts.v[best] < 2) break;
      cchar *nm = funs.v[best]->sym && funs.v[best]->sym->name ? funs.v[best]->sym->name : "?";
      fprintf(stderr, "  TOPFUN pass=%d %-24s contours=%d\n", pass, nm, counts.v[best]);
      counts.v[best] = 0;
    }
    fprintf(stderr, "CONTOUR pass=%d SUMMARY total_ess=%d total_css=%d\n", pass, fa->ess.n, fa->css.n);
    return;
  }
  // PYC_DBG_CSSYM names the container whose CreationSets to dump
  // (default "set"); its data fields are printed with CS ids.
  static cchar *cssym = nullptr;
  static int cssym_checked = 0;
  if (!cssym_checked) {
    cssym = getenv("PYC_DBG_CSSYM");
    if (!cssym) cssym = "set";
    cssym_checked = 1;
  }
  int n_cs = 0;
  for (CreationSet *cs : fa->css) {
    if (!cs || !cs->sym || !cs->sym->name || strcmp(cs->sym->name, cssym)) continue;
    ++n_cs;
    // pyc's `set` is a Python-level class, so there is no sym->element:
    // the element type lives in the _items field. Dump the CS's member
    // AVars instead.
    char flds[512];
    flds[0] = 0;
    int fu = 0;
    for (AVar *av : cs->vars) {
      if (!av || !av->var || !av->var->sym) continue;
      cchar *fn = av->var->sym->name;
      // Data fields only. The method slots (__and__, add, ...) are also
      // in cs->vars and are long enough to overflow this buffer before
      // reaching `_items`, which is the one that matters.
      if (!fn || fn[0] != '_' || (fn[1] == '_' && fn[2])) continue;
      char t[192];
      dbg_atype_str(av->out, t, (int)sizeof t, 1);
      // ifa/issues/055: also report the setter state, since split_css
      // (the demand-driven CS split) only ever sees AVars that carry
      // setters -- an empty `setters` here means the back-flow never
      // reached this field and the CS can never be split from it.
      fu += snprintf(flds + fu, (int)sizeof flds - fu, "%s%s=%s[setters=%d class=%d]", fu ? " " : "", fn, t,
                     av->setters ? av->setters->set_count() : -1,
                     av->setter_class ? av->setter_class->set_count() : -1);
      if (fu >= (int)sizeof flds - 1) break;
    }
    // ifa/issues/055: defs is the number of AVars that created this CS.
    // split_css partitions THAT set, and its loop is `while
    // (starter_set.n > 1)` -- so a CS with a single def can never be
    // split, however the setters partition.
    fprintf(stderr, "  %sCS pass=%d cs=%d defs=%d %s\n", cssym, pass, cs->id, cs->defs.set_count(), flds);
  }
  // ifa/issues/113 link: are the `_items` AVars of distinct set CSs in
  // ONE setter equivalence class? If so their values are merged by the
  // partition, which is what would make three separate sets appear to
  // share all three backing lists.
  {
    Vec<void *> classes;
    int n_items = 0;
    for (CreationSet *cs : fa->css) {
      if (!cs || !cs->sym || !cs->sym->name || strcmp(cs->sym->name, cssym)) continue;
      for (AVar *av : cs->vars) {
        if (!av || !av->var || !av->var->sym || !av->var->sym->name) continue;
        if (strcmp(av->var->sym->name, "_items") && strcmp(av->var->sym->name, "_keys")) continue;
        ++n_items;
        char ft[256];
        dbg_atype_str(av->out, ft, (int)sizeof ft, 0);
        fprintf(stderr, "  ITEMS pass=%d setcs=%d av=%d out=%s setter_class=%p\n", pass, cs->id, av->id, ft,
                (void *)av->setter_class);
        // ifa/issues/055 next step: WHO contributes each CreationSet.
        // Walk the incoming flow edges and name each source AVar by its
        // Var, its contour, and what it carries.
        for (AVar *src : av->backward) {
          if (!src) continue;
          char st[192];
          dbg_atype_str(src->out, st, (int)sizeof st, 0);
          cchar *vn = src->var && src->var->sym && src->var->sym->name ? src->var->sym->name : "?";
          char where[96];
          if (src->contour_is_entry_set) {
            EntrySet *ses = (EntrySet *)src->contour;
            snprintf(where, sizeof where, "es%d:%s", ses ? ses->id : -1,
                     ses && ses->fun && ses->fun->sym && ses->fun->sym->name ? ses->fun->sym->name : "?");
          } else {
            CreationSet *scs = (CreationSet *)src->contour;
            snprintf(where, sizeof where, "cs%d:%s", scs ? scs->id : -1,
                     scs && scs->sym && scs->sym->name ? scs->sym->name : "?");
          }
          fprintf(stderr, "    <- src av=%d %s@%s line=%d out=%s\n", src->id, vn, where,
                  src->var && src->var->def && src->var->def->code ? src->var->def->code->line() : -1, st);
        }
        if (av->setter_class) classes.set_add((void *)av->setter_class);
        if (getenv("PYC_DBG_BACKWALK")) {
          Vec<AVar *> seen;
          dbg_backwalk(av, 0, atoi(getenv("PYC_DBG_BACKWALK")), seen);
        }
      }
    }
    fprintf(stderr, "  ITEMS pass=%d SUMMARY _items_avars=%d distinct_setter_classes=%d\n", pass, n_items,
            classes.n);
  }
  // ifa/issues/055: how much of the setter machinery is engaged at all.
  int with_setters = 0, with_class = 0, cs_with_multidef = 0;
  foreach_avar([&](AVar *a) {
    if (!a) return;
    if (a->setters) ++with_setters;
    if (a->setter_class) ++with_class;
  });
  for (CreationSet *c : fa->css) if (c && c->defs.set_count() > 1) ++cs_with_multidef;
  fprintf(stderr,
          "CONTOUR pass=%d SUMMARY %s_contours=%d %s_CSs=%d total_ess=%d avars_with_setters=%d "
          "avars_with_setter_class=%d css_with_multiple_defs=%d\n",
          pass, want, n_es, cssym, n_cs, fa->ess.n, with_setters, with_class, cs_with_multidef);
}

// ifa/133: name the container whose element has no representation, and
// the EntrySet its creation point lives in.
//
// The element split could not fire because the CreationSet holding the
// unrepresentable union has ONE creation point -- nothing to partition.
// A single creation point whose element holds both types means the
// CONTOUR that creates it is shared, so the thing that has to come apart
// is that EntrySet, not the CreationSet. This says which one.
// ifa/133: VALIDATION for the forward-closure probe.
//
// The [fwd] measurement inside report_mixed_element_owners reported a
// forward closure of 2 AVars against 153 that carry the CreationSet, and
// that contradicts flow_var_to_var maintaining forward/backward
// symmetrically. Before trusting either half, run the same computation on
// every container CreationSet -- including programs with no mixed element
// at all, where the answer is known by construction.
//
// If the probe is sound, a container created once and used locally should
// show closure ~= claim: every AVar holding the CreationSet is reachable
// forward from the creation point.
void report_forward_closure_all() {
  if (!getenv("IFA_DBG_FWDALL")) return;
  for (CreationSet *cs : fa->css) {
    if (!cs || !cs->sym || !cs->sym->element) continue;
    Vec<AVar *> fseen, fwork;
    for (AVar *d : cs->defs)
      if (d && fseen.set_add(d)) fwork.add(d);
    int seeds = fwork.n, fh = 0, fsteps = 0;
    while (fh < fwork.n && fsteps < 100000) {
      AVar *a = fwork.v[fh++];
      ++fsteps;
      for (AVar *x : a->forward)
        if (x && fseen.set_add(x)) fwork.add(x);
    }
    int claim = 0, claim_unreached = 0;
    foreach_avar([&](AVar *a) {
      if (!a || !a->out || !a->out->type || !a->out->type->set_in(cs)) return;
      ++claim;
      if (!fseen.set_in(a)) ++claim_unreached;
    });
    fprintf(stderr, "FWDALL p=%d cs=%d sym=%s defs=%d seeds=%d closure=%d claim=%d unreached=%d\n", analysis_pass,
            cs->id, cs->sym->name ? cs->sym->name : "?", cs->defs.set_count(), seeds, fsteps, claim, claim_unreached);
  }
}

// ifa/133: attribute element writes to CREATION POINTS by back flow.
//
// A set operation writes into an object; its `container` is the
// CreationSet-typed receiver, NOT an allocation site. Reaching the
// creation points requires a backward walk from that container -- testing
// `container` for membership in `cs->defs` is wrong by construction and
// reported 0 attributions every time.

// ifa/133: attribute element writes to CREATION POINTS by back flow.
//
// A set operation writes into an object; its `container` is the
// CreationSet-typed receiver, NOT an allocation site. Reaching the
// creation points requires a backward walk from that container -- testing
// `container` for membership in `cs->defs` is wrong by construction and
// reported 0 attributions every time.
void report_creation_attribution() {
  // ifa/133 FEASIBILITY PROBE for shedskin's ladder routes 1-3.
  // IFA_DBG_ATTRIB below is NOT backflow_path (infer.py:2031): it walks
  // per-writer, starts unfiltered, and groups nothing. This one adds the
  // two differences that matter -- group the element's backward edges by
  // their canonical AType (shedskin's `assignsets`, keyed by
  // merge_simple_types), and filter every hop on "does this AVar carry
  // this CreationSet" (shedskin's `if t in gx.types[incoming]`), so the
  // walk stays inside the CONTAINER's own flow. Reports, per assign set,
  // how many of cs->defs it reaches.
  if (getenv("IFA_DBG_ATTRIB2")) {
    for (CreationSet *cs : fa->css) {
      if (!cs || !cs->sym || !cs->sym->element || !cs->sym->element->var || !cs->added_element_var) continue;
      if (cs->defs.set_count() < 2) continue;
      AVar *elem = unique_AVar(cs->sym->element->var, cs);
      if (!elem || !elem->out) continue;
      // assignsets: canonical AType -> the containers written through it.
      Vec<AType *> keys;
      Vec<Vec<AVar *> *> groups;
      for (AVar *b : elem->backward) {
        if (!b || !b->container || !b->out || !b->out->type) continue;
        int gi = -1;
        for (int i = 0; i < keys.n; i++) if (keys.v[i] == b->out->type) { gi = i; break; }
        if (gi < 0) { keys.add(b->out->type); groups.add(new Vec<AVar *>); gi = keys.n - 1; }
        groups.v[gi]->set_add(b->container);
      }
      if (!keys.n) continue;
      fprintf(stderr, "ATTRIB2 p=%d cs=%d sym=%s defs=%d assignsets=%d\n", analysis_pass, cs->id,
              cs->sym->name ? cs->sym->name : "?", cs->defs.set_count(), keys.n);
      for (int i = 0; i < keys.n; i++) {
        Vec<AVar *> seen, work;
        for (AVar *t : *groups.v[i]) if (t && seen.set_add(t)) work.add(t);
        int h = 0, steps = 0, reached = 0, roots = 0;
        while (h < work.n && steps < 200000) {
          AVar *a = work.v[h++];
          ++steps;
          if (cs->defs.set_in(a)) ++reached;
          if (!a->backward.n) ++roots;
          for (AVar *x : a->backward)
            if (x && x->out && x->out->type && x->out->type->set_in(cs) && seen.set_add(x)) work.add(x);
        }
        fprintf(stderr, "  set[%d] type=", i);
        for (CreationSet *c : keys.v[i]->sorted)
          if (c && c->sym) fprintf(stderr, " %s", c->sym->name ? c->sym->name : "?");
        fprintf(stderr, "  targets=%d walked=%d roots=%d reached_defs=%d/%d\n", groups.v[i]->n, steps, roots,
                reached, cs->defs.set_count());
        // What ARE the roots, and what are the defs? If the walk terminates
        // somewhere other than the creation points, that difference is the
        // finding, not the reach count.
        for (AVar *a : seen)
          if (a && !a->backward.n) {
            EntrySet *aes = a->contour_is_entry_set ? (EntrySet *)a->contour : nullptr;
            int lv_is_def = (a->lvalue && cs->defs.set_in(a->lvalue)) ? 1 : 0;
            int fwd_is_def = 0;
            for (AVar *f : a->forward) if (f && cs->defs.set_in(f)) fwd_is_def = 1;
            fprintf(stderr, "      root av=%d var=%s fun=%s in_defs=%d lvalue=%d lvalue_is_def=%d fwd_is_def=%d\n",
                    a->id, (a->var && a->var->sym && a->var->sym->name) ? a->var->sym->name : "?",
                    (aes && aes->fun && aes->fun->sym && aes->fun->sym->name) ? aes->fun->sym->name : "(cs)",
                    cs->defs.set_in(a) ? 1 : 0, a->lvalue ? a->lvalue->id : -1, lv_is_def, fwd_is_def);
            // The re-point handle question: split_css and split_css_by_defs
            // both act by `v->cs_map->put(sym, new_cs)`, so a node with no
            // cs_map cannot be re-pointed. Measured on ifa/133's reproducer:
            // the walk's roots have NO cs_map, are not in cs->defs, and are
            // neither the lvalue nor a forward neighbour of a def. That gap
            // is what a real routes-1/3 implementation has to bridge.
            // Is this an ABSOLUTE root (no backward edges at all) or only a
            // root within the filtered subgraph (edges exist but none carry
            // this CreationSet)? The two have very different consequences:
            // an absolute root cannot be in any def's forward closure, so a
            // forward-closure bridge from defs to roots is impossible for it.
            int bw_all = a->backward.n, bw_carrying = 0, is_seed = groups.v[i]->set_in(a) ? 1 : 0;
            for (AVar *x : a->backward)
              if (x && x->out && x->out->type && x->out->type->set_in(cs)) ++bw_carrying;
            fprintf(stderr,
                    "           csmap=%d seed=%d backward_all=%d backward_carrying_cs=%d carries_cs=%d global=%d\n",
                    a->cs_map ? 1 : 0, is_seed, bw_all, bw_carrying,
                    (a->out && a->out->type && a->out->type->set_in(cs)) ? 1 : 0,
                    (a->contour == GLOBAL_CONTOUR) ? 1 : 0);
          }
        if (i == 0)
          for (AVar *d : cs->defs)
            if (d) {
              EntrySet *des = d->contour_is_entry_set ? (EntrySet *)d->contour : nullptr;
              fprintf(stderr, "      DEF  av=%d var=%s fun=%s backward=%d\n", d->id,
                      (d->var && d->var->sym && d->var->sym->name) ? d->var->sym->name : "?",
                      (des && des->fun && des->fun->sym && des->fun->sym->name) ? des->fun->sym->name : "(cs)",
                      d->backward.n);
            }
      }
    }
  }
  if (!getenv("IFA_DBG_ATTRIB")) return;
  for (CreationSet *cs : fa->css) {
    if (!cs || !cs->sym || !cs->sym->element || !cs->sym->element->var || !cs->added_element_var) continue;
    if (cs->defs.set_count() < 2) continue;
    AVar *elem = unique_AVar(cs->sym->element->var, cs);
    if (!elem || !elem->out) continue;
    fprintf(stderr, "ATTRIB p=%d cs=%d defs=%d writers=%d\n", analysis_pass, cs->id, cs->defs.set_count(),
            elem->backward.n);
    for (AVar *b : elem->backward) {
      if (!b || !b->container || !b->out || !b->out->type) continue;
      Vec<AVar *> seen, work;
      seen.set_add(b->container);
      work.add(b->container);
      int h = 0, reached_defs = 0, steps = 0;
      while (h < work.n && steps < 50000) {
        AVar *a = work.v[h++];
        ++steps;
        if (cs->defs.set_in(a)) ++reached_defs;
        for (AVar *x : a->backward)
          if (x && seen.set_add(x)) work.add(x);
      }
      fprintf(stderr, "  writer type=");
      for (CreationSet *c : b->out->type->sorted)
        if (c && c->sym) fprintf(stderr, " %s", c->sym->name ? c->sym->name : "?");
      fprintf(stderr, "  backflow_steps=%d reached_creation_points=%d\n", steps, reached_defs);
    }
  }
}

void report_mixed_element_owners() {
  if (!getenv("IFA_DBG_MIXELEM")) return;
  for (CreationSet *cs : fa->css) {
    if (!cs || !cs->sym || !cs->sym->element || !cs->sym->element->var || !cs->added_element_var) continue;
    AVar *elem = unique_AVar(cs->sym->element->var, cs);
    if (!elem || !elem->out || !mixed_basics(elem)) continue;
    int fwd = 0, fwd_setters = 0;
    for (AVar *x : elem->forward)
      if (x) {
        ++fwd;
        if (x->setters) ++fwd_setters;
      }
    fprintf(stderr, "MIXELEM p=%d cs=%d sym=%s defs=%d elem_setters=%d fwd=%d fwd_with_setters=%d elem=",
            analysis_pass, cs->id, cs->sym->name ? cs->sym->name : "?", cs->defs.set_count(),
            elem->setters ? elem->setters->n : -1, fwd, fwd_setters);
    for (CreationSet *c : elem->out->type->sorted)
      if (c && c->sym) fprintf(stderr, " %s#%d", c->sym->name ? c->sym->name : "?", c->id);
    fprintf(stderr, "\n");
    for (AVar *d : cs->defs) {
      if (!d) continue;
      EntrySet *des = d->contour_is_entry_set ? (EntrySet *)d->contour : nullptr;
      fprintf(stderr, "  def var=%s es=%d fun=%s edges=%d backward=%d\n",
              (d->var && d->var->sym && d->var->sym->name) ? d->var->sym->name : "?", des ? des->id : -1,
              (des && des->fun && des->fun->sym && des->fun->sym->name) ? des->fun->sym->name : "(non-ES contour)",
              des ? des->edges.n : -1, d->backward.n);
    }
    // ifa/133: the CONTAINER path. The value path is already split -- the
    // writers are monomorphic. What nothing walks is the receiver: from
    // each writer's container AVar, backward along the flow, to the
    // creation point that made this CreationSet. Those are the EntrySets
    // that must split so the creation point duplicates and the CS can
    // split by creation point.
    if (getenv("IFA_DBG_CPATH")) {
      Vec<AVar *> conts;
      for (AVar *b : elem->backward)
        if (b && b->container) conts.set_add(b->container);
      conts.set_to_vec();
      fprintf(stderr, "  [cpath] %d distinct container AVars\n", conts.n);
      // ifa/133: walk FORWARD from the creation point. Which AVars
      // legitimately receive this CreationSet? Anything that carries it in
      // its type but is not in this closure got it without a flow edge.
      {
        Vec<AVar *> fseen, fwork;
        for (AVar *d : cs->defs)
          if (d && fseen.set_add(d)) fwork.add(d);
        int fh = 0, fsteps = 0;
        while (fh < fwork.n && fsteps < 20000) {
          AVar *a = fwork.v[fh++];
          ++fsteps;
          for (AVar *x : a->forward)
            if (x && fseen.set_add(x)) fwork.add(x);
        }
        int claim = 0, claim_unreached = 0;
        foreach_avar([&](AVar *a) {
          if (!a || !a->out || !a->out->type) return;
          if (!a->out->type->set_in(cs)) return;
          ++claim;
          if (!fseen.set_in(a)) ++claim_unreached;
        });
        fprintf(stderr, "  [fwd] cs=%d forward_closure=%d claim_cs=%d claim_NOT_reached=%d conts_reached=", cs->id,
                fsteps, claim, claim_unreached);
        for (AVar *c2 : conts)
          if (c2) fprintf(stderr, " av%d:%d", c2->id, fseen.set_in(c2) ? 1 : 0);
        fprintf(stderr, "\n");
      }
      for (AVar *c2 : conts)
        if (c2 && c2->out && c2->out->type) {
          fprintf(stderr, "  [cpath-recv] av=%d receiver spans %d CS:", c2->id, c2->out->type->sorted.n);
          for (CreationSet *x : c2->out->type->sorted)
            if (x) fprintf(stderr, " %s#%d", (x->sym && x->sym->name) ? x->sym->name : "?", x->id);
          fprintf(stderr, "\n");
        }
      for (AVar *c : conts) {
        if (!c) continue;
        // Walk back to the creation point: the AVar whose cs_map names cs.
        Vec<AVar *> seen, work;
        seen.set_add(c);
        work.add(c);
        int head = 0, steps = 0, creators = 0, es_on_path = 0, formals_on_path = 0, in_defs = 0;
        std::set<std::string> funs;
        while (head < work.n && steps < 20000) {
          AVar *a = work.v[head++];
          ++steps;
          if (a->cs_map && a->cs_map->get(cs->sym) == cs) ++creators;
          if (cs->defs.set_in(a)) ++in_defs;
          if (a->contour_is_entry_set) {
            ++es_on_path;
            EntrySet *aes = (EntrySet *)a->contour;
            if (aes && aes->fun && aes->fun->sym && aes->fun->sym->name) funs.insert(aes->fun->sym->name);
            if (a->var && a->var->is_formal) ++formals_on_path;
          }
          for (AVar *x : a->backward)
            if (x && seen.set_add(x)) work.add(x);
        }
        {
          std::string fl;
          int n = 0;
          for (auto &f : funs) {
            if (n++ > 11) { fl += " ..."; break; }
            fl += " " + f;
          }
          fprintf(stderr, "  [cpath-funs] av=%d reached=%d in_defs=%d funs:%s\n", c->id, steps, in_defs, fl.c_str());
          for (AVar *d : cs->defs)
            if (d)
              fprintf(stderr, "  [cpath-def] def av=%d var=%s es=%d fun=%s reached_by_walk=%d fwd=%d back=%d\n",
                      d->id, (d->var && d->var->sym && d->var->sym->name) ? d->var->sym->name : "?",
                      d->contour_is_entry_set ? ((EntrySet *)d->contour)->id : -1,
                      (d->contour_is_entry_set && ((EntrySet *)d->contour)->fun &&
                       ((EntrySet *)d->contour)->fun->sym && ((EntrySet *)d->contour)->fun->sym->name)
                          ? ((EntrySet *)d->contour)->fun->sym->name
                          : "?",
                      seen.set_in(d) ? 1 : 0, d->forward.n, d->backward.n);
        }
        fprintf(stderr, "  [cpath] container av=%d es=%d fun=%s -> steps=%d creators=%d es_on_path=%d formals=%d\n",
                c->id, c->contour_is_entry_set ? ((EntrySet *)c->contour)->id : -1,
                (c->contour_is_entry_set && ((EntrySet *)c->contour)->fun && ((EntrySet *)c->contour)->fun->sym &&
                 ((EntrySet *)c->contour)->fun->sym->name)
                    ? ((EntrySet *)c->contour)->fun->sym->name
                    : "?",
                steps, creators, es_on_path, formals_on_path);
      }
    }
    // Who writes the element, and from which contour? That is the set the
    // ES split would have to separate.
    for (AVar *b : elem->backward) {
      if (!b || !b->out || !b->out->type) continue;
      EntrySet *bes = b->contour_is_entry_set ? (EntrySet *)b->contour : nullptr;
      fprintf(stderr, "  writer var=%s es=%d fun=%s type=", (b->var && b->var->sym && b->var->sym->name) ? b->var->sym->name : "?",
              bes ? bes->id : -1, (bes && bes->fun && bes->fun->sym && bes->fun->sym->name) ? bes->fun->sym->name : "?");
      for (CreationSet *c : b->out->type->sorted)
        if (c && c->sym) fprintf(stderr, " %s", c->sym->name ? c->sym->name : "?");
      fprintf(stderr, "\n");
    }
  }
}

void report_demand_ratio() {
  report_creation_attribution();
  report_forward_closure_all();
  report_mixed_element_owners();
  report_cs_flow_graphs();
  report_fun_entry_sets();
  report_cs_defs();
  report_cs_vars();
  report_elem_confluence();
  report_recv_cardinality();
  if (!getenv("IFA_DBG_DEMAND")) return;
  ElemCensus c;
  element_census(c);
  int shapes = c.total_shapes(), pshapes = c.total_pshapes();
  int unkmint = 0, unkres = 0, unkjoin = 0, unkstill = 0;
  cselem_unknown_mint_census(unkmint, unkres, unkjoin, unkstill);
  // Same question over EVERY CreationSet, not just containers: a gate that
  // is vacuous on containers but not in general would be worth knowing about.
  int multidef_all = 0;
  for (CreationSet *x : fa->css) if (x && x->defs.set_count() > 1) ++multidef_all;
  // ifa/129 step 4: how much of the canon's size is the PER-SITE component?
  // cselem_shape_key composes `v<id>|name#id|shape`, so the canon is keyed
  // per allocation site and the floor on CS count is the number of distinct
  // (site, class, shape) triples rather than of (class, shape) pairs. Strip
  // the leading `v<id>|` and count what is left: `canonsiteless` is the
  // number of contours a site-free key would keep, and canon/canonsiteless
  // is the fragmentation the site component costs. Counterfactual only --
  // nothing here changes a decision.
  int nstrip = 0, nmulti = 0, nsame = 0;
  constant_strip_census(nstrip, nmulti, nsame);
  int canon = (int)cselem_shape_canon.size(), canon_siteless = 0;
  if (cssiteless_enabled()) {
    canon_siteless = canon;  // the key already is site-free; nothing to strip
  } else {
    std::set<std::string> sl;
    for (auto &kv : cselem_shape_canon) {
      size_t bar = kv.first.find('|');
      sl.insert(bar == std::string::npos ? kv.first : kv.first.substr(bar + 1));
    }
    canon_siteless = (int)sl.size();
  }
  fprintf(stderr,
          "DEMAND passes=%d ess=%d css=%d container_cs=%d shapes=%d pshapes=%d ratio=%.2f pratio=%.2f "
          "elemtypes=%d empty=%d mixed=%d novar=%d unkmint=%d unkres=%d unkjoin=%d unkstill=%d "
          "multidef=%d multidefall=%d rejoin=%d mintwhy=%d/%d/%d/%d canon=%d canonsiteless=%d resplit=%d/%d cstrip=%d/%d/%d capstrip=%ld\n",
          analysis_pass, fa->ess.n, fa->css.n, c.n_cs, shapes, pshapes,
          shapes ? (double)c.n_cs / shapes : 0.0, pshapes ? (double)c.n_cs / pshapes : 0.0, c.total_types(),
          c.n_empty, c.n_mixed, c.n_novar, unkmint, unkres, unkjoin, unkstill, c.n_multidef, multidef_all,
          census.cselem_rejoins, census.cselem_mint_why[kMintNoSiteCS], census.cselem_mint_why[kMintMoldSplitChild],
          census.cselem_mint_why[kMintMoldCMC], census.cselem_mint_why[kMintMoldIneligible], canon, canon_siteless, census.cselem_resplits,
          census.cselem_resplit_mints, nstrip, nmulti, nsame, census.fa_cap_strips);
  if (getenv("PYC_ELEMSETTER") && (census.es_added + census.es_seeded))
    fprintf(stderr, "ELEMSETTER demand_added=%d starters_seeded=%d\n", census.es_added, census.es_seeded);
  // ifa/133: IFA_DBG_CSDUMP=<csid>, or -1 for every list CS -- dump the
  // and every backward writer, with the writer's function and contribution.
  if (cchar *cw = getenv("IFA_DBG_CSDUMP")) {
    int want = atoi(cw);
    for (CreationSet *cs : fa->css) {
      if (!cs || !cs->sym) continue;
      if (want >= 0 && cs->id != want) continue;
      if (want < 0 && (!cs->sym->name || strcmp(cs->sym->name, "list"))) continue;
      if (want < 0) {
        fprintf(stderr, "[cs] id=%d sym=%s defs=%d vars=%d elem_var=%d", cs->id, cs->sym->name, cs->defs.set_count(),
                cs->vars.n, cs->added_element_var ? 1 : 0);
        for (int vi = 0; vi < cs->vars.n; vi++) {
          fprintf(stderr, "  var[%d]=", vi);
          AVar *pv = cs->vars.v[vi];
          if (pv && pv->out && pv->out->type)
            for (CreationSet *c : pv->out->type->sorted) if (c && c->sym)
              fprintf(stderr, "%s#%d ", c->sym->name ? c->sym->name : "?", c->id);
        }
        for (AVar *d : cs->defs) if (d)
          fprintf(stderr, " | def in=%s",
                  (d->contour_is_entry_set && ((EntrySet *)d->contour)->fun &&
                   ((EntrySet *)d->contour)->fun->sym && ((EntrySet *)d->contour)->fun->sym->name)
                      ? ((EntrySet *)d->contour)->fun->sym->name : "(cs)");
        fprintf(stderr, "\n");
        continue;
      }
      if (!cs->sym->element || !cs->sym->element->var || !cs->added_element_var) continue;
      AVar *e = unique_AVar(cs->sym->element->var, cs);
      fprintf(stderr, "[cselem] cs=%d sym=%s defs=%d elem=", cs->id,
              cs->sym->name ? cs->sym->name : "?", cs->defs.set_count());
      if (e && e->out && e->out->type)
        for (CreationSet *c : e->out->type->sorted) if (c && c->sym)
          fprintf(stderr, " %s#%d", c->sym->name ? c->sym->name : "?", c->id);
      fprintf(stderr, "\n");
      for (AVar *d : cs->defs) if (d)
        fprintf(stderr, "    DEF av=%d in=%s\n", d->id,
                (d->contour_is_entry_set && ((EntrySet *)d->contour)->fun &&
                 ((EntrySet *)d->contour)->fun->sym && ((EntrySet *)d->contour)->fun->sym->name)
                    ? ((EntrySet *)d->contour)->fun->sym->name : "(cs)");
      if (e) for (AVar *b : e->backward) if (b) {
        fprintf(stderr, "    <- av=%-6d var=%-12s in=%-20s gives:", b->id,
                (b->var && b->var->sym && b->var->sym->name) ? b->var->sym->name : "(anon)",
                (b->contour_is_entry_set && ((EntrySet *)b->contour)->fun &&
                 ((EntrySet *)b->contour)->fun->sym && ((EntrySet *)b->contour)->fun->sym->name)
                    ? ((EntrySet *)b->contour)->fun->sym->name : "(cs)");
        if (b->out && b->out->type)
          for (CreationSet *c : b->out->type->sorted) if (c && c->sym)
            fprintf(stderr, " %s#%d", c->sym->name ? c->sym->name : "?", c->id);
        fprintf(stderr, "\n");
      }
    }
  }
  if (getenv("IFA_DBG_AV")) {
    for (EntrySet *es : fa->ess)
      for (Var *v : es->fun->fa_all_Vars) dbg_dump_av(make_AVar(v, es));
    for (CreationSet *cs : fa->css) for (AVar *a : cs->vars) dbg_dump_av(a);
  }
  if (getenv("IFA_DBG_ESPERFUN")) {
    // ifa/133: "the first pass should be one ES per function" -- is it?
    Map<Fun *, int> per;
    for (EntrySet *x : fa->ess) if (x && x->fun) per.put(x->fun, per.get(x->fun) + 1);
    int funs = 0, multi = 0, mx = 0; Fun *worst = nullptr;
    form_Map(MapElemFunPint, e, per) {
      ++funs;
      if (e->value > 1) ++multi;
      if (e->value > mx) { mx = e->value; worst = e->key; }
    }
    if (census.cd_kept + census.cd_dropped) fprintf(stderr, "CONFDEMAND kept=%d dropped=%d prospective_differs=%d no_trigger=%d\n", census.cd_kept, census.cd_dropped, census.cd_prospective_differs, census.cd_no_trigger);
  if (census.ck_single + census.ck_irrep + census.ck_samesym + census.ck_repr)
    fprintf(stderr, "CONFKIND single=%d IRREPRESENTABLE=%d same_sym=%d representable_union=%d\n", census.ck_single, census.ck_irrep, census.ck_samesym, census.ck_repr);
  if (census.grp_total) fprintf(stderr, "GROUPSPLIT total=%d scattered=%d\n", census.grp_total, census.grp_scattered);
  fprintf(stderr, "ESPERFUN pass=%d ess=%d funs=%d funs_with_multiple=%d max=%d worst=%s\n", analysis_pass,
            fa->ess.n, funs, multi, mx,
            (worst && worst->sym && worst->sym->name) ? worst->sym->name : "?");
  }
  if (getenv("IFA_DBG_REDERIVE"))
    fprintf(stderr, "[rederive] TOTAL applies-reporting-split=%ld of which created NO EntrySet=%ld\n",
            census.aes_apply_split, census.aes_apply_split_nogrowth);
  if (getenv("IFA_DBG_CSROUTES")) {
    fprintf(stderr, "CSROUTES");
    for (int i = 0; i < kR_count; i++) fprintf(stderr, " %s=%d", cs_route_name[i], census.cs_route_count[i]);
    fprintf(stderr, " census.esl_hit=%d census.esl_walk=%d census.mint_in_child=%d mint_child_noparentbinding=%d census.mint_child_cmc=%d census.esl_reached=%d census.esl_decline=%d\n", census.esl_hit, census.esl_walk, census.mint_in_child, census.mint_child_novar, census.mint_child_cmc, census.esl_reached, census.esl_decline);
  }
}
