/* -*-Mode: c++;-*-
   Copyright (c) 2004-2013 John Plevyak, All Rights Reserved
*/
#include "ast.h"
#include "builtin.h"
#include "dom.h"
#include "fa.h"
#include "fail.h"
#include "fun.h"
#include "if1.h"
#include "ifa.h"
#include "ifadefs.h"
#include "pattern.h"
#include "pdb.h"
#include "pnode.h"
#include "prim.h"
#include "var.h"

/*
  It is intended that this file contain 2 implementations,
  one based on the AVars and another based on Vars (post concretize_program()).
  Only the former is currently extent.
*/

static int mark_live_again = 0;

static void print_dead(FA *fa) {
  int ndead_pnodes = 0, ndead_vars = 0;
  int nlive_pnodes = 0, nlive_vars = 0;
  char lfn[512];
  strcpy(lfn, fa->fn);
  strcat(lfn, ".dead_log");
  FILE *fp = fopen(lfn, "w");
  fa_dump_types(fa, fp);
  forv_Fun(f, fa->funs) {
    forv_PNode(p, f->fa_all_PNodes) if (!p->live) {
      ndead_pnodes++;
      fprintf(fp, "PNode %d %s:%d DEAD\n", p->id, p->code->filename(), p->code->line());
    }
    else nlive_pnodes++;
    forv_Var(v, f->fa_all_Vars) if (!v->live) {
      ndead_vars++;
      fprintf(fp, "Var %d:%d %s %s:%d DEAD\n", v->sym->id, v->id, v->sym->name ? v->sym->name : "", v->sym->filename(),
              v->sym->line());
    }
    else nlive_vars++;
  }
  fprintf(fp, "%d PNodes DEAD\n", ndead_pnodes);
  fprintf(fp, "%d Vars DEAD\n", ndead_vars);
  fprintf(fp, "%d PNodes LIVE\n", nlive_pnodes);
  fprintf(fp, "%d Vars LIVE\n", nlive_vars);
  fclose(fp);
}

static void mark_live_avar(AVar *av) {
  if (av->var->sym->is_fake) return;
  av->live = 1;
  av->var->live = 1;
  mark_live_again = 1;
  if (av->var->def) av->var->def->live = 1;
  forv_AVar(aav, av->backward) if (aav) {
    if (!aav->live && !constant(aav)) mark_live_avar(aav);
  }
}

static void mark_live_avars(FA *fa) {
  forv_Fun(f, fa->funs) {
    forv_PNode(p, f->fa_all_PNodes) {
      // if a pnode is live, and it is not a function call, it's inputs (rvals)
      // must be live
      if (p->live && !f->calls.get(p)) {
        forv_Var(v, p->rvals) if (!v->constant) {
          form_AVarMapElem(x, v->avars) {
            AVar *av = x->value;
            if (!av->live) mark_live_avar(av);
          }
        }
      }
    }
    forv_Var(v, f->fa_all_Vars) {
      if (!v->constant) {
        form_AVarMapElem(x, v->avars) {
          // if an instance variable is live, then the containing object must be
          // live
          AVar *av = x->value;
          if (!av->live) {
            forv_CreationSet(cs, *av->out) if (cs) {
              forv_AVar(iv, cs->vars) {
                if (iv->live) {
                  mark_live_avar(av);
                  goto Lbreak2;
                }
              }
            }
          }
        Lbreak2:;
        }
      }
    }
  }
}

static bool forward_live(Var *v, int dist = 2) {
  if (dist < 0) return false;
  form_AVarMapElem(x, v->avars) forv_AVar(av, x->value->forward) if (av) {
    if (av->live) return true;
    if (forward_live(av->var, dist - 1)) return true;
  }
  return false;
}

static void mark_live_pnodes(FA *fa) {
  forv_Fun(f, fa->funs) {
    forv_PNode(p, f->fa_all_PNodes) {
      if (!p->live) {
        forv_Var(v, p->lvals) if (v->live) goto Live;
        if (p->code) switch (p->code->kind) {
            default:
              break;
            case Code_LABEL:
              if (p->cfg_pred.n != 1) forv_PNode(x, p->cfg_pred) if (x->live) goto Live;
              break;
            case Code_GOTO:
              if (p->cfg_succ[0]->cfg_pred.n != 1)
                forv_PNode(x, f->fa_all_PNodes)  // potentially expensive
                    if (x->live || x == f->exit) if (p->dom->is_dominated_by(x->dom)) goto Live;
              break;
            case Code_SEND:
              if (p->prim) {
                switch (p->prim->index) {  // handle side effects
                  default:
                    break;
                  case P_prim_reply:
                    forv_Var(v, f->rets) if (v->live) goto Live;
                    break;
                  case P_prim_setter: {
                    if (forward_live(p->tvals.v[0])) goto Live;
                    break;
                  }
                  case P_prim_set_index_object: {
                    if (forward_live(p->tvals.v[0])) goto Live;
                    break;
                  }
                }
              } else {
                Vec<Fun *> *calls = f->calls.get(p);
                if (calls) forv_Fun(x, *calls) if (x->live) goto Live;
              }
              break;
            case Code_IF:
              forv_PNode(x, f->fa_all_PNodes)  // potentially expensive
                  if (x->live) if (x->dom->is_dominated_by(p->dom))
                      forv_PNode(s, p->cfg_succ) if (!x->dom->is_dominated_by(s->dom)) goto Live;
              break;
          }
        continue;  // handle fall through non-live
      Live:
        p->live = 1;
        f->live = 1;
        mark_live_again = 1;
      } else if (!f->live) {
        f->live = 1;
        mark_live_again = 1;
      }
    }
  }
}

static void mark_initial_dead_and_alive(FA *fa, int init = 0) {
  forv_Fun(f, fa->funs) {
    f->live = init;
    forv_Var(v, f->fa_all_Vars) {
      v->constant = constant(v);
      v->live = init;
      for (int i = 0; i < v->avars.n; i++)
        if (v->avars[i].key) v->avars[i].value->live = false;
      if (v->type) {  // mark all instance variables not live
        forv_CreationSet(cs, v->type->creators) if (cs) { forv_AVar(iv, cs->vars) iv->var->live = init; }
      }
    }
    forv_PNode(p, f->fa_all_PNodes) {
      p->live = init;
      if (p->code && p->code->kind == Code_SEND && p->prim && p->prim->index == P_prim_primitive) {
        cchar *name = p->code->rvals[1]->name;
        RegisteredPrim *rp = prim_get(name);
        if (rp && rp->is_visible) p->live = 1;
      }
    }
  }
}

void mark_live_types(FA *fa) {
  forv_CreationSet(cs, fa->css) cs->type->type_live = 0;
  forv_CreationSet(cs, fa->css) if (cs->type && cs->type->creators.n) {
    forv_CreationSet(x, cs->type->creators) {
      forv_AVar(av, x->defs) if (av) if (av->var->live && !cs->type->type_live) cs->type->type_live = 1;
    }
  }
}

int mark_live_code(FA *fa) {
  mark_initial_dead_and_alive(fa);
  do {
    mark_live_again = 0;
    mark_live_avars(fa);
    mark_live_pnodes(fa);
  } while (mark_live_again);
  mark_live_types(fa);
  if (ifa_verbose > 2) print_dead(fa);
  return 0;
}

void mark_live_funs(FA *fa) {
  forv_Fun(f, fa->funs) f->live = 0;
  if1->top->fun->live = 1;
  int changed = 1;
  while (changed) {
    changed = 0;
    forv_Fun(f, fa->funs) {
      if (f->live) {
        forv_PNode(p, f->fa_all_PNodes) if (p->live) {
          Vec<Fun *> *fns = f->calls.get(p);
          if (fns) forv_Fun(x, *fns) if (!x->live) {
              x->live = 1;
              changed = 1;
            }
        }
      }
    }
  }
  Vec<Fun *> funs(fa->funs, Vec<Fun *>::MOVE);
  forv_Fun(f, funs) if (f->live) fa->funs.add(f);
}

/*
  INCOMPLETE VAR BASED VERSION
*/

#if 0
static void 
mark_live_vars(FA *fa) {
  forv_Fun(f, fa->funs) {
    forv_PNode(p, f->fa_all_PNodes) {
      // if a pnode is live, it's inputs (rvals) must be live
      if (p->live) {
        forv_Var(v, p->rvals) {
          if (!v->live) {
            v->live = 1;
            mark_live_again = 1;
          }
        }
    }
    forv_Var(v, f->fa_all_Vars) {
      // if an instance variable is live, then the containing object must be live
      if (!v->live && v->type)
        forv_CreationSet(cs, v->type->creators) if (cs) {
          forv_AVar(iv, cs->vars) {
            if (iv->var->live) {
              v->live = 1;
              mark_live_again = 1;
            }
          }
        }
      }
    }
  }
}

static void
mark_types_live(FA *fa) {
  forv_CreationSet(cs, fa->css)
    if (cs->type && cs->type->creators.n) {
      forv_CreationSet(x, cs->type->creators) {
        forv_AVar(av, x->defs) if (av)
          if (av->var->live)
            cs->type->type_live = 1;
      }
    }
}

static void 
mark_initial_dead_and_alive(FA *fa, int init = 0) {
  forv_Fun(f, fa->funs) {
    f->live = init;
    forv_Var(v, f->fa_all_Vars) {
      v->live = INIT;
      if (v->type) { // mark all instance variables not live
        forv_CreationSet(cs, v->type->creators) if (cs) {
          forv_AVar(iv, cs->vars)
            iv->var->live = init;
        }
      }
    }
    forv_PNode(p, f->fa_all_PNodes) {
      p->live = init;
      if (p->code && p->code->kind == Code_SEND && p->prim && p->prim->index == P_prim_primitive) {
        cchar *name = p->code->rvals[1]->name;
        RegisteredPrim *rp = pdb->fa->primitive_transfer_functions.get(name);
        if (rp && rp->is_visible)
          p->live = 1;
      }
    }
  }
  forv_CreationSet(cs, fa->css)
    cs->type->type_live = init;
}

int 
mark_dead_code(FA *fa) {
  mark_initial_dead_and_alive(fa);
  do {
    mark_live_again = 0;
    mark_live_vars(fa);
    mark_live_pnodes(fa);
  } while (mark_live_again);
  mark_types_live(fa);
  if (ifa_verbose > 2)
    print_dead(fa);
  return 0;
}
#endif
