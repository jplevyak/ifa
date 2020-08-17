/* -*-Mode: c++;-*-
   Copyright (c) 2004-2008 John Plevyak, All Rights Reserved
*/
#include "ifadefs.h"
#include "fail.h"
#include "fun.h"
#include "if1.h"
#include "pdb.h"
#include "pnode.h"

typedef void code_fn_t(Code *);

static void resolve_labels(Code *code);
static void finalize_cfg(Fun *f);
static void build_pn_cfg(IF1 *if1, Code *code, Code *cont, Code *conc_cont);

// build CFG
void Fun::build_cfg() {
  if (ifa_verbose > 2) if1_dump(stdout, sym->code);
  if (!sym || !sym->code || (sym->code->is_group() && !sym->code->sub.n)) return;
  resolve_labels(sym->code);
  build_pn_cfg(pdb->if1, sym->code, NULL, NULL);
  entry = sym->code->pn;
  exit = sym->code->sub[sym->code->sub.n - 1]->pn;
  finalize_cfg(this);
}

static void remove_unreachable(Fun *f, Vec<PNode *> &nodes) {
  Accum<PNode *> v;
  if (!f->entry) return;
  v.add(f->entry);
  for (int i = 0; i < v.asvec.n; i++) forv_PNode(p, v.asvec[i]->cfg_succ) if (p) v.add(p);
  Vec<PNode *> unreachable;
  forv_PNode(n, nodes) if (!v.asset.set_in(n)) unreachable.add(n);
  forv_PNode(n, unreachable) forv_PNode(s, n->cfg_succ) s->cfg_pred.remove(n);
}

static void finalize_cfg(Fun *f) {
  Vec<PNode *> nodes;
  f->collect_PNodes(nodes);
  remove_unreachable(f, nodes);
  if (ifa_verbose > 2) printf("%d cfg nodes\n", nodes.n);
  forv_PNode(p, nodes) {
    p->cfg_pred.set_to_vec();
#ifdef CONC_IMPLEMENTED
    p->conc_pred.set_to_vec();
#endif
  }
}

static void resolve_labels(Code *code) {
  if (code->kind == Code_LABEL) code->label[0]->code = code;
  forv_Code(c, code->sub) resolve_labels(c);
}

static inline PNode *build_PNode(Code *code) {
  PNode *to = code->pn ? code->pn : (code->pn = new PNode(code));
  return to;
}

static Code *get_cont(Code *code, Code *cont) {
  if (code->cont) return code->cont;
  if (!code->is_group()) return code->cont = code;
  return code->cont = get_cont(code->sub[0], cont);
}

static void build_pn_cfg(IF1 *if1, Code *code, Code *cont, Code *conc_cont) {
  PNode *pn = NULL;
  if (!code->is_group()) {
    pn = code->pn;
    if (!pn) pn = code->pn = new PNode(code);

#ifdef CONC_IMPLEMENTED
    get_conservative_conc_dependence_succ(pn->conc_succ, code, data_cont);
    forv_PNode(x, pn->conc_succ) x->conc_pred.set_add(pn);
#else
    (void)conc_cont;
#endif

    switch (pn->code->kind) {
      case Code_IF:
        pn->cfg_succ.add(build_PNode(code->label[0]->code));
        pn->cfg_succ.add(build_PNode(code->label[1]->code));
        break;
      case Code_GOTO:
        pn->cfg_succ.add(build_PNode(code->label[0]->code));
        break;
      default:
        if (cont) pn->cfg_succ.add(build_PNode(cont));
        break;
    }
    forv_PNode(p, pn->cfg_succ) p->cfg_pred.set_add(pn);
  } else {
#ifdef CONC_IMPLEMENTED
    Code *new_conc_cont = get_conc_cont(code, conc_cont);
#else
    Code *new_conc_cont = NULL;
#endif
    for (Code **cc = code->sub.v; cc < code->sub.end(); cc++) {
      Code *new_cont = (cc + 1 < code->sub.end()) ? get_cont(cc[1], cont) : cont;
      build_pn_cfg(if1, *cc, new_cont, new_conc_cont);
    }
    if (code->sub.n) code->pn = code->sub[0]->pn;
  }
}
