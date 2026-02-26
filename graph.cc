#include "graph.h"
#include "ast.h"
#include "builtin.h"
#include "dom.h"
#include "fa.h"
#include "fun.h"
#include "if1.h"
#include "ifadefs.h"
#include "log.h"
#include "loop.h"
#include "pattern.h"
#include "pdb.h"
#include "pnode.h"
#include "var.h"

#define G_BOX (1 << 0)
#define G_ELLIPSE (1 << 1)
#define G_TRIANGLE (1 << 2)
#define G_BLUE (1 << 3)
#define G_GREEN (1 << 4)
#define G_RED (1 << 5)
#define G_ORANGE (1 << 6)
#define G_PURPLE (1 << 7)

// options to pnode print
#define G_DOM (1 << 10)
#define G_LOOP (1 << 11)

char graph_fun[80];
char graph_var[80];
int graph_type = VCG;
int fgraph_frequencies = 0;
int fgraph_constants = 0;

static FILE *graph_start(cchar *fn, cchar *tag, cchar *name) {
  char hfn[512];
  strcpy(hfn, fn);
  strcat(hfn, ".");
  strcat(hfn, tag);
  switch (graph_type) {
    case VCG:
      strcat(hfn, ".vcg");
      break;
    case GraphViz:
      strcat(hfn, ".dot");
      break;
    default:
      assert(!"bad case");
  }
  FILE *fp = fopen(hfn, "w");
  switch (graph_type) {
    case VCG:
      fprintf(fp, "graph: {\n");
      fprintf(fp, "\ttitle: \"%s\"\n", name);
      fprintf(fp,
              "\tedge.arrowsize: 15\n"
              "\tedge.thickness: 3\n");
      break;
    case GraphViz:
      fprintf(fp, "digraph G {\n");
      fprintf(fp, "edge [arraowhead=vee];\n");
      break;
    default:
      assert(!"bad case");
  }
  return fp;
}

static void graph_end(FILE *fp) {
  fprintf(fp, "}\n");
  fclose(fp);
}

static void vcg_colors(FILE *fp, int options) {
  if (options & G_BLUE) fprintf(fp, " color: blue");
  if (options & G_GREEN) fprintf(fp, " color: green");
  if (options & G_RED) fprintf(fp, " color: red");
  if (options & G_ORANGE) fprintf(fp, " color: orange");
  if (options & G_PURPLE) fprintf(fp, " color: purple");
}

static void graphviz_colors(FILE *fp, int options) {
  if (options & G_BLUE) fprintf(fp, " color=blue");
  if (options & G_GREEN) fprintf(fp, " color=green");
  if (options & G_RED) fprintf(fp, " color=red");
  if (options & G_ORANGE) fprintf(fp, " color=orange");
  if (options & G_PURPLE) fprintf(fp, " color=purple");
}

void graph_node(FILE *fp, void *id, cchar *label, int options) {
  switch (graph_type) {
    case VCG:
      fprintf(fp, "node: {title:\"%p\" label:\"%s\"", id, label);
      if (options & G_BOX) fprintf(fp, " shape:box");
      if (options & G_ELLIPSE) fprintf(fp, " shape:ellipse");
      if (options & G_TRIANGLE) fprintf(fp, " shape:triangle");
      vcg_colors(fp, options);
      fprintf(fp, "}\n");
      break;
    case GraphViz:
      fprintf(fp, "n%p [label=\"%s\"", id, label);
      if (options & G_BOX) fprintf(fp, " shape=box");
      if (options & G_ELLIPSE) fprintf(fp, " shape=ellipse");
      if (options & G_TRIANGLE) fprintf(fp, " shape=triangle");
      graphviz_colors(fp, options);
      fprintf(fp, "];\n");
      break;
    default:
      assert(!"bad case");
  }
}

void graph_edge(FILE *fp, void *a, void *b, int options) {
  assert(a && b);
  switch (graph_type) {
    case VCG:
      fprintf(fp, "edge: {sourcename:\"%p\" targetname:\"%p\"", a, b);
      vcg_colors(fp, options);
      fprintf(fp, "}\n");
      break;
    case GraphViz:
      fprintf(fp, "n%p -> n%p [", a, b);
      graphviz_colors(fp, options);
      fprintf(fp, "];\n");
      break;
    default:
      assert(!"bad case");
  }
}

static int compar_fun_ids(const void *ai, const void *aj) {
  uint32 i = (*(Fun **)ai)->id;
  uint32 j = (*(Fun **)aj)->id;
  return (i > j) ? 1 : ((i < j) ? -1 : 0);
}

static void graph_ast(Vec<Fun *> &funs, cchar *fn) {
  FILE *fp = graph_start(fn, "ast", "Abstract Syntax Tree");
  for (Fun *f : funs) f->ast->graph(fp);
  graph_end(fp);
}

static void strcat_sym_node(char *s, Sym *sy) {
  if (sy->name)
    strcat(s, sy->name);
  else {
    char id[80];
    snprintf(id, sizeof(id), "_%d", sy->id);
    strcat(s, id);
  }
}

static void graph_pnode_node(FILE *fp, PNode *pn, int options = 0) {
  char title[256] = "";
  if (pn->lvals.n) {
    for (Var *v : pn->lvals) {
      strcat_sym_node(title, v->sym);
      strcat(title, " ");
    }
    strcat(title, "= ");
  }
  int kind = pn->code ? pn->code->kind : Code_MOVE;
  strcat(title, code_string[kind]);
  for (Var *v : pn->rvals) {
    strcat(title, " ");
    strcat_sym_node(title, v->sym);
  }
  if (options & G_DOM) {
    for (int i = 0; i < pn->dom->intervals.n; i += 2) {
      snprintf(title + strlen(title), sizeof(title) - strlen(title), "[%d %d]", pn->dom->intervals[i],
               pn->dom->intervals.v[i + 1]);
    }
  }
  if (options & G_LOOP) {
    snprintf(title + strlen(title), sizeof(title) - strlen(title), "C(%d %d)", pn->loop_node->pre_dfs,
             pn->loop_node->post_dfs);
    snprintf(title + strlen(title), sizeof(title) - strlen(title), "D(%d %d)", pn->loop_node->pre_dom,
             pn->loop_node->post_dom);
  }
  if (fgraph_frequencies)
    snprintf(title + strlen(title), sizeof(title) - strlen(title), "freq(%f)", pn->execution_frequency);
  graph_node(fp, pn, title);
}

static void graph_loop_node(FILE *fp, LoopNode *n) {
  char title[256] = "";
  snprintf(title, sizeof(title), "%d-%d", n->pre_dom, n->post_dom);
  graph_node(fp, n, title);
}

static void graph_pnode_cfg_edges(FILE *fp, PNode *pn) { for (PNode *ppn : pn->cfg_succ) graph_edge(fp, pn, ppn); }

static void graph_loop_edges(FILE *fp, LoopNode *n) {
  for (LoopNode *nn : n->children) graph_edge(fp, n->node ? n->node : n, nn->node ? nn->node : nn, G_BLUE);
  for (LoopNode *nn : n->loops) graph_edge(fp, n->node ? n->node : n, nn->node ? nn->node : nn, G_RED);
}

static void graph_pnode_dom_edges(FILE *fp, PNode *pn) {
  for (int i = 0; i < pn->dom->children.n; i++) graph_edge(fp, pn, pn->dom->children[i]->node, G_BLUE);
}

static void graph_phi_phy_edges(FILE *fp, PNode *pn) {
  for (PNode *ppn : pn->phi) graph_edge(fp, pn, ppn, G_RED);
  for (PNode *ppn : pn->phy) graph_edge(fp, pn, ppn, G_RED);
}

static void graph_cfg(Vec<Fun *> &funs, cchar *fn) {
  FILE *fp = graph_start(fn, "cfg", "Control Flow Graph");
  for (Fun *f : funs) {
    Vec<PNode *> pnodes;
    f->collect_PNodes(pnodes);
    for (PNode *p : pnodes) graph_pnode_node(fp, p);
    for (PNode *p : pnodes) graph_pnode_cfg_edges(fp, p);
  }
  graph_end(fp);
}

static void graph_dom(Vec<Fun *> &funs, cchar *fn) {
  FILE *fp = graph_start(fn, "dom", "Dominators Graph");
  for (Fun *f : funs) {
    Vec<PNode *> pnodes;
    f->collect_PNodes(pnodes);
    for (PNode *p : pnodes) graph_pnode_node(fp, p, G_DOM);
    for (PNode *p : pnodes) {
      graph_pnode_cfg_edges(fp, p);
      graph_pnode_dom_edges(fp, p);
    }
  }
  graph_end(fp);
}

static void graph_loops(Vec<Fun *> &funs, cchar *fn) {
  FILE *fp = graph_start(fn, "loops", "Loop Nests Graph");
  for (Fun *f : funs) if (f->loops->loops) {
    for (LoopNode *p : f->loops->nodes) if (p->node) graph_pnode_node(fp, (PNode *)p->node, G_LOOP);
    else graph_loop_node(fp, p);
    for (LoopNode *p : f->loops->nodes) {
      if (p->node) graph_pnode_cfg_edges(fp, (PNode *)p->node);
      graph_loop_edges(fp, p);
    }
  }
  graph_end(fp);
}

static void graph_var_node(FILE *fp, Var *v, int options = 0) {
  char id[80] = "";
  strcat_sym_node(id, v->sym);
  if (fgraph_constants) {
    Vec<Sym *> consts;
    if (constant_info(v, consts)) {
      strcat(id, " {");
      for (Sym *s : consts) {
        strcat(id, " ");
        sprint_imm(id + strlen(id), sizeof(id) - strlen(id), s->imm);
      }
      strcat(id, " }");
    }
  }
  graph_node(fp, v, id, options | G_BLUE);
}

static int graph_it(Var *v) {
  if (graph_var[0])
    if (!v->sym->name || strcmp(v->sym->name, graph_var)) return 0;
  if (!fgraph_constants && (v->sym->is_constant || v->sym->is_symbol)) return 0;
  return 1;
}

static void graph_pnode_var_edges(FILE *fp, PNode *pn) {
  for (Var *v : pn->lvals) if (graph_it(v)) graph_edge(fp, pn, v, G_BLUE);
  for (Var *v : pn->rvals) if (graph_it(v)) graph_edge(fp, v, pn, G_BLUE);
  if (!pn->code || pn->code->kind == Code_MOVE) {
    for (Var *a : pn->rvals) for (Var *b : pn->lvals) if (graph_it(a) && graph_it(b)) graph_edge(fp, a, b, G_GREEN);
  }
}

static void graph_ssu(Vec<Fun *> &funs, cchar *fn) {
  FILE *fp = graph_start(fn, "ssu", "Single-Static Use");
  Vec<Var *> vdone;
  for (Fun *f : funs) {
    Vec<PNode *> pnodes;
    Vec<Var *> vars;
    f->collect_Vars(vars, &pnodes);
    for (PNode *p : pnodes) {
      pnodes.append(p->phi);
      pnodes.append(p->phy);
    }
    for (PNode *p : pnodes) graph_pnode_node(fp, p);
    for (Var *v : vars) {
      if (graph_it(v))
        if (vdone.set_add(v)) graph_var_node(fp, v);
    }
    for (PNode *p : pnodes) {
      graph_pnode_var_edges(fp, p);
      graph_pnode_cfg_edges(fp, p);
      graph_phi_phy_edges(fp, p);
    }
  }
  graph_end(fp);
}

static void graph_avar_node(FILE *fp, AVar *av) {
  char label[80];
  snprintf(label, sizeof(label), "%s_%d", av->var->sym->name ? av->var->sym->name : "", av->var->sym->id);
  Vec<Sym *> consts;
  for (CreationSet *cs : *av->out) if (cs) {
    if (cs->sym->constant)
      consts.set_add(cs->sym);
    else {
      consts.clear();
      break;
    }
  }
  consts.set_to_vec();
  if (consts.n) {
    strcat(label, " {");
    for (Sym *s : consts) {
      strcat(label, " ");
      sprint_imm(label + strlen(label), sizeof(label) - strlen(label), s->imm);
    }
    strcat(label, " }");
  }
  graph_node(fp, av, label, av->var->sym->constant ? G_BOX : 0);
}

static void graph_avars(FA *fa, cchar *fn) {
  FILE *fp = graph_start(fn, "avars", "Analysis Variables");
  Vec<AVar *> todo, todo_set;
  for (EntrySet *es : fa->ess) {
    for (Var *v : es->fun->fa_all_Vars) {
      AVar *av = make_AVar(v, es);
      todo_set.set_add(av);
    }
  }
  todo.copy(todo_set);
  todo.set_to_vec();
  for (AVar *av : todo) {
    for (AVar *avv : av->forward) if (avv) if (todo_set.set_add(avv)) todo.add(avv);
    for (AVar *avv : av->backward) if (avv) if (todo_set.set_add(avv)) todo.add(avv);
  }
  for (AVar *av : todo) {
    if (!av->forward.n && !av->backward.n) continue;
    graph_avar_node(fp, av);
  }
  for (AVar *av : todo) if (av) { for (AVar *avv : av->forward) if (avv) graph_edge(fp, av, avv); }
  graph_end(fp);
}

static void graph_es_node(FILE *fp, EntrySet *es) {
  char label[80];
  snprintf(label, sizeof(label), "%d:%s_%d", es->id, es->fun->sym->name ? es->fun->sym->name : "", es->fun->sym->id);
  graph_node(fp, es, label, G_BLUE | G_BOX);
}

static void graph_cs_node(FILE *fp, CreationSet *cs) {
  char label[80];
  if (cs->sym->is_constant)
    sprint_imm(label, sizeof(label), cs->sym->imm);
  else
    snprintf(label, sizeof(label), "%d:%s_%d", cs->id,
             cs->sym->name ? cs->sym->name : (cs->sym->constant ? cs->sym->constant : ""), cs->sym->id);
  graph_node(fp, cs, label, G_RED | G_ELLIPSE);
  for (AVar *ivar : cs->vars) {
    snprintf(label, sizeof(label), "%s_%d", ivar->var->sym->name ? ivar->var->sym->name : "", ivar->var->sym->id);
    graph_node(fp, ivar, label, G_ORANGE | G_TRIANGLE);
    graph_edge(fp, cs, ivar, G_ORANGE);
  }
}

#define NORM_CS(_cs) _cs  // (_cs->sym->is_constant ? _cs->sym->type->abstract_type->v[0] : _cs)

void graph_contours(FA *fa, cchar *fn) {
  FILE *fp = graph_start(fn, "contours", "Analysis Contours");
  Vec<CreationSet *> css_set;
  for (CreationSet *cs : fa->css) if (cs->sym != sym_continuation && !cs->sym->is_symbol) css_set.set_add(NORM_CS(cs));
  for (EntrySet *es : fa->ess) graph_es_node(fp, es);
  for (CreationSet *cs : css_set) if (cs) graph_cs_node(fp, cs);
  for (EntrySet *es : fa->ess) for (AEdge *e : es->out_edges) if (e && fa->ess_set.in(e->from) && fa->ess_set.in(e->to))
      graph_edge(fp, e->from, e->to, G_BLUE);
  for (EntrySet *es : fa->ess) for (CreationSet *cs : es->creates) if (cs) if (css_set.in(NORM_CS(cs)))
      graph_edge(fp, es, NORM_CS(cs), G_PURPLE);
  for (CreationSet *cs : css_set) if (cs) for (EntrySet *es : cs->ess) if (es) if (fa->ess_set.in(es))
      graph_edge(fp, es, NORM_CS(cs), G_GREEN);
  for (CreationSet *cs : css_set) if (cs) for (AVar *ivar : cs->vars)
      for (CreationSet *x : ivar->out->sorted) if (x) if (css_set.in(NORM_CS(x))) graph_edge(fp, ivar, NORM_CS(x), G_RED);
  graph_end(fp);
}

static void strcat_pattern(char *title, Sym *s) {
  strcat(title, "(");
  for (Sym *a : s->has) {
    strcat(title, " ");
    strcat_sym_node(title, a);
  }
  strcat(title, " )");
}

static void graph_fun_node(FILE *fp, Fun *f) {
  char title[256] = "";
  strcat_sym_node(title, f->sym);
  for (MPosition *p : f->arg_positions) {
    if (Var *a = f->args.get(p)) {
      strcat(title, " ");
      if (!a->sym->is_pattern)
        strcat_sym_node(title, a->sym);
      else
        strcat_pattern(title, a->sym);
    }
  }
  if (fgraph_frequencies)
    snprintf(title + strlen(title), sizeof(title) - strlen(title), "freq(%f)", f->execution_frequency);
  graph_node(fp, f, title);
}

static void graph_call(FILE *fp, Fun *f, Fun *ff) { graph_edge(fp, f, ff); }

static void graph_calls(FA *fa, cchar *fn) {
  FILE *fp = graph_start(fn, "calls", "Call Graph");
  for (Fun *f : fa->funs) graph_fun_node(fp, f);
  for (Fun *f : fa->funs) {
    Vec<Fun *> calls;
    f->calls_funs(calls);
    for (Fun *ff : calls) graph_call(fp, f, ff);
  }
  graph_end(fp);
}

static void graph_rec(FA *fa, cchar *fn) {
  FILE *fp = graph_start(fn, "rec", "Recursion (Interprocedural Loops)");
  for (LoopNode *n : fa->pdb->loops->nodes) if (n->node) graph_fun_node(fp, (Fun *)n->node);
  else graph_loop_node(fp, n);
  for (LoopNode *n : fa->pdb->loops->nodes) if (n->node) {
    Fun *f = (Fun *)n->node;
    Vec<Fun *> calls;
    f->calls_funs(calls);
    for (Fun *ff : calls) graph_call(fp, f, ff);
  }
  else graph_loop_edges(fp, n);
  graph_end(fp);
}

static void graph_abstract_types(FA *fa, cchar *fn) {
  FILE *fp = graph_start(fn, "at", "Abstract Types");
  Vec<Sym *> syms;
  syms.set_union(fa->patterns->types);
  for (Sym *s : fa->patterns->types) if (s) syms.set_union(s->implementors);
  for (Sym *s : syms) if (s && s->live && !s->constant) {
    char name[256], *pname = name;
    strcpy(pname, type_kind_string[s->type_kind]);
    pname += strlen(pname);
    *pname++ = ' ';
    *pname = 0;
    if (s->is_symbol) {
      *pname++ = '#';
      strcpy(pname, s->name);
      pname += strlen(pname);
    } else if (s->is_pattern) {
      strcpy(pname, "pattern ");
      pname += strlen(pname);
      if (s->name)
        strcpy(pname, s->name);
      else
        snprintf(pname, sizeof(name) - (pname - name), "%d", s->id);
      pname += strlen(pname);
    } else if (s->fun) {
      pname += strlen(pname);
      if (s->name)
        strcpy(pname, s->name);
      else
        strcpy(pname, "<anonymous>");
      pname += strlen(pname);
      snprintf(pname, sizeof(name) - (pname - name), "%s:%d ", s->fun->filename(), s->fun->line());
      pname += strlen(pname);
    } else if (s->name) {
      strcpy(pname, s->name);
      pname += strlen(pname);
    } else {
      strcpy(pname, "<anonymous>");
      pname += strlen(pname);
    }
    graph_node(fp, s, name);
  }
  for (Sym *s : syms) if (s && s->live && !s->constant) for (Sym *ss : s->implements) if (ss && ss->live && !ss->constant)
      graph_edge(fp, s, ss);
  graph_end(fp);
}

void graph(FA *fa, cchar *fn) {
  Vec<Fun *> tfuns, funs;
  tfuns.copy(fa->funs);
  qsort(tfuns.v, tfuns.n, sizeof(tfuns[0]), compar_fun_ids);
  if (!graph_fun[0])
    funs.move(tfuns);
  else {
    for (Fun *f : tfuns) if (f->sym->name && !strcmp(f->sym->name, graph_fun)) funs.add(f);
  }
  graph_ast(funs, fn);
  graph_cfg(funs, fn);
  graph_ssu(funs, fn);
  graph_dom(funs, fn);
  graph_loops(funs, fn);
  graph_avars(fa, fn);
  graph_contours(fa, fn);
  graph_calls(fa, fn);
  graph_rec(fa, fn);
  graph_abstract_types(fa, fn);
}
