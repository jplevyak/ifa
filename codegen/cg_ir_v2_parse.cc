// SPDX-License-Identifier: BSD-3-Clause
//
// cg_ir_v2_parse.cc — S-expression parser for CG_IR_v2.
//
// Grammar reference: ifa/codegen/CG_IR_TEXT.md.
//
// This is the v0 parser. It handles only the constructs in
// the early tests (test 01 — empty void fn, test 02 — fn
// returning a constant). Each new test landing extends this
// parser by exactly the syntax it needs.
//
// Implementation: two-pass.
//   Pass 1: tokenize + build an SExpr tree (no semantics).
//   Pass 2: walk the tree, build CGv2Program objects.
//
// Errors are reported via the `err` out-parameter as a
// GC-allocated string. Parser is fail-fast: first error wins.

#include "ifadefs.h"

#include "codegen/cg_ir_v2.h"

#include <string.h>
#include <ctype.h>
#include <stdio.h>

// ============================================================
//   S-expression tokenization / tree building
// ============================================================

namespace {

struct SExpr {
  bool is_list;
  Vec<SExpr *> children;       // when is_list
  cchar *atom;                  // when !is_list (GC-dup'd)
  int line;
};

struct ParseCtx {
  cchar *src;
  int pos;
  int line;
  cchar *err;                   // set on first error; sticky

  ParseCtx(cchar *s) : src(s), pos(0), line(1), err(0) {}

  void fail(cchar *msg) {
    if (err) return;
    char buf[256];
    snprintf(buf, sizeof(buf), "line %d: %s", line, msg);
    err = dupstr(buf);
  }
};

static bool skip_ws_and_comments(ParseCtx &c) {
  while (c.src[c.pos]) {
    char ch = c.src[c.pos];
    if (ch == ' ' || ch == '\t' || ch == '\r') {
      c.pos++;
    } else if (ch == '\n') {
      c.line++;
      c.pos++;
    } else if (ch == ';' && c.src[c.pos + 1] == ';') {
      // Line comment to EOL.
      while (c.src[c.pos] && c.src[c.pos] != '\n') c.pos++;
    } else {
      return true;
    }
  }
  return false;
}

static cchar *read_atom(ParseCtx &c) {
  // Atom: identifier, number, %ref, :keyword, or quoted "string".
  // We return a GC-dup'd string for everything (including
  // numeric literals — semantic interpretation comes later).
  int start = c.pos;
  if (c.src[c.pos] == '"') {
    // Quoted string. Capture including the quotes for clarity.
    c.pos++;
    while (c.src[c.pos] && c.src[c.pos] != '"') {
      if (c.src[c.pos] == '\\' && c.src[c.pos + 1]) c.pos++;
      c.pos++;
    }
    if (c.src[c.pos] != '"') {
      c.fail("unterminated string literal");
      return 0;
    }
    c.pos++;
    int len = c.pos - start;
    char *s = (char *)MALLOC(len + 1);
    memcpy(s, c.src + start, len);
    s[len] = 0;
    return s;
  }
  while (c.src[c.pos]) {
    char ch = c.src[c.pos];
    if (ch == '(' || ch == ')' || ch == ' ' || ch == '\t' ||
        ch == '\n' || ch == '\r' || ch == ';') break;
    c.pos++;
  }
  int len = c.pos - start;
  if (len == 0) {
    c.fail("empty atom");
    return 0;
  }
  char *s = (char *)MALLOC(len + 1);
  memcpy(s, c.src + start, len);
  s[len] = 0;
  return s;
}

static SExpr *parse_sexp(ParseCtx &c) {
  if (!skip_ws_and_comments(c)) {
    c.fail("unexpected EOF");
    return 0;
  }
  SExpr *e = new SExpr();
  e->line = c.line;
  if (c.src[c.pos] == '(') {
    e->is_list = true;
    c.pos++;
    while (true) {
      if (!skip_ws_and_comments(c)) {
        c.fail("unclosed list");
        return 0;
      }
      if (c.src[c.pos] == ')') { c.pos++; break; }
      SExpr *child = parse_sexp(c);
      if (c.err) return 0;
      e->children.add(child);
    }
  } else {
    e->is_list = false;
    e->atom = read_atom(c);
    if (c.err) return 0;
  }
  return e;
}

// ============================================================
//   Helpers for navigating SExpr trees during semantic pass
// ============================================================

static bool is_atom_eq(SExpr *e, cchar *s) {
  return e && !e->is_list && e->atom && strcmp(e->atom, s) == 0;
}

static bool starts_with_atom(SExpr *e, cchar *s) {
  return e && e->is_list && e->children.n > 0 &&
         is_atom_eq(e->children[0], s);
}

static bool is_keyword(SExpr *e, cchar *kw) {
  return e && !e->is_list && e->atom && e->atom[0] == ':' &&
         strcmp(e->atom + 1, kw) == 0;
}

// Find the value following a :keyword in a list, starting from
// `start_idx`. Returns the index of the value, or -1 if not
// found. Caller indexes children[result+1] for the value.
static int find_kw(SExpr *list, cchar *kw, int start_idx = 0) {
  for (int i = start_idx; i + 1 < list->children.n; i++) {
    if (is_keyword(list->children[i], kw)) return i;
  }
  return -1;
}

// ============================================================
//   Semantic pass: SExpr -> CGv2Program
// ============================================================

struct BuildCtx {
  CGv2Program *prog;
  cchar *err;
  int next_id;

  // Per-function symbol tables. Populated as we walk a fun.
  ChainHashMap<cchar *, StringHashFns, CGv2Block *> blocks_by_name;
  ChainHashMap<cchar *, StringHashFns, CGv2Value *> values_by_name;

  BuildCtx(CGv2Program *p) : prog(p), err(0), next_id(1000) {}

  void fail_at(SExpr *where, cchar *msg) {
    if (err) return;
    char buf[256];
    int line = where ? where->line : 0;
    snprintf(buf, sizeof(buf), "line %d: %s", line, msg);
    err = dupstr(buf);
  }
};

static CGv2Type *resolve_type(BuildCtx &c, SExpr *type_ref) {
  if (!type_ref || type_ref->is_list || !type_ref->atom) {
    c.fail_at(type_ref, "expected a type reference");
    return 0;
  }
  // Strip leading '%' for user types.
  cchar *name = type_ref->atom;
  if (name[0] == '%') name = name + 1;
  CGv2Type *t = c.prog->lookup_type(name);
  if (!t) c.fail_at(type_ref, "unknown type");
  return t;
}

// Signature: a parenthesized list (ret arg0 arg1 ...).
static CGv2Sig *build_sig(BuildCtx &c, SExpr *sig_expr) {
  if (!sig_expr || !sig_expr->is_list || sig_expr->children.n < 1) {
    c.fail_at(sig_expr, "signature must be (ret arg...)");
    return 0;
  }
  CGv2Sig *s = new CGv2Sig();
  s->ret = resolve_type(c, sig_expr->children[0]);
  for (int i = 1; i < sig_expr->children.n; i++) {
    s->args.add(resolve_type(c, sig_expr->children[i]));
  }
  return s;
}

// Parse a terminator instruction.
static CGv2Inst *build_term(BuildCtx &c, SExpr *term_expr) {
  if (!term_expr || !term_expr->is_list || term_expr->children.n == 0) {
    c.fail_at(term_expr, "terminator must be a list");
    return 0;
  }
  CGv2Inst *inst = new CGv2Inst();
  inst->id = c.next_id++;
  cchar *op = term_expr->children[0]->is_list ? 0 :
              term_expr->children[0]->atom;
  if (!op) { c.fail_at(term_expr, "terminator op must be atom"); return 0; }
  if (strcmp(op, "ret") == 0) {
    inst->op = CG2_RET;
    if (term_expr->children.n >= 2) {
      // (ret %value)
      SExpr *vref = term_expr->children[1];
      if (vref->is_list || !vref->atom) {
        c.fail_at(vref, "ret expects a value ref");
        return 0;
      }
      cchar *vname = vref->atom;
      if (vname[0] == '%') vname++;
      CGv2Value *v = c.values_by_name.get(vname);
      if (!v) {
        // Could be a constant declared at program scope.
        for (CGv2Value *cv : c.prog->constants) {
          if (cv && cv->name && strcmp(cv->name, vname) == 0) {
            v = cv;
            break;
          }
        }
      }
      if (!v) {
        c.fail_at(vref, "ret references unknown value");
        return 0;
      }
      inst->rvals.add(v);
    }
  } else if (strcmp(op, "br") == 0) {
    inst->op = CG2_BR;
    if (term_expr->children.n != 2) {
      c.fail_at(term_expr, "br expects exactly one target");
      return 0;
    }
    SExpr *bref = term_expr->children[1];
    cchar *bname = bref->atom;
    if (bname[0] == '%') bname++;
    CGv2Block *b = c.blocks_by_name.get(bname);
    if (!b) {
      c.fail_at(bref, "br references unknown block");
      return 0;
    }
    inst->br_target = b;
  } else if (strcmp(op, "unreachable") == 0) {
    inst->op = CG2_UNREACHABLE;
  } else {
    c.fail_at(term_expr, "unsupported terminator op");
    return 0;
  }
  return inst;
}

static CGv2Value *build_value_decl(BuildCtx &c, SExpr *e);

// Look up a value reference like `%x`. Checks per-fun
// values_by_name first, then program-scope constants. Returns
// nullptr (with err set) if unresolved.
static CGv2Value *resolve_value_ref(BuildCtx &c, SExpr *vref) {
  if (vref->is_list || !vref->atom) {
    c.fail_at(vref, "expected a value ref");
    return 0;
  }
  cchar *n = vref->atom;
  if (n[0] == '%') n++;
  CGv2Value *v = c.values_by_name.get(n);
  if (v) return v;
  for (CGv2Value *cv : c.prog->constants) {
    if (cv && cv->name && strcmp(cv->name, n) == 0) return cv;
  }
  c.fail_at(vref, "unknown value");
  return 0;
}

// (inst %name OP_TAG SUB_TAG rval* => lval*)
// v0 supports: OP_TAG=binop, SUB_TAG=add.
static CGv2Inst *build_inst(BuildCtx &c, SExpr *e) {
  if (e->children.n < 3) {
    c.fail_at(e, "inst needs op and operands");
    return 0;
  }
  SExpr *nm = e->children[1];
  if (nm->is_list || !nm->atom) {
    c.fail_at(nm, "inst name must be atom");
    return 0;
  }
  cchar *name = nm->atom;
  if (name[0] == '%') name++;

  SExpr *op_atom = e->children[2];
  if (op_atom->is_list || !op_atom->atom) {
    c.fail_at(op_atom, "inst op must be atom");
    return 0;
  }
  cchar *op_tag = op_atom->atom;

  CGv2Inst *inst = new CGv2Inst();
  inst->id = c.next_id++;
  inst->name = dupstr(name);

  if (strcmp(op_tag, "binop") == 0) {
    inst->op = CG2_BINOP;
    if (e->children.n < 4) {
      c.fail_at(e, "binop needs sub-op");
      return 0;
    }
    SExpr *sub = e->children[3];
    if (sub->is_list || !sub->atom) {
      c.fail_at(sub, "binop sub-op must be atom");
      return 0;
    }
    cchar *st = sub->atom;
    if (strcmp(st, "add") == 0) {
      inst->sub_op = CG2B_ADD;
    } else {
      c.fail_at(sub, "unsupported binop sub-op");
      return 0;
    }
    // Parse rvals up to '=>', then lvals after.
    int i = 4;
    bool in_lvals = false;
    for (; i < e->children.n; i++) {
      SExpr *ch = e->children[i];
      if (!ch->is_list && ch->atom && strcmp(ch->atom, "=>") == 0) {
        in_lvals = true;
        continue;
      }
      CGv2Value *v = resolve_value_ref(c, ch);
      if (c.err) return 0;
      if (in_lvals) inst->lvals.add(v);
      else inst->rvals.add(v);
    }
  } else {
    c.fail_at(op_atom, "unsupported inst op");
    return 0;
  }

  // Register the lval name(s) for downstream lookups (single
  // result for binop; extends as more ops land).
  for (CGv2Value *lv : inst->lvals) {
    if (lv && lv->name) c.values_by_name.put(lv->name, lv);
  }
  return inst;
}

static CGv2Block *build_block(BuildCtx &c, SExpr *block_expr) {
  // (block %name :label name? :preds (...) inst* :term term)
  if (!starts_with_atom(block_expr, "block")) {
    c.fail_at(block_expr, "expected (block ...)");
    return 0;
  }
  if (block_expr->children.n < 2) {
    c.fail_at(block_expr, "block needs a name");
    return 0;
  }
  SExpr *nm = block_expr->children[1];
  if (nm->is_list || !nm->atom) {
    c.fail_at(nm, "block name must be atom");
    return 0;
  }
  cchar *name = nm->atom;
  if (name[0] == '%') name++;
  CGv2Block *b = c.blocks_by_name.get(name);
  if (!b) {
    b = new CGv2Block();
    b->id = c.next_id++;
    b->name = dupstr(name);
    c.blocks_by_name.put(dupstr(name), b);
  }

  // Walk keyword args.
  int term_idx = find_kw(block_expr, "term");
  if (term_idx < 0) {
    c.fail_at(block_expr, "block missing :term");
    return 0;
  }
  b->terminator = build_term(c, block_expr->children[term_idx + 1]);
  if (c.err) return 0;

  // Body insts. Walk children after the name (index 2..) and
  // pick out `(inst ...)` lists; skip :preds / :term / :label
  // keyword pairs (their values are at i+1 — we just don't
  // confuse them with insts).
  for (int i = 2; i < block_expr->children.n; i++) {
    SExpr *ch = block_expr->children[i];
    if (!ch->is_list) {
      // Likely a keyword like :preds or :term — skip it AND
      // its following value.
      if (ch->atom && ch->atom[0] == ':') i++;
      continue;
    }
    if (starts_with_atom(ch, "inst")) {
      CGv2Inst *inst = build_inst(c, ch);
      if (c.err) return 0;
      b->body.add(inst);
    }
  }
  return b;
}

static CGv2Fun *build_fun(BuildCtx &c, SExpr *fun_expr) {
  // (fun %name :signature (...) :entry %b0 :formals (...)? block* )
  if (!starts_with_atom(fun_expr, "fun")) {
    c.fail_at(fun_expr, "expected (fun ...)");
    return 0;
  }
  if (fun_expr->children.n < 2) {
    c.fail_at(fun_expr, "fun needs a name");
    return 0;
  }
  CGv2Fun *f = new CGv2Fun();
  f->id = c.next_id++;
  SExpr *nm = fun_expr->children[1];
  cchar *name = nm->atom;
  if (name[0] == '%') name++;
  f->name = dupstr(name);

  // Reset per-fun symbol tables.
  c.blocks_by_name.clear();
  c.values_by_name.clear();

  int sig_kw = find_kw(fun_expr, "signature");
  if (sig_kw < 0) {
    c.fail_at(fun_expr, "fun missing :signature");
    return 0;
  }
  f->signature = build_sig(c, fun_expr->children[sig_kw + 1]);
  if (c.err) return 0;

  int entry_kw = find_kw(fun_expr, "entry");
  cchar *entry_name = 0;
  if (entry_kw >= 0) {
    SExpr *eref = fun_expr->children[entry_kw + 1];
    cchar *en = eref->atom;
    if (en[0] == '%') en++;
    entry_name = dupstr(en);
  }

  bool main_flag = find_kw(fun_expr, "main") >= 0;
  if (main_flag) f->is_main = true;

  // Walk (value ...) decls first; they define formals/locals
  // that block insts and terminators may reference. Each is
  // registered in values_by_name and into f->formals/locals
  // based on scope.
  for (int i = 2; i < fun_expr->children.n; i++) {
    SExpr *ch = fun_expr->children[i];
    if (!ch->is_list || !starts_with_atom(ch, "value")) continue;
    CGv2Value *v = build_value_decl(c, ch);
    if (c.err) return 0;
    c.values_by_name.put(v->name, v);
    if (v->scope == CG2V_FORMAL) f->formals.add(v);
    else if (v->scope == CG2V_LOCAL) f->locals.add(v);
  }

  // :formals (%x %y) — declares the ORDER of formals as they
  // match the signature. Cross-check the count and reorder
  // f->formals to follow this list. If absent, f->formals
  // keeps the order of (value ...) decls.
  int formals_kw = find_kw(fun_expr, "formals");
  if (formals_kw >= 0) {
    SExpr *flist = fun_expr->children[formals_kw + 1];
    if (!flist->is_list) {
      c.fail_at(flist, ":formals must be a list");
      return 0;
    }
    Vec<CGv2Value *> ordered;
    for (SExpr *fref : flist->children) {
      if (fref->is_list || !fref->atom) {
        c.fail_at(fref, "formal ref must be atom");
        return 0;
      }
      cchar *fn = fref->atom;
      if (fn[0] == '%') fn++;
      CGv2Value *v = c.values_by_name.get(fn);
      if (!v) {
        c.fail_at(fref, ":formals references undeclared value");
        return 0;
      }
      ordered.add(v);
    }
    f->formals.clear();
    for (CGv2Value *v : ordered) f->formals.add(v);
  }

  // Pre-pass: declare every block by name so forward
  // references (br to a block defined later) resolve. The
  // grammar allows blocks in any order; this matches LLVM
  // textual IR convention.
  for (int i = 2; i < fun_expr->children.n; i++) {
    SExpr *ch = fun_expr->children[i];
    if (!ch->is_list || !starts_with_atom(ch, "block")) continue;
    if (ch->children.n < 2 || ch->children[1]->is_list ||
        !ch->children[1]->atom) {
      c.fail_at(ch, "block name must be atom");
      return 0;
    }
    cchar *bname = ch->children[1]->atom;
    if (bname[0] == '%') bname++;
    if (!c.blocks_by_name.get(bname)) {
      CGv2Block *b = new CGv2Block();
      b->id = c.next_id++;
      b->name = dupstr(bname);
      c.blocks_by_name.put(b->name, b);
    }
  }

  // Build pass: fill each block with its terminator (and body
  // once test 02+ lands).
  for (int i = 2; i < fun_expr->children.n; i++) {
    SExpr *ch = fun_expr->children[i];
    if (ch->is_list && starts_with_atom(ch, "block")) {
      CGv2Block *b = build_block(c, ch);
      if (c.err) return 0;
      f->blocks.add(b);
    }
  }

  if (entry_name) {
    f->entry = c.blocks_by_name.get(entry_name);
    if (!f->entry) {
      c.fail_at(fun_expr, "entry block not found");
      return 0;
    }
  } else if (f->blocks.n > 0) {
    f->entry = f->blocks[0];
  }
  return f;
}

// Parse an ImmValue subexpression: (int N), (uint N), (float
// N), (bool true|false), (str "..."), (sym name), (nil), (undef).
// Sets `out` on success. Reports a parse error on failure.
static void build_imm(BuildCtx &c, SExpr *expr, CGv2Immediate &out) {
  out.kind = CGv2Immediate::I_NONE;
  if (!expr || !expr->is_list || expr->children.n == 0) {
    c.fail_at(expr, "expected an immediate value form");
    return;
  }
  SExpr *head = expr->children[0];
  if (head->is_list || !head->atom) {
    c.fail_at(expr, "immediate head must be an atom");
    return;
  }
  cchar *tag = head->atom;
  if (strcmp(tag, "int") == 0) {
    if (expr->children.n != 2) { c.fail_at(expr, "(int N) needs one arg"); return; }
    out.kind = CGv2Immediate::I_INT;
    out.v.i = atoll(expr->children[1]->atom);
  } else if (strcmp(tag, "uint") == 0) {
    if (expr->children.n != 2) { c.fail_at(expr, "(uint N) needs one arg"); return; }
    out.kind = CGv2Immediate::I_UINT;
    out.v.u = (uint64_t)strtoull(expr->children[1]->atom, nullptr, 10);
  } else if (strcmp(tag, "float") == 0) {
    if (expr->children.n != 2) { c.fail_at(expr, "(float N) needs one arg"); return; }
    out.kind = CGv2Immediate::I_FLOAT;
    out.v.f = atof(expr->children[1]->atom);
  } else if (strcmp(tag, "bool") == 0) {
    if (expr->children.n != 2) { c.fail_at(expr, "(bool T) needs one arg"); return; }
    out.kind = CGv2Immediate::I_BOOL;
    out.v.b = strcmp(expr->children[1]->atom, "true") == 0;
  } else if (strcmp(tag, "str") == 0) {
    if (expr->children.n != 2) { c.fail_at(expr, "(str S) needs one arg"); return; }
    out.kind = CGv2Immediate::I_STR;
    out.str = expr->children[1]->atom;  // already GC-dup'd; quotes included
  } else if (strcmp(tag, "sym") == 0) {
    if (expr->children.n != 2) { c.fail_at(expr, "(sym N) needs one arg"); return; }
    out.kind = CGv2Immediate::I_SYM;
    out.str = expr->children[1]->atom;
  } else if (strcmp(tag, "nil") == 0) {
    out.kind = CGv2Immediate::I_NIL;
  } else if (strcmp(tag, "undef") == 0) {
    out.kind = CGv2Immediate::I_UNDEF;
  } else {
    c.fail_at(expr, "unknown immediate form");
  }
}

// Parse a CGv2ValueScope name. Returns true on success.
static bool parse_scope(cchar *s, CGv2ValueScope &out) {
  if (!s) return false;
  if (strcmp(s, "local") == 0) { out = CG2V_LOCAL; return true; }
  if (strcmp(s, "formal") == 0) { out = CG2V_FORMAL; return true; }
  if (strcmp(s, "global") == 0) { out = CG2V_GLOBAL; return true; }
  if (strcmp(s, "constant") == 0) { out = CG2V_CONSTANT; return true; }
  if (strcmp(s, "fun_ref") == 0) { out = CG2V_FUN_REF; return true; }
  if (strcmp(s, "symbol") == 0) { out = CG2V_SYMBOL; return true; }
  return false;
}

// (value %name :type TYPE :scope SCOPE)
//
// Per-fun value declaration. Test 03 only needs the formal
// case; locals/globals reuse the same form and land with their
// tests.
static CGv2Value *build_value_decl(BuildCtx &c, SExpr *e) {
  if (e->children.n < 2) {
    c.fail_at(e, "value decl needs %name");
    return 0;
  }
  SExpr *nm = e->children[1];
  if (nm->is_list || !nm->atom) {
    c.fail_at(nm, "value name must be atom");
    return 0;
  }
  cchar *name = nm->atom;
  if (name[0] == '%') name++;

  CGv2Value *v = new CGv2Value();
  v->id = c.next_id++;
  v->name = dupstr(name);
  v->scope = CG2V_LOCAL;

  int type_kw = find_kw(e, "type", 2);
  if (type_kw >= 0) {
    v->type = resolve_type(c, e->children[type_kw + 1]);
    if (c.err) return 0;
  }
  int scope_kw = find_kw(e, "scope", 2);
  if (scope_kw >= 0) {
    SExpr *sx = e->children[scope_kw + 1];
    if (sx->is_list || !sx->atom || !parse_scope(sx->atom, v->scope)) {
      c.fail_at(sx, "unknown :scope");
      return 0;
    }
  }
  return v;
}

// (const %name (int N) :type TYPE)
static CGv2Value *build_const(BuildCtx &c, SExpr *e) {
  if (e->children.n < 3) {
    c.fail_at(e, "const needs %name, ImmValue, :type TYPE");
    return 0;
  }
  SExpr *nm = e->children[1];
  if (nm->is_list || !nm->atom) {
    c.fail_at(nm, "const name must be atom");
    return 0;
  }
  cchar *name = nm->atom;
  if (name[0] == '%') name++;

  CGv2Value *v = new CGv2Value();
  v->id = c.next_id++;
  v->name = dupstr(name);
  v->scope = CG2V_CONSTANT;
  build_imm(c, e->children[2], v->imm);
  if (c.err) return 0;

  int type_kw = find_kw(e, "type", 3);
  if (type_kw < 0) {
    c.fail_at(e, "const missing :type");
    return 0;
  }
  v->type = resolve_type(c, e->children[type_kw + 1]);
  if (c.err) return 0;
  return v;
}

static void build_program(BuildCtx &c, SExpr *root) {
  // The grammar's top-level is a single SExpr that's a list of
  // top-level decls. `((fun ...) (type ...))` means root is a
  // list whose children are the decls.
  if (!root->is_list) {
    c.fail_at(root, "program must be a list");
    return;
  }
  // Two-pass: declare constants first so they're visible to
  // any fun that references them. (Functions can also be
  // forward-referenced — handled when test 07's CGV_FUN_REF
  // resolution lands.)
  for (SExpr *d : root->children) {
    if (starts_with_atom(d, "const")) {
      CGv2Value *cv = build_const(c, d);
      if (c.err) return;
      c.prog->constants.add(cv);
    }
  }

  for (SExpr *d : root->children) {
    if (starts_with_atom(d, "fun")) {
      CGv2Fun *f = build_fun(c, d);
      if (c.err) return;
      c.prog->funs.add(f);
      if (f->is_main) c.prog->main_fun = f;
    } else if (starts_with_atom(d, "const")) {
      // Already handled in pass 1.
    } else {
      // (TypeDecl, GlobalDecl land with their test corpus
      // expansions.) Silently skip unrecognized top-level
      // forms for now.
    }
  }
}

}  // namespace

// ============================================================
//   Public entry point
// ============================================================

CGv2Program *cg_v2_parse(cchar *text, cchar **err_out) {
  if (err_out) *err_out = 0;
  ParseCtx pc(text);
  SExpr *root = parse_sexp(pc);
  if (pc.err) {
    if (err_out) *err_out = pc.err;
    return 0;
  }
  CGv2Program *prog = new CGv2Program();
  BuildCtx bc(prog);
  build_program(bc, root);
  if (bc.err) {
    if (err_out) *err_out = bc.err;
    return 0;
  }
  return prog;
}
