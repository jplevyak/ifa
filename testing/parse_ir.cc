// SPDX-License-Identifier: BSD-3-Clause
// .ir text-format parser. See ifa/testing/IF1_TEXT_FORMAT.md.
//
// Approach: hand-rolled lexer + recursive-descent parser. Two-pass
// declaration resolution within a single file:
//   pass 1 — scan top-level decls, create empty Sym objects for every
//            (sym NAME ...) / (type NAME ...) / (fun NAME ...).
//   pass 2 — walk decls again, fill in attributes / build bodies. All
//            %ref lookups succeed because pass 1 populated the table.
//
// Forward references inside a single file work via this scheme. The
// (import @builtin as %local) form resolves @ at the time of import and
// installs %local in the user table; later refs to %local find it.

#include "ifadefs.h"

#include "code.h"
#include "if1.h"
#include "num.h"
#include "sym.h"
#include "testing/parse_ir.h"

#include <ctype.h>
#include <stdarg.h>
#include <string.h>

// ---------------------------------------------------------------------------
// Lexer
// ---------------------------------------------------------------------------

enum TokKind {
  TOK_EOF = 0,
  TOK_LPAREN,    // (
  TOK_RPAREN,    // )
  TOK_REF_USER,  // %name
  TOK_REF_BLT,   // @name (builtin)
  TOK_REF_SYM,   // #name (atom/symbol)
  TOK_KEYWORD,   // :name
  TOK_STRING,    // "..."
  TOK_INT,       // 42, -7
  TOK_FLOAT,     // 3.14
  TOK_BAREWORD,  // identifier without a sigil (RECORD, true, ...)
};

struct Tok {
  TokKind kind;
  cchar *text;  // payload (refs/keywords have the sigil stripped)
  int64 ival;
  double fval;
  int line, col;
};

struct Lex {
  cchar *filename;
  cchar *src;  // entire buffer
  cchar *cur;
  int line, col;
  Tok t;
  int errors;
};

static void lex_err(Lex &L, cchar *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  fprintf(stderr, "%s:%d:%d: ", L.filename ? L.filename : "<?>", L.line, L.col);
  vfprintf(stderr, fmt, ap);
  fputc('\n', stderr);
  va_end(ap);
  L.errors++;
}

static void advance_char(Lex &L) {
  if (*L.cur == '\n') {
    L.line++;
    L.col = 1;
  } else {
    L.col++;
  }
  L.cur++;
}

static void skip_ws_and_comments(Lex &L) {
  for (;;) {
    while (*L.cur && isspace((unsigned char)*L.cur)) advance_char(L);
    if (*L.cur == ';') {
      while (*L.cur && *L.cur != '\n') advance_char(L);
    } else {
      break;
    }
  }
}

static int is_ident_start(int c) { return isalpha(c) || c == '_'; }
static int is_ident_cont(int c) { return isalnum(c) || c == '_' || c == '-'; }

// Read identifier characters starting at L.cur; intern into IF1's string
// table. Returns NULL if the first character is not an ident-start.
static cchar *read_ident(Lex &L) {
  cchar *start = L.cur;
  if (!is_ident_start((unsigned char)*L.cur)) return NULL;
  advance_char(L);
  while (is_ident_cont((unsigned char)*L.cur)) advance_char(L);
  int len = L.cur - start;
  char buf[256];
  if (len >= (int)sizeof(buf)) {
    lex_err(L, "identifier too long (>%zu)", sizeof(buf) - 1);
    return NULL;
  }
  memcpy(buf, start, len);
  buf[len] = 0;
  return if1_cannonicalize_string(if1, buf);
}

static cchar *read_string(Lex &L) {
  // L.cur points at the opening "
  advance_char(L);  // skip "
  cchar *start = L.cur;
  // Permit \" inside; everything else is literal.
  char buf[1024];
  int n = 0;
  while (*L.cur && *L.cur != '"') {
    if (n >= (int)sizeof(buf) - 1) {
      lex_err(L, "string literal too long");
      return NULL;
    }
    if (*L.cur == '\\' && L.cur[1]) {
      char esc = L.cur[1];
      switch (esc) {
        case 'n': buf[n++] = '\n'; break;
        case 't': buf[n++] = '\t'; break;
        case 'r': buf[n++] = '\r'; break;
        case '\\': buf[n++] = '\\'; break;
        case '"': buf[n++] = '"'; break;
        case '0': buf[n++] = '\0'; break;
        default:
          buf[n++] = esc;  // unknown escape: pass through
          break;
      }
      advance_char(L);
      advance_char(L);
      continue;
    }
    buf[n++] = *L.cur;
    advance_char(L);
  }
  buf[n] = 0;
  if (*L.cur != '"') {
    lex_err(L, "unterminated string starting at line %d", L.line);
    return NULL;
  }
  advance_char(L);  // skip closing "
  (void)start;
  return if1_cannonicalize_string(if1, buf);
}

static void next_token(Lex &L) {
  skip_ws_and_comments(L);
  L.t.line = L.line;
  L.t.col = L.col;
  L.t.text = NULL;
  L.t.ival = 0;
  L.t.fval = 0.0;
  if (!*L.cur) {
    L.t.kind = TOK_EOF;
    return;
  }
  char c = *L.cur;
  if (c == '(') {
    L.t.kind = TOK_LPAREN;
    advance_char(L);
    return;
  }
  if (c == ')') {
    L.t.kind = TOK_RPAREN;
    advance_char(L);
    return;
  }
  if (c == '"') {
    L.t.kind = TOK_STRING;
    L.t.text = read_string(L);
    return;
  }
  // Multi-char punctuation: "=>" is the rvals/lvals separator in (send ...).
  if (c == '=' && L.cur[1] == '>') {
    advance_char(L);
    advance_char(L);
    L.t.kind = TOK_BAREWORD;
    L.t.text = if1_cannonicalize_string(if1, "=>");
    return;
  }
  if (c == '%' || c == '@' || c == '#' || c == ':') {
    advance_char(L);
    cchar *name = read_ident(L);
    if (!name) {
      lex_err(L, "expected identifier after '%c'", c);
      L.t.kind = TOK_EOF;
      return;
    }
    switch (c) {
      case '%': L.t.kind = TOK_REF_USER; break;
      case '@': L.t.kind = TOK_REF_BLT; break;
      case '#': L.t.kind = TOK_REF_SYM; break;
      case ':': L.t.kind = TOK_KEYWORD; break;
    }
    L.t.text = name;
    return;
  }
  // numeric: optional sign + digits, optional decimal
  if (c == '-' || c == '+' || isdigit((unsigned char)c)) {
    cchar *start = L.cur;
    if (c == '-' || c == '+') advance_char(L);
    bool has_digit = false, has_dot = false;
    while (isdigit((unsigned char)*L.cur)) { advance_char(L); has_digit = true; }
    if (*L.cur == '.') {
      has_dot = true;
      advance_char(L);
      while (isdigit((unsigned char)*L.cur)) { advance_char(L); has_digit = true; }
    }
    if (!has_digit) {
      // It was a bare '-' / '+' — treat as bareword.
      char buf[2] = {c, 0};
      L.t.kind = TOK_BAREWORD;
      L.t.text = if1_cannonicalize_string(if1, buf);
      return;
    }
    int len = L.cur - start;
    char buf[64];
    if (len >= (int)sizeof(buf)) {
      lex_err(L, "numeric literal too long");
      L.t.kind = TOK_EOF;
      return;
    }
    memcpy(buf, start, len);
    buf[len] = 0;
    if (has_dot) {
      L.t.kind = TOK_FLOAT;
      L.t.fval = atof(buf);
    } else {
      L.t.kind = TOK_INT;
      L.t.ival = strtoll(buf, NULL, 10);
    }
    return;
  }
  if (is_ident_start((unsigned char)c)) {
    cchar *name = read_ident(L);
    L.t.kind = TOK_BAREWORD;
    L.t.text = name;
    return;
  }
  lex_err(L, "unexpected character '%c' (0x%02x)", isprint((unsigned char)c) ? c : '?', (unsigned char)c);
  L.t.kind = TOK_EOF;
}

static void lex_init(Lex &L, cchar *filename, cchar *src) {
  L.filename = filename;
  L.src = src;
  L.cur = src;
  L.line = 1;
  L.col = 1;
  L.errors = 0;
  next_token(L);
}

// ---------------------------------------------------------------------------
// Parser state
// ---------------------------------------------------------------------------

struct Parser {
  Lex L;
  // %name → Sym* (persists across parses unless parse_ir_reset is called)
  // We keep this module-level so subsequent parse_ir_lookup calls work.
};

static Map<cchar *, Sym *> g_user_syms;
static Map<cchar *, Label *> g_labels;  // per-function scope; cleared per fun

void parse_ir_reset() {
  g_user_syms.clear();
  g_labels.clear();
}

Sym *parse_ir_lookup(cchar *name) {
  cchar *interned = if1_cannonicalize_string(if1, name);
  return g_user_syms.get(interned);
}

// ---------------------------------------------------------------------------
// Token consumers
// ---------------------------------------------------------------------------

static void parse_err(Lex &L, cchar *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  fprintf(stderr, "%s:%d:%d: ", L.filename ? L.filename : "<?>", L.t.line, L.t.col);
  vfprintf(stderr, fmt, ap);
  fputc('\n', stderr);
  va_end(ap);
  L.errors++;
}

static bool expect(Lex &L, TokKind k, cchar *what) {
  if (L.t.kind != k) {
    parse_err(L, "expected %s", what);
    return false;
  }
  next_token(L);
  return true;
}

static bool at_keyword(Lex &L, cchar *kw) {
  if (L.t.kind != TOK_KEYWORD) return false;
  return strcmp(L.t.text, kw) == 0;
}

static bool consume_keyword(Lex &L, cchar *kw) {
  if (!at_keyword(L, kw)) return false;
  next_token(L);
  return true;
}

// ---------------------------------------------------------------------------
// Sym creation / lookup
// ---------------------------------------------------------------------------

// Get or create a user-defined Sym by name. Created syms are registered
// with IF1. Returns the same Sym for the same name.
static Sym *get_or_make_user_sym(cchar *name) {
  Sym *s = g_user_syms.get(name);
  if (s) return s;
  s = new_Sym(name);
  // Don't set s->name=name here; new_Sym already did (via callback).
  g_user_syms.put(name, s);
  return s;
}

// Resolve a reference token to a Sym. Auto-creates user/symbol refs.
// Builtins (@name) must already be registered.
static Sym *resolve_ref(Lex &L, Tok &t) {
  switch (t.kind) {
    case TOK_REF_USER:
      return get_or_make_user_sym(t.text);
    case TOK_REF_BLT: {
      Sym *s = if1_get_builtin(if1, t.text);
      if (!s) {
        parse_err(L, "unknown builtin @%s", t.text);
        return NULL;
      }
      return s;
    }
    case TOK_REF_SYM:
      return if1_make_symbol(if1, t.text);
    default:
      parse_err(L, "expected %%ref, @builtin, or #symbol");
      return NULL;
  }
}

static Sym *parse_ref(Lex &L) {
  Tok t = L.t;
  if (t.kind != TOK_REF_USER && t.kind != TOK_REF_BLT && t.kind != TOK_REF_SYM) {
    parse_err(L, "expected reference (%%, @, #)");
    return NULL;
  }
  next_token(L);
  return resolve_ref(L, t);
}

// ---------------------------------------------------------------------------
// Attribute helpers
// ---------------------------------------------------------------------------

static int parse_intent(Lex &L) {
  if (L.t.kind != TOK_BAREWORD) {
    parse_err(L, "expected IN, INOUT, or OUT");
    return Sym_IN;
  }
  cchar *w = L.t.text;
  next_token(L);
  if (!strcmp(w, "IN")) return Sym_IN;
  if (!strcmp(w, "INOUT")) return Sym_INOUT;
  if (!strcmp(w, "OUT")) return Sym_OUT;
  parse_err(L, "unknown intent '%s'", w);
  return Sym_IN;
}

static int parse_type_kind_word(Lex &L) {
  if (L.t.kind != TOK_BAREWORD) {
    parse_err(L, "expected type kind (RECORD, SUM, FUN, ...)");
    return Type_NONE;
  }
  cchar *w = L.t.text;
  next_token(L);
  if (!strcmp(w, "NONE")) return Type_NONE;
  if (!strcmp(w, "UNKNOWN")) return Type_UNKNOWN;
  if (!strcmp(w, "SUM")) return Type_SUM;
  if (!strcmp(w, "RECORD")) return Type_RECORD;
  if (!strcmp(w, "FUN")) return Type_FUN;
  if (!strcmp(w, "REF")) return Type_REF;
  if (!strcmp(w, "TAGGED")) return Type_TAGGED;
  if (!strcmp(w, "PRIMITIVE")) return Type_PRIMITIVE;
  if (!strcmp(w, "APPLICATION")) return Type_APPLICATION;
  if (!strcmp(w, "VARIABLE")) return Type_VARIABLE;
  if (!strcmp(w, "ALIAS")) return Type_ALIAS;
  parse_err(L, "unknown type kind '%s'", w);
  return Type_NONE;
}

// Parse `(int32 42)` / `(float64 3.14)` / `(bool true)` / `(string "...")`
// / `(symbol "...")`. Caller has already consumed the opening (
// and the next token is the kind bareword. Consumes through the closing ).
static void parse_immediate(Lex &L, Sym *target) {
  if (L.t.kind != TOK_BAREWORD) {
    parse_err(L, "expected immediate kind (int32, float64, bool, ...)");
    return;
  }
  cchar *kind = L.t.text;
  next_token(L);
  Immediate &imm = target->imm;
  target->is_constant = 1;
  if (!strcmp(kind, "bool")) {
    if (L.t.kind != TOK_BAREWORD) { parse_err(L, "expected true/false"); }
    else {
      imm.set_bool(!strcmp(L.t.text, "true"));
      next_token(L);
    }
  } else if (!strncmp(kind, "int", 3)) {
    int width = atoi(kind + 3);
    IF1_int_type it = IF1_INT_TYPE_64;
    switch (width) {
      case 8: it = IF1_INT_TYPE_8; break;
      case 16: it = IF1_INT_TYPE_16; break;
      case 32: it = IF1_INT_TYPE_32; break;
      case 64: it = IF1_INT_TYPE_64; break;
      default: parse_err(L, "unknown int width '%s'", kind);
    }
    if (L.t.kind != TOK_INT) { parse_err(L, "expected integer"); }
    else { imm.set_int(L.t.ival, it); next_token(L); }
  } else if (!strncmp(kind, "uint", 4)) {
    int width = atoi(kind + 4);
    IF1_int_type it = IF1_INT_TYPE_64;
    switch (width) {
      case 8: it = IF1_INT_TYPE_8; break;
      case 16: it = IF1_INT_TYPE_16; break;
      case 32: it = IF1_INT_TYPE_32; break;
      case 64: it = IF1_INT_TYPE_64; break;
      default: parse_err(L, "unknown uint width '%s'", kind);
    }
    if (L.t.kind != TOK_INT) { parse_err(L, "expected integer"); }
    else { imm.set_uint(L.t.ival, it); next_token(L); }
  } else if (!strncmp(kind, "float", 5)) {
    int width = atoi(kind + 5);
    IF1_float_type ft = IF1_FLOAT_TYPE_64;
    switch (width) {
      case 32: ft = IF1_FLOAT_TYPE_32; break;
      case 64: ft = IF1_FLOAT_TYPE_64; break;
      case 128: ft = IF1_FLOAT_TYPE_128; break;
      default: parse_err(L, "unknown float width '%s'", kind);
    }
    double v = 0;
    if (L.t.kind == TOK_FLOAT) v = L.t.fval;
    else if (L.t.kind == TOK_INT) v = (double)L.t.ival;
    else parse_err(L, "expected float");
    imm.set_float(v, ft);
    next_token(L);
  } else if (!strcmp(kind, "string")) {
    if (L.t.kind != TOK_STRING) { parse_err(L, "expected string literal"); }
    else {
      imm.const_kind = IF1_CONST_KIND_STRING;
      imm.v_string = L.t.text;
      target->constant = L.t.text;
      next_token(L);
    }
  } else if (!strcmp(kind, "symbol")) {
    if (L.t.kind != TOK_STRING) { parse_err(L, "expected string literal"); }
    else {
      imm.const_kind = IF1_CONST_KIND_SYMBOL;
      imm.v_string = L.t.text;
      target->constant = L.t.text;
      next_token(L);
    }
  } else {
    parse_err(L, "unknown immediate kind '%s'", kind);
  }
  expect(L, TOK_RPAREN, "')' to close immediate");
}

// Parse a parenthesised list of refs, returning a fresh Vec.
//   ( %a %b @c )
// Caller has already consumed the opening '('.
static void parse_ref_list(Lex &L, Vec<Sym *> &out) {
  while (L.t.kind != TOK_RPAREN && L.t.kind != TOK_EOF) {
    Sym *s = parse_ref(L);
    if (s) out.add(s);
  }
  expect(L, TOK_RPAREN, "')' to close list");
}

// ---------------------------------------------------------------------------
// Top-level: (sym NAME ...attrs)
// ---------------------------------------------------------------------------

static void parse_sym_attrs(Lex &L, Sym *s) {
  while (L.t.kind == TOK_KEYWORD) {
    cchar *kw = L.t.text;
    next_token(L);
    if (!strcmp(kw, "type")) {
      s->type = parse_ref(L);
    } else if (!strcmp(kw, "in")) {
      s->in = parse_ref(L);
    } else if (!strcmp(kw, "aspect")) {
      s->aspect = parse_ref(L);
    } else if (!strcmp(kw, "is-local")) {
      s->is_local = 1;
    } else if (!strcmp(kw, "is-fun")) {
      s->is_fun = 1;
    } else if (!strcmp(kw, "is-constant")) {
      s->is_constant = 1;
    } else if (!strcmp(kw, "is-lvalue")) {
      s->is_lvalue = 1;
    } else if (!strcmp(kw, "is-symbol")) {
      s->is_symbol = 1;
    } else if (!strcmp(kw, "is-pattern")) {
      s->is_pattern = 1;
    } else if (!strcmp(kw, "is-external")) {
      s->is_external = 1;
    } else if (!strcmp(kw, "is-this")) {
      s->is_this = 1;
    } else if (!strcmp(kw, "is-fake")) {
      s->is_fake = 1;
    } else if (!strcmp(kw, "is-builtin")) {
      s->is_builtin = 1;
    } else if (!strcmp(kw, "is-read-only")) {
      s->is_read_only = 1;
    } else if (!strcmp(kw, "intent")) {
      s->intent = parse_intent(L);
    } else if (!strcmp(kw, "nesting-depth")) {
      if (L.t.kind != TOK_INT) parse_err(L, "expected integer");
      else { s->nesting_depth = (int)L.t.ival; next_token(L); }
    } else if (!strcmp(kw, "size")) {
      if (L.t.kind != TOK_INT) parse_err(L, "expected integer");
      else { s->size = (unsigned)L.t.ival; next_token(L); }
    } else if (!strcmp(kw, "alignment")) {
      if (L.t.kind != TOK_INT) parse_err(L, "expected integer");
      else { s->alignment = (unsigned)L.t.ival; next_token(L); }
    } else if (!strcmp(kw, "constant")) {
      if (L.t.kind != TOK_STRING) parse_err(L, "expected string");
      else { s->constant = L.t.text; s->is_constant = 1; next_token(L); }
    } else if (!strcmp(kw, "immediate")) {
      if (!expect(L, TOK_LPAREN, "'(' to open immediate")) continue;
      parse_immediate(L, s);
    } else if (!strcmp(kw, "has")) {
      if (!expect(L, TOK_LPAREN, "'(' to open :has list")) continue;
      parse_ref_list(L, s->has);
    } else if (!strcmp(kw, "kind")) {
      s->type_kind = parse_type_kind_word(L);
    } else if (!strcmp(kw, "ret")) {
      s->ret = parse_ref(L);
    } else if (!strcmp(kw, "cont")) {
      s->cont = parse_ref(L);
    } else if (!strcmp(kw, "alias")) {
      s->alias = parse_ref(L);
    } else if (!strcmp(kw, "element")) {
      s->element = parse_ref(L);
    } else {
      parse_err(L, "unknown attribute :%s", kw);
      // Skip one value to attempt recovery.
      if (L.t.kind != TOK_KEYWORD && L.t.kind != TOK_RPAREN) next_token(L);
    }
  }
}

static void parse_sym_decl(Lex &L) {
  // already consumed `(sym`
  if (L.t.kind != TOK_REF_USER) { parse_err(L, "expected %%name after 'sym'"); return; }
  cchar *name = L.t.text;
  next_token(L);
  Sym *s = get_or_make_user_sym(name);
  parse_sym_attrs(L, s);
  expect(L, TOK_RPAREN, "')' to close sym");
}

static void parse_type_decl(Lex &L) {
  // already consumed `(type`
  if (L.t.kind != TOK_REF_USER) { parse_err(L, "expected %%name after 'type'"); return; }
  cchar *name = L.t.text;
  next_token(L);
  Sym *s = get_or_make_user_sym(name);
  // Default kind: parse_sym_attrs may set :kind explicitly. If not, leave as Type_NONE.
  parse_sym_attrs(L, s);
  if (!s->type_kind) {
    // type decls default to RECORD if no :kind was given.
    s->type_kind = Type_RECORD;
  }
  expect(L, TOK_RPAREN, "')' to close type");
}

// ---------------------------------------------------------------------------
// Function body
// ---------------------------------------------------------------------------

// Look up or allocate a label in the current function's label scope.
static Label *get_or_make_label(cchar *name) {
  Label *l = g_labels.get(name);
  if (l) return l;
  l = if1_alloc_label(if1);
  g_labels.put(name, l);
  return l;
}

// Parse one code form starting at the opening '(' (which has been consumed).
// Returns the head word for dispatch.
static cchar *parse_form_head(Lex &L) {
  if (L.t.kind != TOK_BAREWORD) {
    parse_err(L, "expected form head (move, send, label, goto, if, seq, ...)");
    return NULL;
  }
  cchar *head = L.t.text;
  next_token(L);
  return head;
}

// Forward decl.
static void parse_body_forms(Lex &L, Code **body);

static void parse_move(Lex &L, Code **body) {
  Sym *src = parse_ref(L);
  Sym *dst = parse_ref(L);
  expect(L, TOK_RPAREN, "')' to close move");
  if (src && dst) if1_move(if1, body, src, dst);
}

// Parse (send F A1 A2 ... => R1 R2 ...). The `=>` is a bareword.
static void parse_send(Lex &L, Code **body) {
  Code *send = if1_send1(if1, body);
  // rvals
  while (L.t.kind == TOK_REF_USER || L.t.kind == TOK_REF_BLT || L.t.kind == TOK_REF_SYM) {
    Sym *r = parse_ref(L);
    if (r) if1_add_send_arg(if1, send, r);
  }
  // optional =>
  if (L.t.kind == TOK_BAREWORD && !strcmp(L.t.text, "=>")) {
    next_token(L);
    while (L.t.kind == TOK_REF_USER || L.t.kind == TOK_REF_BLT || L.t.kind == TOK_REF_SYM) {
      Sym *r = parse_ref(L);
      if (r) if1_add_send_result(if1, send, r);
    }
  }
  expect(L, TOK_RPAREN, "')' to close send");
}

static void parse_label_form(Lex &L, Code **body) {
  if (L.t.kind != TOK_REF_USER) { parse_err(L, "expected %%label-name"); return; }
  cchar *name = L.t.text;
  next_token(L);
  Label *l = get_or_make_label(name);
  if1_label(if1, body, NULL, l);
  expect(L, TOK_RPAREN, "')' to close label");
}

static void parse_goto_form(Lex &L, Code **body) {
  if (L.t.kind != TOK_REF_USER) { parse_err(L, "expected %%label-name"); return; }
  cchar *name = L.t.text;
  next_token(L);
  Label *l = get_or_make_label(name);
  if1_goto(if1, body, l);
  expect(L, TOK_RPAREN, "')' to close goto");
}

static void parse_if_form(Lex &L, Code **body) {
  Sym *cond = parse_ref(L);
  if (L.t.kind != TOK_REF_USER) { parse_err(L, "expected %%true-label"); return; }
  cchar *t_name = L.t.text;
  next_token(L);
  if (L.t.kind != TOK_REF_USER) { parse_err(L, "expected %%false-label"); return; }
  cchar *f_name = L.t.text;
  next_token(L);
  expect(L, TOK_RPAREN, "')' to close if");
  if (!cond) return;
  Code *ifcode = if1_if_goto(if1, body, cond);
  if1_if_label_true(if1, ifcode, get_or_make_label(t_name));
  if1_if_label_false(if1, ifcode, get_or_make_label(f_name));
}

// Recognised form heads dispatch here.
static void parse_one_form(Lex &L, Code **body) {
  // already consumed '('
  cchar *head = parse_form_head(L);
  if (!head) {
    // Skip until matching ')'.
    int depth = 1;
    while (depth > 0 && L.t.kind != TOK_EOF) {
      if (L.t.kind == TOK_LPAREN) depth++;
      else if (L.t.kind == TOK_RPAREN) depth--;
      if (depth > 0) next_token(L);
    }
    next_token(L);
    return;
  }
  if (!strcmp(head, "move")) parse_move(L, body);
  else if (!strcmp(head, "send")) parse_send(L, body);
  else if (!strcmp(head, "label")) parse_label_form(L, body);
  else if (!strcmp(head, "goto")) parse_goto_form(L, body);
  else if (!strcmp(head, "if")) parse_if_form(L, body);
  else if (!strcmp(head, "seq")) {
    Code *sub = NULL;
    parse_body_forms(L, &sub);
    expect(L, TOK_RPAREN, "')' to close seq");
    if (sub) if1_seq(if1, body, sub);
  } else {
    parse_err(L, "unknown body form '%s'", head);
    int depth = 1;
    while (depth > 0 && L.t.kind != TOK_EOF) {
      if (L.t.kind == TOK_LPAREN) depth++;
      else if (L.t.kind == TOK_RPAREN) depth--;
      if (depth > 0) next_token(L);
    }
    next_token(L);
  }
}

static void parse_body_forms(Lex &L, Code **body) {
  while (L.t.kind == TOK_LPAREN) {
    next_token(L);  // consume (
    parse_one_form(L, body);
  }
}

// ---------------------------------------------------------------------------
// Top-level: (fun NAME :args (...) :rets (...) :body ...)
// ---------------------------------------------------------------------------

static void parse_fun_decl(Lex &L) {
  // already consumed `(fun`
  if (L.t.kind != TOK_REF_USER) { parse_err(L, "expected %%name after 'fun'"); return; }
  cchar *name = L.t.text;
  next_token(L);

  Sym *f = get_or_make_user_sym(name);
  f->is_fun = 1;
  f->type = f;        // function Syms are their own type per ast.cc convention
  f->meta_type = f;

  Vec<Sym *> args, rets;
  Code *body = NULL;

  g_labels.clear();  // fresh label scope per function

  while (L.t.kind == TOK_KEYWORD) {
    cchar *kw = L.t.text;
    next_token(L);
    if (!strcmp(kw, "args")) {
      if (!expect(L, TOK_LPAREN, "'(' to open :args")) continue;
      parse_ref_list(L, args);
    } else if (!strcmp(kw, "rets")) {
      if (!expect(L, TOK_LPAREN, "'(' to open :rets")) continue;
      parse_ref_list(L, rets);
    } else if (!strcmp(kw, "cont")) {
      f->cont = parse_ref(L);
    } else if (!strcmp(kw, "body")) {
      parse_body_forms(L, &body);
    } else if (!strcmp(kw, "in")) {
      f->in = parse_ref(L);
    } else if (!strcmp(kw, "nesting-depth")) {
      if (L.t.kind != TOK_INT) parse_err(L, "expected integer");
      else { f->nesting_depth = (int)L.t.ival; next_token(L); }
    } else {
      parse_err(L, "unknown fun attribute :%s", kw);
      if (L.t.kind != TOK_KEYWORD && L.t.kind != TOK_RPAREN) next_token(L);
    }
  }

  // Args populate f->has via if1_closure(...) below; don't duplicate here.
  if (rets.n >= 1) f->ret = rets[0];

  if (!body) {
    // empty body — make a NOP group so if1_closure has something
    body = new Code(Code_SUB);
  }
  Sym *arg_arr_local[16];
  Sym **arg_arr = arg_arr_local;
  if (args.n > (int)(sizeof(arg_arr_local) / sizeof(Sym *))) {
    arg_arr = (Sym **)MALLOC(args.n * sizeof(Sym *));
  }
  for (int i = 0; i < args.n; i++) arg_arr[i] = args[i];
  if1_closure(if1, f, body, args.n, arg_arr);

  expect(L, TOK_RPAREN, "')' to close fun");
}

// ---------------------------------------------------------------------------
// Top-level: (entry %name) / (import @blt as %local)
// ---------------------------------------------------------------------------

static void parse_entry_decl(Lex &L) {
  if (L.t.kind != TOK_REF_USER) { parse_err(L, "expected %%name after 'entry'"); return; }
  cchar *name = L.t.text;
  next_token(L);
  expect(L, TOK_RPAREN, "')' to close entry");
  Sym *s = g_user_syms.get(name);
  if (!s) { parse_err(L, "entry %%%s not declared", name); return; }
  if1->top = s;
}

static void parse_import_decl(Lex &L) {
  // (import @blt as %local)
  if (L.t.kind != TOK_REF_BLT) { parse_err(L, "expected @builtin after 'import'"); return; }
  cchar *blt_name = L.t.text;
  next_token(L);
  Sym *blt = if1_get_builtin(if1, blt_name);
  if (!blt) { parse_err(L, "unknown builtin @%s", blt_name); return; }
  if (L.t.kind != TOK_BAREWORD || strcmp(L.t.text, "as") != 0) {
    parse_err(L, "expected 'as' after builtin");
    return;
  }
  next_token(L);
  if (L.t.kind != TOK_REF_USER) { parse_err(L, "expected %%local-name"); return; }
  cchar *local = L.t.text;
  next_token(L);
  expect(L, TOK_RPAREN, "')' to close import");
  // Alias: the local %name maps to the builtin Sym.
  g_user_syms.put(local, blt);
}

// ---------------------------------------------------------------------------
// Top-level dispatch
// ---------------------------------------------------------------------------

static void parse_top_decl(Lex &L) {
  if (!expect(L, TOK_LPAREN, "'(' to open declaration")) return;
  cchar *head = parse_form_head(L);
  if (!head) return;
  if (!strcmp(head, "sym")) parse_sym_decl(L);
  else if (!strcmp(head, "type")) parse_type_decl(L);
  else if (!strcmp(head, "fun")) parse_fun_decl(L);
  else if (!strcmp(head, "entry")) parse_entry_decl(L);
  else if (!strcmp(head, "import")) parse_import_decl(L);
  else {
    parse_err(L, "unknown top-level declaration '%s'", head);
    int depth = 1;
    while (depth > 0 && L.t.kind != TOK_EOF) {
      if (L.t.kind == TOK_LPAREN) depth++;
      else if (L.t.kind == TOK_RPAREN) depth--;
      if (depth > 0) next_token(L);
    }
    if (L.t.kind == TOK_RPAREN) next_token(L);
  }
}

// ---------------------------------------------------------------------------
// Public entry points
// ---------------------------------------------------------------------------

int parse_ir_string(cchar *source, cchar *fake_filename) {
  if (!if1) {
    fprintf(stderr, "parse_ir: ifa_init() must be called first\n");
    return -1;
  }
  Lex L;
  lex_init(L, fake_filename, source);
  while (L.t.kind != TOK_EOF) {
    parse_top_decl(L);
  }
  return L.errors ? -1 : 0;
}

int parse_ir_file(cchar *filename) {
  FILE *fp = fopen(filename, "r");
  if (!fp) {
    fprintf(stderr, "parse_ir: cannot open '%s'\n", filename);
    return -1;
  }
  fseek(fp, 0, SEEK_END);
  long sz = ftell(fp);
  fseek(fp, 0, SEEK_SET);
  char *buf = (char *)MALLOC(sz + 1);
  fread(buf, 1, sz, fp);
  buf[sz] = 0;
  fclose(fp);
  return parse_ir_string(buf, filename);
}
