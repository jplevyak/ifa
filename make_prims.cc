/* -*-Mode: c++;-*-
   Copyright (c) 2003-2010 John Plevyak, All Rights Reserved
*/
#include "plib.h"

static FILE *fp = 0;
static FILE *hfp = 0;

char *catstr(char *s, char *ss) {
  int l = strlen(s) + strlen(ss) + 1;
  char *x = (char *)MALLOC(l + 1);
  strcpy(x, s);
  strcat(x, ss);
  return x;
}

struct Line {
  char *name;
  char *string;
  char *nargs;
  char *pos;
  char *nres;
  char *argtypes;
  char *rettypes;
  char *options;
  int index;
};
#define forv_Line(_x, _y) forv_Vec(Line, _x, _y)

#define EOF_TOK ((char *)(intptr_t)-1)

typedef char *charp;

char *get(charp &p, int optnum = 0) {
  while (*p && isspace(*p)) p++;
  char *s = p;
  if (optnum && (*p != '-' && !isdigit(*p))) return NULL;
  if (*s == '{') {
    while (*p && *p != '}') p++;
    if (*p) p++;
  } else if (*s == '"') {
    p++;
    while (*p && *p != '"') p++;
    if (*p) p++;
  } else
    while (*p && !isspace(*p)) p++;
  if (!*p) return EOF_TOK;
  if (*s == ';') return NULL;
  return dupstr(s, p);
}

void get_lines(char *b, Vec<Line *> &lines) {
  int index = 0;
  while (1) {
    Line *l = new Line;
    l->index = index++;
    do {
      while (*b && isspace(*b)) b++;
      if (*b != '/') break;
      while (*b && *b != '\n') b++;
    } while (*b);
    if ((l->name = get(b)) == EOF_TOK) return;
    if ((l->string = get(b)) == EOF_TOK) return;
    if ((l->nargs = get(b)) == EOF_TOK) return;
    if ((l->pos = get(b)) == EOF_TOK) return;
    if ((l->nres = get(b, 1)) == EOF_TOK) return;
    if ((l->argtypes = get(b)) == EOF_TOK) return;
    if ((l->rettypes = get(b)) == EOF_TOK) return;
    if ((l->options = get(b)) == EOF_TOK) return;
    lines.add(l);
  }
}

void declare_data(Vec<Line *> &lines) {
  forv_Line(l, lines) {
    fprintf(hfp, "extern Prim *%s;\n", l->name);
    fprintf(hfp, "#define P_%s %d\n", l->name, l->index);
  }
}

void define_data(Vec<Line *> &lines) {
  forv_Line(l, lines) fprintf(fp, "Prim *%s = 0;\n", l->name);
}

void build_data(Vec<Line *> &lines) {
  forv_Line(l, lines) {
    int nargs = 0;
    char *rets = l->nres;
    if (!rets) rets = (char *)"1";
    fprintf(fp, "  static PrimType %s_arg_types[] = %s;\n", l->name,
            l->argtypes);
    fprintf(fp, "  static PrimType %s_ret_types[] = %s;\n", l->name,
            l->rettypes);
    fprintf(fp,
            "  %s = new Prim(%d, %s, \"%s\", %s, %s, %s, %s_arg_types, "
            "%s_ret_types, %s);\n",
            l->name, l->index, l->string, l->name, l->nargs, l->pos, rets,
            l->name, l->name, l->options ? "PRIM_NON_FUNCTIONAL" : "0");
    fprintf(fp, "  n = (char*)if1->strings.put((char*)%s);\n", l->string);
    nargs = atoi(l->nargs);
    nargs -= 2;
    if (nargs < 0) nargs = 0;
    fprintf(fp, "  p->prims.add(%s);\n", l->name);
    fprintf(fp, "  p->prim_map[%d][%s].put(n, %s);\n", nargs, l->pos, l->name);
  }
}

int main(int argc, char *argv[]) {
  int i = 1, len = 0;
  char *buf = NULL;
  Vec<Line *> lines;
  cchar *fn = 0;
  if (argc < 2)
    fn = "prim_data.dat";
  else
    fn = argv[1];

  if (argc < 1 || buf_read(fn, &buf, &len) < 0) {
    printf("unable to read file '%s' %d", argv[i], argc);
    exit(-1);
  }
  get_lines(buf, lines);

  hfp = fopen("prim_data.h", "w");
  fprintf(hfp, "#ifndef _prim_data_H\n");
  fprintf(hfp, "#define _prim_data_H\n\n");
  fprintf(hfp, "class Prim;\n\n");
  declare_data(lines);
  fprintf(hfp, "#endif\n");
  fclose(hfp);

  fp = fopen("prim_data.cc", "w");
  fprintf(fp, "#include \"prim_data_incs.h\"\n\n");
  define_data(lines);
  fprintf(fp, "\nvoid prim_init(Primitives *p, IF1 *if1) {\n");
  fprintf(fp, "  char *n;\n");
  build_data(lines);
  fprintf(fp, "}\n");
  fclose(fp);
}
