/* -*-Mode: c++;-*-
   Copyright (c) 1994-2008 John Plevyak, All Rights Reserved
*/
#include <signal.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "ifadefs.h"
#include "ast.h"
#include "if1.h"
#include "var.h"
#include "pnode.h"
#include "fail.h"

int
show_error(cchar *str, IFAAST *a, ...) {
  char nstr[1024];
  va_list ap;
  va_start(ap, a);
  snprintf(nstr, 1023, "%s:%d: %s\n", a->pathname(), a->line(), str);
  vfprintf(stderr, nstr, ap);
  va_end(ap);
  return -1;
}

int
show_error(cchar *str, Var *v, ...) {
  char nstr[1024];
  va_list ap;
  va_start(ap, v);
  if (v->sym->ast)
    snprintf(nstr, 1023, "%s:%d: %s\n", v->sym->pathname(), v->sym->line(), str);
  else if (v->def && v->def->code && v->def->code->ast)
    snprintf(nstr, 1023, "%s:%d: %s\n", v->def->code->pathname(), v->def->code->line(), str);
  else
    snprintf(nstr, 1023, "error: %s\n", str);
  vfprintf(stderr, nstr, ap);
  va_end(ap);
  return -1;
}

char *
get_file_line(cchar *filename, int lineno) {
  static char *last_filename = 0;
  static char *last_buf = 0;
  static Vec<char *> last_lines;

  if (!last_filename || strcmp(filename, last_filename)) {
    int len = 0;
    char *new_buf = 0;
    if (buf_read(filename, &new_buf, &len) < 0)
      return 0;
    last_filename = dupstr(filename);
    last_buf = new_buf;
    char *b = new_buf;
    last_lines.clear();
    last_lines.add(b);
    b = strchr(b, '\n');
    while (b) {
      *b = 0;
      b++;
      last_lines.add(b);
      b = strchr(b, '\n');
    }
  }
  lineno--; // 0 based
  if (lineno < 0 || lineno > last_lines.n)
    return NULL;
  return last_lines[lineno];
}

int 
myassert(cchar *file, int line, cchar *str) {
  printf("assert %s:%d: %s\n", file, line, str);
  *(int*)0 = 1;
 return 0;
}

