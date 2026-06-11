// SPDX-License-Identifier: BSD-3-Clause
//
// See `codegen_common.h` for the header-level contract.

#include "ifadefs.h"

#include "codegen_common.h"
#include "fail.h"
#include "fun.h"
#include "pnode.h"
#include "sym.h"
#include "var.h"

#include <spawn.h>
#include <sys/wait.h>
extern char **environ;

// -------------------------------------------------------------
// Type-name strings
// -------------------------------------------------------------

cchar *c_type(Var *v) {
  if (!v->type || !v->type->cg_string) return "_CG_void";
  return v->type->cg_string;
}

cchar *c_type(Sym *s) {
  if (!s->type || !s->type->cg_string) return "_CG_void";
  return s->type->cg_string;
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

int is_closure_var(Var *v) {
  Sym *t = v->type;
  return (t && t->type_kind == Type_FUN && !t->fun && t->has.n);
}

Fun *get_target_fun_core(PNode *n, Fun *f) {
  Vec<Fun *> *fns = f->calls.get(n);
  if (!fns || fns->n != 1) return nullptr;
  return fns->v[0];
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
    f->cg_string = dupstr(s);
    snprintf(s, sizeof(s), "_CG_pf%d", f_index);
    f->cg_structural_string = dupstr(s);
    f->sym->cg_string = f->cg_structural_string;
    f_index++;
    if (globals && f->sym->var) globals->set_add(f->sym->var);
  }
}

void assign_type_cg_strings_pass1(Vec<Sym *> &allsyms, FILE *fp) {
  for (Sym *s : allsyms) {
    if (s->num_kind) {
      s->cg_string = num_string(s);
    } else if (s->is_symbol) {
      s->cg_string = "_CG_symbol";
    } else if (!s->cg_string) {
      switch (s->type_kind) {
        default:
          s->cg_string = dupstr("_CG_any");
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
            s->cg_string = dupstr(ss);
          } else {
            s->cg_string = "_CG_void";
          }
          break;
        }
      }
    }
  }
}

void assign_type_cg_strings_pass2(Vec<Sym *> &allsyms) {
  for (Sym *s : allsyms) {
    if (s->fun) {
      s->cg_string = s->fun->cg_structural_string;
    } else if (s->is_symbol) {
      s->cg_string = sym_symbol->cg_string;
    }
    if (s->type_kind == Type_SUM && s->has.n == 2) {
      if (s->has[0] == sym_nil_type)
        s->cg_string = s->has[1]->cg_string;
      else if (s->has[1] == sym_nil_type)
        s->cg_string = s->has[0]->cg_string;
    }
  }
}
