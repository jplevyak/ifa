/* -*-Mode: c++;-*-
   Copyright (c) 2004-2008 John Plevyak, All Rights Reserved
*/
#ifndef _IF1_h_
#define _IF1_h_

#include "sym.h"
#include "code.h"
#include "num.h"

class Scope;
class Primitives;
class IFACallbacks;

extern cchar *builtin_strings[];

class IF1 : public gc {
 public:
  StringChainHash<>     strings;                // unique strings
  Map<cchar *, Sym *>   symbols;                // language level symbols
  Map<cchar *, Sym *>   builtins;               // symbols builtin to the compiler
  Map<Sym *, cchar *>   builtins_names;         // names of symbols builtin to the compiler
  HashMap<Immediate *, ImmHashFns, Sym *> constants;        // unique constants (by string)
  Vec<Sym *>            allsyms;
  Vec<Label *>          alllabels; 
  Vec<Sym *>            allclosures;
  Sym                   *int_types[IF1_INT_TYPE_NUM][2]; // 0 = unsigned, 1 = signed
  Sym                   *float_types[IF1_FLOAT_TYPE_NUM];
  Sym                   *complex_types[IF1_FLOAT_TYPE_NUM];
  Sym                   *top;                   // main function
  Primitives            *primitives;
  IFACallbacks          *callback;
  int                   pointer_size;           // defaults to sizeof(void *)
  int                   pointer_alignment;      // defaults to __alignof__(void *)
  Partial_kind          partial_default;
  
  IF1();
};

Sym     *if1_register_sym(IF1 *p, Sym *, cchar *name = 0);
Sym     *if1_const(IF1 *p, Sym *typ, cchar *value, Immediate *imm = 0, Sym *asym = 0);
Sym     *if1_make_symbol(IF1 *p, cchar *name, cchar *end = 0);
void    if1_set_builtin(IF1 *p, Sym *s, cchar *name, cchar *end = 0);
Sym     *if1_get_builtin(IF1 *p, cchar *name, cchar *end = 0);

Code    *if1_nop(IF1 *p, Code **c);
void    if1_gen(IF1 *p, Code **t, Code *a);
void    if1_seq(IF1 *p, Code **t, Code *a);
void    if1_conc(IF1 *p, Code **t, Code *a);
Code    *if1_move(IF1 *p, Code **c, Sym *a, Sym *b, IFAAST *ast = 0);
Code    *if1_goto(IF1 *p, Code **t, Label *label = 0);
Label   *if1_alloc_label(IF1 *p);
Label   *if1_label(IF1 *p, Code **code, IFAAST *ast = 0, Label *l = 0);
Sym     *if1_operator(IF1 *p, Code **t, Sym *a1, Sym *a2, Sym *a3); 
Code    *if1_send(IF1 *p, Code **c, int args, int results, ...);
Code    *if1_send1(IF1 *p, Code **c, IFAAST *ast = 0);
void    if1_add_send_arg(IF1 *p, Code *c, Sym *a, cchar *name = 0);
void    if1_add_send_result(IF1 *p, Code *c, Sym *r);
Code    *if1_if_goto(IF1 *p, Code **t, Sym *ifcond, IFAAST *ast = 0);
void    if1_set_goto(IF1 *p, Code *go, Label *label);
void    if1_if_label_true(IF1 *p, Code *ifcode, Label *l, IFAAST *ast = 0);
void    if1_if_label_false(IF1 *p, Code *ifcode, Label *l, IFAAST *ast = 0);
Code    *if1_if(IF1 *p, Code **t, 
                Code *ifcond, Sym *ifcond_var,
                Code *ifif, Sym *if_var, 
                Code *ifthen, Sym *then_var, 
                Sym *result, IFAAST *ast = 0);
Code    *if1_loop(IF1 *p, Code **t, 
                  Label *cont, Label *brk,
                  Sym *cond_var, Code *before, 
                  Code *cond, Code *after, Code *body, IFAAST *ast = 0);
Sym     *if1_closure(IF1 *p, Sym *f, Code *c, int nargs, Sym **args);

void    if1_set_int_type(IF1 *p, Sym *t, int signd, int size);
void    if1_set_float_type(IF1 *p, Sym *t, int size);
void    if1_set_complex_type(IF1 *p, Sym *t, int size);
int     if1_numeric_size(IF1 *p, Sym *t);
int     if1_numeric_alignment(IF1 *p, Sym *t);
cchar   *if1_cannonicalize_string(IF1 *p, cchar *start, cchar *end = 0);
void    if1_finalize_closure(IF1 *p, Sym *c);

void    if1_write(FILE *fp, IF1 *p, int start = 0);
void    if1_write_log();

void    if1_finalize(IF1 *p);
void    if1_set_primitive_types(IF1 *if1);

// debugging
void    if1_dump(FILE *fp, Code *c);
void    if1_dump_code(FILE *fp, Code *code, int indent);
void    if1_dump_sym(FILE *fp, Sym *sym);

int     is_functional(IF1 *p, Code *code);

extern IF1 *if1;

static inline Sym *new_Sym(cchar *name = 0) { return if1->callback->new_Sym(name); }

void print_syms(FILE *fp, Vec<Sym *> *syms);
void print_code(FILE *fp, Code *code, int indent, int lf = 1);

#endif
