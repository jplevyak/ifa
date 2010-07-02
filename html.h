/* -*-Mode: c++;-*-
   Copyright (c) 2004-2008 John Plevyak, All Rights Reserved
*/
#ifndef _ifa_html_H_
#define _ifa_html_H_

#include <stdio.h>
#include "vec.h"

class Sym;
class Fun;
class FA;

void dump_html(FA *pdb, cchar *fn, char *mktree_dir);
void dump_sym_name(FILE *fp, Sym *s);
void dump_fun_list(FILE *fp, Vec<Fun *> &funs);

void dump_index_header(FILE* f);
void dump_index_footer(FILE* f);

#endif
