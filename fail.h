/* -*-Mode: c++;-*-
   Copyright (c) 1994-2008 John Plevyak, All Rights Reserved
*/
#ifndef _fail_H_
#define _fail_H_

class IFAAST;
class Var;

EXTERN int ifa_verbose EXTERN_INIT(0);
EXTERN int ifa_debug EXTERN_INIT(0);

int show_error(cchar *str, IFAAST *a, ...);
int show_error(cchar *str, Var *v, ...);
char *get_file_line(char *filename, int lineno);
#define ASSERT(_x) ((_x) || myassert(__FILE__, __LINE__, #_x))
int myassert(cchar *file, int line, cchar *str);

#endif
