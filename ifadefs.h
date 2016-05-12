/* -*-Mode: c++;-*-
   Copyright (c) 2003-2008 John Plevyak, All Rights Reserved
*/
#ifndef _ifadefs_H_
#define _ifadefs_H_

#include "plib.h"

typedef float float32;
typedef double float64;
typedef long double float128;
typedef struct {
  float32 r;
  float32 i;
} complex32;
typedef struct {
  float64 r;
  float64 i;
} complex64;
typedef struct {
  float128 r;
  float128 i;
} complex128;

void ifa_version(char *);

EXTERN char system_dir[512] EXTERN_INIT(".");
EXTERN int parser_verbose_non_prelude EXTERN_INIT(0);
EXTERN int codegen_optimize EXTERN_INIT(0);
EXTERN int codegen_debug EXTERN_INIT(0);

#include "ast.h"
#include "builtin.h"
#include "cg.h"
#include "clone.h"
#include "fa.h"
#include "fail.h"
#include "fun.h"
#include "if1.h"
#include "ifalog.h"
#include "pdb.h"
#include "pnode.h"
#include "var.h"

#endif
