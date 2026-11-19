#ifndef _ifadefs_H_
#define _ifadefs_H_

#include "common.h"

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

void ifa_version(char *, int size);

EXTERN char system_dir[512] EXTERN_INIT(".");
EXTERN int parser_verbose_non_prelude EXTERN_INIT(0);
EXTERN int codegen_optimize EXTERN_INIT(0);
EXTERN int codegen_debug EXTERN_INIT(0);
// -o / --output: overrides the derived target/executable path in
// c_codegen_compile (cg.cc) and llvm_codegen_compile (llvm.cc) when
// non-empty. Empty by default -- falls back to deriving the name from
// the input filename, same as always.
EXTERN char codegen_output[512] EXTERN_INIT("");
// Extra flags for compiling and linking the generated program
// (whitespace-separated; pyc --cflags / --libs), and what a frontend adds:
// link entries for the foreign libraries a program names, and one extra C++
// translation unit. The C backend reads its copy of that source through
// IFACallbacks::c_codegen_pre_file; the LLVM backend compiles it beside the
// program's object.
EXTERN char codegen_extra_cflags[2048] EXTERN_INIT("");
EXTERN char codegen_extra_libs[2048] EXTERN_INIT("");
EXTERN Vec<cchar *> codegen_link_libs;
EXTERN cchar *codegen_extra_source EXTERN_INIT(nullptr);

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
