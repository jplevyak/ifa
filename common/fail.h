#ifndef _fail_H_
#define _fail_H_

class IFAAST;
class Var;

EXTERN int ifa_verbose EXTERN_INIT(0);
EXTERN int ifa_debug EXTERN_INIT(0);

// Escape integration into IFA — Phase 1+ (see
// ESCAPE_PLAN.md).  When set, IFA propagates a per-AVar
// EscapeStatus lattice and writes the per-formal result back
// onto Fun::arg_escapes; codegen consumes those instead of
// running the post-IFA Stage 3 pass.  Default off so the
// existing Stage 3 path remains the production behavior
// while the integration is phased in.
EXTERN int ifa_escape_in_fa EXTERN_INIT(0);

// Run simple_inlining between FA convergence passes
// (experimental).  Default off — production runs
// simple_inlining once post-FA via ifa_optimize().  When
// set, FA::analyze converges, runs mark_live_funs +
// simple_inlining, resets per-ES live_pnode sets, then
// re-converges.  Lets the second FA pass benefit from
// elided identity wrappers.  See
// `ifa/issues/026-recursive-self-mutation-struct-collapse.md`
// and the discussion in `ifa/analysis/NOTES.md`.
EXTERN int ifa_fa_inline EXTERN_INIT(1);

// Enable the issue-025 per-branch type narrowing
// recognizer at Code_IF.  Default on (production
// behavior).  Set to 0 to compare FA precision with /
// without narrowing in isolation — useful for
// distinguishing whether a precision win comes from
// narrowing, mid-FA inlining, or both.
EXTERN int ifa_narrow EXTERN_INIT(1);

int show_error(cchar *str, IFAAST *a, ...);
int show_error(cchar *str, Var *v, ...);
char *get_file_line(char *filename, int lineno);
#define ASSERT(_x) ((_x) || myassert(__FILE__, __LINE__, #_x))
int myassert(cchar *file, int line, cchar *str);

#endif
