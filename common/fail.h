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

// Enable the backward element-split pass (ifa/issues/072):
// on full quiescence of the split stages, seed a default
// (nil) element type into container CreationSets whose
// element was never written (a `[]`/`{}` that reaches no
// element assignment), so a downstream read / shared-method
// branch over it type-checks instead of NOTYPE-ing.  The
// pyc analog of shedskin's `emptycsites` separation +
// `empty -> <class>[nil]` seeding.  Default OFF while it is
// validated against the corpus + determinism gate.
EXTERN int ifa_empty_elem_split EXTERN_INIT(0);

int show_error(cchar *str, IFAAST *a, ...);
int show_error(cchar *str, Var *v, ...);
cchar *get_file_line(cchar *filename, int lineno);
void show_source_caret(FILE *fp, cchar *filename, int line, int col = 0);
#define ASSERT(_x) ((_x) || myassert(__FILE__, __LINE__, #_x))
int myassert(cchar *file, int line, cchar *str);

#endif
