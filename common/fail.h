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

// Shedskin-style implicit-return handling (ifa/issues/071 fix
// option 1). Default OFF (CPython-faithful). When set, a function
// that has at least one explicit value `return` does NOT get an
// implicit `None` injected for its fall-off-the-end path -- so a
// `bool`/`int`-returning function that also falls off the end is
// typed `bool`/`int` rather than `bool | None`, which pyc cannot lay
// out in one unboxed field (chess's fatal blocker). This is a
// deliberate CPython divergence for the fall-off path (matches
// shedskin, which fills that path with the return type's default):
// code that actually reaches the end and USES the `None` gets an
// arbitrary-but-typed value instead.
EXTERN int ifa_no_implicit_none EXTERN_INIT(0);

// Lower a single-assignment module-level `NAME = lambda ...` as if it
// were `def NAME(...): return <body>` (ifa/issues/071 fix option 1,
// prototype). Default OFF. A module-level lambda bound to a name that
// is assigned exactly once is semantically a named function; binding it
// as a `def` makes calls resolve directly (via def_internal_fn) instead
// of dispatching through the name's flow-insensitive `{None, closure}`
// global cell (issue 002/031) -- whose `None` arm is what leaves chess's
// `nonpawnBlackAttacks` result a `bool | None` union pyc can't lay out
// unboxed. Skipped when the name is reassigned anywhere at module scope
// (then the `{None, closure}` variable semantics are load-bearing).
EXTERN int ifa_module_lambda_as_def EXTERN_INIT(0);

int show_error(cchar *str, IFAAST *a, ...);
int show_error(cchar *str, Var *v, ...);
cchar *get_file_line(cchar *filename, int lineno);
void show_source_caret(FILE *fp, cchar *filename, int line, int col = 0);
#define ASSERT(_x) ((_x) || myassert(__FILE__, __LINE__, #_x))
int myassert(cchar *file, int line, cchar *str);

#endif
