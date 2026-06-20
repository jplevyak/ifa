#ifndef _escape_h_
#define _escape_h_

class FA;

// Run the local (intra-procedural) escape analysis Phase 2
// (see ESCAPE_PLAN.md).  For each EntrySet, applies per-Code
// transfer functions to refine the per-AVar escape status,
// then projects the result onto Fun::arg_escapes (per-formal).
//
// Phase 2 is single-function: every CALL conservatively
// escapes its args (Phase 3 will pull in the callee's
// arg_escapes).  Fresh allocations from prim_make / prim_new /
// prim_clone start at ES_NoEscape; uses pull them up to
// ES_Escape.
//
// No-op when ifa_escape_in_fa==0 (the Stage 3 codegen pass
// remains the production path until Phase 5).
void compute_escape(FA *fa);

#endif
