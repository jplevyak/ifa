// SPDX-License-Identifier: BSD-3-Clause
//
// cg_normalize_v2.h — Phase B entry point for CG_IR_v2.
//
// Sibling of cg_normalize.cc / cg_normalize(FA*). Translates
// IF1 (Sym/Var/Fun/PNode/Code) into a CGv2Program that the v2
// LLVM emitter (cg_v2_emit_llvm_module) can consume.
//
// Migration plan: see CG_IR_SEMANTICS.md §6. This file is the
// "Phase B" path. It currently produces a minimal CGv2Program
// (predefined types + per-Fun declarations only). Each landing
// adds one more concept of IF1 → CGv2 translation, ratcheting
// against the pyc-suite baseline.
//
// The v1 cg_normalize() in cg_normalize.cc remains unchanged.

#ifndef _cg_normalize_v2_H_
#define _cg_normalize_v2_H_

class FA;
class CGv2Program;

// Translate IF1 (held by `fa`) into a fresh CGv2Program. The
// caller owns the returned pointer; it's GC-allocated.
// Passing nullptr returns a non-null empty program (just the
// predefined types) — same defensive contract as v1's
// cg_normalize().
CGv2Program *cg_normalize_v2(FA *fa);

#endif // _cg_normalize_v2_H_
