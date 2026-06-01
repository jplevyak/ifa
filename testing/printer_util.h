// SPDX-License-Identifier: BSD-3-Clause
// Shared utilities for phase printers (testing/print_*.cc) and the .ir
// writer. Lives outside any single printer so the same Sym → name
// mapping is reused across the writer (write_ir) and each phase's
// printer; otherwise a `(send %a %b)` line in one printer's output
// might disagree with the symbol name in the .expected dump.
#pragma once

#include "common.h"
#include <stdio.h>

class IF1;
class Sym;
class Label;
class Code;
class Fun;
class PNode;

// Assigns a print-time name (no sigil) to every Sym in IF1::allsyms.
//
// Strategy:
//   - Builtin Syms              → name registered in if1->builtins_names
//   - is_symbol Syms with name  → that name (printed with '#' sigil)
//   - is_constant Syms          → "c<N>" (per-section counter)
//   - Other Syms with name      → name, de-duplicated by suffix
//   - Anonymous Syms            → "t<N>" (per-section counter)
//
// Sigils ('%', '@', '#') are emitted by print_ref(), driven by the Sym's
// flags rather than by NameAssigner state.
struct NameAssigner {
  Map<Sym *, cchar *> name_of;
  Map<cchar *, int> name_counts;

  cchar *make_unique(cchar *base);
  void assign_all(IF1 *p);
  // Emit a reference: "%name", "@name", or "#name" depending on Sym kind.
  void print_ref(FILE *fp, Sym *s);
};

// Assigns L0, L1, ... in DFS pre-order through a Code tree. Each Fun's
// CFG/SSU printer wants stable label names that don't depend on
// Label::id (which counts globally across all loaded modules).
struct LabelNames {
  Map<Label *, cchar *> names;
  void assign(Code *root);
  void walk(Code *c, int &counter);
  cchar *get(Label *l);  // returns a fallback for unseen labels
};

// Assigns p0, p1, ... to PNodes in DFS pre-order from f->entry,
// following cfg_succ (true branch before false for IF). PNodes
// unreachable from entry are not named.
void assign_pnode_names(Fun *f, Map<PNode *, cchar *> &names);
