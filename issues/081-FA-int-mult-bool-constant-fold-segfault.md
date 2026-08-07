# 081 — `int * bool` (or similar mixed-numeric constant-fold) segfaults the compiler itself in `add_send_edges_pnode`

**Status:** open, found 2026-08-07 while independently verifying
issue 062's closure (LLVM int/float scalar coercion) — stress-testing
mixed-numeric-type arithmetic beyond what that issue's own repro
covered turned up a genuinely separate, more severe bug: not a
verifier failure or a runtime trap, but `pyc` itself crashing during
compilation, before either backend's codegen is ever reached.

**Affects:** `ifa/analysis/fa.cc`'s `add_send_edges_pnode` (~line
2051), inside constant-folding: `update_in(res, make_constant(imm,
nt->v[0]->sym));`. Core FA — confirmed backend-agnostic (the crash
happens during `FA::analyze`/`analyze_to_convergence`, well before
`cg.cc`/`cg_emit_llvm.cc` codegen begins), so both the C and LLVM
backends are affected identically since neither is even reached.

## Repro (3 lines)

```python
b = True
n = 5
print(n * b)
```

`./pyc -D . repro.py` segfaults (`Segmentation fault (core dumped)`,
exit 139). No diagnostics, no partial output — the process dies
outright.

Confirmed minimal: `b + n` (the same two values, `__add__` instead
of `__mul__`) does **not** crash — it produces a clean, if unrelated,
warning chain (`unresolved call '__add__'`, `illegal call argument
type expression illegal: bool`/`int64`, `expression has no type`)
and compiles successfully with exit 0 (the expression itself becomes
NOTYPE and is presumably salvaged/dropped). Only `n * b` (int times
bool, `__mul__`/`__rmul__`) triggers the crash — not yet checked
whether this is `__mul__`-specific or a coincidence of which
dunder's constant-folding path happens to be reachable for this
particular pair of literal values.

## Backtrace (gdb, this build)

```
Program received signal SIGSEGV, Segmentation fault.
0x000055555572402c in add_send_edges_pnode (p=0x7fffe8f84000, es=0x7fffe88bec00) at analysis/fa.cc:2051
2051	              update_in(res, make_constant(imm, nt->v[0]->sym));
#0  add_send_edges_pnode (...) at analysis/fa.cc:2051
#1  add_pnode_constraints (...) at analysis/fa.cc:2715
#2  add_pnode_constraints (...) at analysis/fa.cc:2963
    ... (recursive add_pnode_constraints frames, ~15 deep, normal PNode-graph traversal) ...
#20 add_es_constraints (es=...) at analysis/fa.cc:2969
#21 analyze_to_convergence () at analysis/fa.cc:7105
#22 FA::analyze (this=..., top=...) at analysis/fa.cc:7134
#23 ifa_analyze (fn="repro.py") at ifa.cc:47
#24 compile (fn="repro.py") at pyc.cc:99
#25 main (argc=4, argv=...) at pyc.cc:270
```

The crash is inside a **constant-folding** attempt: `imm` (an
immediate/literal value) combined with `nt->v[0]->sym` (presumably
the *other* operand's — or the result's — concrete Sym/type) into
`make_constant(...)`, then flowed via `update_in`. Not root-caused
further than this — the exact mechanism by which combining an int
literal and a bool literal's Syms crashes `make_constant` (or
whatever it calls into) is not traced here.

## Why this matters beyond severity alone

This was found via the *exact same class* of mixed-numeric-type
scenario issue 062 (LLVM int/float scalar coercion) and issue 077
(`_CG_prim_equal`-family salvage guard) already hardened at the
*codegen* layer (both backends now degrade a genuinely-unrepresentable
mixed-type binop to a controlled runtime trap instead of a compiler
crash or bad C/LLVM IR). This bug is one layer further upstream —
inside FA's own constant-propagation, before a Send even reaches
codegen — so none of that existing hardening helps: the process is
already dead by the time any salvage-guard code would run.

## Verification plan

- The 3-line repro compiles cleanly (no crash) on both backends,
  either producing a correct constant-folded result (`5` — Python's
  `int * bool` treats `True` as `1`) or, if constant-folding this
  particular combination is out of scope, degrading gracefully
  (a warning + salvaged/untyped expression, matching `__add__`'s
  existing behavior for the same inputs) rather than crashing.
- `python3 repro.py` → `5` is the correct reference output to match
  if a real fix (not just crash-avoidance) is attempted.
- Full `test_pyc.py` both backends, `ifa --test` — this is inside a
  hot, shared FA constant-folding path (`add_send_edges_pnode`), so
  treat any new failure as a signal to narrow the fix, not special-case
  around it.
- Check whether other mixed-literal binop combinations (bool*float,
  bool-bool subtract, etc.) hit the same crash or are unaffected —
  not surveyed here, only `int * bool` was confirmed.

## What this unblocks

Any program containing a literal (or constant-folded) `int`/`bool`
mixed multiplication — plausible in real code (`count * is_active`-
style idioms) — currently cannot be compiled at all, on either
backend, with no diagnostic. This is a compiler crash on valid
Python input, not a "your program is unrepresentable" salvage case;
severity is higher than a typical salvage-guard gap since there's no
workaround available to the pyc user short of avoiding the literal
combination entirely (and they'd have no error message telling them
to).
