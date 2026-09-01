# 121 — codegen emits function clones nothing references: liveness is computed over FA's candidate set, codegen narrows each call site to one target, and nobody reconciles the two

**Status:** **C backend FIXED 2026-09-01**; the LLVM backend still emits
them. Open on that half. Found while auditing whether `pygmy`'s final
contour set is minimal (it is not — see "What this does NOT fix").

**Affects:** `ifa/optimize/dead.cc` — `mark_live_funs` (~294);
`ifa/analysis/clone.cc` — `Fun::calls` construction (~1430);
`ifa/codegen/codegen_common.cc` — `get_target_fun_core` (~457) and
`cg_build_new_to_val_map` (~370); `ifa/codegen/cg.cc` —
`c_codegen_print_c`'s body-emission loop (~2771);
`ifa/codegen/cg_emit_llvm.cc` (unfixed).

## Symptom

`shedskin_examples/pygmy` (351 lines, one file) emitted **244 C
functions from 83 source functions**, and **39 of them (16%) appeared
nowhere in the output except their own declaration and definition** —
not at a call site, not in a table. Verified per symbol:
`_CG_f_12194_163` occurs exactly twice in 373 KB of C, at lines 210 and
7135. They are `extern`, so the C compiler must keep them.

| source function | clones | distinct bodies | never named |
|---|---|---|---|
| `parallellight::light` | 10 | **1** | **9** |
| `pointlight::light` | 10 | **1** | **9** |
| `spotshader::shade` | 7 | 1 mod clone-idx | **6** |
| `everythingshader::shade` | 7 | 1 mod clone-idx | **6** |
| `plane::intersect` | 3 | **1** | 2 |
| `sphere::intersect` | 3 | **1** | 2 |

The ten `parallellight::light` bodies are 50 lines each and identical
down to the callee clone ids, so they are not even distinguished by
devirtualization.

## Root cause

**Two notions of "reachable", never reconciled.**

`mark_live_funs` walks `Fun::calls` — FA's **candidate set** at each call
site, built in `clone.cc:1430` from every analyzed AEdge:

```cpp
for (AEdge *ee : *m) if (used_edges.set_in(ee)) vf->set_add(ee->to->fun);
```

Codegen then narrows every site to a **single** target:

- a direct call takes `get_target_fun_core`'s answer (`fns->v[0]`, or
  nullptr when the candidates' C signatures differ);
- a polymorphic call is dispatched through a method-pointer slot that
  `cg_build_new_to_val_map` fills with **one winner per
  `(constructor, slot)`** — a later registration is dropped unless it is
  strictly more specific.

Nothing recomputes liveness after that narrowing, so every candidate it
discarded is still `f->live` and is still emitted. The slot store is
where it is visible:

```c
((_CG_ps15812)t1)->e14 = (_CG_pf71)((_CG_function*)_CG_f_12194_71/*parallellight::light*/);
```

One clone reaches the slot; the other nine are emitted and unreachable.

## The fix (C backend, 2026-09-01)

`c_codegen_print_c` now emits each body into its own `open_memstream`
buffer, then writes out only what `init` transitively **names**. The
reference relation is read back out of the **emitted bytes**, scanning
for `_CG_f_<symid>_<index>`; `assign_fun_cg_strings` names the i'th live
Fun with that index, so it identifies WHICH CLONE — the whole point.
Prototypes are deliberately left alone: an unreferenced extern
declaration costs nothing, and keeping them all means this can never
produce a call to something undeclared.

**Reading the bytes was the second attempt, and the first one is worth
recording.** `cg_get_string(Fun *)` looks like the only way a function's
name can reach the output, so hooking that accessor looks like the
principled fix. It is not sufficient: `c_rhs` (`cg.cc:277`) emits a
function-valued Var through `cg_get_string(Var *)` instead. That hook
dropped 101 functions, **9 of which were still called**, and the link
failed with `undefined reference to _CG_f_11801_53` and friends. The
byte scan cannot miss an emission path, whatever a future emitter does.

## Results

| pygmy | before | after |
|---|---|---|
| emitted functions | 244 | **149** (95 dropped) |
| emitted C | 14983 lines | **6415** (−57%) |
| rendered `.ppm` | — | **byte-identical** |
| stdout | — | identical but for its own `TIME` line |

95, not 39, because DCE is transitive: dropping the 39 unreferenced ones
makes their callees unreachable in turn, to a fixed point.

**Corpus (`check`, 77 programs), against the pre-fix sweep of the same
tree — a ONE LINE diff, and it is a win:**

```
< linalg   compile_rc=1  (6 C errors)
> linalg   compile_rc=0  run_rc=134
```

`linalg` was one of the corpus's five compile failures. All six of its
errors — `no matching function for call to '_CG_list_mult_internal'` —
were inside functions this drops. **So the dead code was not only bloat;
it was breaking a compile.** Corpus compile failures 5 → 4 (chess, go,
othello3, sudoku5 remain). `linalg` still aborts at run time, which is
[102](102-corpus-programs-compile-then-abort-at-runtime.md)'s class and
unsurprising for a program that does not converge
([099](099-FA-pending-backedge-avoid-veto-forces-period-2.md)'s one
remaining subject). Everything else is byte-identical: `run_rc`,
`cpy_rc` and `stdout_match` match on all 77 programs.

Gate: `ifa --test` 58/0; `test-ir` 0 failed across all 16 phases with no
golden churn (the synthetic fixtures have no dead clones);
`test_pyc.py` 308 passed / 0 failed on both backends; `make -C ifa
test_llvm` and `make test_dparse` pass.

## What this does NOT fix

**The LLVM backend.** `cg_emit_llvm.cc` has its own emission path and
still emits the dead clones. It can reuse the same reachability once its
bodies are buffered the same way.

**Minimality.** This removes the *unreferenced* clones; it does not stop
them being CREATED. pygmy still builds 467 contours and 149 emitted
functions for 83 source functions, and of the survivors **72 have a body
byte-identical to a sibling** (105 once callee clone indices are
stripped). Ten identical contours for `parallellight::light` is a
splitter question — the `TYPE_CONFLUENCE` detach-and-mint growth that
[074](074-FA-cross-pass-oscillation-plan.md) censuses, on a two-level
class hierarchy — not a codegen one. Fixing that would make this DCE
mostly redundant; until then this keeps the excess out of the output.

## Should the pruning move EARLIER? Measured 2026-09-01: no.

The obvious follow-up is to prune before `simple_inlining` /
`mark_live_types`, or to stop `clone` creating the clones at all. Three
measurements say the first is not worth it and locate what the second
actually needs.

**1. How much dead code is there, corpus-wide?** `-m compile -e
"PYC_DBG_CGDCE=1"` over all 77 programs, 73 of which reach codegen:

```
emitted 14320   dropped 415   = 2.8% of all clones were dead
median per program: 1%      programs with zero dead clones: 2 of 73
```

| dropped | emitted | %dead | program |
|---|---|---|---|
| 99 | 430 | 19% | pygasus |
| 95 | 149 | **39%** | pygmy |
| 27 | 710 | 4% | plcfrs |
| 24 | 1014 | 2% | linalg |
| 23 | 190 | 11% | webserver |

**pygmy is an outlier, not the norm.** Moving the pruning earlier would
save the downstream passes ~3% of their input. Compile time already says
the same: pygmy went 7.10 s → ~7.0 s across this fix while its `.c`
shrank 57%, because its 43 FA passes dominate and clang was never the
bottleneck. **The value of this fix is not bulk — it is that dead code
can be WRONG code** (`linalg`'s six C errors were all inside dropped
functions), and that argument does not get better by moving it earlier.

Note the two outliers are not even the same mechanism. pygmy's excess is
user methods on a two-level class hierarchy dispatched through per-class
slots; pygasus's and webserver's is `__new__`, lambdas and **builtin
container methods** (`list::append`, `range::__init__`,
`dict::__setitem__`, `len` — 12-18 clones each), i.e. per-call-site
element specialization.

**2. Could it be done BEFORE cloning at all? No — the information does
not exist yet.** Both narrowings need concrete C types, which
`concretize_types` only produces *inside* `clone`: `get_target_fun_core`
compares C signatures, and `cg_build_new_to_val_map` resolves a slot
index per concrete class layout. Pre-clone, every one of those ten
`parallellight::light` contours is genuinely reached by a real AEdge --
FA is not wrong about them. Their deadness is created by the decision to
dispatch through one method-pointer slot per class, which is codegen's.

**3. The obvious way to stop creating them is guarded, and the guard is
load-bearing.** `ES_FN::equivalent`'s creation-point block
(`clone.cc:276-287`) ends in an **unconditional `return 0`**, which makes
the `cssyms` loop above it dead code: any pnode whose lval has a
`cs_map` splits the two contours whatever the loop just proved. That
reads exactly like the bug behind the duplicate clones. It is not --
letting the loop decide makes pygmy fail with `fail: missmatched
offsets`. The function's own header comment says why: merging also
requires that "the layouts of the '.' targets are compatible at the
symbol (same offset)", which CreationSet equivalence alone does not
establish. **The scoped task is to add that offset check so the loop can
decide**, not to delete the `return 0`.

**A dead end worth recording.** 75 of pygmy's 569 emitted struct fields
are never referenced, at 8 bytes each (`_CG_void` is `void *`) -- `vec`,
a three-double vector, carries 31 fields. That looks like dead clones
inflating field liveness, and it is not: every unreferenced one is
already `_CG_void`-typed, meaning no live Fun was ever assigned to it.
Pruning functions earlier would not remove them. That is a separate
*field* liveness question on top of
[030](030-DISPATCH-polymorphic-dispatch-fat-pointers.md)'s
per-instance method-slot design.

## Verification plan

1. `pygmy` renders a byte-identical `.ppm` — the strongest available
   check, since CPython times out on it (>120 s) and `check` mode
   therefore has no stdout oracle for it.
2. Full `make test` on both backends; the corpus `check` sweep diffed
   against the pre-fix tree. A wrongly DROPPED function shows up as a
   link failure in `compile_rc`; a wrongly kept one cannot change
   behaviour.
3. When the LLVM half lands, `PYC_FLAGS=-b` e2e plus the same sweep.

## What this unblocks

Smaller, faster-compiling output everywhere, and one corpus program that
now compiles. More usefully, it makes "is the contour set minimal?" a
question you can ask of the emitted C without 16% noise: what remains
after this is genuine over-cloning, which is
[074](074-FA-cross-pass-oscillation-plan.md)'s and
[030](030-DISPATCH-polymorphic-dispatch-fat-pointers.md)'s territory.
