# 121 — codegen emits clones nothing references (LLVM half) and narrows call sites without checking equivalence

**Status:** open. The C backend was fixed 2026-09-01. The LLVM half and
one soundness hazard remain. Rewritten 2026-09-28; history in git:
`git show 3f36072b:ifa/issues/121-CGEN-dead-clones-emitted.md`.

## Root cause

There are two notions of reachable, never reconciled. `mark_live_funs`
walks `Fun::calls`, FA's CANDIDATE set per call site. Codegen then
narrows each site to ONE target: `get_target_fun_core` returns
`fns->v[0]` when the candidates' C signatures agree, and
`cg_build_new_to_val_map` installs one winner per `(constructor, slot)`.
Nothing recomputes liveness afterwards, so every discarded candidate is
still emitted. On pygmy that was 39 of 244 functions, named nowhere.

## Fixed (C backend)

`c_codegen_print_c` buffers each body and emits only what `init`
transitively NAMES, reading references back out of the emitted bytes
(`_CG_f_<symid>_<index>` names the exact clone). Hooking
`cg_get_string(Fun *)` instead is NOT sufficient: `c_rhs` names
function-valued Vars through `cg_get_string(Var *)`, and that attempt
dropped 9 live functions. pygmy's C went down 57% with an identical
image, and linalg's six C errors were all inside dead functions. Dead code
can be WRONG code, and that, not the size, is the value. Corpus-wide only
2.8% of clones were dead, so moving the pruning before `clone` is not
worth it (the narrowing needs concrete C types, which only `clone`
produces). Typeless struct placeholders are now zero-width (`char
eN[0]`), keeping the `eN` numbering dense.

## Open

1. **LLVM backend** (`cg_emit_llvm.cc`) still emits dead clones. Buffer
   its bodies the same way and reuse the reachability.
2. **The narrowing assumes equivalence it never checks.**
   `get_target_fun_core` picks `fns->v[0]` whenever the C signatures
   agree. That is sound only if the candidates are INTERCHANGEABLE. In
   2026-09 FA's stage 5 produced per-CreationSet contours of
   `tuple.__getitem__` that each folded a DIFFERENT literal, and the call
   site bound to one of them (`test_heapq` printed "medium" for every
   item). The source is removed (stage 5 now declines when the receiver
   CSs are identical by type; see [134](134-remove-the-frontend-forced-split-opt-in.md)),
   but the narrowing is unchanged. It should require the candidates to be
   clone-equivalent (`ES_FN::equivalent`), or dispatch. It must never
   pick silently.
3. **Creation, not emission, is the real excess:** 72 of pygmy's
   surviving functions are byte-identical to a sibling. That is contour
   minimality, [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md).
   `ES_FN::equivalent`'s unconditional `return 0` in the creation-point
   block is load-bearing (removing it gives `missmatched offsets`), so do
   not "fix" it in isolation.

## Verification

LLVM: the emitted functions equal those the C backend emits, program by
program, with identical output. Narrowing: a fixture where two candidates
agree on the C signature but differ in behaviour (per-literal folds) is
refused or dispatched, never bound to one clone.
