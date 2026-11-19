# 199 — emitted-code performance: a benchmark mode first, then dispatch tables and the tuple header

**Status: open.** Filed 2026-11-03 as the survivor of
[closed/030](closed/030-DISPATCH-polymorphic-dispatch-fat-pointers.md),
[closed/054](closed/054-CGEN-remove-unconditional-tuple-list-header.md),
[closed/190](closed/190-CODEGEN-run-time-dispatch-census.md)'s residuals and
[closed/194](closed/194-review-codegen-2026-11.md) B1–B3.

## 1. There is no baseline (prerequisite for everything else)

`ifa/codegen/PERFORMANCE.md` is pyc's own compile time on five small
tests, 2026-06-11, before `OPTIMIZE=1` was the default. The corpus sweep
records compile and run status, never time. The only emitted-performance
datum in the tree is othello3's `moves/sec` in
[186](186-FA-pass-one-confluence-through-shared-builtins.md) (80 M against
CPython's 0.77 M), seen by accident.

**Do:** `corpus_sweep.sh -m bench` over the programs whose output matches
CPython, both backends, user time per program, cached and committed like
the other TSVs. Stop condition: a repeat on an unchanged tree agrees
within noise.

Then measure, in this order, before changing anything:

- `-fno-strict-aliasing` on the C side (the price of the blind-cast
  object model; the LLVM backend emits no TBAA for the same reason);
- the if/else dispatch chains (item 2);
- the 16-byte tuple header (item 3);
- the LTO-linked runtime on the LLVM side.

## 2. Table dispatch for high fan-out (closed/030)

Every polymorphic site is an O(branches) chain of tag compares. After
closed/186 collapsed the 830-arm case to one slot call when every arm
agrees, the remaining chains are closed/190's 71 legitimate hierarchy
sites. If the benchmark shows they matter, index a per-class method table
by the tag that already exists; a fat pointer is not needed. The Concert
ancestor emitted a switch only up to 5 candidates (closed/195); use the
benchmark to pick the threshold, not that number.

## 3. The tuple header (closed/054)

Every tuple carries a `_CG_list_struct` header read only when its arity
is unioned with another. The decision whether a tuple CreationSet needs
one belongs to the representation layer ([201](201-representation-oracle.md)),
read identically by both backends. Do it when the benchmark shows the 16
bytes matter on a tuple-heavy program, or when the oracle makes the flag
free.

## 4. The 13 builtin-receiver dispatches (closed/190)

Tuple `__eq__` / `__str__` shapes (amaze 8, sunfish 1) and the rest, one
at a time, only if 1 shows cost.

## Verification

A benchmark TSV in `sweeps/`; each later change measured against it on the
same tree, with no stdout verdict change.
