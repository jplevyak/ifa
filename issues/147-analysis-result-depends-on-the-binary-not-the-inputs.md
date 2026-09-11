# ifa/147: the analysis result depends on the BINARY, not only on its inputs

**Adding provably dead, `getenv`-gated diagnostic code to `fa.cc` changes
which corpus programs compile.** Measured 2026-09-10, at the DEFAULT arm,
with no flags:

| tree | `linalg` | `sudoku5` |
| --- | --- | --- |
| `b847e122` | compiles (rc=0) | compiles (rc=0) |
| `b847e122` + probes only | **fails (rc=1)** | **fails (rc=1)** |

The "+ probes only" diff contains nothing but `static void dbg_*` helpers
whose first statement is a `getenv` check, calls to them, and one unused
typedef. No transfer function, no splitter, no key, no ordering, no
default changed. Whole-corpus: `compile_fail` 2 -> 4 at the default arm
and 2 -> 6 at the `PYC_CSDCPA1=2` arm.

## Why this is the most important open issue

Every conclusion this project draws about FA is drawn from
`corpus_sweep.sh`, at a resolution of one or two programs. If a build
perturbation moves two programs, then:

- a one-program change between two sweeps is **not evidence** about the
  change under test;
- "this program passed before and fails now, therefore my change broke
  it" is invalid, and so is its converse;
- a mechanism invented to explain such a movement is an explanation of
  noise.

That happened four times in one session (2026-09-10), and the four
commits it produced were reverted: `9f321c89`, `39079fd9`, `e3c76a51`,
`3f0cb228`. Each attributed a single-program corpus movement to the
change under test and built a root-cause story on it — `sudoku5`'s
"accident at pass 27", `plcfrs`'s "relocated demand", `go`, `linalg`.
None of those attributions can stand, because a build with no semantic
difference at all moves the same programs.

## The likely mechanism, and why it is not yet proven

This codebase hashes on POINTERS in several places
(`combine_hash((uintptr_t)a, (uintptr_t)b)` over `AVar *`, `AEdge *`,
`CreationSet *`, `MPosition *`). Bucket order then follows allocation
addresses, so iteration order over any such map depends on the
allocator's behaviour, which depends on code size and layout. issue 035
records this exact family: "bucket order set the AVar id-assignment order
and made every downstream qsort_by_id canonicalization run-dependent".
That one was fixed at a single site (`form_MPositionAVar` over an edge's
arg positions) by imposing a canonical order.

**Not proven**, and the bisection actively resists a simple story:

- a dead `static volatile int` plus a never-taken early return: NO change;
- an extra per-walk `Map<AVar *, AEdge *>` with `put`/`get` on every node:
  NO change;
- a gated-off walk restriction alone: NO change;
- a gated-off call-site fan alone: NO change;
- BOTH of the last two together: `deepcopy_recursive_nested_growth` flips
  from `CONVERGED=0` to `CONVERGED=1`.

So it is not "any perturbation moves it". Some specific layouts are bad.
That is consistent with an address-order dependence and also with several
other causes, and it has not been narrowed further.

## What to do

1. **Find the remaining pointer-ordered iterations that reach a decision.**
   The rule to apply is the one issue 035 already established: any
   iteration whose ORDER can affect a split, a key, an id assignment, or a
   worklist must be canonicalised (sort by a stable id) before use.
   Auditing for `form_Map` / `form_Vec` over pointer-keyed containers
   inside `split_*`, `creation_point`, `analyze_*` is the place to start.
2. **Until then, treat one- and two-program corpus deltas as noise.** A
   sweep comparison is evidence only at a margin larger than the
   perturbation floor, and that floor is currently at least two programs
   at the default arm.
3. **Establish the floor properly.** Build N trees differing only in dead
   code, sweep each, and record the spread. That number belongs in
   `corpus_sweep.sh`'s documentation, next to the existing warnings about
   concurrent sweeps and stale binaries — it is the same class of trap and
   currently the only one not written down.

## What this does NOT excuse

A mechanism still has to be justified on its own terms. The four reverted
commits also contained two levers that were arbitrary regardless of any
measurement, and they are recorded in ifa/133 so they are not tried again:
a per-call-site fan whose partition size was the CALLER COUNT, and a
"matched call and return" restriction on the CreationSet backflow walk
that answers a different question than the walk is asked. Neither needed a
sweep to be refused; the two-question test refuses both on inspection.
