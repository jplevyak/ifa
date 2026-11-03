# 102 — a condition codegen detects is a refusal or a demand, never a run-time trap

**Status: open.** Rewritten 2026-11-03 to its current scope. The
unresolved-send half landed 2026-10-14 (`codegen_unresolved_send`, refused
in every mode) and the softrender, `{list, tuple}` and bytearray cases are
fixed; that record is in git (`git show 123ac81:ifa/issues/102-corpus-programs-compile-then-abort-at-runtime.md`).
It now also carries [closed/079](closed/079-DISPATCH-single-candidate-dispatch-unchecked-cast.md)'s
safeguard and [closed/192](closed/192-review-analysis-and-issues-2026-11.md)
A6.

## Principle

A condition the compiler has already detected must not become a run-time
crash. Each one is either a **refusal** (fatal in every mode, like every
type violation since closed/158) or a **demand** the analysis can answer
by splitting; it is never a binary that aborts later. Precedent:
issues/107 made undefined names an error instead of a warning-then-segfault,
and three programs left the crash column with no other change.

## What still traps (the work list)

`cg.cc` tests `fruntime_errors` at eleven sites and, when it is set (the
default), writes `assert(!"runtime error: …")` into the program instead
of failing. Three more trap unconditionally. The LLVM backend mirrors the
shape with `emit_salvage_trap`.

| trap | sites | what it is |
| --- | --- | --- |
| field type mismatch (read, store) | 975, 1023 | the slot's C type disagrees with the value's |
| getter not resolved | 985 | a `period` on a receiver with no such field; the None arm of a null test (genetic2, msp_ss, mwmatching: CPython raises `TypeError` there, catchable) |
| list index type mismatch | 1105, 1293 | the index is not an integer type |
| list element type mismatch | 1241, 1291, 1329 | element C type disagrees with the slot |
| primitive operand type mismatch | 2180 | `write_c_prim`'s operand check |
| call argument type mismatch | 2292 | the clone's formal is coarser than the actual (closed/097, [170](170-FA-contours-minted-on-transient-types-are-never-remerged.md) step 2) |
| bad getter | 1170 | an empty record type |
| None coerced to a floating-point value | 1789 | a dead nil arm into a float slot (closed/071) |
| unresolved if condition | 3119 | a condition FA salvaged to NOTYPE |
| jump to unreachable block | 2999, 3078, 3087 | a goto to a label FA salvaged to not-live |
| send does not complete | 3170 | `fa_noreturn` (closed/178 fix 1; correct as a trap: the send raises) |

Each is a comparison of C type strings at the emission site, so it is a
representation fact FA did not observe. Some print
`warn_no_representation_ct`; the rest are silent.

Also on this list: **`convert_NOTYPE_to_void`** (`fa.cc:14071`) voids an
untyped value under `fruntime_errors` at the end of `FA::analyze`, so a
NOTYPE that is filtered as uninformative produces a `return 0` clone with
no diagnostic (IFA.md §11.12 calls it an escape hatch). Delete it, or warn
per site.

## Plan

One trap at a time, each with a same-binary corpus `check` sweep:

1. **Refuse it** in every mode (`codegen_fail` with the site, the two
   types and the candidates, as `codegen_unresolved_send` does). If no
   corpus program and no suite test reaches the trap, that is the end.
2. **If a program reaches it**, the trap is a representation fact FA never
   saw. File it against the analysis with the backtrack
   ([205](205-ai-centric-interface.md) D1's classification says whether
   it is temporal, polymorphism, or separable), and leave the refusal in
   place. [201](201-representation-oracle.md) is where FA will be able to
   ask the question codegen is answering here.
3. **The single-candidate fast path** (closed/079): after the nil test and
   the tag compares, `directs.n == 1` casts the remainder to the one
   candidate with no `cg_note_blind_cast` and no no-match guard. Record
   the cast; if a member of the receiver union remains that the candidate
   does not accept, fail the compile naming it (FA should have refused;
   reaching this is an FA bug). The corpus's emitted C must be
   byte-identical before and after.

Stop condition: no program that matched CPython stops compiling without an
issue naming the demand.

## Why the check lives in codegen (settled 2026-10-14)

Whether candidates can be discriminated depends on representation routes
(tag, null test, arity header, layout bits) over post-clone Funs; four of
its inputs are post-FA. FA cannot restate it without drifting (AGENTS.md:
call it, never restate it). The refusal stays where the plans are tried;
what moves to FA is the oracle ([201](201-representation-oracle.md)).

## The untyped-value work list (kept from closed/149)

86 % of unresolved calls are a cascade from an untyped value. The
actionable roots are the named `'X' has no type` sites; for each, find the
confluence and backtrack the demand. Never add a missing method to make a
cascade go away; a missing method is real only when the named receiver
type lacks it (then it goes to the top-level `issues/`).

## Measurement rules

- `compile_rc=0` is not evidence. Use `./corpus_sweep.sh -m check`.
- `run_rc=1` is not a crash until diffed against CPython.
- The harness stops at the first failing stage; run the binaries.

## Verification

The generated C for the corpus has zero `runtime error:` strings other
than `send does not complete`; every refusal names its site; no program
that matched CPython regresses.
