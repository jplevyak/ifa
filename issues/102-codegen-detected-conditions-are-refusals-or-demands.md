# 102 — a condition codegen detects is a refusal or a demand, never a run-time trap

**Status: open; census and steps 1–2 done 2026-11-07** (below). Rewritten 2026-11-03 to its current scope. The
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

## Census, 2026-11-07 (tree `87924498` + the probe)

**Probe.** `IFA_DBG_TRAPS=1` prints `TRAP <backend> <kind> <fun>:<id>` for
every trap an emitter writes:

- On the C backend, every `runtime error:` literal in `cg.cc` goes
  through `cg_trap_c`, which tags it with its line.
- On the LLVM backend, each `emit_salvage_trap` call, `emit_abort_trap`,
  the arity-dispatch trap, and every `CreateUnreachable` fallback in
  `emit_block_terminator` and on an unterminated block are probed.

**Method.** All 77 corpus programs and all 529 `tests/*.py` were compiled
on both backends with the probe on. A C trap counts as *surviving* when
its literal is still in the `-O2` binary (`strings`), which means clang
could not delete it as dead.

**Never reached anywhere (suite and corpus, both backends):**

- field type mismatch, on read and on store (cg.cc 853, 901);
- list index type mismatch (983, 1169), and LLVM's index load and store;
- list element type mismatch (1120, 1167, 1208);
- primitive operand type mismatch (2071), and LLVM's binop operand
  mismatch;
- call argument type mismatch (2185/2188, and LLVM's);
- bad getter (1047);
- None coerced to a floating-point value (1671);
- `emit_goto_or_trap`'s jump to a dead label (2904). The other dead-arm
  site, 2992, is the same ifa/049 exc_check trap as 2983 and is kept with
  it.

These are plan step 1 with nothing behind it. Each becomes a refusal in
every mode, and that is the end of it.

**Reached:**

| trap | C emitted (corpus / suite) | survives `-O2` | LLVM (corpus / suite) | reading |
| --- | --- | --- | --- | --- |
| unresolved if condition (3024) | 521 / 643, in every program | **0 binaries** | 15 / 23 (astar, dijkstra2, sudoku1, voronoi2; 20 suite tests) | C: the IF is not live, only a successor is (the `isinstance(x, int) or …` chains in numeric `__ne__`/`__eq__`), so the trap is dead text. C and LLVM guard it differently (C tests successor liveness only; LLVM is inside `closer->live && fa_live`), which is [200](200-llvm-backend-parity.md) A3's duplicated `is_unresolved_condition`. The LLVM hits are on live IFs and need a look |
| getter not resolved (864) | 6 / 2: genetic2 (2), msp_ss (1), mwmatching (3), all in `__pyc_getslice__`; `genetic2_idioms`, `none_receiver_dead_path` | genetic2, msp_ss, mwmatching | same 6 / 2 | a read on a None-only receiver on a path FA cannot prove dead. CPython raises a catchable `AttributeError`/`TypeError` there, so this is [165](165-none-reaching-an-operation-is-silently-accepted.md)'s raise, not a refusal. All three programs match CPython |
| jump to unreachable block (2983) | 278 / 14 | bh, msp_ss, othello2, rdb, voronoi2 | — | ifa/049's dead arm after a call that can only raise. FA proved it dead, so it is correct as a trap, like `send does not complete` |
| send does not complete (3075) | 7 / 5 | pylife | — | `fa_noreturn`, correct as a trap |
| polymorphic / arity dispatch: no branch matched (2844, 2293) | 63 + 12 / 29 + 68 | 14 binaries | arity 12 / 68 | the else arm of a dispatch FA proved exhaustive. This is step 3's territory (the guard the single-candidate path lacks), not a mismatch |
| LLVM `unreachable` fallback (no successor block) | — | — | msp_ss `main` 41 + 5 elsewhere; suite: `evaluate` ×2 | an LLVM optimizer licence (UB if reached), not a trap. It was LLVM emitting FA-dead regions; fixed in step 4 below |

No `unterminated_block` fallback fired. Every program that reaches a trap
matches CPython on both backends (sweeps `check__default__10c5f2f6+d9550405`,
`check__PYC_LLVM_1__966472ad+e55772bf`).

**What follows, in order:**

1. **Done.** The nine never-reached kinds are refused in every mode by
   `codegen_fail` with the site, the types and `(ifa/102)`:
   - in `cg.cc`: field read and store, list index, the list element (both
     tuple-list branches and the general store), bad getter, None into a
     float, primitive operand, call argument, `emit_goto_or_trap`;
   - in `cg_emit_llvm.cc`: binary operand, index load and store, and
     `__pyc_c_call__` argument.

   206's own union-read arm check, which in permissive mode fell through
   to the cast, is refused the same way.
2. **Done.** The C unresolved-if trap moved from the dead-IF branch to
   the live one, before `if (cond)`, gated on a live successor. It is
   therefore the same test LLVM makes, and no longer one that only
   fires on dead text. The C census after it:

   - corpus: unresolved-if 521 → 0;
   - suite: unresolved-if 643 → 0;
   - every other count is unchanged.

   No live IF in the suite or the corpus has an unresolved condition on
   the C backend.

   LLVM's 15 corpus hits are dead too. In astar they are the `if False:`
   body, behind `br i1 false` or in blocks with no predecessors. The
   reason is that LLVM treats those IFs as live where C does not walk
   them, which is a liveness divergence for 200 A3, not a trap that
   runs.

   Verification:
   - `make test` is green: 58/0, and 493/0 on both backends.
   - `check__default__87924498+01c0f4aa` is identical to
     `…10c5f2f6+d9550405` in every column.
   - `check__PYC_LLVM_1__87924498+01c0f4aa` is identical to
     `…966472ad+e55772bf` except `bh`, which went rc=124 → 0, the
     contention recorded in 200 A6.
3. Make getter-not-resolved on a None-only receiver raise, with 165.
4. **Done.** LLVM's `unreachable` fallbacks were FA-dead code that LLVM
   emitted and C does not. There were two entries into it:

   - **The exception check after a call that can only raise** (ifa/049).
     C traps on the arm FA never reached (`dead0`/`dead1`). LLVM gave
     that arm a block, branched into it, and emitted the region behind
     it. Its nodes had no values, so its IFs had no condition and ended
     in `unreachable`. That was msp_ss's 46, and every dead arm in the
     corpus.
   - **The code after a send that cannot complete** (`fa_noreturn`
     without a raise, ifa/178). C writes `send does not complete`. LLVM
     wrote nothing and ran on into the region. That was
     `tests/expr_evaluator`'s `evaluate` ×2.

   `succ_dead` (`cg_emit_llvm.cc`) now keeps both regions out of
   `discover_blocks` and `emit_pnode`. A dead exception arm branches to
   an `exc.dead` trap block. A non-completing send traps after itself,
   whether or not DCE kept it, as C does.

   LLVM census after the fix:

   | | corpus | suite |
   | --- | --- | --- |
   | `unreachable` fallbacks | 46 → 0 | 2 → 0 |
   | dead-arm traps | 278 | 14 |
   | `send does not complete` | 7 | 5 |

   The trap counts are now exactly C's. Unresolved-if fell 15 → 7 in the
   corpus and 23 → 19 in the suite, because those were inside the
   regions that are no longer emitted.

   Verification:
   - `make test` is green.
   - `check__PYC_LLVM_1__04baeca8+f4c07c87` is identical to
     `…87924498+01c0f4aa` in every column.
5. Step 3 for the dispatch else arms.

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
