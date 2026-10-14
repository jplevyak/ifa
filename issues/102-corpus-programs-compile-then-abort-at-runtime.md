# 102 — programs that compile cleanly and then abort: codegen turns an unresolved call into a silent runtime stub

**Status:** open. Re-measured 2026-10-14 against sweep
`check__default__4e218622+37947902` and the C it generated. It also carries
[149](closed/149-the-largest-diagnostic-class-reports-nothing.md)'s work
list. Earlier versions are in git: the 2026-08 census (37% of compiling
programs crashing) at
`git show 3f36072b:ifa/issues/102-corpus-programs-compile-then-abort-at-runtime.md`,
and the 2026-09-28 rewrite at `6f429396`.

## The defect

When codegen cannot resolve a call, it emits
`assert(!"runtime error: matching function not found")` (`cg.cc`, the end
of the dispatch emitter) or a `getter not resolved` /
`list element type mismatch` stub, and in permissive mode the compile
succeeds. A condition the compiler has already detected becomes a run-time
crash. Under `--strict` the same point refuses (`codegen_fail`, added by
issues/171), but in codegen, not as an analysis violation, so nothing
upstream can answer it.

## Where it stands (2026-10-14)

**The five programs of the 2026-09-28 table no longer abort.** Each was
fixed by other work; none by this issue's fix:

| program | 2026-09-28 | 2026-10-14 |
| --- | --- | --- |
| `life` | `matching function not found` | runs, matches CPython |
| `loop` | SIGSEGV | runs, matches CPython |
| `quameon` | `matching function not found` | runs, matches CPython |
| `adatron` | `matching function not found` | runs (rc 0); CPython exceeds the 120 s cap, hand-verified 2026-10-12 (issues/025) |
| `pisang` | `list element type mismatch` | same as adatron |

**Stubs still emitted in the corpus's C** (grep of the generated
`NAME.py.c` for the three stub strings):

| program | stubs | what |
| --- | --- | --- |
| `softrender` | 4 `matching function not found` | **live: this issue.** `RenderContext.copy_pixel`'s `src.components[...]` has a `{bytes, bytearray}` receiver: two candidates, one C receiver type, no tag (the shape of `tests/list_tuple_union_method.py`). The corpus run never reaches `copy_pixel`, so it exits 0; a textured render would abort. [ifa/190](190-CODEGEN-run-time-dispatch-census.md) counted these 4 as run-time dispatches; on the C backend they are stubs |
| `genetic2` 2, `msp_ss` 1, `mwmatching` 3 | 6 `getter not resolved` | **not this issue.** Each is the None arm of a null test (`if (!t) <stub> else str.__pyc_getslice__(t, ...)`): `__pyc_any_type__::__pyc_getslice__` on a `_CG_nil_type` argument. CPython raises `TypeError: 'NoneType' object is not subscriptable` on that path, so the program fails under both; pyc aborts where CPython raises a catchable exception, which is a fidelity gap of its own |

`tests/list_tuple_union_method.py` is still a `.known_issue`.

## Done 2026-10-14

**softrender.** Its `{bytes, bytearray}` was an inference imprecision,
not a property of the program: the only `bytes` it passes is the literal
`b''`, which is falsy, so under CPython `components or bytearray(n)` is
always the bytearray. `len` of a str/bytes CONSTANT now folds to its
length (`P_prim_len`, `fa_prims.cc`), so `__pyc_to_bool__` of a constant
folds and the `or` keeps only the arm that runs. softrender's 4 stubs are
gone, and its dynamic calls are 5 -> 1. Test:
`tests/const_bytes_truthiness_or.py`.

The fold needs the truth test's contours to see ONE constant. Where they
see several, it is CONST_DEMAND's job (ifa/151) to split them, and it
cannot yet: `y = "" or 5` next to an unrelated `len("abc")` shares one
`str.__len__` contour, and the walk from `y`'s violation cannot reach that
formal, because the truth is `self.__len__() != 0`, a primitive whose
result has no backward link to its operands. Seeding the conditions that
control a value's DEFINITION (the `or`'s own test, which does not control
the violation site) was tried and is necessary but not sufficient; the
walk also has to cross the comparison and `len` primitives, which the walk
deliberately does not do for primitive operands (following every one cost
`rubik` 15.6 s -> 34 s). Open.

**A bytearray that is only read had no element type.** Found on the way:
`bytearray(3)[0]` was refused with "expression has no type", because a
@vector record's elements are typed only by writes, and `bytearray(n)`
wrote none. `__init__` now stores the zero the allocation already holds.
Test: `tests/bytearray_read_only.py`.

**An unresolvable send is refused, in every mode** (fix step 1's
safety half). `codegen_unresolved_send` (`codegen_common.cc`) is called
where each backend gives up: the C backend's stub site, and the LLVM
backend's no-callee and no-result-produced exits. It names the send and
its candidates and exits:

```
real.py:3: codegen: cannot resolve the call to '__getitem__' (candidates:
bytes.__getitem__, bytearray.__getitem__): no dispatch plan can tell the
candidates apart at run time. pyc refuses it rather than emit a run-time
stub (ifa/102)
```

Before, the C binary aborted on that call, and the LLVM binary printed `0`
for one arm (a salvaged null) and trapped on the other.
`PYC_DISPATCH_STUB=1` restores the stub, for debugging only.
`tests/list_tuple_union_method.py` is now refused at compile time the
same way (still `.known_issue`: its goal is CPython's output).

**Why the check lives in codegen, not FA.** Step 1 asks for an FA
violation, so the demand ladder could answer it. Whether codegen can
discriminate a candidate set is decided by codegen's dispatch plans (class
tag, null test, function-pointer identity, tuple arity, a single untagged
remainder), over post-clone Funs. FA cannot call that without restating
it, which AGENTS.md forbids, and a restatement that drifts would refuse
programs codegen can compile. The refusal is therefore made where the
plans are tried, which is exactly the predicate. What is still missing is
the DEMAND half: a way for FA to learn of the refusal and split instead.

Corpus check -R `check__default__68c8b0c3+c646815d` against
`check__default__4e218622+37947902`: every verdict and warning count is
identical on all 77, and no program is refused by the new rule. The
generated C has 0 `matching function not found` stubs (softrender's 4 are
gone); what remains is the 6 None-arm getters above. Contours moved by one
on three programs (ess +1 doom, -1 softrender; css +1 doom, softrender,
sokoban).


## Principle

**An unresolved dispatch that reaches codegen is a demand that FA never
observed.** It must be a compile-time VIOLATION, not a stub:

- it is fatal-by-default like every other violation (closed/158), so the
  program is refused rather than miscompiled;
- once it is a violation, the demand ladder can answer it. For example,
  CONST_DEMAND can split `int.__ne__` over 1/3 in `itertools.product`,
  which is what retires one of [134](134-remove-the-frontend-forced-split-opt-in.md)'s
  annotations.

Precedent: `issues/107` made undefined names an error instead of a
warning-then-segfault, and three programs left the crash column with no
other change.

## Fix (original plan; see "Done 2026-10-14" for what landed)

1. In FA's post-convergence checks, report any send whose candidate set
   codegen will not be able to discriminate: no candidate, or ≥2
   candidates that share a C receiver type and have no tag. Use the SAME
   predicate codegen uses (`poly_dispatch_classtag_targets`,
   `get_target_fun_core`). Call it; do not restate it (AGENTS.md: never
   decide by name, never reimplement codegen's answer).
2. Keep the codegen stub only as an internal-error backstop that cannot be
   reached when FA reported nothing. An `IFA_DBG` count of emitted stubs
   should read 0 on the whole corpus.

## The untyped-value work list (from 149)

Ranking the corpus's unresolved calls by cause (2026-09-12): the
missing-method era is over. 86% of unresolved calls are a **cascade from
an untyped value**, because every named operand type has the operator.
So the work is "why is this value untyped". The actionable roots are the
NAMED `'X' has no type` sites. Not re-ranked since. On 2026-09-12 the top was
doom `data = self.entry_data[b'VERTEXES']` (a dict value channel, driving
all 20 of doom's unresolved calls), then msp_ss `dataOut` / `blkin`,
plcfrs `rule` (plcfrs has since been fixed), othello2 `value`.

For each root: find the confluence and backtrack the demand (AGENTS.md).
Never add a missing method to make a cascade go away. A missing method is
only real when the named receiver type lacks it (then it goes to the
top-level `issues/`, as `list.copy`/`dict.copy` do, see
[086](closed/086-list-and-dict-have-no-copy-method.md)).

## Measurement rules (kept, because each was learned the hard way)

- `compile_rc=0` is not evidence a change is safe. Use
  `./corpus_sweep.sh -m check`, which records run status and CPython
  agreement.
- `run_rc=1` is not a crash until diffed against CPython (some programs
  exit 1 by design).
- The harness stops at the first failing stage, so a COMPILE-OUT diff can
  hide a runtime crash behind it. Run the binaries.

## Verification

softrender's `copy_pixel` either compiles to a real dispatch or is
refused at compile time with a diagnostic naming the send, and the stub
count reads 0 on the corpus apart from the None-arm getters above. `tests/list_tuple_union_method.py`
is refused with a diagnostic (update its `.check`), and no program that
matched CPython regresses.
