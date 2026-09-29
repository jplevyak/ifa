# 039 — a local read on a path that never assigned it: analysis done, enforcement not

**Status:** open. The analysis and the `--safe` fill are done; the runtime
check for "possibly unbound" is not. Rewritten 2026-09-28; the full design
record is in git:
`git show 3f36072b:ifa/issues/039-FA-uninitialized-local-reads-silent.md`.

## Symptom

```python
def f(c):
    if c: y = 42
    return y
print(f(1)); print(f(0))     # CPython: 42, then UnboundLocalError
```

pyc prints `42 42`. FA sees the unassigned path contribute bottom, the
union reaching `return y` is `{42}`, and the function constant-folds. The
unassigned path is treated as unreachable, so nothing downstream of FA can
repair it.

## Design (author, 2026-08-25)

Two facts from one dataflow over two meets. MUST (intersection) gives
definitely-assigned; MAY (union) gives assigned-on-some-path.

| | definitely unbound (no path assigns) | possibly unbound (some path does not) |
| --- | --- | --- |
| strict | compile error | warning + runtime check |
| permissive (default) | compile error | runtime check (`UnboundLocalError`) |
| `--safe` | compile error | auto-initialize to the zero of the type the assigning paths infer |

"Possibly" cannot be a strict compile error. `if first or d < bd:` is a
real possibly-unbound read in a VALID program (`or` short-circuits). The
same goes for exception edges and `match` captures. All are correlation
facts a MUST/MAY dataflow cannot represent, so the compile-time warning is
advisory and off by default, and enforcement is the runtime check. **ifa is
not a Python compiler:** what a check raises and what `--safe` fills with
are frontend decisions behind `IFACallbacks`. Nothing in ifa may name
`UnboundLocalError`.

## Landed (default off: `IFA_UNBOUND=1`, or implied by `--safe`)

- `find_maybe_unbound` (`optimize/ssu.cc`): both meets, one sweep, in
  REVERSE POSTORDER (collection order does not converge). Facts live on
  `Sym::maybe_unbound` / `definitely_unbound`, because SSU renames Vars
  after this pass. Formals come from `f->sym->has` (not `Var::is_formal`,
  which is set inside FA).
- `ATypeViolation_kind::{MAYBE,DEFINITELY}_UNBOUND`, appended last in the
  enum. DEFINITELY is always fatal; MAYBE is always a warning.
- `--safe`: `mark_unbound_phi_operands` (after `rename_vars`) gives each
  unassigned phi operand a fresh `is_unbound_fill` Var, and
  `apply_unbound_fills` (between passes) rewrites it to a typed zero.
  Between-pass IF1 rewriting is sound because every pass re-reads the IF1
  from scratch (it is a decision, not a snapshot). The fill prints `2.5 /
  0.0` on both backends. Non-numeric types decline, awaiting the frontend
  hook. A constant introduced between passes must be registered in
  `fa_all_Vars` / `fa_Vars`, or it silently contributes bottom.

## Open

1. **Turn DEFINITELY on by default.** It has no false positives (no path
   assigns). Measure the suite and corpus with only DEFINITELY enabled.
2. **The runtime check for POSSIBLY.** A shadow flag per flagged Sym, set
   at each write and tested at each read, calling
   `IFACallbacks::unbound_read_handler()` (the hook exists, with no
   callers), in both backends. It is blocked on the same erasure as the
   symptom: FA must not fold the unassigned path away. So the unassigned
   operand has to contribute a value the check can see, the same shape as
   the `--safe` fill, but a sentinel instead of a zero.
3. **The frontend fill hook** for non-numeric types under `--safe`.

## Verification

The repro raises `UnboundLocalError` by default and prints `42 / 0` under
`--safe`. `tests/scope_read_before_write.py` still passes (valid
short-circuit). The `match` capture tests stay clean. Six gates green.
