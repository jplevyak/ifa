# ifa/issues/105 reduction artifacts

> **WITHDRAWN 2026-08-18 — `105-plcfrs-reduced.py` is NOT a valid
> reproducer.** Reading it revealed six functions that read names bound
> nowhere in scope (`parse` reads `A`, `tolabel`, `unary`; also `getmpd`,
> `pprint_chart`, `do`, `nextunset`, `__init__`). The original assigns all
> of them locally — the reducer deleted the assignments and kept the uses.
> `nameck.py` missed this because it was **scope-insensitive**: it
> collected every `Store` in the file into one flat set, so a name
> assigned inside *another function* counted as defined. It is now
> scope-aware, correctly rejects this artifact, and passes the original.
> The file is kept only as the worked example of the failure mode.

`105-plcfrs-reduced.py` — `shedskin_examples/plcfrs.py` delta-reduced
from **638 to 212 lines** while preserving its compile failure:

```
fail: mismatched field sizes: class 'closure' field 'x' mixes 8- and 1-byte members ('bool')
```

It is a **valid, working program**: CPython runs it to completion (exit 0,
56 bytes of stdout) and it references no undefined names. Compile time is
**3.9 s against plcfrs's 12.4 s**, so it is a 3× faster iteration loop on
the same bug.

## The oracle (`check5.sh`), and why it is this strict

Four earlier oracles were each defeated in a different way. A reducer
optimises against exactly what you check, so every omission became a
degenerate "reproducer":

| oracle | result | how it was defeated |
|---|---|---|
| v1 target error only | 93 lines | **invalid Python** — CPython raises `IndentationError`; pyc's parser accepts an empty `if:` body ([issues/106](../../../issues/106-empty-if-body-silently-accepted.md)) |
| v2 + `ast.parse` | 94 lines | **executed nothing** — 0 bytes stdout vs the original's 3456 |
| v3 + non-empty stdout | 182 lines | printed 7 bytes, then **died of `NameError`** |
| v4 + CPython `rc == 0` | 187 lines | exited 0, but **10 undefined names** survived in never-executed paths |
| v5 + *flat* undefined-name check | 212 lines | **6 functions read names bound only in OTHER functions** — the checker was scope-insensitive |
| v6 + scope-aware check | — | superseded: see below |

The v4 loophole is the subtle one and the reason `nameck.py` exists:
CPython resolves names at runtime, so deleted definitions are invisible if
their call sites never run — but pyc is a whole-program analyser, it
analyses those paths anyway, and it **accepts undefined names with no
diagnostic**. Since the bug under investigation is a type degenerating to
a union of everything, undefined names are a plausible way to manufacture
exactly that, which would have made the reproducer point at the wrong
mechanism.

## What it does not yet do

212 lines is where **line-granularity** reduction converges. The program
keeps every `def`/`class` from plcfrs with heavily gutted bodies, and its
executed path is trivial (it prints the usage message). Going further
needs statement-level (AST) reduction, or manual inspection of what in
these 212 lines manufactures the degenerate type.

Reproduce with: `python3 ddmin.py <input.py>` (edit `CHECK`/`dst`).


## Superseded by the issues/107 fix

`nameck.py` is no longer required. pyc now reports an undefined name as a
**compile error** (issues/107), so a candidate containing one is rejected
by the compiler itself, in the same run that checks for the target
failure. The oracle reduces to: `ast.parse` + CPython `rc == 0` with
output + the target error.

The scripts are kept as the record of the failure modes -- each oracle
here was defeated by a different degenerate program, and that history is
the reason the checks exist.

Verifying the point: `105-plcfrs-reduced.py` (the withdrawn artifact) now
fails to compile with

```
error line 18, name 'tolabel' is not defined
error line 22, name 'A' is not defined
error line 26, name 'unary' is not defined
```

— exactly the unbound reads found by reading the file, now caught
automatically.
