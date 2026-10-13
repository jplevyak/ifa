# 189 — numeric coercion warns once per widened expression, not once per origin

**Status:** closed 2026-10-13. Filed 2026-10-12 when
[156](156-FA-split-int-from-float-coerce-last.md) closed. The warning is now
one per origin; see *Resolution*.

## Symptom

The corpus check sweep `check__default__82e9b98f+d1d82217` carries 546
`holds both int and float; widened to float64` warnings. doom has 271,
ac_encode 102, fysphun 38 and softrender 30. They come from roughly fifty
places where an int and a float actually meet:

| program | warnings | mixed AVars | origins |
| --- | --- | --- | --- |
| doom | 271 | 763 | 11 local |
| ac_encode | 102 | 963 | 12 local |
| softrender | 30 | 800 | 17 local, 6 field |
| fysphun | 38 | 310 | 1 local, 8 field |

The origin counts are from `IFA_DBG_NUMORIGIN` (fa.cc, `numorigin_probe`).
An origin is an AVar whose direct sources are all pure and of both kinds.
Every AVar downstream of it is widened too, and each one that maps to a
source expression gets its own warning. doom's `textureX0`
(`= seg.offset + frontSidedef.offset_x`, an int, then `= textureX1 + p *
(...)`, a float, on the `za <= 0.1` path) warns at every line that
computes from it, from 780 to 850.

## Why it matters

AGENTS.md: a permissive accommodation must *warn when it changes what the
program would do under CPython*. The deviation happens once, at the
origin, where an int is widened. A warning there, saying which assignments
meet, is the actionable one. The downstream ones are consequences, and
most of them change nothing CPython could observe: `textureX0 / za` is a
float in CPython too.

## Proposed

- Warn at each origin, naming the int and float sources that meet there
  (the lines of the assignments). Report how many downstream expressions
  that origin widens, instead of one warning per expression.
- Keep a per-expression warning only where the widened value reaches a use
  that CPython would observe as an int: `print`/`str`/`repr`/format, `//`
  and `%` producing a float, `range`, an index, or a bit operation.
- `--strict` is unaffected: it refuses the mix either way (issues/171).

## Verification

- The corpus warning count falls from 546 towards the origin count (~50),
  and every remaining warning names a source line.
- No `.check` golden loses a warning that marks an observable deviation.
  Re-bless only lines this change is about.

## Resolution (2026-10-13)

`show_numeric_coercions` (fa.cc) now groups the coerced AVars by origin.

- **Origin.** A coerced AVar into which an UNWIDENED narrower value flows
  (an `int` into a `float64` slot). The wide side may itself already be
  widened: `u = 0` ... `u = halfway` in a loop reads its float through the
  loop-carried, coerced `u`. The first version required two unwidened kinds
  at one AVar and missed exactly that case.
- **Attribution.** Every other coerced AVar walks back through what it reads
  (flow sources, and the operands of its defining node) to the origins it
  depends on. The walk passes through coerced AVars and through values that
  hold ONLY the widened type. That second part is needed because a read of a
  widened field is a plain `float64` and is not annotated itself, while the
  AVar it feeds was annotated when it was still mixed. Without it,
  `mixed_numeric_field`'s reads of `a.x` reported as separate origins.
  A coerced AVar that reaches no origin is reported on its own, as before.
- **Where the values come from.** Each source is located at its defining
  node. A value with none (a constant, a field write's value) is located at
  the node that OWNS it as a temporary. `prim_setter` flows its value
  through `p->tvals[0]` (fa_prims.cc), so that temporary names its write
  exactly, with no name match. A field origin has no line of its own
  (`find_avar_user_loc` gives its class's), so it is reported at its first
  write.
- **Wording.** `'x' holds both int and float; widened to float64, ... The int
  comes from lines 9 and 28, the float from lines 16, 19 and 30; it widens 4
  more expressions, on lines 13-31.` An unnamed element origin reads `the
  elements of this list hold both int and float`. The sources clause is
  dropped when every source is on the warning's own line (a merge inside a
  library call, such as `sum`'s start value).
- `IFA_DBG_NUMWARN=1` lists each origin, its contour and its reach, and
  every attributed line.

**Not done: the observable-use warnings** (the second proposed bullet).
Deciding that a widened value reaches `print`, `//` or an index means
classifying its USES. In pyc those are calls into `__pyc__`, so the only
handle the warning has is the callee's name, which the project forbids.
The downstream count and line range carry the same information without
that judgement.

**Measured.**

| program | before | after |
| --- | --- | --- |
| doom | 271 | 12 |
| ac_encode | 102 | 6 |
| fysphun | 38 | 5 |
| softrender | 30 | 7 |

Corpus, sweep `check__default__3eb03ec8+d0c8ef56` against
`check__default__82e9b98f+d1d82217`:
- **Coercion warnings: 546 → 54.** That is close to the ~50 origins 156
  counted, and each warning that has a source line elsewhere names it.
- **Contours and verdicts.** EntrySets, CreationSets and every compile,
  stdout and CPython verdict are identical. This change only prints.
- **The two `run_fail` are load.** rubik2 and tonyjpegdecoder hit `rc=124`
  while other processes were saturating the machine. Alone, rubik2 passes
  in 105 s, and tonyjpegdecoder finishes in 102 s with CPython's output
  (the `TIME` line aside).

- Every corpus origin was checked against the source. doom's 12 are the
  `za <= 0.1` `textureX0` merge, the six `backCeil`-family portal/non-portal
  branches (lines 602-616), and the texture-Y steps. ac_encode's are
  `c0`/`c1` defaults against `c0*1.0` and `decode`'s `u`/`v`.
- Goldens: five `.check` files lose only their downstream lines and gain the
  sources and reach on the lines that stay. `numeric_rows_split_by_caller`'s
  warning moves from the read (line 24) to the rows' creation (line 13),
  which is where the int `0` meets the float written on line 16.
- New test `tests/numeric_coercion_warns_per_origin.py`: one warning, its
  sources (lines 10 and 12) and its two downstream expressions.
- `--strict` is unaffected: it never coerces, so it never reaches this code.
- `make test`: green on both backends, 445/0. The first run hit five
  compile timeouts while another project's `cargo test` loaded the machine
  (load average 71). All five pass alone, and full reruns of both e2e
  backends were clean.
