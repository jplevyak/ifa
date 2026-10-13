# 189 — numeric coercion warns once per widened expression, not once per origin

**Status:** open. Filed 2026-10-12 when
[156](closed/156-FA-split-int-from-float-coerce-last.md) closed. Nothing is
changed yet.

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
