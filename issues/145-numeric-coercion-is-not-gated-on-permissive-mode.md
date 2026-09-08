# 145 — automatic numeric coercion fires in `--strict`, and changes the answer

**Status: open.** Found 2026-09-08 while root-causing ifa/144's excess
contours. **Author's directive:** *"pyc has a strict and permissive mode,
and any automatic coercion should be permissive only."*

## The defect

`coerce_annotate` (`fa.cc`) widens a pure-numeric confluence — `{int64,
float64}` — to the wider type and records it on `AVar::num_coerce`, which
`type_coerce_numeric_constants` then applies. **It consults no mode flag.**
Neither the annotator nor its two callers test `fruntime_errors`.

The widening is OBSERVABLE, so this is not a silent internal choice:

```python
import sys
class V:
    def __init__(self): self.d = 0.0
    def set(self, v):   self.d = v
a = V(); a.set(1.5)
b = V(); b.set(len(sys.argv))    # runtime int into a float member
print(a.d, b.d)
```

| | output |
| --- | --- |
| CPython | `1.5 1` |
| `pyc` (permissive, the default) | `1.5 1.0` |
| `pyc --strict` | `1.5 1.0` |

`--strict` is documented as *"hard compile errors on type violations, no
permissive-Python fallbacks"* (`pyc.cc:101`). Silently widening an `int`
member to `float` and printing a different value than CPython is exactly a
permissive-Python fallback, and strict mode performs it anyway.

## What the modes are

`pyc.cc:65-73` — the two knobs, both bundled:

```c
static void strict_mode_arg(...)     { runtime_errors = false; ifa_no_implicit_none = 1; }
static void permissive_mode_arg(...) { runtime_errors = true;  ifa_no_implicit_none = 0; }
```

`runtime_errors` reaches ifa as `fruntime_errors` (`pyc.cc:334`). So the
gate exists and is already threaded through; coercion simply never asks.

## Why it matters beyond correctness

ifa/144 root-causes `bh`'s excess contours to a numeric confluence that
NEVER resolves, because `coerce_annotate` handles only constants — its own
comment: *"Runtime (non-constant) narrow members are left alone — they
would need an inserted conversion — so their violations persist and are
reported honestly."* A permanent violation is read by every splitting stage
as a permanent demand, and `Vec3` ends with 18 byte-identical contours.

Measured: inserting the conversion by hand in the source takes `Vec3` from
**20 contours to 6**, the `{int64,float64}` confluences from **150 to 0**,
and route-4 mints from **24 to 7**.

So the fix ifa/144 wants is "insert the conversion at the narrow write" —
and **that fix must be permissive-only from the start**, or it compounds
this issue instead of fixing it. In strict mode the same program must
REPORT the mix, not widen it.

## What the fix looks like

Two independent pieces:

1. **Gate the existing coercion.** `coerce_annotate` returns 0 unless
   `fruntime_errors`. In strict mode a `{int64, float64}` confluence is a
   type violation and is reported as one.
2. **Extend it to runtime values, permissive only.** Insert an explicit
   conversion at the narrow write instead of leaving the violation
   standing. shedskin does exactly this and emits the cast:
   `b->_set( ((__ss_float)(len(__sys__::argv))) )`. Note shedskin has no
   strict mode to answer to, so its behaviour is the permissive arm only.

Piece 1 is small and is the one the directive names. Piece 2 is what
retires ifa/144's contour waste.

## Verification plan

- The repro above under `--strict` must fail to compile (or report the
  violation), and under the default must keep whatever behaviour is chosen
  for permissive mode.
- `bh` under `PYC_CSDCPA1=2`: `Vec3` contours 20 → 6 once piece 2 lands.
- Six CI gates; corpus `check` sweep on both arms. Note the corpus is
  compiled in the DEFAULT (permissive) mode, so piece 1 alone should be
  corpus-neutral — if it is not, something else was relying on coercion in
  a strict-mode path.

## Not to be confused with

- ifa/144 — the contour waste this feeds. Same root, different symptom.
- ifa/143 — `bh`'s actual runtime failure, an unrelated `__slots__` merge.
  Fixing the numeric mix does NOT fix `bh`: measured, the `str` warnings
  and `run_rc=134` are unchanged by it.
