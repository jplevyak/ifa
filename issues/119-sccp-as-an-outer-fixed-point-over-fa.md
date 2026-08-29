# 119 — SCCP inside FA: a plan, grounded in what FA already does

**Status:** plan, filed 2026-08-29. Splits out of
[050](050-FA-general-constant-propagation-unreachable-code.md), whose
"no SCCP" framing turns out to be too strong once measured. This issue
is about the part that is genuinely missing and how to build it.
**Affects:** `ifa/analysis/fa.cc` (the `Code_IF` transfer, the pass
loop), `ifa/ifa.h` (the fact-provider hooks), `pyc.cc` (`compile()`'s
pipeline).

## What FA already is

Sparse conditional constant propagation, near enough. Measured:

```python
c = 0
if c: x = "str"
else: x = 5
print(x + 1)          # compiles, prints 6
```

The dead arm contributes **nothing** — if it did, `x` would be
`{int64, str}` and the program would be refused. It cascades, too: with
an intermediate `d`, `c` folds, which folds `d`, which types `x`.

So three of SCCP's four ingredients are present:

| SCCP ingredient | in FA today |
|---|---|
| a value lattice | `AType` (sets of CreationSets) — richer than constants |
| constants in it | FA's canonical `true_type`/`false_type` Syms |
| iteration to a fixed point | FA's own pass loop |
| **executability of edges** | **only indirectly, and only within one analysis** |

The fourth is the interesting one. FA has no explicit "is this CFG edge
executable" bit. It gets the effect from issue 025's per-branch SSU
narrowing: `Code_IF` (`fa.cc:3458`) creates a per-branch renamed Var for
each arm, and when the condition is a constant the non-taken arm's
narrowed type comes out bottom, so nothing flows from it. The BRANCH
itself is pruned much later, in codegen (`const_if_successor`, see 050).

## What is actually missing

**1. Executability is not retractable, because the lattice is monotone.**
`update_gen()` unions; it never replaces. A condition that is constant
from the start works (above). A condition that becomes constant only
*later* — after some other fold — has already let both arms flow, and
those types are permanent. This is the single wall behind every negative
result in 050: the seeding prototype (2026-07-28), and the
transfer-function fold prototype (2026-08-29) that folded to `int64` on
an early pass and `str` on a later one and ended up with both.

**2. Facts are fed once, by hand.** `exc_check_fold` is still the only
detector, wired into `pyc.cc` at a fixed point in the pipeline. A fold it
performs cannot expose a second FA-derivable fact, because nothing
re-enters FA.

**3. Only a branch CONDITION benefits.** A Var FA proves constant that
does not feed a `Code_IF` gets nothing from any of this machinery.

## The plan

The design decision that makes this tractable: **do not try to make FA's
lattice retractable.** Monotonicity is load-bearing — it is what makes
the pass loop terminate. Instead, put the fixed point one level up.

### Stage 1 — an explicit executability bit, within one analysis

Give each `Code_IF` successor edge an `executable` flag, set by the
`Code_IF` transfer when the condition's AType is a canonical constant.
Have `add_pnode_constraints` skip a non-executable successor entirely
rather than relying on the narrowed arm coming out bottom.

Buys: unreachable code stops creating contours at all (today it creates
them and they carry bottom), which is a size/time win before it is a
precision one. Makes the executability notion explicit and inspectable,
which stages 2-3 need.

Does NOT buy retraction: within one analysis the flag can only ever go
from unset to set, same as everything else.

Verification: no test output changes; `ess`/`css` counts drop on programs
with statically dead branches; `PYC_DBG_OSC`/contour counts are the
metric.

### Stage 2 — the outer fixed point ("reflow after folding", done properly)

```
  dead_edges = {}
  loop:
    run FA to convergence, with dead_edges suppressed
    collect the constant conditions FA proved this round
    new_dead = edges those conditions kill
    if new_dead ⊆ dead_edges: break
    dead_edges ∪= new_dead
```

Each round is a **fresh** analysis, so nothing has to be retracted — the
monotonicity problem disappears rather than being solved. A fold that
exposes a second fold is caught on the next round.

Cost: one extra full analysis per round. In practice the loop terminates
after one extra round on almost everything (nothing new is killed), so
the common case is 2 analyses instead of 1. Gate it behind a flag and
measure before defaulting it on; abandon it if the corpus cost is worse
than the precision gain.

Verification: the two-level cascade above still works; a program whose
second fold depends on the first (which today does NOT type) starts to;
corpus analysis time and `pass_limit_hit` set unchanged or better.

### Stage 3 — a fact-provider registry

Turn `exc_check_fold`'s hand-wiring into a list. A provider is
`(name, run(FA*) -> bool changed)`, consulted between rounds of stage 2's
loop. `mark_exc_checks_constant` becomes the first entry; 050's
directions 1 and 2 collapse into this.

Prerequisite for a provider, learned the hard way and worth stating in
the interface: **its fact must be stable given the round's inputs.** A
provider that sharpens its answer as it is repeatedly consulted poisons
the lattice. The two extension points that exist today obey this by
construction — `provably_constant_isinstance` reads `EntrySet::can_raise`
after the ES graph exists, and `provably_constant_load` reads
`if1->allclosures`, which is static.

### Stage 4 — generalize past `Code_IF`

Let codegen fold any send whose result FA proved constant, not just a
branch condition. The hook (`virtual_cg_is_const_folded_send`) already
exists for the exception-check case; this widens its input rather than
adding machinery.

## Why the stages are in this order

Stage 2 is the one that matters and it needs stage 1's explicit
executability to have something to suppress. Stage 3 is small but
pointless before stage 2, since there is nothing to re-enter. Stage 4 is
independent and could be done at any time, but it is the smallest win.

## What this does NOT address

Interprocedural precision for a global slot — that is
[050](050-FA-general-constant-propagation-unreachable-code.md)'s 3b,
whose stage 1 landed 2026-08-29 and whose stages 2-3 (a per-Fun mod-set,
then a per-ES summary) are a separate line of work. SCCP would consume
that precision; it does not produce it.
