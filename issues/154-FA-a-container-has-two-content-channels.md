# ifa/154 — a container has TWO content channels, and half the ladder looks at one

**Status:** `bh` fixed 2026-09-15 by `PYC_CSBACKTRACK=1 PYC_CSCONTENT=1`.
The filter bug below is fixed unconditionally; both flags stay default 0 on
one named blocker.

## The symptom

`bh` compiled with zero warnings and aborted at runtime with
`"runtime error: getter not resolved"`. After
[153](153-FA-positional-record-slots-lose-identity-and-reads.md) tightened
the layout contract it stopped compiling instead, with 17 blind casts:

```
error: object layout: 'Body' is blind-cast to 'str' and read at e1,
       but classtag header present in one and not the other
```

A `{Body, str}` union — and by CLAUDE.md's directive that is pyc's
inference, not a property of the program.

## Root cause — one merged arity-1 literal

`IFA_DBG_ELEMCONF` finds six list element channels holding `{str, Body}` or
`{str, Body, Cell}`, all `defs=1`. Walking back from them:

```
CSVARS cs=1197 sym=list vars=1 defs=5 arity=1 elem=
  DEF es=78  fun=create_test_data      <- self.bodies = [None] * nbody
  DEF es=351 fun=__init__
  DEF es=350 fun=__init__
  DEF es=173 fun=__init__
  DEF es=47  fun=___init___
  var=? type= str#8
```

**An arity-1 list literal with FIVE creation points whose single slot holds
`str`.** `[None]` shares a CreationSet with a one-element string literal, so
`list.__mul__`'s `merge` copies that slot into the result element, `Body` is
appended alongside, and the union is born. Exactly
[152](152-FA-backtrack-the-demand-to-the-merged-creation-set.md)'s shape,
with `defs=5` — partitionable, if anything nominates it.

## Why neither half of the ladder reached it

Both halves ask about a CreationSet's CONTENT through `cs_content_avars`,
and that helper returns the **element channel** and only falls through to the
**positional slots** when `PYC_CSCONTENT` is on — which defaults to 0. For an
arity-N literal with an empty element it therefore yields **nothing**.

- **152's backtrack** reached `cs=1197` and rejected it. Measured with
  `IFA_DBG_BACKTRACK`: `BTREACH ... -> cs=1197 sym=list defs=5` followed by
  `BTREJECT ... (supplies none)`, 256 times. The walk found it; the filter
  could not see its `str`.
- **Route 4's content key** then declined on it with
  `KEY sets=0 defs=3 groups=1 informative=0` — the CS flow graph built over
  the same empty channel.

`cs_content_avars`' own comment already names this defect
(ifa/133: *"a container has TWO content channels (ifa/104) and HAVING an
element channel does not mean the content is IN it … the demand reaches the
rung; the rung looks in the wrong channel"*). `PYC_CSCONTENT` is its fix and
had never been measured to a default.

## The fix

**Unconditional:** 152's backtrack filter now uses `cs_content_avars_both`,
which adds the element AND the positional slots with no gate. Its question is
"does this CreationSet SUPPLY one of the offending types", and that has to
see whichever channel holds the content. Deliberately NOT a change to
`cs_content_avars` itself — its gate governs the flow graph's content key,
a different question with its own default.

**Flagged:** `PYC_CSCONTENT=1` supplies the other half, so route 4 can
actually partition `cs=1197`.

With both, `bh` compiles with **1 warning**, runs to completion, and its
**stdout matches CPython byte for byte** (verified directly; the sweep cannot
compare it because CPython exceeds the 120 s cap).

## Measured — corpus `-m check`, one binary, env-toggled

| | default | `CSCONTENT=1` | both |
| --- | --- | --- | --- |
| compile failures | 8 | 5 | **3** |
| total warnings | 1973 | 1607 | **1364 (−31%)** |
| container CS / shapes | 2138/629 = 3.40 | 1987/613 = 3.24 | 2051/614 = 3.34 |
| run failures | 32 | 35 | 35 |

Only `othello3`, `rdb` and `sudoku4` still fail to compile. Per program,
against the default:

| program | default | both flags | |
| --- | --- | --- | --- |
| `bh` | compile fail | **compiles, 1 warning, runs rc=0** | matches CPython |
| `sudoku3` | compile fail, 121 warnings | **compiles CLEAN, runs** | |
| `sudoku5` | compile fail, 216 warnings | compiles, 19 warnings, runs | |
| `plcfrs` | compile fail, 379 warnings | compiles, **122** warnings | |
| `chull` | compile fail | compiles, `run 139` | 153's residual |
| `linalg` | 108 warnings | **33** warnings | verdict unchanged |
| `pygasus` | 2 warnings | 52 warnings | verdict unchanged (`run 134` both) |
| `quameon` | runs, wrong stdout | `run 134` | **the one regression** |

`quameon` aborts in `coulomb_pot::compute_en_value` with
`"matching function not found"` — the SAME
[132](132-arity-is-representation-not-provenance.md) defect already recorded
in 152: `coulomb_pot.charges` holds an arity-1 record AND an appended
element-channel list, so the slot has no representation and is emitted
`_CG_void`. It was already printing the wrong answer.

**Both flags therefore stay default 0 on the same blocker as 152** — land
132's arity drop at a member confluence, then flip the pair together. The
suite is 316/0 at the defaults.

## A cleanup found on the way

`list.__mul__` (`__pyc__/04_sequence.py`) carried a stray unpaired `")"`
after its last `(type, value)` pair, introduced with the body in `04a85584`
and referenced by nothing. `c_call_codegen` steps `i += 2` from `rvals[5]`,
so it landed on a TYPE offset and was never emitted — removed, with the
generated C verified byte-identical. It was NOT the source of the `str`
(checked first, and the union survived its removal).

## Verification plan

- [x] `bh` compiles, runs, stdout matches CPython
- [x] six CI gates green at the defaults; suite 316/0
- [x] corpus `-m check` three-arm A/B above
- [ ] **to default the pair on:** land
      [132](132-arity-is-representation-not-provenance.md)'s arity drop, then
      re-measure `quameon`
- [ ] `PYC_CSCONTENT=1` removes 3 SPURIOUS warnings from
      `tests/match_seq.py` (a `case [[a, b], c]` pattern complaining about an
      `int64` that simply does not match). Its golden needs re-blessing at
      the flip, not before.

## What this unblocks

The `defs=1 DECLINED` and `sets=0` lines that dominate route 4's late passes
are, for every arity-N literal in the program, this one gated helper. Fixing
the pair moves five corpus programs out of compile failure and a third of the
corpus's warnings.
