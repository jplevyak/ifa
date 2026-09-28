# 074 — FA contour growth on a recursively rebuilt nested container

**Status:** open. Rewritten 2026-09-27. The long record this replaces
(2026-07-30 → 2026-09-25, ~2900 lines) is in git:
`git show 9dfbf0fc:ifa/issues/074-FA-cross-pass-oscillation-plan.md`.

**Affects:** `ifa/analysis/fa.cc`: `creation_point`, type splitting
(`decide_entry_set_split` and the edge-type compatibility it uses), the
CS_DEF_PART stage, and the split ledger.

**Blocks:** [168](168-FA-recursive-overlap-gate-refuses-its-own-split.md),
and through it `shedskin_examples/linalg`, which fails only because of
this. Replacing `Minor`'s `copy.deepcopy(M)` with a comprehension takes it
from 46 errors to 0.

## The problem

This issue began as "the splitter oscillates across passes". That half is
largely fixed (see *Landed*). What remains is **unbounded contour growth**:
when a recursive function rebuilds a nested container and passes the copy
back into itself, FA adds a contour level on every pass and never reaches
a fixed point.

`tests/deepcopy_recursive_nested_growth.py` (known issue; its `.env` turns
the stall guards off and sets `PYC_DBG_CONVERGED=1`):

```python
def shrink(M):  M1 = copy.deepcopy(M); del M1[0]; return M1
def total(M):   return 0.0 if len(M) == 0 else M[0][0] + total(shrink(M))
total([[1.0], [2.0], [3.0]])
```

**Target.** The program is monomorphic. `total` and `shrink` each have one
type at every recursion depth, and only two list shapes exist:
`list[list[float]]` and `list[float]`. The right answer is about 20
contours: `total`, `shrink` and `deepcopy` 1 each, `list.__deepcopy__` 2,
and each container method 2, one per shape. **The count must not depend on
recursion depth.** On 2026-09-25 it reached ess 533 at the pass cap, still
growing.

**Mechanism.** Every contour of `list.__deepcopy__` (`r = []`, append,
return; `__pyc__/04_sequence.py`) ends up with its own CreationSet for `r`.
The copy has the same shape as its source but a different CS identity.
Type splitting compares edge types by CS identity, so the caller gets a new
contour. That contour calls `__deepcopy__` with a new receiver CS, which
mints another `r`. The result is one extra level per pass. In linalg,
applying 168's targeted gate lets `list.__deepcopy__` reach 109 contours,
still splitting at pass 74 of 76.

**`deepcopy` is not the trigger; recursion over a nested list is.** As of
2026-09-25, a manual copy `M1 = [list(r) for r in M]` also runs to the cap
(it types correctly), while a flat list or a non-recursive loop converges.
Any fix aimed only at `deepcopy` is therefore incomplete.

**What shedskin does.** For type analysis, shedskin treats `deepcopy` as
the identity (`shedskin/lib/copy.py` returns `a`), and it generates the
actual copy as a C++ template over `list<T>`. `list<T>` is a type, not an
identity, so two `list<float>` allocations share one type, and
instantiation ends because the element type shrinks structurally. On a
cyclic container (`a.append(a)`), shedskin refuses exactly as pyc does.

**What is wrong, in one line:** pyc names a container's type by its
CreationSet identity, which is decided by structure (which contour
allocated it). A copy minted inside a contour, with the same shape as its
source, should not count as a distinction that justifies a new contour.
This is the identity-vs-shape question in
[146](146-remove-all-arbitrary-splitting.md), and a demand-splitting
defect: nothing asked for the new level.

## Forward options

Do these in order. Each one carries its stop condition, written before the
measurement is taken.

### 0. Locate the mint — DONE 2026-09-27; parent-first landed

`creation_point` minted only 3 list CSs on the reproducer. The per-level
CSs came from `CS_DEF_PART`, and the cause was the ROUTE: a split-child
contour's `r = []` went through the start-merged route to the sym's ROOT,
not to the CS its split parent's `r` was already in. That re-raised a
demand already settled, and the split minted a fresh CS, a new type, and
another ES split.

**The rule (author, 2026-09-27): an ES split must not split a CS. The CS
keeps the new creation points until a demand on the CS itself splits it.**
`PYC_CSPARENTFIRST=1` (default) puts the split-parent route ahead of the
start-merged one. Closures are excluded: `partial_application` asserts a
closure CS has exactly one def.

| | parent-first off | on |
|---|---|---|
| corpus CS / shape (`check`, 9dfbf0fc+71fbd58a) | 4.00 | **3.03** (-27% CSs) |
| compile failures | 27 | 26 |
| reproducer, `total` contours at the cap | 21 | 14 (still no convergence) |
| linalg | 46 errors, 49 s | **19 errors, 6 s** |
| `chull`, `quameon` | compile fail | compile; each then hit an OLDER bug |
| `plcfrs` | compiles, 131 s | **times out (> 600 s)** |

`chull` then crashed on two CPython-semantics bugs, both fixed alongside:
`enumerate` was eager (a loop that deletes from what it enumerates saw a
snapshot), and every float printed with `%.17g`. `quameon` aborts at run
time: `coulomb_pot.charges` holds an arity-1 record list in one object and
a list-layout list in another, FA raises no demand for it, and codegen
merges the clones into one struct with a `void` member instead of refusing.

**`plcfrs` is the open regression, and it is not the reproducer's
mechanism.** Refuted, each measured: (a) children made by the setter
stages skip parent-first — worse, 276 mints/pass; (b) `continuation`
skips parent-first — still times out; (c) `SETTER_OF_SETTER` acts only on
members whose type is a union (`PYC_SOSDEMAND=1`) — still times out. (c)
did expose a separate ifa/146 item: that stage's confluence test has no
type condition, and before the gate its main target was
`__list_iter__.position`, always `int64`, with a setter count growing
every pass (348 → 1212).

The actual chain, backtracked with `IFA_DBG_CSVARS` / `CSROUTE` at pass
42 (first line with parent-first on, last with it off):

1. All 13 dicts in the program sit in ONE CS (1707). No demand lands on
   it: its members each hold a single list CS, which is representable.
2. So `dict.__init__` has ONE contour for the whole run, and its
   `self._keys = []` / `self._vals = []` each have one creation point;
   by pass 10 both are in `list#4059`.
3. `list#4059` holds every key and value in the program,
   `{int64, str, ChartItem, Edge, Entry, ...}`. `CS_DEF_PART` sees the
   demand every pass, but its creation points all lie on the same assign
   sets (sharing makes every writer reach every def), so it finds 1-2
   groups and stops.
4. ifa/152's backtrack cannot reach 1707: it walks VALUE FLOW to
   suppliers, and 1707 is the OWNER of the demanded list, not a supplier.
5. `toid[...]` carries the union into `Rule.lhs` and `ChartItem.label`,
   and `SETTER_OF_SETTER` splits at those use sites every pass.

With parent-first off, pass 42 has several dict CSs, each with its own
separate `_keys` and `_vals`, and `label` is `int64`.

**Next step:** split the dict CS by the setters of what it holds. See
option 0b below.

### 1. Shape-equivalent compatibility in type splitting (the 168 follow-on)

When `decide_entry_set_split` compares the argument types of two edges,
treat two container CreationSets as the same if their **resolved recursive
element shapes** agree (`list[list[float]]`). Scalars and user classes stay
as they are. This keys on what the types ARE, not on where the value came
from, so it passes the provenance rule. It is the fix 168 names.

- **Measure:** the reproducer (expect `CONVERGED=1` and about 20 contours),
  then linalg with 168's targeted gate applied. That gate is: admit an
  overlapping recursive edge only when the ES owns the single creation
  point of a CS with an irrepresentable element (described in 168's
  2026-09-27 section).
- **Stop:** if equal shapes still merge two CSs that a later violation
  needs separated (linalg's error count rises, or `richards`/`chull`
  regress), then shape is not sufficient as the identity. Record which
  distinction was lost and do not widen the key to recover it.

### 2. Revisitable shape canonicalization of container CSs (066)

`PYC_CSELEM=3` (off) already keys container CS identity on the receiver's
structural shape. On the reproducer it bounds the growth: −62% ess at pass
101, oscillating in a band around 210 instead of climbing linearly.
Corpus-wide it is not shippable: failures go from 5 to 9 (`kanoodle`,
`plcfrs` and `rdb` time out; `quameon` fails to compile).

The measured limitation is that the decision is made once, inside
`creation_point`, before the shape has resolved: `type_key_pass` is −1 on
every ES that reaches it, and `cs_map` keeps the answer permanently. The
fix is to re-decide at quiescence on every pass. `analyze_to_convergence`
already re-derives types from bottom, and taking back a `cs_map` decision
has been measured to work (AGENTS.md, 2026-09-05). That is
[066](066-FA-cs-split-decision-keyed-per-pass-not-per-creation-site.md).

- **Stop:** if the re-decided canon still oscillates inside a band on the
  reproducer, the band is a second defect (canon vs. splitter disagreeing).
  Isolate it on the reproducer before measuring the corpus.

### 3. Type-preserving copy signature (narrow; last)

Give the copy a fresh CS whose element is constrained to the source's
(`elem(result) = elem(source)`) instead of being accumulated from appends.
This is not [048](048-FA-deepcopy-flow-divergence-genetic2.md)'s CS
sharing, which fed the copy's writes back into the source. Because a manual
copy also grows (see above), this alone cannot close the issue. Build it
only if 1 and 2 leave `deepcopy` as a special case.

## Landed (still default; keep)

| mechanism | what it fixed |
|---|---|
| `PYC_SELFPROD=6` | Accepts a period-2 flip-flop (`key(N)==key(N-2)`) as settled. Removed the reproducer's assignment churn; `key_hash` has 3 entries. Inert on the corpus. |
| `PYC_CSKEY=3` (includes 2's `split_origin` canonicalization) | Durable setter type in the CS ledger signature. Stopped the ledger missing repeats because of drifting `s->out->type`. |
| `FA::rederive_churn` | The divergence guard counts "mint anyway", not ledger routes (recovery). `pass_limit_hit` went 10 → 4 programs. |
| `PYC_CSMOLD=3` | The mold fallback is limited to containers and never used for a split child. Fixed the bounded sibling `tests/deepcopy_copy_of_copy_chain.py`. |
| display out of identity ([100](closed/100-FA-display-removed-from-contour-identity.md)); marks retired ([146](146-remove-all-arbitrary-splitting.md)) | ess −40-80% corpus-wide; retired this plan's old Stages 0 and 4. |

The rest of the corpus's non-convergence is a different disease,
characterized in [101](101-FA-first-time-forever-splitting.md): 95-98% of
split decisions are first-time signatures, so nothing that keys on a repeat
can help. The first-stage-wins cascade that starves later stages is
[157](157-FA-all-demand-must-be-evaluated-at-quiescence.md).

## Dead ends (measured; do not repeat)

| tried | result | why it fails |
|---|---|---|
| Route `check_split`'s lineage-mint to a bounded display variant | net negative | The display is not identity (100). |
| Detach-route reuse, `PYC_HARDREUSE` | oscillators 20→49, violations +62% | The `x!=split` veto plus a symmetric test makes flip-flops. |
| `find_best_entry_sets` on the detach route / hard-match reuse | 59 / 6 test failures (incl. `recursive_polymorphic`) | Soft matching is load-bearing. |
| Global hard type gate | non-convergence | Same: soft matching is load-bearing. |
| Durable type keys alone, `PYC_TYPEKEY` | a wash | Necessary, not sufficient. |
| Canonicalize on the durable key, `PYC_CANON` | mode 2: −32 tests | The splitter separates on dimensions that are not arguments. |
| CPA names in place of marks, `PYC_CPAMARK` | ≡ `NOMARK` | Adds nothing. |
| Callee-side CPA | +78% time, 10 new failures | `_CG_void` from products that are never reached. Removed (146 E). |
| `PYC_SELFPROD=1/2` | unsound | Disjointness is not stable as types widen. |
| `PYC_SELFPROD=3/4` | vacuous | Never fires. |
| `PYC_SELFPROD=5` (was "stopped moving?") | cannot fire | The test's own remedy keeps the contour moving. |
| Gate the ledger route, `PYC_ROUTEGATE` | repro ess 144 → 257-279 | Declining a route means minting instead. |
| Drop `s->out->type` from the CS ledger key, `PYC_CSKEY=1` | stops growth; linalg 43 → 651 violations | Merges real distinctions. It proved the diagnosis; it is not a fix. |
| CS-directed ES fan-out in the main loop (old Stage 2) | ruled out | No growing container union exists in the oscillators. |
| Dispatch coherence (old lever b) | ruled out | No dispatch bounce exists. |
| Naive complement eviction | wrong | Evicts contours that are still needed. |
| Raise the stall-guard constants | `rdb`/`amaze` false wins; `yopyra`/`rubik` regress | The guard is not the defect. |
| Admit every overlapping recursive edge, `PYC_RECOVERLAP` | linalg 46 → 106 | Splits unrelated recursions (`binary`, `determinant`, `sign`). |
| 168's targeted gate, alone | linalg 46 → 88 | Correct, but exposes this growth: 109 `__deepcopy__` contours. |
| Copy shares the source's CS (048) | reverted | Creates a feedback edge from the copy's writes into the source. |
| `PYC_CSELEM=3` as default | corpus failures 5 → 9 | Decided once, before the shape resolves (option 2 fixes this). |

Traps worth remembering:

- A canonicalization consulted in `creation_point` has to be memoized: an
  unmemoized shape string hung `adatron` inside a single pass.
- The unfilled-element placeholder must not be `_`, which appears in
  dunder names.
- Select `self` by position number (2), not by sorted index.

## Probes and tests

- `PYC_DBG_CONVERGED=1` prints `CONVERGED=0|1`, the one bit a test can
  assert.
- `PYC_DBG_STAGES=1` prints the set of stages that made progress.
- `IFA_DBG_STAGE` (per-pass stage trace), `IFA_DBG_CHURN` (the edge and
  ledger entry that move), `IFA_DBG_KEYSPACE`, `PYC_DBG_OSC`.
- `IFA_DBG_CSDEFSPLIT`, `IFA_DBG_CSROUTE`, `IFA_DBG_FUNES`, `IFA_DBG_CSVARS`
  for option 0.
- `tests/deepcopy_recursive_nested_growth.py`: this issue (known issue;
  also 168).
- `tests/deepcopy_copy_of_copy_chain.py`: bounded sibling, passing.
- `tests/splitter_*.py`: one test per split stage.

## Verification

1. The reproducer prints `CONVERGED=1` and `6.0`, with the contour count
   independent of list length. Try `[[1.0]]*3` and `*8`.
2. `make test`, green on both backends.
3. `./corpus_sweep.sh -m check`, compared against the current default
   sweep: linalg fixed (with 168 applied), no program regressed, and no
   rise in `cs/shapes`.
4. When the reproducer passes, remove its `.known_issue` and close this
   issue together with 168.
