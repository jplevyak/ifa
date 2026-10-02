# 178 — route 4 cannot separate two creation points of a class built through one constructor

**Status:** open. Filed 2026-09-30 from
[132](132-arity-is-representation-not-provenance.md), whose synthetic test it
blocks. **The repro passes by default since 2026-10-02**: None is kept in the
type projection (see "Flipped" at the end), so `__init__` and `Pot` split on
`{list}` vs `{None}`. It is `tests/none_default_arg_splits_record.py`.
Still open: route 4's key is blind to a store's receiver (Root cause, fact 2),
which the keep-nil split went around, not through.

## Symptom

```python
class Pot:
    def __init__(self, n, labels=None):
        self.n = n
        if labels == None:
            self.labels = []
            for i in range(n):
                self.labels.append(i * 10)
        else:
            self.labels = labels
    def show(self):
        r = 0
        for j in range(self.n):
            r += len(str(self.labels[j]))
        return r
class Wave:
    def __init__(self):
        self.pot = Pot(3)
    def total(self):
        return self.pot.show()
def run():
    w = Wave()
    print(w.total())
    w.pot = Pot(1, ["abcd"])
    print(w.total())
run()
```

CPython prints `5` then `4`. pyc refuses it: `expression has mixed basic
types: ( int64 str )` at `self.labels[j]`.

## Cause

Both `Pot(...)` calls produce ONE `Pot` CreationSet (`IFA_DBG_FUNES=show`:
one contour, `self = {Pot#…}`), so its `labels` member holds the int-list
and the str-list. `self.labels.append(i * 10)` then runs through that merged
member and writes ints into BOTH lists' elements; both `list.__getitem__`
contours return `{int64, str}`. The union is real by then, and nothing
downstream can separate it.

The record should have split. Route 4 sees it (`IFA_DBG_CSDEFSPLIT`:
`cs=… sym=Pot defs=2`) and declines: `1 group: every creation point on the
same assign sets`. Its partition key groups creation points by which
content each can reach, and here both reach the same writes. The writes go
through the class's constructor and `__init__` contours, which serve both
creation points. `PYC_CSDCPA1=0` (per-site identity) does not help either,
because construction runs inside one shared constructor wrapper: one
allocation site.

The confluence is that shared contour. By AGENTS.md's method, the demand
(the violation at `self.labels[j]`) must be backtracked to it and the
contour split, so the two creation points reach different writes. That is
ESBLOCK's job (`find_blocking_es`), and here it is not reached. Check
first whether the CreationSet is in `demanded` at all: the violation's
backward walk may not reach the member AVar.

## Root cause, measured 2026-09-30

Three facts, each from a probe:

1. **Not a missing demand alone.** Pot is not in `demanded` (only its
   member lists are). Letting a CreationSet that owns a demanded member
   inherit the demand does reach the ES-block rung, which finds the shared
   contour `Pot.__init__` (`[esblock] candidates=1: es52/__init__`). But it
   still declines, for the reason in 2.
2. **Route 4's key is blind to the receiver of a store.** It groups creation
   points by which CONTENT each can reach along value flow. A store
   `self.labels = X` flows X into the record's member. The receiver's
   creation point is not on that path, so both creation points reach the
   same sets. `cs_def_groups` with `__init__`'s formals held terminal finds
   one group as well, which is why `find_blocking_es` rejects its only
   candidate.
3. **`None` is stripped from the type projection by design.** That is issue
   060 in `type_cannonicalize`: a `{T*, None}` union stays one clone,
   because None is a null pointer. So CPA does not split `__init__` between
   the `labels=None` call and the `labels=[...]` call. (A misleading probe
   on the way: `IFA_DBG_FUNES` prints `av->out->type`, which drops
   constants, so a None argument prints as `[]`.) Per-site identity
   (`PYC_CSDCPA1=0`) does not help either: the second `__new__` contour is
   a split child, and parent-first (`PYC_CSPARENTFIRST`) joins its creation
   point to the parent's CreationSet.

## Fix needed

A route-4 key that associates a store with its RECEIVER: for each creation
point, which writer contours it flows into as the receiver of a member
store, and what those contours write. Plus, when the writer contour is
shared, an ES split of that contour per receiver creation point (the ES
split as a MEANS, which AGENTS.md sanctions only under a demand). The
demand is the member union that the violation backtracks to.

## Verification

The repro prints `5` and `4` on both backends, with two `Pot`
CreationSets. It then becomes 132's synthetic test, for the untagged-dispatch
demand and the clone-collapse rule.

## Probe: keep None in the type projection (`PYC_KEEPNIL=1`), 2026-10-01

Fact 3 strips `nil_type` from `->type` whenever the rest of the union is
pointer-shaped (issue 060). That hides a real type confluence. A None-only
writer projects to `{}`, so `collect_type_confluence` skips it, and
`{list}` vs `{None}` at `__init__`'s `labels` never registers.
Nullability is a type, so `PYC_KEEPNIL=1` (`fa_flags.cc`, used in
`type_cannonicalize`) keeps nil unconditionally.

**The repro passes with it.** `__init__` splits into a `{list}` and a
`{None}` contour. Per-contour folding of `labels == None` then gives each
contour one live store, there are two `Pot` CreationSets, and the program
prints `5` and `4`. No route-4 or ESBLOCK change was needed for this
program. (Open: `range` also gets a second CreationSet, `#1116`, in the
None contour. Check that this is demand-driven, not a CS following the ES
split.)

**`make test` (C e2e): 375 passed, 3 failed.** `test-ir` passed. The LLVM
e2e did not run, because `make test` stops at the first failure.

- `nil_union_prealloc`: ifa/164's primitive-argument check reads `->type`
  and now rejects the temporal `{str, None}` in `r + x`. The check must
  accept nil beside a pointer as a nullable pointer by representation,
  not depend on the projection having dropped it.
- `expr_evaluator`: `evaluate` gets `{None}`-only contours (es=81, 92).
  Every path in them raises, so they return bottom, and
  `r = evaluate(e.rhs)` reports `'r' has no type` on a runtime-dead
  branch. Contours: `evaluate` 1 -> 11, `Expr` CreationSets 1 -> 9. The
  split unrolls the literal trees by the nullness of `lhs`/`rhs`.
- `deepcopy_objects`: NOT a None problem at the failing site. Reduced
  repro: T tree + `Node` deepcopy + copy-of-copy. In the final pass,
  `node.args` is four tuples and no None, and `tuple.__getitem__` returns
  `{T, T}`. But the bound `node.args.__getitem__` closure has no dispatch
  edge after a late split (SETTER, pass 15/16) re-mints `count`'s receiver
  as a new `T` CreationSet. The flag only changes the split sequence. The
  next step is a probe of the closure CreationSet and the reason for the
  dispatch refusal.

**Corpus `check` sweep**, `e21cc929+d4673d70`, default vs `PYC_KEEPNIL=1`:

| | default | KEEPNIL |
|---|---|---|
| compile_fail | 25 | 26 (+fysphun) |
| run_fail | 16 | 14 |
| stdout_differs | 4 | 5 |
| cs/shapes | 2176/664 = 3.28 | 2196/692 = 3.17 |

- `fysphun` regresses, by the same mechanism as `expr_evaluator`, reached
  a different way. `Link.p1/p2` start None and are overwritten (temporal).
  Both `applyme` edges carry `{None, Point x3}`, and stage 5's
  per-CreationSet fan counts None as one of the parts, minting
  `twopoint(p2={None})` (es=228). There `p2.x` is bottom, so `'xd' has no
  type`. No edge distinguishes None from Point here, so this is not a
  confluence. The fan is partitioning within one edge's union.
- `chull` 124 -> 0 and `tonyjpegdecoder` 124 -> 0 are timing noise. Both
  sit at the 120 s cap. chull's output is identical apart from its
  `TIME` line (54 s vs 59 s, single sample). tonyjpegdecoder's contour
  counts are identical between the arms.
- Contour growth: `life` +146 CS, `loop` +62 ES / +133 CS, `chull` +24 ES,
  `tarsalzp` +21 ES. Shrinkage: `sudoku3` -380 CS, `go` -34 CS. No other
  verdict changed.

**What this says.** Keeping None is a net win for the repro and close to
neutral on the corpus, but it exposes two consumers that assumed nil
would be stripped:

1. **A `{None}`-only contour.** It comes from either a real confluence
   (`expr_evaluator`) or the per-CS fan (`fysphun`). Only the confluence
   is legitimate. The fan splitting nil off a pointer union that every
   edge carries is the arbitrary stage-5 fan (ifa/146), now with one more
   part. In a legitimate `{None}` contour, a dereference is a guaranteed
   AttributeError. Bottom after it means unreachable, not "no type".
2. **Representation checks that read `->type`** (ifa/164).

## Measured: KEEPNIL + NORETURN together, 2026-10-01

Tree `220794a9+c2afdd06`, built clean (`fa.h` changed). It adds three
things to the probe above:

- `220794a9` decides ifa/164's "nullable pointer" directly, which fixes
  `nil_union_prealloc` (point 2 above).
- **`nil_rides`** in `split_edges`' per-CS fan. None is a part of its own
  only when some edge brings `{None}` alone or a num_kind scalar sits
  beside it. Otherwise it rides in the pointer parts. This is aimed at
  `fysphun` (point 1, the fan case).
- **`PYC_NORETURN=1`**. A non-primitive call whose result is bottom gates
  the walk past it, as `Code_IF` does for a bottom condition.
  `propagate_out_change` releases the gate when the result becomes
  non-bottom. `IFA_DBG_GATE=<fun>|*` lists the calls that are still gated.
  This is aimed at `expr_evaluator`'s `'r' has no type` (point 1, the
  confluence case).

All three arms ran on the same binary:

| | default | KEEPNIL | KEEPNIL+NORETURN |
|---|---|---|---|
| compile_fail | 24 | 24 | 26 |
| run_fail | 13 | 13 | 12 |
| stdout_differs | 5 | 5 | 5 |
| ess / css, 50 programs whose verdict is the same in all three arms | 17621 / 65437 | 17722 / 65247 | 17381 / 64608 |
| C e2e failures | 0 | `deepcopy_objects`, `expr_evaluator` | `expr_evaluator`, `field_mixed_write_demand` |

The LLVM e2e matches the C e2e in the KEEPNIL+NORETURN arm, at 376 passed.
Sweeps: `check__{default,PYC_KEEPNIL_1,PYC_KEEPNIL_1_PYC_NORETURN_1}__220794a9+c2afdd06`.

**KEEPNIL alone is now verdict-neutral on the corpus.** `nil_rides` fixes
`fysphun`: it compiles and its output matches CPython. The other change
from the `e21cc929` sweep is that `nil_union_prealloc` passes. Two e2e
failures remain, `deepcopy_objects` (a late re-mint, see above) and
`expr_evaluator`.

**NORETURN is a net regression.** Every verdict change on the corpus is
NORETURN's:

| program | default and KEEPNIL | with NORETURN |
|---|---|---|
| `fysphun` | runs, output matches CPython | compile fail |
| `ac_encode` | runs to the 120 s cap (CPython does too) | compile fail |
| `yopyra` | runs to the 120 s cap (CPython does too) | compile fail |
| `life` | compile fail | compiles, then aborts: `matching function not found` with a `_CG_nil_type` argument |

`life` is not an improvement. A compile-time type error became a runtime
abort.

**Mechanism: the gate cuts off the flow that would resolve its own call.**
In `fysphun`, `twopoint`'s `xd = p1.x - p2.x` stays gated, and its operand
is `{int64, float64}`. The write that widens `x`, `p1.x += xd`, comes
after the gated call. In `ac_encode`, `w = b - a` at the head of
`encode`'s loop stays gated. With NORETURN off, `a`, `b`, `boundary`, `u`
and `v` are widened to float across all of `encode` and `decode` (about
200 "holds both int and float" warnings). With it on, the widening stops
at line 26, so nothing past the gated call was ever walked. Each gated
`__sub__` fails to dispatch, so its result is bottom. Bottom keeps the
gate shut, and the shut gate keeps out the loop-carried or later write
that would make the dispatch succeed.

So "a failed dispatch is reported where it happens" (the probe's comment)
is not enough. A failed dispatch is not "never returns". Gating it is a
decision taken on transient types, and it holds itself in place. That is
the AGENTS.md quiescence rule in another form. The numeric widening is
decided between passes, which is what lets a dispatch fail at all on a
program that CPython runs cleanly.

**The `ifa-test` `fa-init` goldens show the same defect from the other
side.** NORETURN alone fails 15 fixtures. KEEPNIL alone passes them all.
Each diff is `@__main__ rets=1 -> rets=0`. In the IR harness a callee's
`ret0` is bottom even with both flags off (`05_call`: `%add`'s formals are
typed, `ret0` is empty), so the gate shuts the caller's reply. Here the
bottom result means "the harness never carries the return value back",
not "never returns". Every other `ifa-test` phase passes.

**`expr_evaluator` is not a legitimate `{None}` contour.** With NORETURN,
`'r' has no type` is gone. What is left is
`unresolved member 'kind' of class '__pyc_None_type__'` at `e.kind`. But
`evaluate(e.rhs)` runs only under `if e.kind == 1`, and every
`kind == 1` object comes from `make_binop`, which always sets `rhs`.
CPython never calls `evaluate(None)`. The `{None}` edge exists because the
analysis does not tie `kind` to which object it holds: a unary `Expr`
CreationSet has `rhs = {None}`, and the `kind == 1` branch is live for it.
Point 1 above called this a real confluence. It is a type confluence, but
on a path that never runs. Two consequences:

- A dereference on a `{None}`-only receiver must compile to a runtime
  `AttributeError`, not a compile error, even under `--strict`. A
  well-formed CPython program can reach one on a path that never
  executes.
- A bottom after that dereference means "unreachable". That is right for
  an attribute read on `{None}`, but not for a dispatch that failed for
  some other reason.

**What NORETURN would have to be.** Decide "never returns" from the
CALLEE, not from the caller's result AVar. That means a call whose
dispatch succeeded, whose callee contours were all reached, and all of
whose `rets` are bottom at quiescence. An unresolved call never gates.
The `{None}` dereference becomes a raise inside the callee, so its
contour's `rets` are bottom for the right reason. The harness case
(`ret0` bottom with the callee walked) still trips this test and needs
its own look before NORETURN can be revisited.

## Fix 1 landed: a send that cannot complete, 2026-10-02

This is the callee-decided rule from the section above, and it replaces
`PYC_NORETURN`. It is on unconditionally, not behind KEEPNIL.

- **`gate_send`** (`fa.cc`) stops the walk after a send that cannot
  complete, and the send's result AVar holds the gate. There are two
  cases:
  - An attribute read on a receiver that is None and nothing else. Its
    result can only become typed through flow from upstream (the receiver
    gaining a class), never from the code the gate holds back.
  - A call that dispatched, where no callee contour reaches its exit
    (`Fun::exit`, which is the `reply` in pyc). Generators and coroutines
    are excluded. An unresolved call never gates, so the self-holding
    gate behind `ac_encode` and `fysphun` cannot form.

  `release_gate` reopens a gate in two situations: when the result gets a
  type, and when a callee first reaches its exit (`release_callers`).
- **`settle_gated_liveness`** runs at the top of `complete_pass`, at
  quiescence. A gate is decided during the walk, but liveness only grows,
  so code walked before its gate engaged stayed live. That is what
  `expr_evaluator` showed: `r = evaluate(e.rhs)` was walked while
  `e.rhs` was still bottom, so there was no edge and no gate yet. The
  settle step re-derives each gated contour's `live_pnodes`, and the
  `live_arg` marks that follow from them, on the converged state.
  `live_arg` is re-derived over every node in the function, because
  selective invalidation (ifa/111 M3) carries a preserved AVar's
  `live_arg` across passes.
- **`collect_member_violations`** no longer reports a gated nil-only
  read. That read is CPython's `AttributeError` at run time, not a
  compile error, including under `--strict`.
- **Codegen.** `PNode::fa_noreturn` is set in `fixup_clone_ess` when the
  send is gated in EVERY contour of the clone. `fa_live` cannot answer
  this, because it is the union over the clone's contours. The C backend
  emits `assert(!"runtime error: send does not complete")` after such a
  send, whether or not DCE kept the send. DCE drops it, since its result
  is unused once nothing after it is reached. Before this, the function
  fell off its end (`-Wreturn-type`). The LLVM backend needed no change:
  its output matches CPython. It has no explicit trap, though.

New test: `tests/none_receiver_dead_path.py`. A `get(None)` sits on a
branch that is never taken, under a condition FA cannot fold. Under
KEEPNIL it reported `'r' has no type` before this fix (twice, as above).
With defaults, nil is stripped, `get(None)` shares `get(o)`'s contour,
and the test passes either way.

**Measured.** `make test`: green, 379/0 on both backends. KEEPNIL e2e:
378/1 on both backends, and the 1 is `deepcopy_objects` (fix 2).
`expr_evaluator` passes under KEEPNIL with output identical to CPython
and no C warnings. KEEPNIL `make test-ir`: all 16 phases pass. The
earlier `fa-init` `rets=0` diffs are gone, because the IR fixtures'
functions reach `Fun::exit` and have no `reply`.

Corpus `check`, `220794a9+4ac9b1b8`, against `+c2afdd06`:

| | default | KEEPNIL |
|---|---|---|
| compile / run / stdout fail | 24 / 14 / 5 | 24 / 14 / 5 |
| verdict changes vs before | `brainfuck` | `brainfuck` |

`brainfuck` is timing, not a regression. It ran 115-120 s in every earlier
sweep. Run alone it takes 114 s, exits 0 and matches CPython, and its C
has no trap. The default and KEEPNIL arms agree on every verdict.

Contours fall where a gated send cut dead code out of the analysis. With
defaults: `pylife` ESS 383 -> 346 and CS 1647 -> 1544, `sudoku3` CS
2119 -> 1739, `othello` CS 1051 -> 1026. Every other program moves by
four contours or fewer.

ifa/049's repro is unchanged, as intended. A raise reaches the `reply`,
so `risky` counts as returning and nothing gates.

## Fix 2 landed: stale edge routes are re-taken at quiescence, 2026-10-02

**`deepcopy_objects` under KEEPNIL, root cause.** In the final pass,
`count`'s contour es54 has `node = T#1238`, and `node.args` holds tuples
#1088/#1230/#1231/#1236. The bound `node.args.__getitem__` closure call
has four edges. All four are bound (`e->to`) to `tuple.__getitem__`
contours es130-es133, which stage 5's per-CS fan made in an earlier pass
and which are filtered on tuples #1138/#1143/#1149. A later split
re-minted those tuples. So every edge's actuals have an empty
intersection with its target's filter. `analyze_edge` takes `LskipEdge`
for each one, and the result is bottom. The only report is
`illegal call argument type` (`dispatched_this_pass` false).

Nothing undoes this, because it is a deadlock. The edges never run, so
`__getitem__` never sees the receiver union, so no demand re-fans it.
`make_entry_set` returns early on an edge that already has a `to`, and
`clear_edge` keeps `to` by design: it is the routing ledger. KEEPNIL
only changed the split sequence that led here. This was a latent
defect: a routing decision taken on CreationSets that no longer exist,
never revisited (ifa/170's rule).

**Fix.** `reroute_uncovered_edges` (`fa.cc`) runs as step 0 of the split
sequence, at quiescence. For each live send, it unbinds a skipped edge
only when some CreationSet it carries is admitted by no edge that ran
this pass. That is, only when the call has lost part of its dispatch. A
fanned copy that is idle because another copy covers its CreationSet is
left alone. The next pass routes the unbound edge with the filter-aware
`make_entry_set`. The step mints nothing. When it acts, it sets
`analyze_again` and the split stages wait for the next pass.
`IFA_DBG_REROUTE=1` prints each reroute. On `deepcopy_objects` it fires
once, in pass 21, on es54's four edges, and the analysis then converges.

**Measured.** `make test`: green, 379/0 on both backends. Under KEEPNIL:
e2e 379/0 on both backends, and `make test-ir` passes all 16 phases.
**That clears both blockers to defaulting KEEPNIL on.**

Corpus `check`, `220794a9+eced1824`: default and KEEPNIL both 24 / 13 / 5
(compile / run / stdout fail), with identical verdicts. Against the fix-1
tree, no program's contour count moves in either arm, so the reroute
never fires on the corpus. The only verdict change is `brainfuck`, which
is back under the 120 s cap (timing, see above).

## Flipped: None stays in the type projection, 2026-10-02

The nil strip is deleted, not defaulted off. `type_cannonicalize` keeps
`nil_type` in `->type` unconditionally, and `PYC_KEEPNIL` is gone.
Whether `{T, None}` can be represented as one nullable pointer is still
decided, but where a representation is chosen: ifa/164's
`nil_member_is_representable` and stage 5's `nil_rides`. The comments that
described the strip as current are updated (`fa_flags.cc`'s ifa/164 note,
and the ifa/124 and ifa/133 notes in `fa.cc`). `split_type_view` (ifa/124)
now changes nothing for nil. It is left in place, and removing it is its
own cleanup.

**Measured** on a clean build. `make test`: green, 379/0 on both backends,
with no golden changes. Corpus `check` `5ca26ec4+36c6dfe7`: 24 / 14 / 5.
Per program, this is identical to the `PYC_KEEPNIL=1` arm of
`220794a9+eced1824` in every verdict and every contour count, except
`brainfuck`. It hit the 120 s cap with identical contours, so that is
timing: it ran 114 s alone, see above. Against the old default
(`220794a9+eced1824`), the cost is container CreationSets per shape
3.26 -> 3.15, CS 2162 -> 2181, and shapes 664 -> 692.

The repro prints `5` and `4` on both backends by default and is now
`tests/none_default_arg_splits_record.py`.
