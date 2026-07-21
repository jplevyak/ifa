# Issue 006: simple_inlining misses straight-line multi-SEND wrappers

**Status:** **closed June 2026** by commit `65bdc7e`
(`ifa/optimize: chain-aware matcher in simple_inlining`).  The
chain matcher (`match_prim_chain` + `inline_prim_chain` +
`INLINE_PRIM_CHAIN` event) implements the spec below.  The
issue's specific `add_one` example no longer reproduces in HEAD
— the existing pipeline already collapses it — so the
practical coverage win is zero on the current pyc test set.
Infrastructure stays in place for future wrappers; the natural
follow-on is Gap A (iterative inlining), filed separately as
[022-iterative-inlining.md](022-iterative-inlining.md).
**Affects:** `ifa/optimize/inline.cc:inline_single_sends`,
`ifa/testing/phases/07_dce_optimize.md` (Inline test cases).
**Related:** [005-retire-speculative-sym-level-dce.md](005-retire-speculative-sym-level-dce.md)
(predecessor — established that `simple_inlining`'s purpose is to
clean up IR-gen-emitted trivial wrappers so the IR generator can stay
naive; the cost/benefit + iterative work belongs to a future "full
inliner," not here).

## Symptom

Pyc test program:

```python
class Box:
    def __init__(self, v): self.v = v
    def get(self): return self.v             # inlines
    def add_one(self): return self.v + 1     # does NOT inline
b = Box(10)
print(b.get())       # → emits prim_period directly in __main__
print(b.add_one())   # → emits a real call to Box::add_one
```

`b.get()` is fully inlined — no `Box::get` function emitted. But
`b.add_one()` survives as `Box::add_one` (`_CG_f_2206_1` in the .c
output) and the call site emits a real function call. The bodies
differ only in length:

| Function | Body | Inlined? |
|----------|------|----------|
| `get` | one SEND: `prim_period(self, 'v') → t` | yes |
| `add_one` | two SENDs: `prim_period(self, 'v') → t; prim_add(t, '+', 1) → r` | no |

Both are straight-line wrappers consisting entirely of primitive
SENDs. Both *should* inline if the goal is "IR-gen can emit naive
wrappers and the inliner cleans them up."

## Root cause

`inline_single_sends` (`inline.cc:260`) finds bodies with exactly one
non-MOVE, non-closure-create, non-reply SEND. The matcher walks
PNodes once and bails on the second non-skipped SEND:

```c
for (PNode *n : f->fa_all_PNodes) {
  if (!n->code || n->code->kind == Code_MOVE || !n->live) continue;
  if (n->prim == prim_reply) { ... track reply ... }
  if (n->code->kind == Code_SEND && is_closure_create(n)) continue;
  if (!p) p = n; else { p = f->exit; break; }  // bail on 2nd SEND
}
```

The single-SEND restriction is what cuts `add_one` (and every
similar `def m(self): return f(self.field)` pattern) out of the
inliner's reach. There's no fundamental reason a *straight-line
chain* of primitive SENDs can't be inlined — the splice is just n
PNodes instead of 1.

## Proposed fix: chain-aware matcher

Extend the matcher to accept any straight-line chain of primitive
SENDs feeding a single reply. Constraints that keep it "simple":

1. **Single-pass.** Still one walk of `fa_all_PNodes`. No iteration
   to fixed point.
2. **Pattern-match, not heuristic.** No size threshold, no cost
   estimation. Either the body matches the shape or it doesn't.
3. **Primitives only.** Each chain element must have `p->prim` set
   AND `f->calls.get(p)` null. (Closure-call forwarding is Gap C —
   deferred, see "Out of scope" below.)
4. **Straight-line.** No branches, no loops, no labels with multiple
   in-edges. (Easy check: every PNode in the chain has exactly one
   cfg_pred and one cfg_succ, except the entry and exit.)
5. **No cross-flow.** Each SEND's rvals reach only: callee args,
   constants/symbols, or lvals of *earlier* SENDs in the chain.
   No reads from globals, no reads from later SENDs (would imply
   data-flow that isn't straight-line).
6. **Single reply.** Exactly one `prim_reply`, reading the last
   chain element's lval (or directly an arg/constant — same as the
   existing identity check).

### Sketch

```c
// Returns the chain as a Vec<PNode*> if matched, else null.
// Chain[0] is the first SEND, Chain[N-1] is the last SEND that
// feeds the reply.
static Vec<PNode*> *match_prim_chain(Fun *f) {
  Vec<PNode*> *chain = new Vec<PNode*>;
  PNode *reply = 0;
  for (PNode *n : f->fa_all_PNodes) {
    if (!n->code || n->code->kind == Code_MOVE || !n->live) continue;
    if (n->prim == prim_reply) {
      if (reply) return 0;  // bail: multiple replies
      reply = n;
      continue;
    }
    if (n->code->kind == Code_SEND && is_closure_create(n)) continue;
    if (n->code->kind != Code_SEND) return 0;  // bail: non-SEND
    if (!n->prim || f->calls.get(n)) return 0;  // bail: closure call
    // Each chain element's rvals must reach only args / constants /
    // earlier chain lvals.
    for (Var *v : n->rvals) {
      Sym *fs = first_var(v)->sym;
      if (fs && f->sym->has.index(fs) >= 0) continue;
      if (v->sym->is_constant || v->sym->is_symbol) continue;
      // Reaches a Var: must be an earlier chain element's lval.
      bool ok = false;
      for (PNode *prior : *chain)
        for (Var *lv : prior->lvals) if (reaching_var(v, lv->sym->var)) { ok = true; break; }
      if (!ok) return 0;
    }
    chain->add(n);
  }
  if (!reply || chain->n == 0) return 0;
  // Reply must read last chain element's lval (or be identity — already handled).
  Var *retv = reply->rvals[reply->rvals.n - 1];
  PNode *last = chain->v[chain->n - 1];
  bool reply_ok = false;
  for (Var *lv : last->lvals) if (reaching_var(retv, lv->sym->var)) { reply_ok = true; break; }
  if (!reply_ok) return 0;
  return chain;
}
```

### Splice mechanics

`inline_single_pnode` already splices one PNode at a call site. For
a chain, splice all N PNodes in order, substituting:
- The chain's reads of the callee's args → caller's actual args.
- The chain's writes to intermediate temps → fresh temps in the
  caller's namespace (so the inlined chain doesn't collide).
- The reply's read → the caller's call-site lval.

Should be a generalization of `inline_single_pnode` to a `Vec<PNode*>`
input. Most of the substitution scaffolding is already there.

## What this does NOT include (out of scope)

These are reasonable next questions but belong to follow-up issues
or the full inliner, not here:

- **Gap A: iterative inlining.** `def chain(self): return self.get()`
  would inline if the inliner ran a second pass after `get()` was
  inlined into it. Adding iteration to `simple_inlining` blurs the
  line between the simple peephole and a full inliner. Deferred:
  the full inliner is the right home, and the *reason for building
  it* will partly be cases like this.
- **Gap C: closure-call forwarding.** `def __new__(...): return
  clone(...)` style wrappers where the body's one SEND is a closure
  call, not a primitive. Inlining requires updating `f->calls` and
  possibly re-running dispatch. Bounded if the callee's callee is
  already resolved by FA, but the rewrite mechanics are non-trivial.
  Re-evaluate after this issue lands — if Gap B catches the common
  cases, Gap C may not be worth the rewrite cost.

## Verification plan

1. **Inline-coverage probe.** Before/after counts of surviving
   wrapper functions across the pyc test suite. A simple heuristic:
   `grep -c '_CG_f_' tests/*.py.c` before and after. Expect a
   measurable drop on tests that exercise method wrappers
   (`class_inheritance.py`, `operator_overload.py`,
   `class_init.py`).
2. **Targeted ifa-test fixtures.** Add to `ifa/tests/ir/inline/`:
   - `03_prim_chain_two`: two-SEND straight-line wrapper — expect
     `INLINE_CHAIN` event at the call site (new event kind).
   - `04_prim_chain_three`: three-SEND wrapper.
   - `05_chain_with_constant`: chain mixing args and constants.
   - `06_chain_with_branch_rejected`: branch in body → no inline
     (proves the matcher is selective).
3. **Full regression.** `make test` (pyc) + `make test-ir` (ifa) all
   green after the change. Re-bless `dce/01_baseline.ir.dce.expected`
   etc. if per-fun PNode counts shift (they will — more callees get
   inlined, leaving call sites empty).

## What this unblocks

- **IR-gen simplification.** Pyc emits two-SEND patterns like
  `prim_period + prim_add` whenever a method reads a field and
  computes on it. Currently each such method is a real function in
  the .c output. Removing them shrinks the .c output and eliminates
  one function-call's worth of indirection per use.
- **Realistic operator-overload performance.** Idiomatic Python
  classes with `__add__`, `__lt__`, etc. that read a field and
  delegate to a primitive are the *most common* method pattern.
  Currently each call goes through a real wrapper function. With
  Gap B closed, the wrappers vanish.
- **Pressure-test for the "simple" boundary.** If Gap B turns out
  to require iteration to be effective (chains exposing new chains
  in inlined bodies), that's evidence the full inliner is overdue.
  If it doesn't, the peephole is doing its job.

## Why deferred (instead of fixed inline with 005)

- Independent change. 005 is about removing a redundant pass;
  this is about extending a kept pass. Bundling would muddy the
  bisection story.
- Different test surface. 005 validates "behavior unchanged after
  removing speculation"; this validates "more wrappers inlined."
  Different goldens, different fixtures.
- Needs measurement first. The "inline-coverage probe" should be
  built and run *before* the matcher change, so we know which
  tests' goldens are expected to shift and which aren't.

## Follow-up — June 2026

The chain-aware matcher (`match_prim_chain` + `inline_prim_chain`)
landed per the spec above:

- `match_prim_chain(Fun *f)` accepts chains of ≥2 primitive SENDs
  feeding a single (optional) reply, with each chain element
  reading only formals / constants / earlier chain lvals.
- `inline_prim_chain(Fun *f, PNode *p, Fun *fn, Vec<PNode*> *chain)`
  mutates `p` in place as chain[0] (preserving call-site lvals via
  allocator), then inserts new PNodes for chain[1..N-1] linked
  into the CFG. Intermediate writes go to fresh Vars in the
  caller's namespace; the final chain element writes to `p`'s
  original lval. Reuses `inline_single_pnode`'s rval-substitution
  logic for formals and type-coercion MOVEs.
- New `INLINE_PRIM_CHAIN` event kind; printer recognizes it as
  `prim-chain`.
- Wired into both call-site loops (direct SEND and
  closure-collapsed SEND), prioritized over single_send/identity
  so >=2 chains take precedence over their (non-existent) single
  matches.

**Verification results.** Implementation is correct in shape and
fires on real pyc code, but the issue's specific example
(`add_one` surviving in the .c output) does not reproduce in HEAD.

| Probe | Before chain matcher | After |
|---|---|---|
| `_CG_f_*` symbols across all pyc test `.py.c`s | 10 | 10 |
| Total `.py.c` line count | 10280 | 10282 |
| `./test_pyc` | 73 / 2 / 0 | 73 / 2 / 0 |
| `./ifa --test` | 52 / 0 | 52 / 0 |
| `make test` (all phases) | clean | clean |

The chain matcher fires on real pyc-generated code (e.g. 4 funs
in `sieve.py`, 1 in `dict_basic.py`), but reaches no wrappers that
weren't already optimized away by another pipeline phase. The
likely reason: the existing closure-collapse + single_send + DCE
chain already handles the original example's `add_one` shape via
a different path, so the surviving-wrapper set was effectively
already at the floor.

**Why the example no longer reproduces.** The issue was filed
prior to the tier 0-3 cleanups. Several of those (in particular
the dispatch / clone / DCE changes that landed around the
phase-09 round) reshape the post-FA IR enough that the example
`add_one` is fully consumed by the existing pipeline. The chain
matcher is correct insofar as it implements the spec, but its
*practical* coverage win for the current pyc test set is zero.

**What this leaves.** Two things worth carrying forward:

1. The chain matcher infrastructure is in place. If a future pyc
   change exposes a 2+ SEND wrapper that survives FA / clone, the
   matcher will catch it without further work.
2. **Gap A (iterative inlining) is the natural next ask.** When
   the single_send pass inlines a callee body into a caller, the
   caller's own body may *become* a chain of N SENDs. Without
   iteration, that chain is never reconsidered. The issue's "Out
   of scope" §A flagged this; with the chain matcher in place,
   the iteration's gain would now be real (one pass to inline
   single_send, second pass for the matcher to consume the
   resulting chain). Filing as a follow-on if anyone wants to
   pursue it.

**Test-fixture gap.** The `.ir` text format and the ifa-test
inline phase harness don't currently produce a state where
multiple user funs land in `fa->funs` (only `__main__` makes it
through; even the existing `02_identity` fixture documents this
gap with a `0 events` baseline). Attempts to construct a
chain-firing fixture all degenerated to a single-fun analysis
state and didn't exercise the matcher. The matcher is verified
on pyc compilation (where multiple funs do reach the inliner)
rather than via a per-issue ifa-test fixture.
