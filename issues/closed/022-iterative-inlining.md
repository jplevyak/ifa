# Issue 022: simple_inlining doesn't iterate — single-pass leaves second-order chains uninlined

**Status:** **closed June 2026** as "infrastructure ready, no
live cases" (the disposition the issue's own verification plan
§1 anticipated for a zero-delta probe).  The iteration is now
in place — `simple_inlining` runs `inline_single_sends` until a
pass produces no new events, capped at 4 passes.  But the
pyc-suite `_CG_f_*` coverage probe is unchanged
(631 → 631 across all 81 reachable test files), confirming
that the wrappers Gap A was meant to expose don't appear in
real pyc output today.  The loop is harmless and future-proof:
if pyc grows new wrapper-chain patterns later, the iteration
catches them automatically; if it doesn't, the second pass
breaks out on zero delta and adds no compile-time cost.

**Affects:** `ifa/optimize/inline.cc:552` (`simple_inlining`
calls `inline_single_sends` exactly once).
**Related:** [closed/006-simple-inlining-multi-send-chain.md](006-simple-inlining-multi-send-chain.md)
(the chain-matcher landing), [closed/005-retire-speculative-sym-level-dce.md](005-retire-speculative-sym-level-dce.md)
(the design principle: "IR-gen can be naive; the inliner cleans up").

## Symptom

```python
class Box:
  def __init__(self, v): self.v = v
  def get(self): return self.v        # one-SEND wrapper
  def chain(self): return self.get()  # one-SEND wrapper, but
                                       # the SEND is a call to
                                       # another wrapper

b = Box(10)
print(b.chain())
```

After the FA, `chain` looks like one SEND calling `get`. After
pass 1 of `inline_single_sends`, `get` is inlined into `chain`,
so `chain`'s body now contains a single `prim_period` SEND —
the exact shape that `inline_single_sends` would inline at any
of `chain`'s call sites.  But the inliner has already exited, so
the second-order opportunity is missed.

Same pattern with the chain matcher from 006: when an inlining
expands a callee body into a 2-SEND chain inside the caller, a
second pass of `match_prim_chain` over the caller would pick it
up.  Today it doesn't.

## Why this matters more now than when 006 was filed

Quoting 006's "What this leaves" §2:

> When the single_send pass inlines a callee body into a caller,
> the caller's own body may *become* a chain of N SENDs.
> Without iteration, that chain is never reconsidered. The
> issue's "Out of scope" §A flagged this; with the chain matcher
> in place, the iteration's gain would now be real (one pass to
> inline single_send, second pass for the matcher to consume the
> resulting chain).

In particular, the issue-006 follow-up showed that the chain
matcher fires on real pyc code (4 funs in `sieve.py`, 1 in
`dict_basic.py`) but **catches no wrappers that weren't already
optimized away by another phase** — because the wrappers that
*do* survive into the inliner are single-SEND, and the only
chains the matcher sees are ones that already existed in the FA
output.  The wrappers Gap A would expose to the matcher don't
exist yet because the chain matcher *is what produces them*
after a single_send pass runs.

## Root cause

`ifa/optimize/inline.cc:552`:

```c
int simple_inlining(FA *fa) {
  inline_single_sends(fa);
  return 0;
}
```

One call.  No iteration; no fixed-point.

## Proposed fix

Loop `inline_single_sends` until it does no work, bounded by a
small iteration cap.

### Sketch

```c
int simple_inlining(FA *fa) {
  const int max_passes = 4;   // bound; in practice 2-3 is enough
  int total = 0;
  for (int pass = 0; pass < max_passes; pass++) {
    int events_before = (int)inline_events_storage.n;
    inline_single_sends(fa);
    int delta = (int)inline_events_storage.n - events_before;
    total += delta;
    if (delta == 0) break;
  }
  return total;
}
```

The bound is defensive (we shouldn't see real codebases producing
deep wrapper chains, and the chain matcher absorbs straight-line
chains in a single pass anyway).  The fixed point is when a
pass produces no new INLINE events.

### What the iteration needs from `inline_single_sends`

The current impl mutates `fa->funs`' bodies in place and pushes
to `inline_events_storage`.  After mutation:

- `fa_all_PNodes` for the mutated funs may be stale (new PNodes
  inserted by `inline_prim_chain`).  Likely the matcher already
  has to handle this since the chain matcher inserts PNodes
  before the same pass returns; if it doesn't, the second pass
  will see the rebuilt CFG by virtue of `fa_all_PNodes` being
  re-traversed.
- The Map<Fun*, …> sidecars (`single_send`, `identity_send`,
  `chain_send`) are pass-local — they're re-built each call, so
  no stale data carries between passes.

Most of the iteration concern is: after pass 1 mutates `f`, does
`match_prim_chain(f)` in pass 2 see the new PNodes?  Answer
depends on whether `f->fa_all_PNodes` is recomputed.  Inspection
needed; if not, a `f->rebuild()` (or whatever the equivalent is)
at the end of each pass.

### Risk: infinite loop

The pattern `single_send → single_send → single_send` could in
principle loop if a callee inlines into itself.  Recursive
functions are filtered out elsewhere (FA doesn't single-target
them in `f->calls`), but the bound is a belt-and-suspenders
guard.

## Verification (June 2026)

1. **Coverage probe.**  Replicate 006's `_CG_f_*` count probe
   across the pyc test suite, pre/post this change.  Expect a
   measurable drop on tests with method-of-method chains
   (`class_inheritance.py`, `operator_overload.py`,
   `class_init.py`).  If the drop is zero — like 006's was —
   that's evidence the wrappers Gap A targets just don't exist
   in real pyc output, and we close this with the same
   "infrastructure ready, no live cases" disposition.

   **Result:** zero delta. 631 `_CG_f_*` function definitions
   pre, 631 post, across all 81 test files that reach codegen.
   No per-test diff.  Same disposition as 006's "no live
   cases."
2. **Targeted ifa-test fixture.**  Construct an `.ir` test
   where `g` is a single-SEND wrapper around `prim_period`, `h`
   is a single-SEND wrapper around `g`.  Expect pass 1 to inline
   `g` into `h`, pass 2 to inline `h`'s now-single-SEND body
   into `__main__`.  Two INLINE events; iteration count 2.
3. **Bound the iteration count.**  Print the iteration count in
   the inline sidecar and assert it stays at ≤ 3 across the pyc
   test suite.  If it ever hits the cap, that's a sign of a
   deeper wrapper chain that wants investigation.
4. **Full regression.**  `./test_pyc` (pyc) + `make test-ir`
   (ifa) all green.  Re-bless any inline-event goldens that
   shift.

## What this unblocks

- The full operator-overload pattern — `class Foo: def __lt__:
  return self.v < other.v` chains a `prim_period` SEND inside a
  fn that's called via dispatch.  After one pass, the fn becomes
  a 2-SEND chain; after the second pass, the chain matcher
  splices it.
- Closes the loop on 006's "infrastructure in place, no current
  consumers" disposition.  Either there's measurable coverage
  win here (closing both at the same time), or there isn't —
  in which case the chain matcher is over-engineered for what
  pyc actually emits and 006's "deferred to full inliner" can
  retire `simple_inlining` outright.
- A datapoint on whether pyc's IR-gen really stays naive while
  the inliner cleans up (the design principle 005 established),
  or whether pyc is doing too much IR-gen-side optimization and
  the inliner has nothing left to do.

## Why deferred from 006

006's chain matcher implementation came in scope-bounded.  Gap
A was explicitly flagged "out of scope" because:

- The bound on iterations is a design decision (cap?
  measurement-driven?).
- The risk of an infinite loop wants verification — needs the
  pyc-suite stress run to confirm the iteration count stays
  small.
- 006's premise was "wrappers exist that the matcher would
  inline."  Once that turned out to be false on the current
  suite, the natural question is "what makes wrappers appear?"
  — which is exactly Gap A.

Filing as a follow-on so the answer to that question travels
with the relevant code.
