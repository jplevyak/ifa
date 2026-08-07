# 048 — genetic2 FA flow divergence after issues/029 deepcopy synthesis (unbounded matcher allocation)

**Status:** open. Found 2026-07-17 finishing issues/029; genetic2 is
its only known member (the gtree micro family in
tests/deepcopy_objects.py — including copy-of-copy — converges
fine).

## Symptom

Compiling `shedskin_examples/genetic2/genetic2.py` no longer
terminates in practical time: FA runs 32 fast passes (~2s total,
violations converge 79 → 3), the stall guard fires (dup-free
non-improving passes hit IFA_NONIMPROVE_LIMIT), and then the FINAL
analysis pass never reaches a flow fixed point — RSS grows ~300MB
per 45s (2GB+ observed) allocating `Match` objects under
`Matcher::update_match_map` ← `pattern_match` ←
`function_dispatch` ← `add_send_edges_pnode`, processing
`TreeNode::execute` / deepcopy-related sends.

Before issues/029 (shallow deepcopy), genetic2 compiled in ~2 min
(and then crashed at RUNTIME on the cyclic trees the shallow copy
produced — the miscompile 029 exists to fix).

## What's known / suspected

- genetic2 deep-copies REPEATEDLY (every epoch, every individual)
  and grafts copies into other trees (crossover). Statically this
  chains: copy CSs of copies of copies... each `T::__deepcopy__`
  contour's `P_prim_copy` creation_point makes a fresh CS per
  contour, receivers union `{original, copy1, copy2, ...}`, the
  recursive-ES separability gate keeps splitting descent levels,
  and the growing unions make every re-analysis of the big
  dispatch sites re-run the matcher over an ever-larger CS × 
  candidate product.
- A P_prim_copy CS-SHARING experiment (result = source CSs) removed
  the chain but created a tighter within-pass feedback loop (copy
  contours writing their fresh result lists into the SOURCE CS's
  fields re-widened their own input) and was reverted — see the NB
  comment at fa.cc's P_prim_copy case.
- The stall guard cannot help: it bounds SPLIT passes, and the
  runaway is the flow/matcher work of a single pass.

## Fix directions

1. Memoize/lookup dispatch matches by (send, receiver-AType)
   fingerprint so re-analysis of a widened send re-derives only the
   NEW CSs' matches instead of the full product (the backtrace is
   dominated by re-matching, not by genuinely new information).
2. Bound the copy-CS chain: a P_prim_copy whose source ALREADY
   includes copy-CSs of the same class could reuse the existing
   copy CS for that (creation point, class) instead of minting
   another (one CS per copy SITE, not per copy GENERATION) — keeps
   original/copy separation (the part that matters for layouts and
   for issue 040-style precision) while collapsing generations.
3. Within-pass work cap analogous to the stall guard (wall-clock or
   allocation budget → treat as divergence, surface
   `pass_limit_hit`), so pathological inputs fail fast with
   diagnostics instead of thrashing.

## Verification

- genetic2 compiles in minutes and RUNS (its deepcopy semantics are
  now correct per tests/deepcopy_objects.py, so the GP simulation
  should complete — the pre-029 runtime crash was cyclic trees from
  shallow copies).
- tests/deepcopy_objects.py and the suites stay green.
