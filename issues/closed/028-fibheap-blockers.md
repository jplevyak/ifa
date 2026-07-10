# 028 — Fib-heap readiness: two blockers found

## Status

**Closed.**  Both fixes landed in commit `97f6a6c4`
("Interim") and are verified in the current tree:
`tests/ring_selfcycle.py`, `tests/ring_splice.py`,
`tests/fibheap_insert_min.py`,
`tests/fibheap_property_narrow.py`, and
`tests/bst_insert.py` all pass.

**Bug A: FIXED.**  Codegen-side fix in
`ifa/codegen/cg.cc` — `resolve_union_receiver` picks
the non-nil component of a `None | T` union receiver
when emitting getter/setter accesses.  Soundness: the
narrowing guarded the runtime value to non-None in
that branch, so emitting code against the Node
component is the correct semantic.

**Bug B: FIXED.**  One-line condition change in
`ifa/optimize/ssu.cc` (`while (phi && phy)` →
`while (phi || phy)`), see "Bug B" section below.

## Background

Investigation of whether pyc + IFA are ready for a
Fibonacci-heap implementation.  Three-step gradient:

1. **Single-node ring** (`x.next = x; x.prev = x`) —
   passes (`tests/ring_selfcycle.py`).
2. **3-node ring with 4-pointer splice** — passes
   (`tests/ring_splice.py`).
3. **Heap class with ring-based `insert` + `minimum`** —
   **fails**.  Two distinct bugs surface.

## Bug A — `is None` narrowing doesn't propagate to property re-reads

```python
class Heap:
  def insert(self, k):
    x = Node(k)
    if self.min is None:    # narrows on the synthetic temp
      ...
    else:
      x.next = self.min.next   # FRESH read of self.min — not narrowed
```

The frontend narrowing infrastructure (issue 025, the
predicate-restrict fix in 026) gates the SSU-renamed view
of the conditional's operand.  Here the operand is a
temp holding the result of `period(self, "min")`.  Inside
the else branch, that temp's view is narrowed to Node, but
`self.min.next` is a NEW `period(self, "min")` whose
receiver re-reads `self.min` from the field's storage.
The field's stored type is still the union (None | Node),
and codegen picks `_CG_nil_type` as the cast base for
`->next` access:

```c
t34 = (_CG_ps3708)((_CG_nil_type)t36)->e1; /* next */
//                  ^^^^^^^^^^^^^ wrong: dereferences void*
```

Plus an `assert(!"runtime error: getter not resolved")`
for `self.min.key` access in the same branch.

This is the well-known TypeScript / Kotlin rule
"narrowing applies to locals, not to properties."  Pyc
follows the same rule by accident — the narrowing
machinery only attaches to the operand Var, and a fresh
property read produces a fresh Var.

**Initial workaround** (no longer needed after fix):
bind to a local once at the top of the method
(`m = self.min`).  Still good Python style for
performance, but not required for correctness.

**The fix that landed.**  The narrowing problem was
real, but the actual *user-visible breakage* was
narrower than the IFA-side analysis suggested.  At
codegen, getter and setter for a period-access did:

```cpp
Sym *obj = n->rvals[1]->type;
if (obj->type_kind == Type_SUM) obj = obj->has[0];
```

`has[0]` is just "the first union component" — for a
typical `None | T` it's `nil_type`.  Then
`cg_get_string(obj)` = `"_CG_nil_type"` (= `void *`),
and the emitted `(_CG_nil_type)recv->eN` failed to
compile.

Replaced with a helper that prefers a non-nil
component, and within that, prefers one that actually
declares the field being accessed:

```cpp
static Sym *resolve_union_receiver(Sym *obj, cchar *symbol) {
  if (!obj || obj->type_kind != Type_SUM) return obj;
  // Prefer a non-nil component that declares `symbol`.
  for (Sym *component : obj->has)
    if (component && component != sym_nil_type)
      for (Sym *field : component->has)
        if (field && field->name == symbol) return component;
  // Otherwise prefer any non-nil component.
  for (Sym *component : obj->has)
    if (component && component != sym_nil_type) return component;
  return obj->has[0];  // fall back; "getter not resolved" path still fires
}
```

Applied at both period-getter (line 200) and setter
(line 236) call sites.

Soundness: this CHANGES the cast type but not the
underlying memory access.  At runtime, the narrowed
branch guarantees the receiver is non-None; the
non-None component's struct layout is the correct
one to cast through.  If the union happens to be a
non-{None, T} shape (e.g. {Node, str}), the helper
prefers a component declaring the field — still
correct for the resolved field's slot.

Why this works even without IFA-side narrowing: the
*type-level* receiver still has the full union, but
codegen only needs ONE consistent cast that maps the
field offset.  The cast doesn't change the runtime
value; it just satisfies the C type checker.  The
runtime correctness was already guaranteed by IFA's
type analysis — codegen just needed to pick a
sensible projection of the union.

## Bug B — SSU/phy merge after if/else doesn't unify `self` rename

Even with the workaround applied to Bug A, the next
issue surfaces.  Test:

```python
def insert(self, k):
  m = self.min
  if m is None:
    x.next = x; x.prev = x; self.min = x
  else:
    x.next = m.next; m.next = x   # simplified, no prev/min update
  self.n = self.n + 1   # <-- post-merge: doesn't run from else branch
```

Generated C structure (simplified):

```c
if (t4) {
  t0 = t5;
  ((_CG_ps3600)t0)->e3 = ...;
  goto L321;
 L321:;
  t2 = ((_CG_ps3600)t0)->e4;     // read self.n
  t1 = t2 + 1;
  ((_CG_ps3600)t0)->e4 = t1;     // write self.n
  return 0;
} else {
  t6 = t5;
  goto L321;                      // <-- jumps INTO if-true block
}
```

`L321` is placed inside the if-true branch.  The else
branch jumps to L321 via `goto`.  Inside L321, `t0` is
read — but **t0 is only assigned in the if-true
branch**.  The else writes to t6 (its own SSU rename of
self) but never to t0.

Observed effect on the test: first insert (m is None,
if-true path) sets n=1 correctly.  Subsequent inserts
(else path) jump to L321 with `t0` uninitialized.  In
practice t0 happens to hold a NULL-ish or stale value
that doesn't segfault but writes to garbage memory.
`h.count()` returns 1 forever.

The bug was a **missing phi at the post-if merge** for
`self`.  pyc's SSU pass renames `self` to different
temps in each branch (t0 in if-true, t6 in else), but
no phi at the merge label means codegen has no way to
converge the renames.  Result: the post-merge body
references whichever branch's rename rename_edge
happened to leave in `env`, and the other branch
reaches the merge with an uninitialized local of that
name.

### Root cause in ssu.cc

`Fun::build_ssu` iterates `place_phi` and `place_phy`
to a fixpoint.  Each pass feeds the other:

- `place_phy` placing a phy at node `y` calls
  `v->ssu->defs.append(y->cfg_succ)` — the phy's
  per-branch lvals act as new defs at the branch
  entries.  Next round of `place_phi` should see those
  and place a phi at their DF (the post-if merge).
- `place_phi` placing a phi at node `y` calls
  `v->ssu->uses.append(y->cfg_pred)` — phi rvals
  act as new uses at the predecessors.

The convergence loop was:

```cpp
int phi = place_phi(vrs);
int phy = place_phy(vrs);
while (phi && phy) {
  phi = place_phi(vrs);
  phy = phi && place_phy(vrs);
}
```

Read literally: "iterate while BOTH made progress."
For a read-only parameter like `self` (zero defs,
many uses) the initial `place_phi` placed nothing, so
`phi == 0`, and the loop never entered — the cascade
from phys-creating-defs back into phi-placement
never ran.

### The fix

```cpp
int phi = place_phi(vrs);
int phy = place_phy(vrs);
while (phi || phy) {
  phi = place_phi(vrs);
  phy = place_phy(vrs);
}
```

Terminate when BOTH return 0 (fixpoint reached),
not when EITHER returns 0.  Termination is bounded
by `v->ssu->phis.add(y)` / `v->ssu->phys.add(y)`
deduping each (pnode, var) pair.

Also restored unconditional `place_phy(vrs)` on each
iteration — the previous `phi && place_phy(vrs)`
short-circuit was a symptom of the same misread.

Compare to the working case in `bst_insert.py`:
`insert(node, v)` has the same `if node is None: …
else: …` shape but **both branches end with `return`**
— no post-merge code, no SSU convergence needed.

The trigger is "an if/else where both branches modify
state AND the function continues past the if".  This is
basic imperative code; every method with a discriminator
followed by shared cleanup hits this.

## Verification matrix

| pattern | result | tests/ |
|---------|--------|--------|
| Self-cycle (`x.next = x`) | PASS | ring_selfcycle.py |
| 3-node ring + splice | PASS | ring_splice.py |
| Heap class, ring insert + post-merge state update | PASS (after Bug B fix) | fibheap_insert_min.py |
| Property narrowing across re-reads, no workaround | PASS (after Bug A fix) | fibheap_property_narrow.py |

## What this unblocks

Both bugs are common patterns — not fib-heap-specific.
Bug B in particular blocks **any method that uses a
None-check and then does shared cleanup**, which is most
non-trivial mutators.

**Bug A: done** (codegen-side, ~30 lines).
`resolve_union_receiver` picks a non-nil union
component for the period-access cast.  This works
because the actual type-level narrowing isn't
required — codegen just needed to pick a consistent
projection of the union for the C cast.  IFA's
analysis was already correct; only the cast emission
was wrong.

**Bug B: done.**  Unblocked the canonical "if-None
then shared cleanup" method shape.

Next: proceed to extract_min (ring removal,
multi-tree merge — likely-novel patterns).

## Related

- [025-intra-function-union-narrowing.md](025-intra-function-union-narrowing.md) — narrowing scope discussion (this is Case 3).
- [026-recursive-self-mutation-struct-collapse.md](026-recursive-self-mutation-struct-collapse.md) — Bug 5 fix; unblocks bst_insert.
- `CLAUDE.md` "Critical IFA Code Generator Bug Fixed" — similar-flavor LABEL-placement bug, may share root cause with Bug B.
