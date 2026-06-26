# FA notes — non-obvious findings

Things that took a session to figure out.  Read
the code first; come here when something doesn't
add up.

## prim_reply is called once per (PNode, ES) at setup

`add_send_constraints` runs at constraint setup, not
per fixed-point iteration.  The trace in
`P_prim_reply` fires when constraints for that PNode
are added to the ES — typically just a couple of
times (once on initial setup, again if the ES gets
re-extended).  Looking at `r->out` (= `fn->ret`'s
AVar) inside that trace shows the value **at setup
time**, not at fixed point.  To inspect the
converged ret, dump in a post-FA hook (e.g. after
`set_void_lub_types_to_void()` in `FA::analyze`).

Implication: don't conclude "fn->ret is missing
CSx" from a setup-time trace.  Compare to the
verbose `-v -v -v` per-ES AVar dump, which prints
converged state.

## All Python return paths share one prim_reply

`PY_return_stmt` lowers to `if1_move(val, fn->ret);
goto label[0]`.  The function's body ends with one
`if1_label(label[0])` followed by one
`prim_reply(fn->cont, fn->ret)`.  Multiple `return`
statements all funnel into the same `fn->ret`,
which is one Var with one AVar per ES.

So fn->ret's backward chain is the union of all
return-source AVars.  At setup time backward.n can
match the number of returns; at fixed point SSU
phys can have collapsed some.

## Narrowing uses type-level predicates, not CS snapshots

History: the issue-025 narrowing block in
`add_pnode_constraints` (Code_IF case) originally
partitioned `operand_av->out->sorted` into two AType
snapshots (tt / ft) at constraint-setup time and
stamped them into `lv->restrict` via
`flow_var_type_permit`.  Bug: `restrict` is never
re-derived, so CSs that arrive at `operand_av->in`
later in the fixed-point get filtered out by the
stale snapshot.  This is bug 5 of issue 026 — the
BST-insert recursive pattern returned 10 instead
of 8 because Node(3) appeared in `n` post-setup
and never reached the non-None SSU view.

Current: `AVar` has a `restrict_pred` (enum) +
`restrict_pred_cls` (for isinstance), and
`update_in` consults `apply_restrict_pred` after
the static `restrict` intersection.  Narrowing
sites for `is None`, `is not None`, and isinstance
against a single class install a predicate via
`flow_var_permit_pred(lv, RP_…, cls)`.  The
predicate keeps re-evaluating as new CSs arrive.

If you add a new narrowing form, prefer a
predicate over a snapshot.  The recipe:

1. Add a new `RP_…` enum case in `fa.h`.
2. Handle it in `restrict_pred_keeps` in `fa.cc`.
3. Set `narrow_*_pred` (and `narrow_pred_cls` if
   needed) in the discriminator-recognition block
   in `Code_IF`.

For predicates that need richer state than one
Sym, extend the AVar fields or factor the
predicate into a separate struct.

The fallback snapshot path is still there for
isinstance against multi-class type_av — that
case isn't expressible as a single-arg predicate,
and the snapshot bug is latent rather than active
(no test exercises it today).

## Two __new__ ESs ≠ two callers reach insert's ret

Class instantiation `Node(v)` dispatches through
`__init__` and an auto-generated `__new__` wrapper.
With different argument-shape callers, you get
**multiple __new__ ESs** (each holds its own clone
PNode firing).  These ES proliferations are
visible in `[clone]`-style traces; they are not
the same as insert getting multiple ESs.

A function called recursively from multiple call
sites with compatible argument types stays in **one
ES**.  insert in the BST example has a single ES
even though it's called twice in the source.  Its
fn->ret holds the union of both branches' returns.

## flow_var_type_permit growth semantics

```cpp
if (!v->restrict) v->restrict = t;
else v->restrict = type_union(t, v->restrict);
```

If permit is called again with more CSs, the
restrict **grows** (union).  But the call site
itself has to be re-executed for that to happen.
In the narrowing case, the call sits inside
`add_pnode_constraints` which runs once per
(PNode, ES) — so no re-permit happens when only
`operand_av->out` grows.

To make narrowing dynamic, either re-call
`add_pnode_constraints` (heavyweight) or hook a
re-permit into operand_av's update path.

## A/B knobs for narrowing vs mid-FA inlining

Two pyc flags isolate the two precision sources
that interact at the IF/narrowing site.  Both
default to production behavior.

| flag | env | default | effect |
|------|-----|---------|--------|
| `--narrow N` | `IFA_NARROW` | 1 | gates the discriminator-recognition + per-branch narrowing in `add_pnode_constraints` Code_IF |
| `--fa_inline N` | `IFA_FA_INLINE` | 0 | runs `mark_live_funs` + `simple_inlining` between FA convergence passes; clears per-ES `live_pnodes` and re-converges |

Use these to A/B precision claims.  Production
runs with `narrow=1 fa_inline=0`.  On the
bst_insert + recnode3 patterns, disabling
narrowing surfaces a latent codegen bug
(`dict::___init___` → `NULL = NULL`) that
narrowed types otherwise mask via DCE.
Mid-FA inlining is precision-neutral on
today's suite — it's a knob for measuring
future inlining wins, not a fix for any
current case.

The mid-FA-inlining reset is conservative: it
drops every ES's `live_pnodes` and re-runs
`add_es_constraints` on each ES that ends up
back on the worklist.  AVars on Vars survive
(Vars are stable across inlining).  Don't add
state on PNodes that has to survive across the
reset, or extend the reset to clear it.

## Generated C `_CG_prim_add(5, "+", t4)` literally means 5

When `n.value` is constant-folded to a single
integer at FA, codegen inlines the literal at the
call site rather than emitting a struct read.
If you see a literal value where you expect a
field load, the AVar of that period read has a
single-CS `out` whose iv resolves to a constant.
Trace back to the period source's per-CS field
tracking.
