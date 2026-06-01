# Issue 001: Keepalive crashes when user fun has explicit reply

**Status:** open
**Found:** while wiring up the `inline` phase test fixtures.
**Affects:** `ifa/testing/fa_setup.cc`, `ifa/analysis/fa.cc`.
**Related commits:** `087b127` (keepalive added), `ac62d25` (docs trade-off).
**Workaround:** don't put `(send @primitive @reply …)` in user funs in
`.ir` fixtures; let `fa_setup` provide the only reply in
`sym___main__`. Costs us higher-coverage `inline` goldens.

## Symptom

A `.ir` fixture like:

```text
(fun %id
  :args (%id %a)
  :rets (%r)
  :cont %icont
  :body
    (move %a %r)
    (send @primitive @reply %icont %r))
```

…crashes `ifa-test` during `FA::analyze`. Backtrace:

```
make_AVar (v=…, es=0x1)            ifa/analysis/fa.cc:192
add_send_edges_pnode (p=…, es=0x1)  ifa/analysis/fa.cc:1546
FA::analyze (…)                     ifa/analysis/fa.cc:3864
```

`0x1` is `GLOBAL_CONTOUR`. The crashing `make_AVar` is called on an
rval whose `sym->nesting_depth > 0`, which then deref's
`es->fun->sym->nesting_depth` at line 192 — but `es` is
`GLOBAL_CONTOUR`, not a real `EntrySet*`.

## Root cause

`ifa/testing/fa_setup.cc:build_synthetic_main()` appends a "keepalive"
SEND after the synthetic reply:

```c
Sym *kp_result = new_Sym();
kp_result->type = sym_nil_type;
if1_send(if1, &code, 3, 1, sym_primitive, kp_sym, user_entry->ret, kp_result);
```

The keepalive is a stub primitive (`__test_keepalive`) marked
`is_visible=1` so `mark_initial_dead_and_alive` (dead.cc:180) sets
the SEND's PNode `live=1`. That liveness back-propagates via
`mark_live_avars` to keep user code alive through DCE.

`kp_result` is created with default `nesting_depth = 0`, so its `Var`
ends up in `GLOBAL_CONTOUR`. FA's worklist puts the keepalive's
result AVar on `send_worklist` with `contour = GLOBAL_CONTOUR`, and
each pass:

```c
add_send_edges_pnode(send->var->def, (EntrySet *)send->contour);
//                                    ^^^ GLOBAL_CONTOUR
```

Inside `add_send_edges_pnode`, the per-rval loop calls
`make_AVar(rval, es)`. For rvals whose `sym->nesting_depth == 0`
(symbols, constants), `make_AVar` routes to `GLOBAL_CONTOUR` itself
and never deref's `es->fun` — no crash.

But when the call is also reachable through a path where one of the
rvals has `nesting_depth > 0` (e.g. a user-local like `%r` from a
fun's `(send @primitive @reply %cont %r)`), the same `make_AVar`
enters the `if (v->sym->nesting_depth)` branch at line 191 and
dereferences `es->fun->sym->nesting_depth` — boom.

In short: the global-contour AVar for `kp_result` becomes a fan-in
point for SENDs from across the whole program; when *any* of them
involves a non-global rval, FA tries to evaluate it in the wrong
contour.

## Why the obvious fix doesn't work

Setting `kp_result->nesting_depth = LOCALLY_NESTED` so the result is
in `sym___main__`'s ES instead of `GLOBAL_CONTOUR` *does* eliminate
the crash:

```c
kp_result->nesting_depth = LOCALLY_NESTED;
```

But it also defeats the keepalive's purpose: the keepalive PNode
gets dropped from `fa_all_PNodes` before FA even runs, so the
FA-level DCE has nothing to anchor user-code liveness to.
`dce/01_baseline` regresses to the pre-keepalive skeleton
(`pnodes=0/N vars=0/N`, funs=1).

The actual mechanism (traced by instrumenting
`mark_initial_dead_and_alive`): when the LOCALLY_NESTED variant
runs, `__main__`'s `fa_all_PNodes` shows only the user SEND and
the synthetic reply — not the keepalive. The keepalive Code was
pruned earlier, in `if1_simple_dead_code_elimination`
(`ifa/if1/if1.cc:501`, called from `if1_finalize_dce`) — i.e. at
the *Sym-level* live pass, before FA::analyze runs.

The chain in `if1.cc`:

1. **Seed live roots** (line 505-508):
   ```c
   for (Sym *s : p->allsyms)
     if (!s->nesting_depth || s->asymbol) mark_sym_live(s);
   ```
   Globals (`nesting_depth == 0`) are the live root set. With
   LOCALLY_NESTED (-1, becoming 1 after fixup), `kp_result` is
   **not** a live root.

2. **Propagate** `mark_live(p, code)` (line 453-478). For
   `Code_SEND` (line 466-471):
   ```c
   if (!code->lvals.n || code->lvals[0]->live || !is_functional(p, code)) {
     for (Sym *r : code->rvals) mark_sym_live(r);
     for (Sym *l : code->lvals) mark_sym_live(l);
   }
   ```
   For the keepalive: `lvals[0]` is `kp_result` (`Var->live = 0`
   from step 1); `is_functional` returns `1` because the
   `RegisteredPrim` ctor defaults `is_functional(1)` and we never
   change it. All three conditions false → no propagation.

3. **Sweep** `mark_dead(p, code)` (line 480-495). For `Code_SEND`
   (line 488-490):
   ```c
   if (is_functional(p, code) && !code->lvals[0]->live)
     code->live = 0;
   ```
   Both conditions true → **the keepalive Code is marked dead.**

4. When `Fun::build_cfg` runs later (during the printer's setup
   loop), dead Code is skipped — the keepalive never gets a PNode
   in `fa_all_PNodes`. The FA-level DCE in `ifa/optimize/dead.cc`
   then has nothing to mark initially-live via `is_visible`, so
   user-code back-propagation never gets seeded.

In the global-`kp_result` case, step 1 fires: kp_result has
`nesting_depth == 0` so it's a live root, which keeps the keepalive
Code live through the Sym-level DCE, which keeps the PNode in
`fa_all_PNodes`, which lets `mark_initial_dead_and_alive` set its
`p->live = 1` via the `is_visible` check. Back-propagation works.

So the two DCE passes are stacked: `if1_simple_dead_code_elimination`
(Sym-level, IR-level pruning) gates whether the keepalive PNode
even exists for `mark_live_code` (FA-level, AVar/PNode liveness) to
look at. Hand-wave of "per-ES processing doesn't propagate liveness"
was wrong; the real issue is "Sym-level DCE removes the keepalive
before FA-level DCE gets a chance."

A possible workaround that keeps `kp_result` LOCALLY_NESTED:
register the keepalive prim with `is_functional = 0` (so step 3's
sweep skips it). That would be a one-line `rp->is_functional = 0;`
addition next to `rp->is_visible = 1;`. Verify the crash actually
goes away in that configuration and the dce goldens stay sane —
the proposed-fix section below remains the more principled answer.

## Proposed fix: capture the user ret via an intermediate move

Replace the direct `user_entry->ret` rval with a fresh
sym___main__-local that's MOVE'd from the user ret. The move is the
fan-in point that's local to sym___main__'s ES; the keepalive SEND
reads only that local, so its global-contour evaluation never
encounters a non-global rval.

Concretely, in `build_synthetic_main()`:

```c
// before keepalive SEND:
Sym *kp_arg = new_Sym();
kp_arg->type = sym_nil_type;            // or copy from user_entry->ret if known
kp_arg->nesting_depth = LOCALLY_NESTED; // per-ES, like other locals
if1_move(if1, &code, user_entry->ret, kp_arg);

// keepalive SEND now reads only kp_arg (locally nested) — safe to
// evaluate in any contour because no nesting_depth>0 cross-context
// flows.
Sym *kp_result = new_Sym();
kp_result->type = sym_nil_type;
if1_send(if1, &code, 3, 1, sym_primitive, kp_sym, kp_arg, kp_result);
```

Why this should work:

- `kp_arg` is `LOCALLY_NESTED` → its Var lives in sym___main__'s ES,
  so when FA processes the keepalive in any contour, `make_AVar`
  routes `kp_arg` through `unique_AVar(v, es)` correctly.
- `kp_result` stays global, so the SEND can be initially-live via
  `is_visible` like today. The "fan-in" liveness mechanism that the
  current keepalive relies on is preserved.
- The MOVE `user_entry->ret → kp_arg` is a per-ES PNode in
  sym___main__'s body. mark_live_avars marks its rvals live when
  `kp_arg` is live, and the existing per-ES liveness propagation
  back to user code continues to work.

## Verification plan

1. Apply the refactor in `fa_setup.cc`.
2. Re-bless all phase goldens. Expect minor diffs in:
   - `fa-init` ESs/CSes (+1 PNode, +1 Var per fixture).
   - `dispatch` (one more send rval target).
   - `clone` (one more equivalence class candidate).
   - `dce` should stay at "user code alive" (the regression test).
   - `codegen-c` shows one extra `t = …;` MOVE before the keepalive
     call.
3. Add a new inline fixture with a user-fun explicit reply to prove
   the crash is gone:
   ```text
   (fun %id …
     :body (move %a %r)
            (send @primitive @reply %icont %r))
   ```
   Verify `events=0` flips to `events>=1` (an `identity` event for
   the now-detected identity fun).
4. Run the full pyc e2e suite to confirm no regression (the fix is
   test-harness only, so this should be a no-op for production).

## What this unblocks

- Higher-coverage `inline` goldens: `INLINE_IDENTITY` and
  `INLINE_SINGLE_SEND` events become observable once user funs can
  have explicit replies.
- Any future fixture that wants to embed primitive sends with
  non-global rvals reachable from `__main__`.
- Removes a footgun for anyone authoring `.ir` fixtures — today the
  crash is silent until you happen to write the wrong shape.
