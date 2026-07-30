# 071 — chess.py: `squares` NOTYPE is a downstream salvage of accumulated union churn, not a setter-stage root

**Status:** open (chess now **compiles clean** behind ONE opt-in flag,
`--no_implicit_none`; a deeper *runtime* blocker remains). Deeply
root-caused 2026-07-28/29 via delta-debugging the actual chess source
and instrumenting FA. **Four** contributor bugs found and **fixed**
(`e544f6aa`, `4cfe9609`, `8644be59`); the fatal-compile blocker traced
to **two independent `None` sources**:

- **Source A — implicit fall-off-the-end returns** (`rowAttack`): a
  `for` loop that can complete without returning gets an injected
  `return None`, typing the function `bool | None`. → `--no_implicit_none`
  (shedskin-style: don't inject the fall-off `None` when the fn has an
  explicit value return; opt-in, default off — a deliberate CPython
  divergence).
- **Source B — a lambda-lowering bug (now FIXED at root):**
  `gen_lambda_pyda`/`PY_lambda` never set `fun_returns_value`, so
  `goto_exc_target`'s `!fun_returns_value` guard injected a spurious
  `sym_nil` into a lambda's return on the **raise edge** of any can-raise
  call in its body (`nonpawnAttacks`→`max` here) — giving `bool | None`.
  Fixed by one line in `PY_lambda` (build_if1.cc): a lambda always
  returns its body value, so set `fun_returns_value = 1` before the body
  is walked. This is exactly the nil-move a `def` with an explicit
  `return` already avoids (`PY_return_stmt` sets the flag).

**With Source B fixed and `--no_implicit_none` on, unmodified chess
compiles CLEAN (0 warnings, exit 0); suite 235/6/0/4 both backends.** The
remaining chess blocker is then a **runtime** `matching function not
found` on the heterogeneous `linePieces` tuple-of-tuples (issue-018/047),
not a compile error. Structurally this is the issue-033 splitter-churn
family, tipped over by an accumulation of small union sources — no
single "the bug".

**Retired:** the `--module_lambda_as_def` prototype flag (and its
`rewrite_module_lambdas_as_defs` AST pre-pass) was a *workaround* for
Source B — it rewrote `NAME = lambda …` to a `def`, which incidentally
set `fun_returns_value` via `PY_return_stmt`. With Source B fixed at the
root, the flag is redundant and has been removed.

## Correction history (three drafts before the root cause)

This file churned through two wrong mechanism drafts before the fix.
Kept here so superseded prose isn't mistaken for current:

- **Draft 1 (wrong):** the `{None, bool}` carrier is "the
  partial-application closure pyc builds for the VALUE of an `and`/`or`
  chain." **False** — the `and`/`or` chain neither introduces nor
  carries the `None` (a single `max([rowAttack(…) …])` with no `or`
  fails identically). The *carrier* is a bound-method partial application
  (`recv.__not__()` capturing its receiver, `make_period_closure`,
  `fa.cc:2134`), fed by whatever produces the union.
- **Draft 2 (wrong, commit `b7a80efd`):** "the `None` is *solely*
  `rowAttack`'s fall-off; the lambda→def flag only perturbs issue-033
  splitting." **False** — there are two independent `None` sources, and
  the lambda one is a real bug, not a splitting side-effect. Draft 2's
  minimal repros never built the full-scale lambda contour.
- **Draft 3 (this file's prior revision, `146512b3`) — right that there
  are two sources, wrong on Source B's mechanism:** it described Source B
  as "the closure-value dispatch builds a NOTYPE contour whose reply
  injects nil," carried by `--module_lambda_as_def`, and left the exact
  FA edge "one layer deeper than fully pinned." The pinned answer
  (below) is simpler and is a one-line frontend bug, not a dispatch
  subtlety: `goto_exc_target` moves `sym_nil` into the lambda's ret
  because the lambda's `fun_returns_value` was never set.

## How Source B was pinned (2026-07-29)

"Where does the `None` come from — dataflow or `AVar::gen`?" Answer:
**dataflow, from the `sym_nil` constant's `gen`.** Instrumented
`update_in` to print a C backtrace the first time a None-bearing type
reaches any AVar at chess.py:133, on **full chess under
`--no_implicit_none`** (the then-failing one-flag case):

```
NONE-INJECT via update_in ...
  update_in  <-  flow_vars_assign  <-  add_pnode_constraints (Code_MOVE)
MOVE@133  rhs id=1 None <unknown>:0  out=[ __pyc_None_type__ ]  nback(rhs)=0 gen(rhs)=set
```

- The injecting node is a `Code_MOVE` at chess.py:133 whose **`rhs` is
  `sym_nil`** (id 1, the None constant: `gen = None`, no backward). So
  the `None` is a plain dataflow move of the nil literal into the
  lambda's return — it is **not** `rowAttack`'s fall-off (trace also
  confirmed `--no_implicit_none` skips `rowAttack`'s nil-move and that
  `rowAttack`/`nonpawnAttacks` bodies carry zero `None`), and **not** the
  global cell (`SYMDUMP nonpawnBlackAttacks GLOBAL -> <closure>`, no
  `None`).
- A `sym_nil → fun->ret` move is emitted by exactly one place:
  **`goto_exc_target`** (`python_ifa_build_if1.cc:1277`), on a raise edge
  that targets the function's own return label, guarded by
  `!fun_returns_value && !is_generator`:

  ```c
  if (ctx.fun() && target == ctx.lreturn() &&
      !ctx.fun()->fun_returns_value && !ctx.fun()->is_generator)
    if1_move(if1, code, sym_nil, ctx.fun()->ret, ast);
  ```

- The lambda body's call to `nonpawnAttacks` is can-raise (it reaches
  `max`, which indexes `a[0]`; chess also has real `raise`s that keep the
  can-raise gating active), so the body emits a raise edge to the
  lambda's `lreturn`. `gen_lambda_pyda`/`PY_lambda` never set
  `fun_returns_value` (a lambda has no `return` statement; only
  `PY_return_stmt` sets it), so the guard is always true for lambdas and
  the nil-move always fires. Result: `lambda->ret = {body bool} ∪ {nil}`
  = `bool | None`.

**Fix (one line, `PY_lambda` in build_if1.cc), verified:** set
`ast->rval->fun_returns_value = 1` *before* the body is walked (build
order matters — the raise edge is emitted during the body walk). A
lambda body is an expression, so `ret` always has the body's def;
leaving the raise edge undefined is correct (the return value is dead on
a raise — see `goto_exc_target`'s own comment). Effect: the `sym_nil`
move at 133 disappears and chess compiles clean with `--no_implicit_none`
alone; suite 235/6/0/4 both backends, no regressions.

**Why a minimal crashing repro couldn't be isolated:** the spurious nil
lands on a *dead* raise edge, so it never affects runtime and only
becomes the fatal `bool | None` layout crash when it reaches a
bound-method closure field under chess's full contour pressure (the
issue-033 splitter). Isolated lambdas with can-raise bodies (even with a
`raise` present and the result forced into a class field or tuple) do
**not** crash — consistent with this file's whole thesis: accumulated
churn, no single minimal repro.

**History correction (2026-07-27 draft):** the very first draft
hypothesised a *setter-stage FA gap* — `range`'s `__pyc_more__`
method-pointer prototype-init slot resolving to NOTYPE for
`clone_methods_per_cs` classes. **Wrong: the range collapse is a
downstream EFFECT.** Once `squares` is NOTYPE, `convert_NOTYPE_to_void`
voids the whole `range(128)` list-comp, and codegen emits the degenerate
argless-`__new__` shell. `range` is the victim.

**Affects (real roots):** `__pyc__/00_runtime.py` (bool ordering + the
`__pyc_any_type__.__not__` gap — both fixed),
`ifa/codegen/cg_emit_llvm.cc` (int(bool) sign-extension — fixed),
`python_ifa_build_if1.cc` raise lowering (`raise <str>` — fixed) and the
`PY_lambda` `fun_returns_value` gap (Source B — fixed);
`python_ifa_build_syms.cc` `gen_fun_pyda` implicit-return-`None`
injection (Source A — behind `--no_implicit_none`). The unboxed
`bool | None` layout failure surfaces in `ifa/analysis/clone.cc`
`determine_layouts`; all amplified by the issue-033 non-idempotent
splitter.
**Surfaced while:** digging into `shedskin_examples/chess/chess.py`
(user request, following the
[chaos.py `== None` dig](../../issues/closed/031-eq-none-dispatch-crash.md)).

## Symptom

`chess.py` compiles with exactly one warning and exit 0:

```
shedskin_examples/chess/chess.py:26: warning: expression has no type
    squares = tuple([i for i in range(128) if not i & 8])
```

then the binary aborts on the first line of `main`
(`Assertion !"runtime error: matching function not found"`). `squares`
is a module-level global and the only surviving runtime use of `range`'s
class-based iterator protocol (every other `range(...)` in chess has
literal args and is const-folded/unrolled away).

## Why the generated C is a red herring

The crash site's C shows `range` with an argless `__new__()` and a
`struct {__pyc_tag; e5;}` (no `i/j/s` fields, no `__next__`), and
`___init___` storing an *uninitialised* local into the `__pyc_more__`
slot. This looks like a constructor/method-slot bug, but it is FA's
salvage of a NOTYPE subtree: once `squares` is NOTYPE,
`convert_NOTYPE_to_void` voids the whole list-comp-over-`range(128)`
expression, so the `range(128)` construction, its fields, and its
iterator methods all become dead and codegen emits the degenerate
shell. **`range` is the victim, not the cause.** Confirmed: the
identical `squares` line compiles and runs correctly in isolation and
in every small/medium reduction — `range` only collapses inside the
full program.

## The four landed contributor fixes

Delta-debugging the *chess source* (not the consumer of `squares`)
pinned four independent contributors, each survivable alone but which
together push FA into the issue-033 dup-split churn (24 passes, stuck
~38 violations across passes 7–17, then 18 dup_splits at pass 19) that
eventually salvages `squares`:

### (1) bool had no ordering dunders — FIXED (`e544f6aa`)

`nonpawnAttacks` does `max([board[ix+i] == color*2 for i in ...])` —
`max()` over a list of **bool**. `max`'s body does `x > m`, i.e.
`bool.__gt__`, which did not exist (bool defined only `__eq__`/`__ne__`).
The unresolved `__gt__` union is a churn seed. Minimal repro:
`max([v == 0 for v in [1,2,3]])` → `unresolved call '__gt__'`. Fixed by
adding `__lt__/__le__/__gt__/__ge__` to `bool` (int-subtype 0/1 ordering,
branch-on-self form, no numeric primitive). `tests/bool_ordering.py`.

### (2) int(bool) sign-extended to -1 on LLVM — FIXED (`e544f6aa`)

An early version of the bool dunders routed through `int(self) < int(x)`
and exposed this: the LLVM backend widened a bool with `SExtOrTrunc`, so
`int(True)` == **-1** (i1 `1` sign-extends to all-ones). Fixed by
zero-extending an i1 source in `emit_send_coerce`. `tests/bool_to_int.py`.
(The dunders were then rewritten to avoid `int()` entirely, but the
sign-extension bug is real and independently affects any `int(bool)`.)

### (3) `raise <str>` pollutes the `__pyc_exc__` slot — FIXED (`4cfe9609`)

The single decisive experiment: with (1) fixed, changing chess's
`raise "no move found"` / `raise "faulty castling"` (Python-2-style
string raises) to `raise ValueError(...)` **clears the `squares`
NOTYPE**. `PY_raise_stmt` moved the raised value straight into the
`__pyc_exc__` global slot; a `str` literal is not `Type_RECORD`, so it
was stored as-is, making `__pyc_exc__` a `{None, str, <exceptions...>}`
union that the program-wide `if __pyc_exc__ is not None` can-raise
checks then thread everywhere. In Python 3 `raise "str"` is itself a
TypeError; the fix wraps a raise operand whose static type is
string/bytes in `Exception(...)`, keeping the slot exception-typed
(`str(Exception("m")) == "m"` preserves the message, and the result is
catchable). `tests/raise_string.py`. Isolated `raise "str"` does *not*
repro the cascade — it needs the accumulation, consistent with the
churn model, which is why this was invisible until (1) was fixed and
the reduction narrowed to it.

### (4) chess.py:314 `not <list>` — FIXED (`8644be59`)

With (1) and (3) fixed, `squares` types cleanly and the failure moved to
`legalMoves`'s
`if not [i for i in pseudoLegalCaptures(board2) if board2[i&0xff] == kingVal]:`
(chess.py:314). This was **initially mis-attributed to the empty-list
element-inference family (issue 043)** — but it was a plain
`not <container>` dispatch gap: containers don't derive from `object` and
`__pyc_any_type__` had no `__not__`, so `not <list>` dispatched to
nothing for *every* list, empty or not (`not [1,2,3]` failed identically).
Fixed by adding `__not__` to `__pyc_any_type__` (`tests/not_container.py`).

## (5) The fatal blocker — a `bool | None` closure field from two `None` sources

After (1)–(4), chess dies on ONE hard error:

```
mismatched field members: __pyc_None_type__(8) bool(1)
fail: mismatched field sizes: class 'closure' field '<anon>' mixes 8- and 1-byte members ('bool')
```

`bool` is a 1-byte unboxed scalar; `None` (`__pyc_None_type__`) is an
8-byte pointer — one unboxed struct slot can't be both, so
`clone.cc:determine_layouts` fails hard. The union reaches a closure
field via a bound-method partial application (`recv.__not__()` capturing
a `bool | None` receiver). The `bool | None` has **two independent
origins** (see the pinned traces above), and **both** must be removed:

### Source A — implicit fall-off-the-end return (`--no_implicit_none`)

A function that returns a bool but can also **fall off the end** gets an
injected `None` → `bool | None`. Minimal standalone repro (fixed
completely by `--no_implicit_none` alone):

```python
def maybe(k):
    if k > 0:
        return k < 5      # bool
    # falls off the end -> pyc injects None
def f(vals):
    return max([maybe(v) for v in vals])   # list[bool|None]
print(f([1, 2, 3]))
```

**Proximate cause (shedskin-verified) — implicit-`None` modeling, not a
missing boxing subsystem.** Ran shedskin (0.9.13) on this repro:

- *Implicit* fall-off-the-end (chess's shape): shedskin types the
  function `__ss_bool` and fills the missing path with `return False` —
  it does **NOT** model an implicit fall-off as `None`, so **no union
  forms and nothing is boxed** (a deliberate CPython divergence:
  `maybe(-1)` is `False`, not `None`). Scalars are native/unboxable in
  *both* compilers (`typestr.py:unboxable`); shedskin is not "box
  everything."
- *Explicit* `return None`: shedskin *does* box the genuine
  `scalar | None` union — emits `void *` (None = null pointer, bool
  boxed).

**Fix — IMPLEMENTED behind `--no_implicit_none` (default off).**
`gen_fun_pyda` (`python_ifa_build_syms.cc`) normally moves `sym_nil`
into `fn->ret` for the fall-off path; when the flag is set and the
function is a non-generator with an explicit value `return`
(`fun_returns_value`), that move is **skipped**, so `fn->ret` is the
union of the explicit returns only. Same principle `goto_exc_target`
already uses on the raise edge. Deliberate CPython divergence for a
program that reaches the end and *uses* the `None` — hence opt-in.
**Measured:** fixes the minimal repro and drops `rowAttack`'s `None` in
full chess (trace-confirmed); suite 235/6/0/4 both backends with the
flag on AND off.

### Source B — a lambda-lowering `fun_returns_value` gap (FIXED at root)

Root cause and fix are in "How Source B was pinned" above: a lambda body
with a can-raise call gets a spurious `sym_nil` moved into its return by
`goto_exc_target`, because `PY_lambda` never set `fun_returns_value`.
Fixed by setting it (a lambda always returns its body value) before the
body walk. No flag; no CPython divergence. The former
`--module_lambda_as_def` workaround (which set the flag indirectly by
rewriting the lambda to a `def`) is retired.

### The durable general fix (subsumes Source A)

**Box the `bool | None` union** — the general
[018](../../issues/018-dict-mixed-key-types-boxing-failure.md) /
[030](030-polymorphic-dispatch-fat-pointers.md) /
[060](closed/060-none-branch-dropped-mixed-with-literal-bool-sequence.md)
work (tagged / fat-pointer `scalar | None`, None = null pointer). This
would handle Source A (and genuine dynamic `x = None; x = 5`) without the
CPython divergence, at the cost of the representation. A narrower codegen
mitigation is possible: a nullable-scalar / boxed representation for a
*single* field that mixes exactly one pointer-`None` with one scalar.

## Status

Source B is fixed at the root (one line in `PY_lambda`). Source A is
behind `--no_implicit_none` (default off); with it on, chess
compiles-clean. Before defaulting `--no_implicit_none` on, weigh the
CPython divergence (fall-off path yields an arbitrary-but-typed value
instead of `None`) against the corpus, or land the durable `scalar |
None` boxing which subsumes it. The next chess blocker is a **runtime**
one — the issue-018/047 heterogeneous `linePieces` dispatch (a
tuple-of-tuples mixing `()` with `bishopLines`/`rookLines`). Once that
clears, a `(null)*` C-backend null-element error may also surface
([061](061-c-backend-multi-tuple-list-null-element-type.md)). The genuine
empty-container element-inference family (issue 043 /
[072](072-empty-container-notype-current-mechanism-and-plan.md)) turned
out **not** to be a chess blocker once #4 was correctly diagnosed.

Structurally behind everything is the issue-033 splitter non-idempotency
that turns any residual union into a program-wide NOTYPE cascade rather
than a local, attributable error.

## What actually lands vs. what remains

- **Landed (`e544f6aa`):** bool ordering + LLVM int(bool) — both genuine
  correctness bugs on their own (`True > False`, `max/min/sorted` over
  bools, `int(True)`), independent of chess.
- **Landed (`4cfe9609`):** `raise <str>` wrapping — cleared chess's
  `squares` NOTYPE.
- **Landed (`8644be59`):** `__pyc_any_type__.__not__` — cleared chess's
  line-314 (`not <list>`), a plain dispatch gap.
- **Landed (this change):** `PY_lambda` `fun_returns_value = 1` — Source
  B; removes a spurious `bool | None` from any lambda with a can-raise
  body. `--module_lambda_as_def` retired.
- **Open — Source A:** `rowAttack`'s implicit fall-off `None`, behind
  `--no_implicit_none`; durable fix is `scalar | None` boxing
  (018/030/060). Behind it, a `(null)*` C-backend null-element error
  (issue 061).
- Suite 235/6/0/4 both backends after all fixes, with `--no_implicit_none`
  on or off.

## Verification plan

1. `chess.py` compiles warning-free with `--no_implicit_none` and
   `./shedskin_examples/chess/chess` runs to completion (currently
   blocked on the issue-018/047 runtime dispatch).
2. Regression for (3): a `clone_methods_per_cs`-adjacent global
   (`squares`-style range comprehension) plus a helper that does
   `raise "some string"`, in a non-`__main__` function.
3. Keep suite green both backends and re-check dijkstra2/fysphun per
   issue 063's checklist (the issue-033 canaries).

## Other Source-A instances

- **`shedskin_examples/adatron/adatron.py`** (2026-07-30, after its FA
  non-convergence was fixed — [073](073-teach-splitter-productive-vs-inert-context.md)):
  `calculate_error`'s `return 1.0 * error / len(kernel_table)` is nested
  **inside** a `for` loop, so an empty `kernel_table` falls off the end →
  injected `None` → `float64 | None`. codegen then emits the illegal
  `t0 = (_CG_float64)NULL` (`simple_move`, `cg.cc:918` guards a nil *lhs*
  at `:910` but not a nil *rhs* into a scalar *lhs*). `--no_implicit_none
  1` makes it compile clean and run correctly (error `0.025`, matches
  CPython). Same shape and resolution as chess's `rowAttack`.

## What this unblocks

- `shedskin_examples/chess/chess.py` (a corpus benchmark).
- Correct `bool` ordering and `int(bool)` everywhere (already landed).
- Any lambda whose body can raise no longer carries a spurious `None` in
  its return type (Source B fix — general, not chess-specific).
- More generally, making an accumulated-union NOTYPE degrade to a
  *local, attributable* diagnostic instead of salvaging an unrelated
  global's construction into an uninitialised-memory crash.

## Related

- [063-no-type-bucket-triage.md](063-no-type-bucket-triage.md) — the
  multi-rooted NOTYPE bucket and setter/mark-stage churn.
- [033-splitter-non-idempotent-divergence.md](033-splitter-non-idempotent-divergence.md)
  — the non-idempotent splitting that amplifies these unions.
- [closed/043-empty-container-inference-options.md](closed/043-empty-container-inference-options.md)
  — chess's residual (4).
- [closed/045-receiver-cs-method-cloning.md](closed/045-receiver-cs-method-cloning.md)
  — `clone_methods_per_cs` (the earlier, wrong hypothesis's subject).
- [issues/025-shedskin-examples-coverage.md](../../issues/025-shedskin-examples-coverage.md)
  — corpus tracker; chess entry updated alongside this.
