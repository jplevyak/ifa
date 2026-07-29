# 071 — chess.py: `squares` NOTYPE is a downstream salvage of accumulated union churn, not a setter-stage root

**Status:** open (chess now **compiles clean** behind two opt-in
prototypes; a deeper *runtime* blocker remains). Deeply root-caused
2026-07-28 via delta-debugging the actual chess source; **four**
contributing bugs found and **fixed** (`e544f6aa`, `4cfe9609`,
`8644be59`), then the fatal-compile blocker traced to **two independent
`None` sources**, each addressed behind a flag:

- **Source A — implicit fall-off-the-end returns** (`rowAttack`): a
  `for` loop that can complete without returning gets an injected
  `return None`, typing the function `bool | None`. → `--no_implicit_none`
  (shedskin-style: don't inject the fall-off `None` when the fn has an
  explicit value return).
- **Source B — the module-level lambda-closure dispatch**
  (`nonpawnBlackAttacks`/`…White`, chess.py:133): calling a lambda
  *through its closure value in the global* injects a `None` into the
  lambda's return that is **entirely separate** from Source A. →
  `--module_lambda_as_def` (lower a single-assignment `NAME = lambda`
  like a `def`, so the call resolves directly and never builds the
  closure-value contour).

**Both flags are needed** because the two `None`s are independent:
`--no_implicit_none` alone still fails (Source B survives);
`--module_lambda_as_def` alone still fails (Source A survives). With
**both on, unmodified chess compiles CLEAN (0 warnings, exit 0); suite
235/0 both backends, and each flag is a no-op when off.** The remaining
chess blocker is then a **runtime** `matching function not found` on the
heterogeneous `linePieces` tuple-of-tuples (issue-018/047), not a
compile error. Structurally this is the issue-033 splitter-churn family,
tipped over by an accumulation of small union sources — no single "the
bug".

## Correction history (read this before the older mechanism prose)

This file went through two *wrong* mechanism drafts before the traced
finding above. Both are called out here so the superseded prose below
isn't mistaken for current:

- **Draft 1 (wrong):** the `{None, bool}` carrier is "the
  partial-application closure pyc builds for the VALUE of an `and`/`or`
  chain." **False** — the `and`/`or` chain neither introduces nor is
  required to carry the `None` (a single `max([rowAttack(…) …])` with no
  `or` fails identically). The carrier is a *bound-method* partial
  application (`recv.__not__()` capturing its receiver,
  `make_period_closure`, `fa.cc:2134`), fed by whatever produces the
  union — not an and/or value closure.
- **Draft 2 (wrong, commit `b7a80efd`):** "the `None` is *solely*
  `rowAttack`'s fall-off; the lambda→def flag only perturbs issue-033
  splitting, not a `None` source." **False for full chess** — traced
  2026-07-29 (below): the lambda dispatch is a *genuine, independent*
  second `None` source, not a splitting side-effect. Draft 2 was based
  on minimal repros that never build the full-scale lambda contour, so
  they only ever exercised Source A.

## The traced finding (2026-07-29) — two independent `None` sources

Instrumented `clone.cc:determine_layouts` at the layout failure to (a)
walk the offending closure field backward and flag every AVar whose
`out` carries `__pyc_None_type__`, and traced `gen_fun_pyda`'s nil-move
decision. On **full chess under `--no_implicit_none` only** (the
one-flag failing case):

1. **`--no_implicit_none` is working.** Trace: `rowAttack
   returns_value=1 → SKIP nil-move`. Source A's fall-off `None` is
   suppressed for `rowAttack`.
2. **`rowAttack` and `nonpawnAttacks` return pure `bool`.** The backward
   walk (14,685 AVars, graph exhausted) found **zero** None-carriers
   anywhere in their bodies (lines 120–131). So under the flag the
   fall-off `None` really is gone — it does **not** well up from
   `rowAttack`'s return into the failure.
3. **The residual `None` originates at exactly one frontier: the lambda,
   chess.py:133.** It is the single AVar whose `out = {None, bool}` but
   whose backward *dataflow* neighbors carry no `None`. The `bool` is the
   body (`nonpawnAttacks`) result; the `None` arrives via a
   **dispatch/call-return edge** (not `gen`, not backward dataflow),
   alongside an **empty/NOTYPE sibling write** into the lambda's return.
4. **The global cell holds just the closure**, not `{None, closure}`
   (`SYMDUMP nonpawnBlackAttacks … GLOBAL -> <closure>`), so the naive
   "uninitialised global carries `None`" story is also not it.
5. The failing closure the layout check reports is at **chess.py:167** —
   the `not nonpawnBlackAttacks(board, N)` bound-method (a
   `make_period_closure` receiver capture). It captures the lambda's
   `{None, bool}` result from (3); that receiver field is the 8-byte
   `None` / 1-byte `bool` mix `clone.cc` rejects.

**Mechanism of Source B (confident on *where*/*that*; the exact FA edge
is one layer deeper than fully pinned):** calling the lambda through its
closure value is a `partial_application`/closure dispatch. That dispatch
produces an analysis contour whose body result is empty/NOTYPE (the
typeless sibling write in (3)), and the closure-reply machinery
contributes a nil there, which unions with the real `bool` →
`{None, bool}`. A `def` call resolves **directly** (`def_internal_fn`),
never builds that closure-value contour, and so never gets the injected
`None` — which is why `--module_lambda_as_def` fixes Source B. This is a
genuinely different dispatch *path*, **not** a splitter accident.

**One real splitter red herring:** adding an explicit `return False` to
`rowAttack` *also* makes full chess compile under `--no_implicit_none`
alone — but since `rowAttack` is already pure `bool` under the flag (see
(2)), that "fix" works only by perturbing the issue-033 contour graph so
the NOTYPE lambda contour of (3) doesn't form. It removes Source B's
*trigger conditions*, not a `None` at `rowAttack`. Do not read it as
evidence that Source A is the culprit.

**History correction:** the first draft of this file (2026-07-27)
hypothesised the root was a *setter-stage FA gap* — `range`'s
`__pyc_more__` method-pointer prototype-init slot resolving to NOTYPE
for `clone_methods_per_cs` classes, with codegen then emitting an
uninitialised-C-local field store. **That was wrong: the range collapse
is a downstream EFFECT, not the root.** The generated C that looked like
a fieldless, argless `range` (`__new__()` with no args, struct with only
`e5`) is what codegen emits *after* FA salvages the whole `squares`
subtree to void — because `squares` went NOTYPE for an entirely
different, upstream reason. The evidence below supersedes that draft.

**Affects (real roots):** `__pyc__/00_runtime.py` (bool ordering + the
`__pyc_any_type__.__not__` gap — both fixed),
`ifa/codegen/cg_emit_llvm.cc` (int(bool) sign-extension — fixed),
`python_ifa_build_if1.cc` raise lowering (`raise <str>` — fixed);
`python_ifa_build_syms.cc` implicit-return-`None` injection (Source A)
and the lambda-closure dispatch (Source B) are the remaining fatal
blocker — a `bool | None` union pyc can't lay out unboxed
(`ifa/analysis/clone.cc` `determine_layouts` is where it fails), all
amplified by the issue-033 non-idempotent splitter.
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
field via a bound-method partial application (`recv.__not__()` /
`recv.__gt__(...)` capturing a `bool | None` receiver). As traced above,
the `bool | None` has **two independent origins in chess**, and **both
must be removed**:

### Source A — implicit fall-off-the-end return (`--no_implicit_none`)

A function that returns a bool but can also **fall off the end** gets an
injected `None` → `bool | None`. Minimal standalone repro (fixed
completely by `--no_implicit_none` alone — this repro only ever
exercises Source A):

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
  `maybe(-1)` is `False`, not `None` — safe here because the path is
  runtime-dead). Scalars are native/unboxable in *both* compilers
  (`typestr.py:unboxable`); shedskin is not "box everything."
- *Explicit* `return None`: shedskin *does* box the genuine
  `scalar | None` union — emits `void *` (None = null pointer, bool
  boxed).

**Fix — IMPLEMENTED behind `--no_implicit_none` (default off).**
`gen_fun_pyda` (`python_ifa_build_syms.cc`) normally moves `sym_nil`
into `fn->ret` for the fall-off path; when the flag is set and the
function is a non-generator with an explicit value `return`
(`fun_returns_value`), that move is **skipped**, so `fn->ret` is the
union of the explicit returns only (no `None`). Same principle
`goto_exc_target` already uses on the raise edge (its `!fun_returns_value`
guard). Deliberate CPython divergence for a program that reaches the end
and *uses* the `None` (it gets an arbitrary-but-typed value) — hence
opt-in. **Measured:** fixes the minimal repro and drops `rowAttack`'s
`None` in full chess (trace-confirmed); suite **235/0 both backends with
the flag on AND off**. Does **not** touch bare `return`/`return None`
(explicit, routed through `PY_return_stmt`) or lambdas (`gen_lambda_pyda`
injects no nil of its own).

### Source B — the module-level lambda-closure dispatch (`--module_lambda_as_def`)

`nonpawnBlackAttacks = lambda board, ix: nonpawnAttacks(board, ix, -1)`
(chess.py:133) is a closure value bound to a module global and called via
that value. Under `--no_implicit_none` (Source A gone), the residual
`None` originates **here** (trace: sole None-frontier at chess.py:133,
`out = {None, bool}`, `None` in via a dispatch edge with an empty/NOTYPE
sibling write; `rowAttack`/`nonpawnAttacks` bodies carry no `None`). The
closure-value dispatch builds a contour whose body result is
empty/NOTYPE and the closure reply contributes nil → `{None, bool}`; the
`not nonpawnBlackAttacks(...)` bound method at chess.py:167 then captures
that as its un-layout-able receiver field.

**Fix — PROTOTYPED behind `--module_lambda_as_def` (default off).** An
AST pre-pass (`rewrite_module_lambdas_as_defs`,
`python_ifa_build_syms.cc`, before `build_syms_pyda`) rewrites a
top-level `NAME = lambda p: body` into `def NAME(p): return body` when
`NAME` is bound exactly once at module scope (`top_level_binding_count
== 1`; else left alone). A `def` resolves calls directly
(`ctx.def_internal_fn`) instead of through the global's closure value, so
the NOTYPE closure-dispatch contour never forms and no `None` is
injected. **Measured:** with this flag **and** `--no_implicit_none`,
unmodified chess compiles **clean (0 warnings, exit 0)**; suite 235/0
both backends; default-off no-op. chess then *runs* to a **deeper,
separate** runtime `matching function not found` in
`pseudoLegalCapturesWhite` iterating `linePieces` (issue-018/047), not a
compile error.

### The durable general fix (subsumes both flags)

**Box the `bool | None` union** — the general
[018](../../issues/018-dict-mixed-key-types-boxing-failure.md) /
[030](030-polymorphic-dispatch-fat-pointers.md) /
[060](closed/060-none-branch-dropped-mixed-with-literal-bool-sequence.md)
work (tagged / fat-pointer `scalar | None`, None = null pointer). This
handles *both* chess `None` sources and genuine dynamic `x = None; x = 5`
without either CPython divergence, at the cost of the representation. A
narrower codegen mitigation is possible: a nullable-scalar / boxed
representation for a *single* field that mixes exactly one pointer-`None`
with one scalar, localizing the representation problem to where it
occurs.

## Status

Both flags are opt-in prototypes; **together** they make chess
compile-clean. Before defaulting either on, weigh the CPython
divergences (`--no_implicit_none`: fall-off path yields an
arbitrary-but-typed value instead of `None`; `--module_lambda_as_def`:
assumes single-assignment) against the corpus, or land the durable
`scalar | None` boxing which subsumes both. The next chess blocker is a
**runtime** one — the issue-018/047 heterogeneous `linePieces` dispatch
(a tuple-of-tuples mixing `()` with `bishopLines`/`rookLines`). Once that
clears, a `(null)*` C-backend null-element error may also surface
([061](061-c-backend-multi-tuple-list-null-element-type.md)). The genuine
empty-container element-inference family (issue 043 /
[072](072-empty-container-notype-current-mechanism-and-plan.md)) turned
out **not** to be a chess blocker once #4 was correctly diagnosed.

Structurally behind everything is the issue-033 splitter non-idempotency
that turns any residual union into a program-wide NOTYPE cascade rather
than a local, attributable error.

## What actually lands vs. what remains

- **Landed (`e544f6aa`):** bool ordering + LLVM int(bool) — both are
  genuine correctness bugs on their own (fix `True > False`,
  `max/min/sorted` over bools, `int(True)`), independent of chess.
- **Landed (`4cfe9609`):** `raise <str>` wrapping — cleared chess's
  `squares` NOTYPE.
- **Landed (`8644be59`):** `__pyc_any_type__.__not__` — cleared chess's
  line-314 (`not <list>`), a plain dispatch gap affecting every
  `not <container>`, initially mis-attributed here to issue 043.
- **Open — chess's fatal blocker (#5):** a `bool | None` closure field
  pyc can't lay out unboxed, from **two independent `None` sources** —
  `rowAttack`'s implicit fall-off (`--no_implicit_none`) and the
  lambda-closure dispatch at chess.py:133 (`--module_lambda_as_def`).
  Both flags are prototyped and clear the compile together; the durable
  fix is `scalar | None` boxing (018/030/060). Behind it, a `(null)*`
  C-backend null-element error (issue 061).
- Suite 235/0 both backends after all four fixes and with both flags on
  or off; corpus sweep buckets within parallel-timeout noise.

## Verification plan

1. `chess.py` compiles warning-free with `--no_implicit_none
   --module_lambda_as_def` and `./shedskin_examples/chess/chess` runs to
   completion (currently blocked on the issue-018/047 runtime dispatch).
2. Regression for (3): a `clone_methods_per_cs`-adjacent global
   (`squares`-style range comprehension) plus a helper that does
   `raise "some string"`, in a non-`__main__` function — the shape that
   tips it (`raise <str>` in isolation does not).
3. Keep suite green both backends and re-check dijkstra2/fysphun per
   issue 063's checklist (the issue-033 canaries).

## What this unblocks

- `shedskin_examples/chess/chess.py` (a corpus benchmark).
- Correct `bool` ordering and `int(bool)` everywhere (already landed).
- More generally, making an accumulated-union NOTYPE degrade to a
  *local, attributable* diagnostic instead of salvaging an unrelated
  global's construction into an uninitialised-memory crash — the real
  robustness win hiding behind chess's confusing generated C.

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
