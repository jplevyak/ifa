# 071 — chess.py: `squares` NOTYPE is a downstream salvage of accumulated union churn, not a setter-stage root

**Status:** open (chess now **compiles clean** behind two opt-in
prototypes; a deeper *runtime* blocker remains). Deeply root-caused
2026-07-28 via delta-debugging the actual chess source; **four**
contributing bugs found and **fixed** (`e544f6aa`, `4cfe9609`,
`8644be59`), then the two fatal-compile blockers each addressed behind a
flag:
- `bool | None` from **implicit fall-off-the-end returns** (`rowAttack`
  et al.) → `--no_implicit_none` (shedskin-style: don't inject `None`
  when the fn has an explicit value return).
- `bool | None` leaking through **module-level lambda globals**
  (`nonpawnBlackAttacks`/`…White`, `{None, closure}` dispatch) →
  `--module_lambda_as_def` (lower a single-assignment `NAME = lambda`
  like a `def`, so calls resolve directly).

**With both flags on, unmodified chess compiles CLEAN (0 warnings, exit
0); suite 235/0 both backends, and each flag is a no-op when off.**
Verified against shedskin: it compiles the identical implicit-`None`
shape because it does NOT model implicit-`None` (native/unboxable
scalars in both). The remaining chess blocker is now a **runtime**
`matching function not found` on the heterogeneous `linePieces`
tuple-of-tuples (issue-018/047), not a compile error. The overall shape
is the issue-033 splitter-churn family, tipped over by an accumulation
of small union sources — no single "the bug".

**History correction:** the first draft of this file (same day, earlier)
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
`python_ifa_build_if1.cc` raise lowering (`raise <str>` — fixed) and its
implicit-return-`None` injection (the remaining fatal blocker — a
`bool | None` union pyc can't lay out unboxed; `ifa/analysis/clone.cc`
`determine_layouts` is where it fails), all amplified by the issue-033
non-idempotent splitter.
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

## Real root cause: accumulated union sources tip the issue-033 splitter

Delta-debugging the *chess source* (not the consumer of `squares`)
pinned three independent contributors, each of which alone is survivable
but which together push FA into the issue-033 dup-split churn (24 passes,
stuck ~38 violations across passes 7–17, then 18 dup_splits at pass 19)
that eventually salvages `squares`:

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

### (5) chess's fatal blocker — `bool | None` from an implicit-`None` return

After (1)–(4), chess dies on ONE hard error at chess.py:167:

```
mismatched field members: __pyc_None_type__(8) bool(1)
fail: mismatched field sizes: class 'closure' field '<anon>' mixes 8- and 1-byte members ('bool')
```

**The mechanism (minimal, confirmed):** a function that returns a bool
or **falls off the end** (pyc injects `None` for the missing return) is
typed `bool | None`; flowing that through a list comprehension / `max`
makes pyc lower the comprehension to a synthesized `closure` carrier
whose anonymous element field must hold `bool | None`. `bool` is a
1-byte unboxed scalar; `None` (`__pyc_None_type__`) is an 8-byte
pointer — one unboxed struct slot can't be both, so
`clone.cc:determine_layouts` fails hard. Minimal standalone repro:

```python
def maybe(k):
    if k > 0:
        return k < 5      # bool
    # falls off the end -> pyc injects None
def f(vals):
    return max([maybe(v) for v in vals])   # list[bool|None]
print(f([1, 2, 3]))
```

**But it is NOT a single-function fix — chess has MULTIPLE implicit-`None`
sources.** `rowAttack` is one (its `for` loop can complete without
returning). Adding `return False` to `rowAttack` clears the error *in
isolation* (and in the minimal repro), but **full chess still fails with
the identical `bool | None` field error**, just attributed to the next
fall-off-the-end function in the search's call graph. So per-function
patching is a losing game; the fix has to be the general one (below).

**Proximate cause (shedskin-verified) — implicit-`None` modeling, not a
missing boxing subsystem.** Ran shedskin (0.9.13) on the minimal repro:

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

So the entangling cost is pyc injecting `None` for an implicit
fall-off-the-end return, **then** the unboxed representation. Fix
options, cheapest first:

1. **Match shedskin's implicit-return handling — IMPLEMENTED behind
   `--no_implicit_none` (default off).** `gen_fun_pyda`
   (`python_ifa_build_syms.cc`) normally moves `sym_nil` into `fn->ret`
   for the fall-off path; when the flag is set and the function is a
   non-generator with an explicit value `return` (`fun_returns_value`),
   that move is **skipped**, so `fn->ret` is the union of the explicit
   returns only (no `None`). Same principle `goto_exc_target` already
   uses on the raise edge (its `!fun_returns_value` guard). Deliberate
   CPython divergence for a program that reaches the end and *uses* the
   `None` (it gets an arbitrary-but-typed value) — hence opt-in.
   **Measured:** fixes the minimal repro above and the isolated
   `nonpawnAttacks` (rowAttack's `None` dropped); suite **235/0 both
   backends with the flag on AND off** (no test relies on the implicit
   `None` in a breaking way). Does **not** touch bare `return`/`return
   None` (explicit, routed through `PY_return_stmt`) or lambdas
   (`gen_lambda_pyda` injects no nil).

   *It gets chess most of the way but not all:* with the flag on **and**
   chess's two module-level lambdas (`nonpawnBlackAttacks`/`…White`)
   rewritten as `def`s, full chess compiles **clean (0 warnings, exit
   0)**. Unmodified, chess still fails on those lambdas — a separate
   blocker: a module-level `lambda` global is `{None, closure}`
   (issue 002's None-initializer) and its result field still mixes in
   `None` in a way the flag doesn't reach (not minimally reproduced —
   `nb = lambda k: base(k); max([not nb(v) …])` compiles fine, so it is
   scale/context-specific). Closing chess needs that lambda-global-None
   handled too, or option 2.
2. **Box the `bool | None` union** — the general
   [018](../../issues/018-dict-mixed-key-types-boxing-failure.md) /
   [030](030-polymorphic-dispatch-fat-pointers.md) /
   [060](closed/060-none-branch-dropped-mixed-with-literal-bool-sequence.md)
   work (tagged / fat-pointer scalar|None), which also handles genuine
   `x = None; x = 5` and the lambda-global-None. Bigger, but the
   principled fix.

### Plan — the lambda-global-`None` blocker (chess's last one, with option 1 on)

**What it is (traced 2026-07-28, `PYC_DBG_LAYOUT`):** the residual fatal
error with `--no_implicit_none` on is a `{None, bool}` union in a
synthesized `closure` class's anonymous field, defined at chess.py:167
(`... and not nonpawnBlackAttacks(board, 2) and ...`). Both members are
*abstract* types (no specific creation site) flowing into the field. It
forms **only at full-program scale** — main = `pseudoLegalMovesWhite`
alone gives a *different* (null-element C) error, and small
lambda-in-`not`/`max` repros compile clean. So it is a scale-dependent
union (issue-033 family) whose `None` enters through the module-level
`lambda` globals: `nonpawnBlackAttacks`/`…White` are `NAME = lambda …`
at module scope, so the global is `{None, closure}` (issue 002's
None-initializer, flow-insensitive); rewriting both as `def`s clears it.
The exact path by which that global-`None` reaches the `bool` field's
union is not fully traced (the dispatch through `{None, closure}`
creates different contours than a direct `def` call — some function's
implicit-`None` may survive the flag in those contours). **Not easy.**

**Options, feasibility/risk ascending:**

1. **Lower a single-assignment module-level `NAME = lambda …` like a
   `def`** (frontend) — **PROTOTYPED, works, behind `--module_lambda_as_def`
   (default off).** An AST pre-pass (`rewrite_module_lambdas_as_defs`,
   `python_ifa_build_syms.cc`, before `build_syms_pyda`) rewrites a
   top-level `NAME = lambda p: body` into `def NAME(p): return body` when
   `NAME` is bound exactly once at module scope (`top_level_binding_count
   == 1`; else left alone). Mechanism (traced): the real difference
   isn't the None-init — a non-method `def` *also* binds its name via
   `if1_move` — it's that a `def` registers `ctx.def_internal_fn`, so
   calls resolve **directly** instead of dispatching through the name's
   `{None, closure}` global value, whose `None` arm is what leaked into
   the `bool` union. **Measured: with this flag AND `--no_implicit_none`,
   unmodified chess compiles CLEAN (0 warnings, exit 0)** — the
   scale-dependent `bool|None` did *not* re-form. Suite 235/0 both
   backends with both flags on, and default-off no-op. chess then
   *runs* to a **deeper, separate** runtime `matching function not
   found` in `pseudoLegalCapturesWhite` iterating `linePieces` (a
   heterogeneous tuple-of-tuples: empty `()` mixed with
   `bishopLines`/`rookLines`) — the issue-018/047 heterogeneous-dispatch
   family, unrelated to this fix. So chess is now **compile-clean**,
   blocked only on that runtime dispatch.
2. **Suppress the None-initializer arm for a provably-assigned-before-use
   closure global** (issue 002 refinement, FA-level). More general than
   (1) but runs against flow-insensitivity; the None-init is there for a
   reason.
3. **Box `scalar | None`** (option 2 above) — the durable general fix;
   subsumes (1)/(2) and the implicit-`None` cases; large.
4. **Codegen mitigation: nullable-scalar / boxed representation for a
   single `{None, scalar}` field** — narrower than full boxing; box or
   tag only a field that mixes exactly one pointer-`None` with one
   scalar, localizing the representation problem to where it occurs.

**Status:** option (1) is prototyped and clears the compile (see above);
`--no_implicit_none` + `--module_lambda_as_def` make chess compile-clean.
Both are opt-in prototypes — before defaulting either on, weigh the
CPython divergences (implicit-`None` → arbitrary value; the lambda→def
rewrite assumes single-assignment) against the corpus, or land the
durable general fix (3, scalar|None boxing) which subsumes both. The
next chess blocker is now a **runtime** one — the issue-018/047
heterogeneous `linePieces` dispatch (a tuple-of-tuples mixing `()` with
`bishopLines`/`rookLines`), not a compile error.

Once that clears, a `(null)*` C-backend null-element error may also
surface ([061](061-c-backend-multi-tuple-list-null-element-type.md)). The
genuine empty-container element-inference family (issue 043 /
[072](072-empty-container-notype-current-mechanism-and-plan.md)) turned
out **not** to be a chess blocker at all once #4 was correctly
diagnosed.

## What actually lands vs. what remains

- **Landed (`e544f6aa`):** bool ordering + LLVM int(bool) — both are
  genuine correctness bugs on their own (fix `True > False`,
  `max/min/sorted` over bools, `int(True)`), independent of chess.
- **Landed (`4cfe9609`):** `raise <str>` wrapping — cleared chess's
  `squares` NOTYPE.
- **Landed (`8644be59`):** `__pyc_any_type__.__not__` — cleared chess's
  line-314 (`not <list>`), a plain dispatch gap affecting every
  `not <container>`, initially mis-attributed here to issue 043.
- **Open — chess's fatal blocker (#5 above):** a `bool | None` closure
  field pyc can't lay out unboxed, whose *proximate* cause is pyc
  injecting `None` for the *several* chess functions that fall off the
  end (rowAttack is one — but patching it alone just shifts the error to
  the next). shedskin-verified: the general fix is to match shedskin's
  implicit-return handling (don't inject `None` when the function has an
  explicit non-`None` return), or scalar|None boxing (018/030/060).
  Behind it, a `(null)*` C-backend null-element error (issue 061).
  Structurally behind everything is the issue-033 splitter
  non-idempotency that turns any residual union into a program-wide
  NOTYPE cascade rather than a local, attributable error.
- Suite 234/0 both backends after all four fixes; corpus sweep buckets
  within parallel-timeout noise (the fixes touch only bool ordering,
  `int(bool)`, `raise <str/bytes>`, and `not <container>` — no other
  example's behavior changes).

## Verification plan

1. `chess.py` compiles warning-free and
   `./shedskin_examples/chess/chess` runs to completion.
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
