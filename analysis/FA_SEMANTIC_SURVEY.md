# fa.cc semantic survey — bugs, omissions, design issues (2026-07-05)

A full read-through of the flow analysis rooted in `fa.cc` (all
~4,750 lines, plus the corners of `dead.cc`, `clone.cc`,
`pattern.cc`, `ssu.cc`, and `num.cc` it leans on), focused on
**analysis semantics**: soundness, precision, convergence, and the
protocols between FA and its consumers. This is the companion to
[AUDIT.md](AUDIT.md), which covers code shape, reentrancy, and
determinism — findings already listed there are only
cross-referenced, not repeated. Line numbers are as of commit
`ab861a8`.

Confirmed items are labeled **B*n*** (bug), design-level items
**D*n***, precision/soundness gaps **S*n***, performance items
**P*n***. Actionable confirmed bugs are also filed as
[issues/032-fa-survey-findings.md](../issues/032-fa-survey-findings.md).

---

## 1. Confirmed bugs

### B1 — canonical-hash accumulator is overwritten, not combined

`type_cannonicalize` (fa.cc:530) and `setters_cannonicalize`
(fa.cc:3726):

```cpp
for (int i = 0; i < t->sorted.n; i++)
  h = (uint)(intptr_t)t->sorted[i] * open_hash_primes[i % 256];
```

`h =` discards every element except the **last**. Both canonical
hash-cons tables (`cannonical_atypes`, `cannonical_setters`)
therefore hash an N-element set as if it contained only its final
element: every AType sharing a last-sorted CS collides. Correctness
survives (the tables compare full contents on collision) but lookup
degrades toward linear scans in exactly the programs with many
ATypes. Almost certainly meant `h +=` or `h ^=` — or better,
`combine_hash()` (fa.h:50), which was added for exactly this
purpose and is not used here. NOTE: fixing this changes canonical
table iteration order, so expect golden churn and re-run the flake
checks (issue 021 class) when landing.

### B2 — `coerce_num` indexes precision tables by `num_kind` instead of `num_index`

fa.cc:417-423 (and the same mistake in `num.cc:695,700` inside
`fold_constant`):

```cpp
if (int_type_precision[b->num_kind] <= float_type_precision[a->num_kind]) return a;
if (int_type_precision[b->num_kind] >= 32) return sym_float32;
```

`int_type_precision[5] = {1,8,16,32,64}` is indexed by
`num_index` (`IF1_INT_TYPE_1/8/…/64`); `num_kind` is the *kind*
enum (`NONE/UINT/INT/FLOAT/COMPLEX` = 0..4). So for any int
operand `b`, `int_type_precision[b->num_kind]` reads
`precision[1]=8` or `[2]=16` — always `<= any float precision`
and always `< 32`. Consequences:

- int⊕float coercion always returns the float operand's type
  unchanged, regardless of the integer's width (`int64 + float32`
  types as `float32` — the widening the comment block above the
  function promises never happens), and the `>= 32` branches are
  dead code. Ditto the complex branches.
- The same kind-vs-index confusion sits in `fold_constant`'s
  mixed int/float constant folding (`im2->const_kind` used as a
  precision index), so constant folding and type coercion at
  least agree with each other — but both are wrong versus the
  documented intent.

Latent for pyc today (pyc only produces `float64`, and
`{1,8,16}` ≤ 64 gives the right answer by accident); real for the
V-language f32 path and for any future f32 support. Fix both
sites together and add a V-side test (`int64 + float32` should
widen, per the function's own comment).

### B3 — range-for over a growing `Accum` vector: truncated closures + realloc UB

The bug class was already found once (the comment at fa.cc:3598
records issue 007's discovery that a range-for here capped the
transitive closure at one hop) and fixed in `build_type_marks` by
switching to index-based loops — but the two sibling functions
were left with the same defect:

- `build_setter_marks` (fa.cc:3644, 3646): both closure loops
  `for (AVar *x : acc.asvec) ... acc.add(y)` append to the vector
  being range-iterated.
- `back_reaching` (fa.cc:4081): `for (AVar *x : seen.asvec) ...
  seen.add(r)` — same shape, used by the violation-driven
  splitter (`collect_violation_imprecisions`).

Two distinct consequences: (a) the closure silently stops at
whatever elements existed when the loop began — under-collection
means under-splitting (missed precision) in the setter-marks and
violation-splitting stages; (b) `Vec::add` reallocates `v`, and a
range-for holds raw `begin()/end()` pointers into the old
allocation — iterating freed memory is UB that happens to "work"
under Boehm GC (the old block isn't recycled while referenced)
but is still wrong. Fix exactly like `build_type_marks`:
index-based loops.

### B4 — extend_analysis stage 4 never clears its type marks

fa.cc:4205-4223 (stage 4, "split based on setters of type using
marks"): each iteration calls `build_type_marks(av, acc)` and then
**never calls `clear_marks(acc)`** — contrast
`split_with_type_marks` (fa.cc:3997), which does. Marks are stored
in `AVar::mark_map` and are only otherwise reset by
`clear_results()` — which runs *only if some splitter requested
another pass*. So:

- Within stage 4's own loop, iteration *k* sees the union of the
  marks seeded by iterations 1..k-1 — `different_marked_args`
  comparisons are contaminated across unrelated confluences, and
  the outcome depends on confluence iteration order (a
  determinism hazard of exactly the issue-009/021 flavor).
- If stage 4 (and 5) find nothing, the stale marks survive into
  the *final converged state*. Nothing after FA is known to read
  `mark_map`, so today this is invisible — but it is one consumer
  away from a haunting.

Fix: `clear_marks(acc)` at the bottom of the stage-4 loop body.
Also note stage 4 calls `split_for_setters_of_setters(confluences)`
(the outer list) from inside the per-`av` loop and re-runs a
global `collect_cs_marked_confluences` per confluence — see P3.

### B5 — `split_edges` can null an edge's target on constant-typed actuals

fa.cc:3449-3468 (dynamic dispatch splitting): `cs_es_map` is keyed
by the CSs of `av->out->type->sorted` — the **constant-stripped**
type view — but the single-CS fast path looks up
`earg->out->v[0]`, the raw out which may be a *constant* CS
(`"3"` rather than `int64`). `Map::get` misses and
`ee->to = nullptr`, leaving a corrupt edge for the next
`analyze_edge`/`qsort` to trip over. Reachable only via the
`SPLIT_DYNAMIC` path (`split_for_violations`) with
constant-folded arguments at a violating call site — rare, which
is why it hasn't been seen — but it is exactly the shape of crash
that gets reported as "FA segfault, no idea why". Fix: look up
`earg->out->type->v[0]` (or fall through to the general loop when
the raw single CS is a constant).

---

## 2. Soundness and precision gaps

### S1 — the restrict propagators are missing update_in's `is_if_arg` resume

`flow_var_type_permit` (fa.cc:1005) and `flow_var_permit_pred`
(fa.cc:1025) duplicate `update_in`'s propagation (arg_of_send
enqueue + forward walk) but **omit the `is_if_arg` EntrySet
re-enqueue**. `add_pnode_constraints` hard-stops its CFG walk at a
`Code_IF` whose condition is still bottom (fa.cc:2138 `return`)
and relies on that re-enqueue to resume. A condition whose `out`
transitions only via a *restrict* (permit narrowing an
already-arrived `in`) will not resume a blocked walk. Today's
narrowing targets branch-local SSU temps rather than the `IF`
condition itself, so no known repro — but the three propagators
should share one implementation (extract the common tail of
`update_in`) rather than hoping they stay in sync.

Related, documented-in-code precision loss: `flow_var_permit_pred`
silently bails when a second, different predicate lands on the
same AVar ("composition not implemented") — chained narrowing
(`isinstance(x, A)` inside `if x is not None:`) keeps only the
first predicate.

### S2 — snapshot-style transfer functions depend on a fragile re-trigger convention

The prim transfers in `add_send_edges_pnode` iterate
`operand->out->sorted` at execution time (isinstance, issubclass,
len, sizeof, typeof, merge, coerce, index_object, destruct, the
period/setter machinery, …). Monotonicity holds only because
fa.cc:1707-1709 blanket-registers *every rval* as
`arg_of_send(result)` — **but only when the pnode has lvals**. Any
prim used in a statement position (no result) never re-runs when
operand types arrive later. Today's IF1 always gives sends an
lval, so this is a landmine rather than a live bug — worth an
assertion (`p->lvals.n || no snapshot-style prim`) or moving the
registration out of the `if (result)` guard. Two adjacent
instances of the same pattern with *narrower* registration:

- `vector_elems` (fa.cc:1148) only registers `e->arg_of_send`
  inside the tuple branch; an operand that is bottom on first
  visit and later becomes a tuple re-runs only by luck of the
  enclosing prim's blanket registration.
- `record_arg` (fa.cc:1311) destructures pattern formals from a
  snapshot of `t->sorted`, and `record_args_rets` records args
  **once per edge** (`if (!e->args.n)`). A pattern-typed formal
  whose actual gains CSs after the edge's first analysis never
  extends the recorded sub-positions. V-language pattern formals
  only; pyc doesn't emit them.
- `record_arg` also has a hard `assert(s->has.n == cs->vars.n)` —
  an arity mismatch between a pattern formal and an actual's CS
  is a *user-reachable* condition and should be a
  `type_violation`, not an abort.

### S3 — per-pass wipe vs. persistent caches: the protocol that keeps biting

The convergence design is "each outer pass wipes ALL AVar state
(`clear_results`) and re-derives it, while some structures
persist": `AVar::cs_map` (CS identity across passes — load-bearing,
see the issue-030 fixpoint fix), `AVar::match_cache`,
`AVar::container`, `CreationSet::vars` (positional slots),
`PNode::tvals`, `Fun::fa_*` vectors. Every mixed
persistent/transient interaction is a potential repeat of the 030
bug (positional CS slots starved because the re-derived flow keyed
an AVar by a Var that changed across passes). Current state of the
known interactions:

- `make_closure_var` — fixed (feeds `cs->vars[i]` positionally on
  re-derivation).
- `make_kind` (fa.cc:1106) — always used the positional slot;
  correct by construction.
- `structural_assignment` (fa.cc:1590-1612) — named fields go
  through keyed AVars (`unique_AVar(h->var, cs)`, stable because
  class field Vars are stable), unnamed overflow vars use
  positional slots with the same `if (!vars[i])` guard as
  make_kind; looks correct, but nobody has written the invariant
  down until now: **"a CS's positional `vars[i]` must be fed by
  every pass that feeds the CS, regardless of which Var carries
  the value that pass."**
- `AVar::match_cache` is *not* cleared by `clear_avar` — entries
  are validated against argument ATypes so stale entries miss
  safely (ATypes are canonical and persistent), but the cache
  only ever grows (see P2).

Recommendation: a short invariants comment block at `clear_avar`
listing what intentionally survives a pass and why; plus a debug
assertion in `remove_unused_closures` that a closure CS consumed
by a live call site has `closure_used` (that assertion would have
turned the 030 fixpoint bug from "wrong void types + runtime
abort" into a one-line FA-time failure).

### S4 — dead three-valued contract in entry-set compatibility

`entry_set_compatibility` (fa.cc:898) switches on
`edge_type_compatible_with_entry_set(...)` including a
`case -1: return 0;` — but that function returns only 0/1. The
dead case misleads readers into thinking there's a hard-reject
path (AUDIT §1 item 3's int-as-bool complaint, plus one). Either
restore a real -1 meaning or delete the case. Similarly
`make_entry_set` (fa.cc:995-1001) declares `EntrySet *es =
nullptr`, never assigns it between, then tests `if (!es)` — the
`preference` fallback logic reads as more subtle than it is.

### S5 — `P_prim_coerce` operand indexing is inconsistent with the prim offset convention

fa.cc:1987-1997: the target type is taken from
`p->rvals[p->rvals.n - 2]` but the CS filter compares
`cs->sym->type == p->rvals[1]->sym`. Everywhere else the prim
argument base is computed as
`o = rvals[0]->sym == sym_primitive ? 2 : 1`. If coerce ever
appears in the 4-rval `__primitive`-prefixed form, `rvals[1]` is
the *prim name symbol*, and the filter never matches (silently
falls through to the abstract-type path). Verify which form(s)
the frontends emit and normalize to the `o` convention.

### S6 — `await` is typed as identity

The new `P_prim_await` transfer (fa.cc:1715) is
`flow_vars(operand, result)`, and `fold_constant` folds it as
unary `+`. That means FA sees `await coro()` as returning whatever
the coroutine *function* returns — there is no
coroutine/future/awaitable object in the type system at all, and
`async def` bodies are analyzed as if synchronous. That is a
defensible v1 (matches the C++20-coroutine codegen shim), but it
should be written down in the async issue: `await` on anything
that isn't immediately the result of an async call, storing
coroutines in data structures, or driving them through an event
loop will type incorrectly rather than fail loudly. Also
`fold_constant`'s `DO_FOLD1(+)` for await quietly constant-folds
`await 3` to `3` — semantically nonsense in Python (TypeError),
silently accepted here.

### S7 — `GLOBAL_CONTOUR` semantics: what remains after issue 031

Step 1 (real singleton EntrySet) and step 2 (pyc-side load temps)
landed; the remaining semantic comparisons
(`contour != GLOBAL_CONTOUR` as "contour is a CreationSet") are
correct but stringly-implicit — a
`bool contour_is_creation_set(AVar*)` helper would make ~8 sites
self-documenting. Note also that step 2 was implemented in the
*pyc frontend only*: the V-language frontend still emits direct
global reads, so V programs keep the flow-insensitive
global-union behavior documented in issue 031. Fine for now;
worth one line in 031 so nobody assumes the ifa layer does it.

### S8 — narrowing recognizer: setup-time classification of `is None` operands

The `__is__`/`__nis__` recognition (fa.cc:2247-2272) decides
*which operand is the None constant* by scanning both operands'
`out` **at constraint-setup time**. If neither side has its nil CS
yet (types arrive later), no narrowing is installed and the
constraint is not revisited with the new information (the
IF-constraint block re-runs only via the is_if_arg resume, which
re-enters `add_pnode_constraints` — that does re-run this block,
so in practice late arrival usually recovers; but if *both* sides
transiently contain nil, the "both None → skip" branch permanently
declines). Low severity; the structural fix is recognizing the
None side syntactically (the frontend knows which operand is the
literal) rather than semantically.

### S9 — `convert_NOTYPE_to_void` as policy

With `fruntime_errors` on (pyc's default), every bottom-typed
non-internal AVar becomes `void` after violations are printed
(fa.cc:3110). The 030 investigation showed how effectively this
converts analysis bugs into silent wrong answers downstream
("expression has no type" warning, then a `return 0` clone). The
mechanism is fine as a *runtime-errors-mode* semantic, but
consider gating a debug mode (`IFA_NO_VOID_CONVERT=1`) that keeps
bottoms hard so codegen fails loudly — it would have shortened
both the 030 and the survey-era diagnoses.

---

## 3. Performance observations

### P1 — canonical-table hashing (see B1)

Both hash-cons tables degrade to last-element hashing. Cheap fix,
real effect on large programs (ATypes are created per
union/intersection/diff result).

### P2 — `AVar::match_cache` grows monotonically across passes

`cannonicalize_matches` appends a new `MatchCacheEntry` per full
match; `clear_avar` doesn't reset the cache between passes (safe —
entries validate against canonical ATypes — but never evicted).
A 6-pass convergence leaves each hot send AVar with every
historical entry, all scanned linearly by `match_cache_hit`.
Either clear in `clear_avar` (simplest; costs re-matching per
pass) or cap/LRU.

### P3 — extend_analysis stage 4 is quadratic in confluences

fa.cc:4205-4223 re-runs `collect_cs_marked_confluences` (a full
`fa->css` scan) and a full `split_for_setters` cascade once per
confluence in the outer list, inside a stage that usually finds
nothing. Combined with B4's missing `clear_marks`, the whole
stage deserves a rework: seed all marks once, collect once, split
once.

### P4 — deep recursion in flow walks

`update_setter` (backward recursion), `build_type_mark` /
`build_setter_mark` (mark recursion), `back_reaching` — all
recurse per flow edge on top of AUDIT §5's existing list.
`build_type_mark` recursion depth is bounded only by flow-graph
diameter. Programs with long MOVE chains (machine-generated code)
will hit stack limits inside the splitter before anything else.

---

## 4. Design observations (no action urgently required)

- **D1 — the splitter is a fixed 5-stage cascade with early-out.**
  Each stage's `if (!analyze_again)` gating means the *ordering*
  of stages is semantic: a type split in stage 1 hides a setter
  split that stage 3 would have made this pass (it gets made next
  pass instead). That's correct but makes pass counts — and
  therefore anything order-sensitive (B4, issue-021-class
  nondeterminism) — sensitive to stage ordering. Worth keeping the
  FAPassEvent sidecar (already present) permanently enabled in
  test builds to catch stage-ordering drift.
- **D2 — `type_num_fold` ignores its `Prim`** (`p = 0; // for
  now`, fa.cc:440): the fold cache conflates all prims. Sound
  today because the result is a pure operand-type coercion (bool
  results are special-cased by the caller), but the signature
  advertises per-prim folding it doesn't do.
- **D3 — three closure-construction paths** (`make_closure`,
  `make_period_closure`, `partial_application`'s implicit
  re-entry) share subtle slot conventions; after the 030 fix they
  agree, but a single `closure_cs_feed(cs, i, value_av)` helper
  would make the invariant structural instead of conventional.
- **D4 — `update_display`'s "verify everything" loop** re-asserts
  the invariant on every edge instead of only in debug builds;
  harmless cost, but it *is* the stack-discipline assumption that
  issue 001 showed Python violates — any future frontend feature
  that routes closures through displays again will hit these
  asserts first. A comment pointing at issues/closed/001 would
  save the next person a day.
- **D5 — verified-OK items** (checked during this survey, no
  action): `find_or_make_filtered_entry_set`'s
  `Map::some_disjunction` use is correct for the empty-filters
  case; `AEdge::initial_types` is consumed (print_dispatch);
  blanket rval re-trigger registration covers the snapshot
  transfers for all IF1 the current frontends emit; `fill_rets`'s
  extra-returns Var creation; `get_filtered`'s per-edge internal
  Vars.

---

## 5. Issues found on the way (outside fa.cc)

- **The async/await commits (`ec176ce`…`7aea8dc`)** shipped four
  collateral problems, two already fixed in `ab861a8`: the
  hardcoded `/opt/homebrew/opt/llvm/bin/clang++` + `-stdlib=libc++`
  (broke every `-b` compile on Linux; now env-overridable and
  Apple-gated) and a committed `out` binary (removed; CLAUDE.md
  forbids artifacts). Still open: `-O2` silently added to the
  `.ll → .o` step (slows every LLVM test compile; decide
  intentional or move behind `-O`), and B2's twin in
  `fold_constant` predates async but sits next to the new await
  case.
- **`sym.h` layout changes and stale builds** — the `is_async`
  bitfield addition coincided with a spectacular 142-test phantom
  breakage under incremental rebuilds. CORRECTION on
  investigation: both Makefiles do generate AND include `.d`
  files for their main object groups; the one concrete gap found
  is that `IFA_TEST_OBJS` (testing/ifa_test_main.o) was missing
  from ifa's `-include` line, so `ifa-test` itself could go stale
  on header changes (fixed). The pyc-binary staleness that
  produced the phantom breakage likely came from the
  checkout-heavy bisect churn rather than a systematic Makefile
  hole; `make clean` after header-layout changes remains the safe
  habit MEMORY.md already prescribes.
- **Synthetic golden instability**: `iterator_copy` /
  `vector_iterator` finalize goldens flipped constant-numbering
  order between two sessions' regenerations (issue-021 class,
  fixture-builder evaluation order) and had to be reblessed. The
  synthetic harness would benefit from emitting constants in a
  canonical (value-sorted) order so goldens stop encoding
  evaluation order.
- **`ifa-test` must run from `ifa/`** (fixtures resolved
  relatively). CORRECTION: it does exit 1 on "no fixtures found"
  — the false-green during this survey was the caller's grep
  pipeline swallowing the exit code. It also prints no
  pass/fail summary in that case, so grep-for-"failed:" scripts
  see nothing; the message now says ERROR and points at the cwd
  requirement / `--fixtures-root`.

---

## 6. Recommended order of attack

1. **B3 + B4** (splitter correctness: index loops + clear_marks)
   — small diffs, fix under-splitting and mark contamination;
   watch for golden churn since splitting decisions can change.
2. **B5** (null edge target) — two-line guard; removes a latent
   crash class.
3. **B1** (hash combine) — one-line × 2 with `combine_hash`;
   land separately because it perturbs canonical ordering.
4. **B2** (precision-table indexing, fa.cc + num.cc together) —
   with a V-language mixed-width test.
5. **S1** (unify update_in/permit propagation tails) — refactor,
   no behavior change intended, removes a divergence class.
6. **S3's assertions** (`closure_used` debug check, clear_avar
   invariants comment) — cheap insurance from the 030 experience.
7. The rest as opportunity allows; S6 (await typing) belongs to
   whoever picks the async work back up.
