# Issue 003: fa-converge needs deterministic instrumentation

**Status:** open (deferred from phase 05)
**Affects:** `ifa/analysis/fa.cc:FA::analyze`, `ifa/testing/`.
**Related:** `ifa/testing/phases/05_fa_analyze.md` §3.2.
**Workaround:** the `fa-init` phase covers one-pass FA state; the
`fa-converge` analog (which would lock in pass counts, splitter
outcomes per pass, and convergence behavior) doesn't exist yet.

## Symptom

The plan calls for `fa-converge.expected` to include blocks like:

```
(pass-counts
  total-passes: 3
  splits: 2 type, 0 setter, 1 violation-driven
  time: <suppressed>
)

(history
  pass 1: 4 type confluences; split ES_add → ES_add_int, ES_add_float
  pass 2: no progress
  pass 3: no progress
)
```

Today FA's analyze loop runs to fixed point without exposing any of
this. The only observable surface is:

- `fa->ess.n`, `fa->css.n`, `fa->funs.n` (counts at end).
- `analysis_pass` (the file-scope counter — global state).
- `ifa_verbose >= 3` log lines on stderr (not stable, not diffable).

So we can lock "what came out" via the `fa-init` golden but not
"how many passes it took" or "which splitter stage fired on which
pass" — both of which are valuable regression markers for the
splitter implementation.

## Root cause

FA's analyze loop:

```c
do {
  initialize_pass();
  ...
  complete_pass();
} while (extend_analysis() || if1->callback->reanalyze(type_violations));
```

`extend_analysis` returns 0 or 1 — no record of which of its 5
stages fired. `initialize_pass` and `complete_pass` are also
opaque. Pass count is in a static `analysis_pass`. Timing is
collected in `Timer` instances that contain wallclock measurements
(non-deterministic) and are reset per pass.

In short: the data exists in flight but isn't recorded anywhere a
test printer can read after the fact.

## Proposed fix: a `FAPassEvent` sidecar

Same pattern as `InlineEvent` (`ifa/optimize/inline.h`,
landed in `513eeb9`): an enable flag + per-pass event record +
getter, with all bookkeeping skipped when disabled. Production
pays nothing.

### API sketch

```c
// ifa/analysis/fa.h (or a new ifa/analysis/fa_events.h)

enum FAPassStage {
  FA_STAGE_TYPE_CONFLUENCE,  // split_ess_for_type returned 1
  FA_STAGE_MARK_TYPE,        // split_ess_for_mark_type returned 1
  FA_STAGE_SETTER,           // split_for_setters returned 1
  FA_STAGE_SETTER_OF_SETTER, // split_for_setters_of_setters
  FA_STAGE_MARK_SETTER,      // marks-based setter stage
  FA_STAGE_VIOLATION,        // split_for_violations returned 1
};

struct FAPassEvent {
  int pass;                 // 1-indexed analysis_pass at the time
  FAPassStage stage;        // which stage fired this pass
  int splits;               // splits produced (from the return value)
  int ess_before, ess_after;
  int css_before, css_after;
  int violations_before, violations_after;
};

void fa_events_enable();
void fa_events_disable();
void fa_events_reset();
const Vec<FAPassEvent *> &fa_events_get();
```

### Recording sites in fa.cc

Inside `extend_analysis`:

```c
static int extend_analysis() {
  int before_ess = fa->ess.n, before_css = fa->css.n;
  int analyze_again = 0;

  analyze_again = split_ess_for_type(confluences, SPLIT_EDGES);
  if (analyze_again) record_fa_event(FA_STAGE_TYPE_CONFLUENCE, ...);

  if (!analyze_again) analyze_again = split_ess_for_mark_type(...);
  if (analyze_again) record_fa_event(FA_STAGE_MARK_TYPE, ...);

  // …same for stages 3-5
}
```

Recording is gated on `fa_events_enabled`; production builds get
the same dead-branch elimination as `InlineEvent`.

### Pass counts

After the analyze loop returns, the total pass count is
`analysis_pass`. Locking it directly is fine if `ifa_reset` resets
it (it should — verify). The "splits per stage" totals come from
`fa_events_get()`.

### Time

The plan deliberately marks time as `<suppressed>`. The printer
omits all `*_timer.time` fields. If anyone wants timing back, they
can read stderr with `ifa_verbose >= 1` — out of scope for
goldens.

## Printer sketch

```c
// ifa/testing/print_fa_converge.cc

void print_fa_converge_normalized(FILE *fp, IF1 *p) {
  // common setup (same as print_fa.cc / fa-init)
  ...
  fa_events_reset();
  fa_events_enable();
  int rc = pdb->fa->analyze(if1->top->fun);
  fa_events_disable();

  fputs("(pass-counts\n", fp);
  fprintf(fp, "  total-passes: %d\n", final_pass_count());
  int type=0, mark=0, setter=0, violation=0;
  for (FAPassEvent *e : fa_events_get()) switch (e->stage) {
    case FA_STAGE_TYPE_CONFLUENCE: type++; break;
    case FA_STAGE_MARK_TYPE: mark++; break;
    case FA_STAGE_SETTER:
    case FA_STAGE_SETTER_OF_SETTER:
    case FA_STAGE_MARK_SETTER: setter++; break;
    case FA_STAGE_VIOLATION: violation++; break;
  }
  fprintf(fp, "  splits: %d type, %d mark, %d setter, %d violation\n",
          type, mark, setter, violation);
  fputs(")\n\n", fp);

  fputs("(history\n", fp);
  // group events by pass
  ...
  fputs(")\n", fp);

  // then re-use print_fa_normalized's body for the final state
}
```

## Test cases

| # | Test | Locks |
|---|---|---|
| 01 | `monomorphic_one_pass` | total-passes=1, splits=0 |
| 02 | `splitter_two_pass` (= `06_splitter`) | total-passes=2, splits: 1 type |
| 03 | `cascade_three_pass` (= `09_cascade`) | total-passes=3, splits: 2 type |
| 04 | `class_setter_split` (= `13_setter_split`) | shows mark or setter splits |
| 05 | `violation_no_progress` (= `08_violation`) | reaches stage 5 but no split fires |

These are largely existing fixtures pointed at the new phase.

## Verification plan

1. Add the `FAPassEvent` sidecar in `fa.cc` (small, mirrors
   `InlineEvent`).
2. Implement `print_fa_converge_normalized`.
3. Register `fa-converge` phase, write the 5 fixtures.
4. Run pyc e2e — sidecar disabled in production, so this must be a
   no-op.

## What this unblocks

- Regression coverage for splitter behavior changes: today, a
  refactor of `extend_analysis` could silently change *which* stage
  produces a given split (or how many passes converge takes); the
  golden would only catch end-state differences.
- A meaningful "splitter coverage" gate — we can assert each of the
  5 stages is exercised by at least one fixture.
- Performance regression flags via pass count (a stage-1 split that
  used to converge in 2 passes now needing 5 is visible).

## Why deferred

- Modest implementation effort but adds new public surface area to
  the analysis module.
- The end-state coverage (`fa-init`, `clone`, `dispatch`) catches
  most regressions today; pass-count drift is a quality-of-life
  improvement, not a correctness gate.
