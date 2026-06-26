# CG_IR_META_PLAN.md — how we'll arrive at a real CG_IR semantics doc

Written: 2026-06-13. Authored as a response to the question "do we
need a more substantial plan for CG_IR's semantics?"

This is a **plan to make a plan**, not the design itself. The
design (CG_IR_SEMANTICS.md) is the output of executing this
plan.

## Constraints (accepted, 2026-06-13)

The project owner answered the open questions. These are
no longer assumptions — they're the design contract:

1. **C backend stays until LLVM achieves parity.** Both
   backends coexist. The C backend is the reference (74/0)
   and the validation gate. The LLVM backend can replace it
   only when it matches the C backend on the full pyc-suite.
   Practical implication: CG_IR must encode everything
   `cg.cc` emits; the IR design that can't lower to working C
   isn't viable.

2. **IF1 stays.** No replacement on the table. CG_IR is **the
   emission layer**, not a replacement IR. It can lean on
   IF1 (escape hatches are FEATURES not bugs). What CG_IR
   owns is "how the post-FA / post-SSU information flows into
   the backends." The boundary is:

   ```
   IF1 (analysis IR)  →  CG_IR (emission IR)  →  C / LLVM
   ```

   This is a sharper boundary than the original CG_IR_PLAN
   imagined. It SIMPLIFIES the design.

3. **More frontends are coming with diverse language
   semantics.** CG_IR cannot be pyc-shaped. It has to be a
   general post-analysis emission target multiple frontends
   can lower to. Likely: each frontend has its own front-end
   AST → IF1 lowering; the shared analysis (FA + clone + DCE
   + SSU) is generic; CG_IR is the shared emission target.

   The IR design pulls toward LLVM-IR-shape (low-level,
   language-agnostic) over pyc-flavored.

4. **Low appetite for breaking changes. Incremental work
   preferred.** The owner has watched me revert code several
   times this session: the formal-arg / live-gate / setLLVMValue
   attempts on issue 017, the production swap iterations on
   Phase 3.4, the field-index investigation. The reverts are
   correct — broken code that's reverted is better than
   committed broken code — but the PATTERN says I've been
   landing changes that are too big to verify in one step.

   The meta-plan's process must address this directly. Every
   code-touching phase needs:

   - An explicit rollback plan
   - Per-change verification (the suite test runs, not just
     a "looks right" review)
   - Smaller commits with smaller blast radius
   - A stop condition: "if N attempts fail to land cleanly,
     escalate to a design pause and reconsider."

5. **More pain points are certain without a principled
   process.** This is the deepest motivation for the
   meta-plan. The semantics doc must be **the canonical
   answer** to "is this a bug?" questions. A bug becomes
   "the IR doesn't allow this" or "the emitter doesn't match
   the IR's contract" — both have clear next actions.
   Without the doc, every new bug is a fresh investigation
   into what the IR was supposed to mean.

## Implications for the phases

Given the constraints, the phases adjust:

- **Phase 0 (Survey)** unchanged. Pure reading.
- **Phase 1 (Distill)**: the "what backends need" tabulation
  now MUST be done for BOTH backends, in equal depth. The
  intersection is what CG_IR needs to expose; the union is
  what CG_IR must support.
- **Phase 2 (Sketch)**: explicitly rejects "replace IF1 refs."
  Escape hatches to IF1 (source_pn for dispatch, source_var
  for value resolution) are part of the contract. What
  changes is making the escape hatches EXPLICIT (typed,
  named, contractual) rather than incidental.
- **Phase 3 (Textual form)**: now constrained to be
  language-agnostic. A pyc-specific S-expression won't do
  if V or the next frontend has different semantics.
- **Phase 4 (Build incrementally)**: stronger ratchets.
  Smaller commits. Rollback plan per landing. Explicit
  acceptance criteria per synthetic test. If I land code,
  it's because the suite stays at 38/37+, ifa-test stays
  58/0, and the new synthetic test passes.
- **Phase 5 (Semantics doc + migration)**: the doc explicitly
  defines:
  - The IF1 ↔ CG_IR boundary
  - The escape hatches as contractual interfaces
  - The migration path from C-backend-reference toward
    LLVM-backend-parity
  - What "parity achieved" measurably means (e.g.,
    "LLVM-suite ≥ 74/0 for one full cycle")

## On the revert pattern (acknowledgment)

The owner is right to flag this. Let me be specific about
what's changing:

**What I did**: landed bigger changes than I could verify in
one step. The setLLVMValue + cross-function leak chain (issue
017) is the clearest example — three sequential attempts, all
reverted, because the underlying interaction with
program-scoped state wasn't understood before I tried fixing
it.

**Why this happened**: I treated "the suite still passes" as
sufficient verification. It isn't: the suite passing doesn't
mean the architecture is right, only that the existing tests
don't exercise the new bug. For an architectural change,
I need to first articulate what should be true, then verify
that.

**What changes in the meta-plan's execution**:

- **Phases 0-3 are pure docs.** No production code. No
  revert risk because there's nothing to revert.
- **Phase 4's per-synthetic-test cadence is tight.** Each
  example forces ONE new piece of emission logic. If a piece
  doesn't land cleanly in 1-2 attempts, I stop and write up
  why before trying again.
- **No "big bang" landings.** If a change would touch
  emit_cg.cc, llvm_codegen.cc, AND llvm.cc, it's too big.
  Split.
- **Verification is per-CG_OP, not per-suite.** The synthetic
  test corpus is the unit of verification. When I land
  CG_BR support, the synthetic CG_BR test passes; the suite
  is a regression catcher, not a verification.
- **Explicit "this is an experiment" flag.** When I'm trying
  something I'm not sure about, I say so in the commit
  message. Easier to track which commits to scrutinize on
  next review.

## Why this exists

The existing docs each cover a different stage:

| Doc | What it is |
|---|---|
| `CODE_GEN_IR.md` | Investigation (pre-implementation, ranks 3 options) |
| `CG_IR_PLAN.md` | Execution plan for Option B's implementation |
| `CG_IR_v2.md` | Audit of what shipped vs what was needed |

None of them is a **semantics specification**. As a result, when
issue 017 surfaced, we couldn't ask "does the IR even allow a fix
of this shape?" because there's no IR contract to consult. We
patched symptomatically (three failed attempts) and ended at
"deferred to v2."

This pattern will repeat. The audit identified ~5 fields that
are decorative, escape hatches that prevent CG_IR from being
independent, and a missing concept (function-scoped value cache)
that's the root of issue 017's class. A semantics-first redesign
re-establishes the slope.

## What the project owner already believes (working hypotheses)

Stated in the conversation that prompted this doc:

1. **The Sym/Var/AVar pyramid likely flattens at the CG_IR
   boundary.** IF1 keeps the pyramid (analysis needs all three
   layers). CG_IR projects it down to a flat value namespace
   at emission time. Important nuance given constraint #2
   (IF1 stays): we don't flatten IF1 itself; we lower IF1's
   pyramid INTO CG_IR's flat shape during cg_normalize.

2. **SSU/SSA likewise flattens.** After SSU resolution in IF1,
   each Var has a single definition. cg_normalize translates
   this to "each CG_IR value has one definition" without
   re-running SSU. The renaming machinery has done its job;
   CG_IR records the result.

3. **What's left is small.** Functions, types, and code with
   operations like IF / SEND / PRIMITIVE / MOVE. This is
   recognizably an LLVM-IR-shape post-mem2reg. With the
   language-agnostic constraint (#3), this needs to be
   genuinely low-level — not just "pyc primitives."

4. **A textual form enables synthetic tests.** Decouples
   emitter validation from FA invariants. Lets us write
   stress-case programs by hand without going through any
   frontend or IF1 analysis. Every IR I know of that ships
   robustly (LLVM, MLIR, GCC RTL with `-dRTL`, Cranelift CLIF)
   has a textual form. With future frontends coming (#3), the
   textual form is also the joining point for cross-frontend
   work — a CG_IR program is the same regardless of whether
   it came from pyc, V, or anything else.

5. **Minimal translation with simple tests, built
   incrementally.** Start by emitting one trivial program
   through CG_IR; expand the test corpus one shape at a time;
   each addition forces one new IR / emission piece. With the
   low-breakage constraint (#4), this approach is mandatory,
   not optional.

These are hypotheses to test, not foregone conclusions. The
meta-plan is the structure for testing them.

## Phases of the work

Each phase is a focused session (~1 day equivalent), ending in
a committed artifact.

### Phase 0 — Survey

**Goal**: understand what's actually in the IF1 layer, what's
load-bearing, what's vestigial.

**Files to read systematically**:

- `if1/sym.h`, `if1/code.h`, `if1/pnode.h`, `if1/var.h`,
  `if1/fun.h`, `if1/prim.h`, `if1/num.h`
- `analysis/fa.h`, `analysis/fa.cc`, `analysis/clone.h`,
  `analysis/clone.cc`, `analysis/pdb.h`
- `optimize/cfg.cc`, `optimize/ssu.cc`, `optimize/dead.cc`,
  `optimize/dom.cc`, `optimize/inline.cc`, `optimize/loop.cc`
- `codegen/cg.cc`, `codegen/llvm.cc`, `codegen/llvm_codegen.cc`,
  `codegen/llvm_primitives.cc`, `codegen/cg_normalize.cc`,
  `codegen/emit_cg.cc`, `codegen/codegen_common.cc`

**For each field on Sym/Var/AVar/PNode/Code/Fun/Prim**, classify:

- **DECLARATIVE** — Sym-level identity, scoping, type
- **ANALYSIS** — FA lattice, AType, flow facts
- **STRUCTURAL** — PNode/Code shape, CFG, dominators
- **BACKEND-ANNOTATION** — cg_string, llvm_value (side-channels)
- **PHASE-SPECIFIC** — meaningful only at a particular phase
  (e.g., `clone_for_constants`, `dispatch_types_built`)

**Output**: `ifa/codegen/CG_IR_SURVEY.md`. A table per data
structure, every field, classification, readers, writers, sample
usage. Plus a narrative summary: "here's what survives FA +
clone + DCE + SSU and feeds emission."

**Pass criterion**: every classified field is either (a) clearly
needed at emission time or (b) clearly only needed during an
earlier phase. The "needed at emission" set is small enough to
list in a section.

### Phase 1 — Distill

**Goal**: from the survey, derive the minimum information set
the BACKENDS actually consume.

**Method**:

- For C backend: trace each per-PNode emission site in `cg.cc`.
  Tabulate every field read. Already half-done by CG_IR_v2.md
  audit; finish.
- For LLVM backend: same exercise for
  `llvm_codegen.cc::translatePNode` + the per-prim emitters
  in `llvm_primitives.cc`. Tabulate.

**Cross-check against Phase 0**: every backend-read field
should appear in Phase 0's "needed at emission" set. If not,
either the survey missed something or the backend reads
something it shouldn't.

**Output**: `ifa/codegen/CG_IR_NEEDS.md`. A 5-to-10-item list
of "this is what the emitter genuinely needs," with the
provenance trace. Each item has: name, why it's needed,
which backend(s) consume it.

**Pass criterion**: a backend port that reads only the listed
items would lose nothing. The list is tight enough to fit on
one screen.

### Phase 2 — Hypothesize

**Goal**: sketch the minimal CG_IR that exposes only the
distilled-needed set.

**Methodology**:

1. Start with the project owner's intuition (flat namespace +
   functions + types + IF/SEND/PRIMITIVE). Write it as a
   header-only declaration.
2. Walk through 3-5 program shapes and trace how each lowers
   to the sketch. Identify gaps.
3. Refine. Iterate.

**The sketch should include**:

- A type system: handful of kinds (int with width, float with
  width, ptr-to, struct-of, fun-signature). Resolve typing
  AHEAD of CG_IR (analyzer does this; we just hold the
  result).
- A function shell: name, signature, list of blocks.
- A block: list of operations, terminator.
- A value system: each value has one definition, a type, and
  is referenced by other operations. NO per-Var llvm_value
  cache — value identity is per-(function, value-id), implicit
  in operation references.
- An operation set: small. Likely IF / CALL / PRIM / MOVE /
  ALLOC / LOAD / STORE / GEP / RET. Aim for ≤10.

**Output**: `ifa/codegen/CG_IR_SKETCH.h` — header only, with
prose-heavy comments explaining the design choices. No
implementation. No textual form yet.

**Pass criterion**: I can hand-walk through 5 program shapes
(simple expression, branching, struct alloc, struct field
read, recursive call) on the sketch and explain exactly what
the IR contains for each.

### Phase 3 — Textual form

**Goal**: define an S-expression (or LLVM-IR-like) textual form
for CG_IR. This is the synthetic-test enabler.

**Why this phase**: until there's a textual form, we can't
write hand-crafted programs that stress the emitter
independently of FA. Every successful IR has a textual form
for exactly this reason.

**Choices to make**:

- LLVM-IR-style (`%name = op type, args`) — pyc-flavored
- S-expressions (`(op result type args)`) — already what
  cg-normalize golden uses
- YAML — verbose but trivial to write

S-expressions are the natural fit because the cg-normalize
phase already uses them. Building on what we have lowers risk.

**Output**:

- `ifa/codegen/CG_IR_TEXT.md` — grammar + 10-15 example
  programs, escalating complexity:
  - empty void fn
  - fn returning a constant
  - fn returning its arg
  - two-block fn with branch
  - loop (one block branches back)
  - struct alloc + field write + field read
  - call to another fn
  - recursive call
  - one with a phi (post-SSA, exposes the value-identity model)

**Pass criterion**: I can hand-write each example, and a
human reader can predict what LLVM IR (or C code) it should
generate.

### Phase 4 — Build up incrementally

**Goal**: implement parser → minimal emitter → exec for the
textual examples, ONE example at a time.

**Methodology**:

1. Take example #1 (empty void fn). Write the parser pieces
   needed for it. Write the emitter pieces needed for it.
   Run it end-to-end. Commit.
2. Take example #2. Same, additive only.
3. Continue.

Each example forces exactly ONE new piece of emission logic.
If an example requires changes to the IR sketch, that's
information: the sketch was incomplete. We revise.

**Why this works**: pyc's actual test corpus is ~75 programs.
A synthetic corpus of 10-15 covers the IR shape. If the
synthetic corpus emits cleanly, the real corpus is a forward
port problem (or a "your FA produces unusual IR" problem
that we'd find quickly).

**Output**:

- `ifa/codegen/cg_ir_v2.{h,cc}` — implementation
- `ifa/codegen/cg_ir_parse.{h,cc}` — textual form parser
- `ifa/codegen/cg_ir_emit_llvm.{h,cc}` — minimal LLVM emitter
- `ifa/tests/cg_ir/*.cgir` — the 10-15 synthetic test programs
- Per-program `*.exec.check` expected output
- Test runner integration

**Pass criterion**: all 10-15 synthetic programs compile,
link, run, and produce the expected output. The implementation
that gets us there is the minimal one.

### Phase 5 — Semantics doc + migration plan

**Goal**: write `ifa/codegen/CG_IR_SEMANTICS.md` as the
synthesis of phases 0-4.

The doc explains:

- The flat namespace model (provenance: phases 0-2)
- The invariants (single-definition, types known, function
  scope) and how they're enforced
- The textual form (provenance: phase 3)
- The contract: frontend lowering (IF1 → CG_IR) and backend
  emission (CG_IR → C / LLVM)
- What the IR explicitly does NOT model (e.g., FA's lattice,
  SSU's renaming, optimization passes)
- How issue 017's class is structurally impossible in this
  model (value identity is per-function)
- How issue 016's SSU formal-arg binding is handled (post-SSU,
  it's just a regular value)

Plus a **migration plan**: how do we move production codegen
onto this without dropping the suite below 38/37?

The migration plan ties to CG_IR_PLAN's existing phases:

- The minimal emitter (Phase 4) replaces the unit-test
  emit_llvm_module shim
- The full LLVM emitter migration (CG_IR_PLAN Phase 3.3
  redux) replaces translateFunctionBody — one PNode kind at
  a time, ratchet on the suite count
- The C backend port (CG_IR_PLAN Phase 4) reuses the same IR
- The cleanup (CG_IR_PLAN Phase 5) deletes the old Sym/Var
  side-channels

**Output**: `ifa/codegen/CG_IR_SEMANTICS.md` and a revised
`CG_IR_PLAN.md` that references it.

**Pass criterion**: the project owner can read CG_IR_SEMANTICS
and tell me whether they agree with the model. If yes, we
have a contract. If no, we know what to change.

## What I can do without further input

- **Phase 0 (survey)** — pure reading + classification.
- **Phase 1 (distill)** — pure tabulation from existing
  emitter code.
- **Phase 2 (sketch)** — design synthesis; might propose
  something the project owner disagrees with, but the sketch
  is reviewable.
- **Phase 3 (textual form)** — grammar + examples; risk is low
  because no production code changes.

That's ~4 focused sessions, all documentation, no production
risk. Result: a complete design proposal with examples.

## What I need the project owner for

- **Phase 4 (implementation)**: writing real code that might
  regress the suite. Each example landing needs your
  go-ahead.
- **Phase 5 (semantics doc review)**: the design call is
  ultimately yours. I can articulate trade-offs but you weigh
  them.
- **Continuous course-correction**: at the end of each phase,
  you tell me if I'm chasing the wrong rabbit. The sooner you
  do, the cheaper the correction.

## Open questions for the project owner (lower-priority but
will sharpen the design)

~~All five answered 2026-06-13 — see "Constraints (accepted)"
at the top of this doc. Kept here for history.~~

1. ~~Is the C backend a permanent fixture?~~ **No, but it's
   the reference until LLVM parity. Cannot be deleted before
   the LLVM suite reaches 74/0.**

2. ~~Is IF1 stable?~~ **Yes. No replacement on the table.
   CG_IR is the emission layer, not a replacement IR. The
   escape hatches to IF1 are part of the contract.**

3. ~~Future-frontend story?~~ **More frontends are coming
   with diverse language semantics. CG_IR must be
   language-agnostic.**

4. ~~Appetite for breaking changes?~~ **Low. Incremental
   work strongly preferred. The owner has explicitly noted
   the revert pattern as a signal that changes have been too
   big. The execution methodology updates to address this.**

5. ~~Pain points beyond 014/016/017?~~ **More are certain
   without a principled process. The semantics doc must be
   the canonical answer to "is this a bug?" so we stop
   re-investigating from scratch each time.**

## Why this is more than just "redo CG_IR"

Three things make this different from the original CG_IR
investigation (CODE_GEN_IR.md):

1. **We have ground truth.** We've shipped the LLVM swap and
   know what works (block structure, terminator metadata,
   per-block iteration) and what doesn't (program-scoped Var
   cache, decorative parallel type model, source_pn as a
   "debug aid"). The original design was speculative; this
   one is empirical.

2. **The owner's intuition is sharper.** "Flatten Sym/Var/AVar"
   and "textual form for synthetic tests" are concrete design
   directions that didn't appear in CODE_GEN_IR.md. They reflect
   patterns from successful IRs in the wild.

3. **The bar is different.** The original CG_IR aimed for "a
   plausible IR for both backends." The new bar is "the IR
   that makes issue 017's class structurally impossible." That's
   a much sharper success criterion.

## What we'd give up by NOT doing this

- Issue 017 doesn't close; future bugs of the same shape don't
  close either (5-7 of them in flight at any given time, if
  the audit is right).
- CG_IR_PLAN Phase 5 (cleanup) can't actually clean up — the
  decorative fields stay because removing them breaks goldens.
- Phase 4 (C backend port) migrates onto an architecture we
  don't believe in.
- New backends inherit unclear semantics.

## What we'd give up by DOING this

- ~3-4 sessions of design work with no test-count improvement.
- A risk that the design we converge on isn't what the project
  owner would have picked. Mitigated by the per-phase
  pass-criteria gates.
- If the survey (Phase 0) discovers that the IR shape needs
  changes to FA / SSU / clone, scope grows. Mitigated by
  flagging early.

## Recommendation

**Greenlight Phase 0.** It's pure survey work. End state:
a classification table and a written assessment. If you don't
like what comes out, we stop without having touched
production. If you do like it, Phase 1 follows.

The minimum useful chunk of work is exactly Phase 0. Sessions
after that are optional commitments.

## Glossary (terms I'll use in the survey)

- **load-bearing**: read or written by the production emission
  path; if absent, output is wrong or compilation fails.
- **decorative**: written by some phase, read only by
  documentation / golden printers / debugger helpers. Removable
  without changing output.
- **escape hatch**: a back-reference in CG_IR to an IF1 type
  (e.g. `CGType::source`). Indicates the abstraction isn't
  complete.
- **scaffold**: a structural seam (e.g., CGBlock) that
  organizes emission but doesn't carry semantic content.
- **post-SSA**: after the SSU pass has resolved into single-
  definition form. CG_IR is post-SSA; that's the key insight
  that lets values be uniquely identified.

## Next step

Say go and I start Phase 0. End of that session:

- `ifa/codegen/CG_IR_SURVEY.md` committed
- A note here marking Phase 0 complete with a one-line
  summary

Say no and we hold at 38/37 with the current docs.
