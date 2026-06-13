# CG_IR_META_PLAN.md — how we'll arrive at a real CG_IR semantics doc

Written: 2026-06-13. Authored as a response to the question "do we
need a more substantial plan for CG_IR's semantics?"

This is a **plan to make a plan**, not the design itself. The
design (CG_IR_SEMANTICS.md) is the output of executing this
plan.

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

1. **The Sym/Var/AVar pyramid likely flattens.** At emission
   time, after FA + clone + DCE + SSU, much of the layered
   abstraction isn't load-bearing. Sym + Var + AVar may
   collapse to a flat namespace of values.

2. **SSU/SSA likewise flattens.** After SSU resolution, each
   value has a single definition. The renaming machinery has
   done its job; the IR doesn't need to re-represent it.

3. **What's left is small.** Functions, types, and code with
   operations like IF / SEND / PRIMITIVE / MOVE. This is
   recognizably an LLVM-IR-shape post-mem2reg.

4. **A textual form enables synthetic tests.** Decouples
   emitter validation from FA invariants. Lets us write
   stress-case programs by hand. Every IR I know of that
   ships robustly (LLVM, MLIR, GCC RTL with `-dRTL`, Cranelift
   CLIF) has a textual form.

5. **Minimal translation with simple tests, built incrementally.**
   Start by emitting one trivial program through CG_IR; expand
   the test corpus one shape at a time; each addition forces
   one new IR / emission piece.

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

1. **Is the C backend a permanent fixture?** If so, the IR
   needs to express things both backends can consume — that
   constrains the operation set. If the C backend is
   maintenance-mode, we can design for LLVM and back-port to C
   as needed.

2. **Is IF1 stable, or is "eventually replace IF1" on the
   table?** If IF1 is forever, CG_IR is genuinely
   "scaffolding on top of IF1 for emission." If IF1 might
   change, CG_IR should be self-contained.

3. **Is there a future-frontend story?** Lua, V, Python 4,
   etc. If pyc-the-frontend grows siblings, CG_IR is the
   joining point. If pyc is THE frontend forever, CG_IR can
   be pyc-shaped.

4. **What's the appetite for breaking changes?** Some
   semantic clarifications (e.g., changing the cg-normalize
   golden format) imply rebless work. How much of that is
   acceptable per session?

5. **Are there pain points beyond issues 014/016/017 that
   the redesign should consciously address?** Things you've
   noticed but haven't filed.

Default assumptions if you don't answer: C and LLVM both
permanent, IF1 stable, pyc-only frontend, willing to break
the cg-normalize golden format, only known pain points
addressed.

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
