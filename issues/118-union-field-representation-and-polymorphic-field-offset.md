# 118 — the last two corpus compile failures: union-field width, and polymorphic field offsets

**Status: OPEN but RESCOPED 2026-09-03.** Only the `{bool, None}` width
half survives. Every claim in the original status line has since failed,
so it is restated here rather than left to mislead:

- ~~"the last two corpus compile failures"~~ — **both now compile.**
  `go` compiles AND RUNS (rc=0); `chess` compiles and segfaults at
  runtime for an unrelated reason (see below).
- ~~"each blocks exactly one corpus program"~~ — neither blocks one now.
- ~~"Both are REPRESENTATION limits, not analysis bugs"~~ — **the `go`
  half was an analysis bug.** It was imprecision, exactly as this
  issue's own "worth checking first whether the union is genuine"
  note suggested, and [124](124-FA-refuse-imprecise-inference.md)'s
  splitter fix resolved it. Neither proposed representation change was
  needed.

**What actually remains:** the `{bool, None}` mixed-width slot. A
1-byte `bool` unioned with a pointer-sized `None` has no unboxed
representation, `determine_layouts` refuses it, and
`tests/bool_or_none_fallthrough.py` still fails today with
`mismatched field sizes: class 'closure' field '<anon>' mixes 1- and
8-byte members`. That is real and unfixed.

**`chess` no longer exercises it.** Its union came from `rowAttack`
falling off the end of a `for` — an implicit `return None` against two
`return <bool>`s — and the source now says `return False` explicitly on
that (unreachable) path. So chess compiles on the DEFAULT path; the
representation gap it used to hit is untouched, it simply stopped
producing one. chess's remaining runtime SIGSEGV is heap corruption in
the Boehm free list, the same signature `bh` shows, and is not this
issue.

**Affects:** `ifa/analysis/clone.cc` (`determine_layouts`).
**Pinned by:** `tests/bool_or_none_fallthrough.py` (`.known_issue`).

## chess: a slot mixing `bool` (1 byte) with `None` (8 bytes)

**Minimal repro: `tests/bool_or_none_fallthrough.py`, 7 lines**, reduced
from chess (377 lines) by delta-debugging, with a `.known_issue`
sidecar; `.exec.check` holds CPython's `False`.

```python
def f(xs):
    for k in xs:
        if k:
            return False
    # falls off the end -> None

print(f([0, 1]))
```

`f` returns `False` on one path and falls off the end -- implicit `None`
-- on another, so the result is `{bool, None}`: 1 byte against
pointer-sized. Deterministic across runs.

The **loop is load-bearing**. The same function written with a plain
`if` and no loop COMPILES:

```python
def f(x):
    if x:
        return False        # compiles fine
```

so the implicit-None fall-through is collapsed there but survives as a
separate path out of a loop. Also measured NOT required, though chess
has all of them: the lambda (a plain `def` reproduces), the branch that
merges two callers' results, a second caller, and the second `return
<bool>` statement.

This is exactly chess's shape -- its `rowAttack` returns `False`, or a
bool, or falls off the end of its `for`, and that flows through the
`nonpawnWhiteAttacks` lambda's closure slot.


    mismatched field members: bool(1) __pyc_None_type__(8)
      def: <anon> chess.py:167
    fail: mismatched field sizes: class 'closure' field '<anon>'
          mixes 1- and 8-byte members ('__pyc_None_type__')

A closure slot whose value is `bool | None`, from the long
`if (board[10] and ... and not nonpawnBlackAttacks(...))` chain at
chess.py:165-167 (Python's `and` yields the last operand, so the
temporary's type is the union of the operands').

**`{None, int64}` already works** -- an integer and a pointer round-trip
bit-for-bit, which is exactly what `float_ct`'s comment in
`codegen/cg.cc` says. `{None, bool}` fails only on WIDTH. So widening
the slot to its largest member looks like the whole fix.

**Measured: it is not.** `PYC_WIDEN_UNION_FIELD=1` (added, default off)
takes the max size instead of refusing, and chess gets PAST
`determine_layouts` -- then produces 18 C errors: `sizeof` applied to an
incomplete `void`, illegal indirection, and casts from pointer to
smaller type. The member's declared C TYPE is chosen elsewhere and comes
out `void` for such a union.

So the real work is codegen: pick a common pointer-width integer
representation for a mixed-width union slot and coerce at every read and
write, the way `{None, int64}` gets for free. The flag is kept only to
record that the FA-side check is necessary but nowhere near sufficient.
A FLOAT member stays a genuine refusal either way (`(void*)2.5` is not
legal C).

## go: one field name, two classes, two offsets

    mismatched offsets for field 'color':
      __pyc_None_type__#13@8  Square#1388@16  UCTNode#2309@8
    fail: missmatched offsets

(The diagnostic prints every CreationSet carrying the field; the CHECK
itself already skips `nil_type`, so the real conflict is Square@16 vs
UCTNode@8.)

`color` is a field on `Square` (go.py:60) and on `Board` (go.py:174);
`UCTNode` has only a local of that name, yet carries a `color` member
here -- worth confirming on its own. A union receiver reaches `.color`,
and `prim_period_offset` requires ONE offset across the union because
the emitted access is a single `->eN`.

`determine_layouts` assigns offsets in name-sorted order PER CLASS, so
two classes sharing a field name get the same offset only by accident of
what else they declare.

Two ways out, neither small:

1. **Per-class dispatch on field access.** pyc already branches on the
   classtag for polymorphic METHOD dispatch (`cg_has_classtag`,
   `cg_new_to_val_map`, `PolymorphicSlot`); extending that to field
   reads/writes is the direct analogue.
2. **A global slot assignment** so a field name that ever appears in a
   union receiver gets one offset program-wide. Simpler to state, but it
   constrains every class's layout.

Worth checking first whether the union is genuine or imprecision: if
`Square` and `UCTNode` never actually flow to the same `.color` site,
this is a precision bug and neither of the above is needed.

## Why these are the last two

After issues/117 the corpus is 72 of 77 compiling. Of the five that do
not: `voronoi2` now compiles and runs; `othello3` is FA stagnation
(ifa/057 family); `sudoku5` and `linalg` are BOXING refusals
(issues/018, the same no-representation decision as chess's); and these
two. So chess and `sudoku5`/`linalg` are three faces of one thing -- pyc
does not box, so a union it cannot represent is refused -- and go is the
only one that is really about LAYOUT rather than representation.

> **CORRECTION (2026-08-28): `linalg` is NOT one of them.** Reducing it
> gives `tests/deepcopy_nested_list_recursion.py`, 10 lines, and the
> `{list, int64}` union is manufactured by `list.__deepcopy__`'s shared
> accumulator when it copies a list-of-lists -- the program itself never
> writes a `{container, scalar}` union. Replacing linalg's one
> `copy.deepcopy(M)` with `[row[:] for row in M]` makes it compile
> cleanly. It belongs to
> [105](105-type-degeneration-in-shared-generic-methods.md), which now
> carries the full write-up, and it is a PRECISION bug with a known fix
> direction, not a representation wall. Four faces, not three: only
> `chess` and `sudoku5` remain in the 018 family here.


## What shedskin does with the repro — and pyc already has the mechanism

`shedskin translate` compiles the 7-line repro, and the generated C++
says how:

```cpp
__ss_bool f(list<__ss_int> *xs) {
    FOR_IN(k,xs,0,2,3)
        if (k) { return False; }
    END_FOR
    return False;      // <-- the implicit None became False
}
```

It types `f` as returning `__ss_bool` and gives the fall-off path the
other member's ZERO. There is no union, so there is nothing to
represent. That is a deliberate CPython divergence -- `print(f([0,0]))`
is `None` in CPython and `False` here -- and it is the whole trick.

**pyc already has exactly this**, as `ifa_no_implicit_none`, described in
pyc.cc as "a shedskin-style typed default". `pyc --strict` compiles and
runs the repro, printing `False`, matching shedskin.

### The two `--strict` knobs are orthogonal, and bundling them hides this

`strict_mode_arg` sets `runtime_errors = false` AND
`ifa_no_implicit_none = 1` together, and `permissive_mode_arg` clears
both. But they address unrelated things: the first turns type violations
from warnings-plus-runtime-checks into hard errors; the second is what
removes the `{bool, None}` union. Wanting the second does not imply
wanting the first, and under `--strict` chess clears the BOXING refusal
only to die on `unable to resolve to a single function at call site`,
which is fatal *because* of the first knob.

`PYC_NO_IMPLICIT_NONE` (added) sets the second on its own. With it, in
permissive mode:

    chess     BOXING refusal -> COMPILES (still segfaults at runtime)
    sudoku5   unchanged ("program does not type")
    linalg    unchanged -- its union is {list, int64}, a CONTAINER vs
              SCALAR mix, which implicit-None has nothing to do with

Getting chess to compile also needed a second instance of the
zero-element-tuple cast bug: `_CG_prim_tuple_list(_CG_void, 0)`, where
the DESTINATION's type is voidish rather than the element's, and the
macro's `sizeof(*((_c)0))` is illegal indirection on `void *`. Fixed the
same way as the element-side case and on the default path.

### So the boxing refusal has a shipped escape hatch

Three of the five remaining corpus failures are BOXING refusals, and for
the implicit-None flavour pyc can already do what shedskin does. What is
missing is not a representation -- it is the DECISION about whether
permissive mode should take the shedskin compromise by default, which
trades CPython fidelity (`None` becomes `False`) for compiling. That is
a language-semantics call, not an implementation gap, and it is why this
issue does not simply flip the default.

linalg is unaffected either way -- and per the correction above it does
not belong here at all: its `{list, int64}` is manufactured inside
`list.__deepcopy__`, not written by the program, and it is
[074](074-FA-cross-pass-oscillation-plan.md).

## 2026-09-03: chess's `printBoard` restored, and is waiting on this

`shedskin_examples/chess/chess.py` had its board renderer commented out
and never called. It has been restored and wired into `__main__`, so
when this issue is fixed chess produces checkable output instead of only
a nondeterministic `TIME %.2f` line.

It changes nothing about chess's status here: still
`fail: mismatched field sizes: class 'closure' field '<anon>' mixes 1-
and 8-byte members ('__pyc_None_type__')`, at chess.py:177 rather than
:167 purely because the restored function is 10 lines longer.

The renderer itself is NOT blocked — pyc compiles and runs it with zero
warnings and byte-identical output to CPython. `tests/chess_print_board.py`
pins it standalone (0x88 indexing, `' '.join` over a list of str, and
negative string indexing: black pieces are stored negative and index
`pieces` from the end, so `pieces[-1] == 'P'`). So the only thing between
chess and a printed board is this issue.

## 2026-09-03: chess DOES compile monomorphically — and its board prints

Re-measured end to end, because "chess should compile monomorphically,
shedskin compiles it" is exactly right and worth pinning down.

**The union is not in chess's data.** The author went out of their way to
avoid one: `iNone = -999`, `iTrue = 1`, `iFalse = 0` (chess.py:20-22) are
plain ints, so the 0x88 board stays a homogeneous `list[int]`.

**It is in a return type.** `rowAttack` (chess.py:130) has three exits:

```python
def rowAttack(board, attackers, ix, dir):
  own = attackers[0]
  for k in [i + ix for i in dir]:
    if k & 0x88:
      return False                                           # bool
    if board[k]:
      return (board[k] * own < 0) and board[k] in attackers   # bool
  # loop completes -> implicit `return None`
```

The third exit is the `for` running out. Whether it is REACHABLE depends
on 0x88 ray arithmetic — every ray eventually leaves the board, so the
programmer knows it is dead, and no flow analysis is going to prove it.
That `None` flows out through `nonpawnAttacks`'s `or` chain and the
`nonpawnBlackAttacks` lambda into the closure capture the diagnostic
names, and `{bool, None}` is 1 byte unioned with 8.

**So it compiles with the escape hatch this issue already documents:**

```
$ PYC_NO_IMPLICIT_NONE=1 pyc -D . shedskin_examples/chess/chess.py -o chess
$ (no diagnostics at all)
```

which is precisely shedskin's trick — type the function `__ss_bool` and
give the fall-off path the zero value — and precisely the CPython
divergence it costs. Nothing about chess needs a new representation. The
open question here remains the one this issue already states: whether
permissive mode should take that compromise by default. That is a
language-semantics call.

### Two new facts from running it

**1. The restored board renderer works, in chess itself.** Byte-identical
to CPython:

```
R N B Q K B N R
P P P P P P P P
. . . . . . . .
...
r n b q k b n r
```

This retires the caveat in the section above: `printBoard` is no longer
verified only by `tests/chess_print_board.py` standing in for it.

**2. The runtime failure is HEAP CORRUPTION, not a type problem.** This
issue previously recorded only "still segfaults at runtime". Located:

```
#0  GC_clear_fl_marks (q=0xc <error: Cannot access memory at address 0xc>)
#1  clear_all_fl_marks ()
#2  GC_finish_collection ()
#3  GC_try_to_collect_inner ()
...
#6  GC_generic_malloc_inner_small ()
```

The crash is inside Boehm GC walking a free list, on a pointer of `0xc`.
A free list does not corrupt itself: emitted code wrote through a bad
pointer earlier and the GC is where it surfaces, so the stack is the
symptom and not the site. Note stdout is buffered and lost on the
segfault — the board only appears under `stdbuf -o0`, which is why this
looked like "crashes before printing anything" at first.

That is a separate defect from this issue's representation question, and
it is the thing actually standing between chess and a working binary.

## 2026-09-03 (later): chess FIXED at the source, on the default path

The flag was the wrong lever. `PYC_NO_IMPLICIT_NONE` is global and
changes what `None` MEANS everywhere, to buy one program's compile; and
pyc's goal is not to compile all Python, it is to compile what can be
analyzed monomorphically. `rowAttack` could always be analyzed
monomorphically — the source just did not say so.

```python
  for k in [i + ix for i in dir]:
    if k & 0x88:
      return False
    if board[k]:
      return (board[k] * own < 0) and board[k] in attackers
+ return False     # unreachable: every ray leaves the board
```

The fall-off path is dead — only 0x88 ray arithmetic says so, which is
why FA cannot see it. **CPython never reaches it either**, and would
`TypeError` inside `max()` if it did (`max([False, None])`), which is
independent proof the path is dead in the program as written. So the
explicit `return False` is a no-op semantically and, unlike the flag,
costs **no CPython divergence at all**.

Result, with no flags:

    pyc -D . shedskin_examples/chess/chess.py     # zero diagnostics
    corpus compile_fail: 4 -> 3

and it prints its board byte-identically to CPython.

### What is left is not a typing problem

chess then segfaults inside the FIRST `alphaBeta` — it reaches **0 of
the 2** `print(res)` lines CPython produces. The crash surfaces in Boehm
GC walking a free list on a `0xc` pointer (`GC_clear_fl_marks` <-
`clear_all_fl_marks` <- `GC_finish_collection` <- an allocation).

Two things measured, so the next person does not repeat them:

- **valgrind does not localize it.** 46 findings, every one inside
  Boehm's conservative scan (27 uninitialised-value, 18 conditional
  jump, 1 invalid read) and **zero** stacks mentioning emitted `_CG_f_`
  code. Boehm is deliberately valgrind-hostile; this needs a GC built
  with valgrind tracking before the tool says anything useful.
- **A large `GC_INITIAL_HEAP_SIZE` does not avoid it**, so "collection
  merely surfaces pre-existing corruption" is not sufficient as a
  theory on its own.

A single `speedTest()` reproduces, which makes the workload small enough
to work with. Careful with that reduction: `t0` is only assigned when
`m == 5`, so cutting the outer loop below 6 makes CPython raise
`NameError` on the final TIME line — an artifact of the reduction, not a
finding. pyc crashes long before that line either way.

This is a runtime memory bug, unrelated to this issue's representation
question, and it is now the only thing between chess and a working
binary.

## 2026-09-04: PYC_NO_GC + valgrind — the first heap bug named

Boehm is deliberately valgrind-hostile: it scans conservatively and reads
uninitialised bytes by design, so a heap bug in emitted code only ever
surfaced as a crash inside `GC_clear_fl_marks` / `GC_set_fl_marks` on a
garbage free-list pointer, naming nobody. An earlier valgrind attempt on
bh produced 46 findings, every one inside Boehm's own scan and **zero**
mentioning emitted `_CG_f_` code.

`PYC_NO_GC=1` (new) routes the generated program's allocations to
`calloc` — `calloc`, not `malloc`, because `GC_MALLOC` returns ZEROED
memory and emitted code relies on it. The collector stays linked and
initialised for `pyc_runtime.o` / `libifa_gc.a`; it simply manages almost
nothing. The mode LEAKS by construction and is for debugging only.
Plumbed as `NO_GC=1` through `Makefile.cg`.

chess still segfaults under it — so the bug is real and not a GC
artifact — and valgrind names it in one line:

```
Invalid write of size 8
  at _CG_f_2581_423   /* list::__setitem__ */
Address is 0 bytes after a block of size 16 alloc'd by
  _CG_list_mult_internal
  by _CG_f_2779_22    /* list::__mul__ */
```

`SIZEOF_LIST_HEADER` is 16 and the block is exactly 16, so the list got
**zero data bytes**. The emitted call says why:

```c
t3 = 0;
t1 = (_CG_list)_CG_list_mult(t2, 128, t3);   /* clearCastlingOpportunities = [None] * 0x80 */
```

The element SIZE is emitted as 0. `IFA_DBG_ELEMSZ` (new) shows two
shapes reaching that: an element type of `void` (unresolved) and a
`Type_SUM` (unioned). Both are emitted as `void *` and both carry
`size == 0`, so `size * s1 * l + HEADER` allocates only the header and
the next `__setitem__` writes past it.

**Fix:** a pointer-shaped element with size 0 still occupies a POINTER
SLOT — floor it at `if1->pointer_size`. chess's call becomes
`_CG_list_mult(t2, 128, 8)` and that overrun is gone. `make test`
309/18/0; go, pygmy, richards and kanoodle all still run clean.

**chess is not fixed — there is a second bug.** With the first one
gone, valgrind moves to an `Invalid read of size 8` in
`_CG_f_12374_171` down a deep `alphaBeta` recursion chain. Same method
applies; it just needs the next pass.

### Second bug: a tuple index was never negative-normalised

With the zero-size allocation fixed, valgrind moved straight to an
`Invalid read of size 8` in `evaluate`, **16 bytes before** a 1368-byte
block it had allocated itself. The emitted code shows two adjacent index
expressions that do not agree:

```c
t23 = ((_CG_int64*)(_CG_list_ptr(t13)))[_CG_norm_idx(t24,_CG_prim_len(0,t13))-0];  /* board[i]        */
t21 = ((_CG_int64*)(t10))[t23-0];                                                   /* evals[board[i]] */
```

`cg.cc` normalises a LIST index and emits a RECORD (tuple) index **raw**:

```c
if (single_idx && t->type_kind != Type_RECORD)   ... _CG_norm_idx(...) ...
else                                             ... "[%s-%d]" ...
```

chess indexes `evals` with a board square, which holds a NEGATIVE code
for a black piece (`-1`, `-4`, …). `evals[-4]` therefore read 32 bytes
below the tuple's data — 16 bytes before its allocation, exactly the
address valgrind named. Under the collector this was silent corruption
that surfaced much later as a mangled GC free list, which is why it
survived so long.

A record's arity is STATIC, so the fix is `_CG_norm_idx` with a constant
length; the C compiler folds it for a constant index, the common case.
Pinned by `tests/tuple_negative_index.py`.

### Result

```
chess:  SIGSEGV  ->  prints its board, runs the search, and reports
                     (99999999, 17460) then "no move found" (rc=1)
bh:     SIGSEGV  ->  runs to completion (rc=0)
```

`make test` 309/18/0; go, pygmy, richards, kanoodle unchanged.

**chess is still wrong** — CPython gives `(0, 33571891)` — but it is now
a wrong ANSWER rather than memory corruption, which is a different and
much more tractable class of bug. `bh` is fixed outright.

Two genuine codegen defects, both found by the same method: build with
`PYC_NO_GC=1`, run under valgrind, read the first error. Neither was
visible to `make test`, and neither was reachable at all while Boehm was
absorbing the corruption.

### Third bug: `copy.copy` of a generic list returned an ALIAS

With the memory corruption gone, chess ran but answered
`(99999999, 17460)` against CPython's `(0, 33571891)` — `(beta, mv)`,
i.e. the `value[0] >= beta` cutoff firing on the very first move.

Bisected by comparing pieces against CPython rather than reading the
search: `evaluate(initial)` = 0 ✓ and `len(legalMoves(b))` = 20 ✓ both
matched, and so did every raw move integer and its `toString`. What did
not match was the board AFTER calling `legalMoves`:

```
board unchanged by legalMoves:  CPython True,  pyc False
  differs at 1:  2 -> 0        (the b1 knight vanished)
```

`legalMoves` does `board2 = copy(board)` and then mutates `board2`, so
the copy was aliasing. Six-line repro:

```python
from copy import copy
setup = (1, 2, 3, 4)
b = list(setup); c = copy(b); c[1] = 99
# CPython: b=[1,2,3,4]  c=[1,99,3,4]
# pyc:     b=[1,99,3,4] c=[1,99,3,4]
```

**Cause.** `cg.cc`'s `P_prim_copy` emits plain assignment — identity —
for any destination whose `type_kind` is not `Type_RECORD`:

```c
if (dt->type_kind != Type_RECORD) { ... "%s = %s;\n" ... break; }
```

Right for scalars and immutable strings, wrong for a `list`, which is
`Type_PRIMITIVE`. It hid because a small list LITERAL gets record shape
and copies correctly; only a list that stays generic — here
`list(setup)` — aliased.

**Fix.** `copy.copy` becomes one dispatch, exactly as `deepcopy`
already is: `obj.__pyc_copy__()`, with the value-type fallback on
`__pyc_any_type__` (the old primitive), identity on `__pyc_None_type__`,
and a real element loop on `list`. Pinned by
`tests/copy_generic_list.py`, which covers the generic list, the literal
that already worked, and independence of two copies from each other.

### chess is CORRECT

```
$ diff <(pyc-built chess) <(python3 chess.py)   # modulo the TIME line
FULL OUTPUT MATCHES
```

`(0, 33571891)`, `(0, 33567556)`, … exactly CPython's, rc=0. From
`SIGSEGV` at the start of the day to byte-identical output, via three
codegen defects: a zero-size list allocation, an unnormalised negative
tuple index, and an aliasing shallow copy.

`tests/minmax_3arg.py.check` was re-blessed: the only diff is
`__pyc__.py:1769` → `:1794`, a line shift from adding `__pyc_copy__` to
the builtin library (issues/111, checks embed builtin-library line
numbers). Full gate green — `make test` 310/18/0, LLVM e2e 310/0,
`ifa test_llvm`, `test_dparse`.
