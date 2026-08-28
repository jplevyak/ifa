# 118 — the last two corpus compile failures: union-field width, and polymorphic field offsets

**Status:** open, filed 2026-08-27. Both are REPRESENTATION limits, not
analysis bugs, and each blocks exactly one corpus program.
**Affects:** `ifa/analysis/clone.cc` (`determine_layouts`,
`prim_period_offset`), `ifa/codegen/cg.cc` (member C-type selection).
**Blocks:** `shedskin_examples/chess`, `shedskin_examples/go` — neither
COMPILES, so neither runs.

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
