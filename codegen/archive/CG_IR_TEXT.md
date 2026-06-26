# CG_IR_TEXT.md — textual form + synthetic test corpus

Phase 3 of `CG_IR_META_PLAN.md`. Defines the S-expression
textual form for CG_IR and hand-writes 13 example programs of
escalating complexity that exercise every concept from the
Phase 2 sketch.

**Why this phase exists**:

- A textual form makes CG_IR programs hand-writable.
- Hand-writable programs are stress tests that don't depend
  on any frontend or IF1.
- The synthetic corpus becomes the per-CG_OP unit of
  verification for Phase 4's incremental implementation.
- Cross-frontend stability (constraint #3 in the meta-plan):
  a CG_IR program is the same regardless of whether it came
  from pyc, V, or any future frontend.

**What this is NOT**:

- A parser implementation. Phase 4 writes the parser. This
  doc defines what it parses.
- A complete spec for every edge case. The textual form will
  evolve with the IR. This is the v0 grammar + a corpus
  enough to validate it.

**Design choices**:

- **S-expressions**, not LLVM-IR-style. Rationale:
  - The cg-normalize golden already uses S-expressions; we
    build on what's working.
  - S-expressions are trivial to parse (~100 LOC) and easy
    to write by hand.
  - Keyword arguments (`:keyword value`) match the existing
    pyc IF1 dump format.
- **One file per program**. Extension `.cgir`. Each file is
  a complete CGProgram.
- **Comments** with `;;` to end of line. Familiar to
  Scheme/Common Lisp readers; doesn't conflict with anything.

---

## Grammar

```
Program  ::= ( TopLevel* )

TopLevel ::= TypeDecl | ConstDecl | GlobalDecl | FunDecl

Comments ::= ";;" .* "\n"          ; line comments, allowed anywhere
```

### Types

```
TypeDecl ::= "(type" %id  :kind Kind  PropList ")"
Kind     ::= "void" | "int" | "uint" | "float" | "bool"
           | "ptr"  | "struct" | "fun_ptr" | "ref" | "sum"
           | "symbol"

PropList ::= ( ":bits"   <int> )?
             ( ":fields" "(" Field* ")" )?
             ( ":element" %TypeRef )?
             ( ":alias_of" %TypeRef )?
             ( ":is_heap_aggregate" Bool )?
             ( ":name" String )?

Field    ::= "(" %Name :type %TypeRef ":idx" <int> ")"
Bool     ::= "true" | "false"
```

Predefined types (no declaration needed):

```
void, bool, int8, int16, int32, int64,
uint8, uint16, uint32, uint64,
float32, float64,
sym (the symbol-literal scalar)
nil (the nil singleton type)
```

These resolve at parse time. User types use `%id` references
to user-declared TypeDecls.

### Constants and globals

```
ConstDecl  ::= "(const"  %id ImmValue  ":type" %TypeRef ")"
GlobalDecl ::= "(global" %id ":type" %TypeRef Init? ")"

ImmValue   ::= "(int"   <int64> ")"
             | "(uint"  <uint64> ")"
             | "(float" <double> ")"
             | "(bool"  Bool ")"
             | "(str"   StringLit ")"
             | "(sym"   %name ")"
             | "(nil)"
             | "(undef)"

Init       ::= ":init" ImmValue
```

### Functions

```
FunDecl  ::= "(fun" %name
              ":signature" Signature
              ":entry"     %BlockRef
              ":formals"   "(" %ValueRef* ")"?
              FlagList
              ValueDecl*
              BlockDecl+
              ")"

Signature ::= "(" %TypeRef %TypeRef* ")"   ; (ret arg0 arg1 …)

FlagList  ::= ( ":external"     )?    ; declaration only
              ( ":varargs"      )?
              ( ":main"         )?    ; pyc __main__ marker
              ( ":live" Bool    )?    ; default true

ValueDecl ::= "(value" %id
                  ":type"  %TypeRef
                  ":scope" Scope
                  PropList
                  ")"

Scope     ::= "local" | "formal" | "global" | "constant"
            | "fun_ref" | "symbol"
```

### Blocks and instructions

```
BlockDecl ::= "(block" %id
                ":label"  %LabelName?
                ":preds"  "(" %BlockRef* ")"?
                Inst*
                PhiBlock?
                ":term"   Term
                ")"

PhiBlock  ::= ":phi_by_pred" "(" PhiEntry* ")"
PhiEntry  ::= "(" %PredBlockRef MoveList ")"
MoveList  ::= "(" PhiMove* ")"
PhiMove   ::= "(move" %ValueRef "=>" %ValueRef SrcLoc? ")"

Inst      ::= "(inst" %id Op Operands? "=>" Result? SrcLoc? ")"

Op        ::= "nop" | "move" | "load" | "store" | "alloc"
            | "field_load" | "field_store"
            | "index_load" | "index_store"
            | "call" | "prim"
            | "binop"  BinOp
            | "unop"   UnOp
            | "cast"   CastOp

Operands  ::= %ValueRef+              ; the rvals

Result    ::= %ValueRef               ; the lval (single)
            | "(" %ValueRef+ ")"      ; multiple lvals (rare; for tuple destructure)

Term      ::= "(br"     %BlockRef ")"
            | "(cond_br" %ValueRef %BlockRef %BlockRef ")"
            | "(ret"    %ValueRef? ")"
            | "(unreachable)"

BinOp     ::= "add" | "sub" | "mul" | "div" | "mod"
            | "lt"  | "le"  | "gt"  | "ge"  | "eq"  | "ne"
            | "and" | "or"  | "xor" | "shl" | "shr"
UnOp      ::= "neg" | "not" | "bitnot"
CastOp    ::= "trunc" | "zext" | "sext" | "bitcast"

SrcLoc    ::= ":line" <int> (":col" <int>)? (":file" String)?
```

### References

- `%id` — any identifier. By context, refers to a value,
  block, type, or function.
- Identifiers are lexically scoped to their declaration:
  - `%name` after `(type %name ...)` declares a TypeRef.
  - `%name` after `(value %name ...)` declares a ValueRef
    (CGValue) within the enclosing function (or program-scope
    for globals/constants).
  - `%name` after `(block %name ...)` declares a BlockRef
    within the enclosing function.
  - `%name` after `(fun %name ...)` declares a function name.
  - `%name` after `(inst %name ...)` is the CGInst's id (used
    for diagnostics; references are by the result value's id,
    not the inst's id).

### Whitespace and formatting

- Lisp-style: ignored between tokens. Indentation is for
  humans.
- Convention: one inst per line; blocks indented inside
  functions; functions at top level.

---

## Synthetic test corpus

Each test program is a `.cgir` file. The corpus escalates
complexity: each program adds exactly one new construct.

### Test 01 — empty void fn

```cgir
;; Test 01: the trivial baseline.
;; A void function with no body and no formals.
;;
;; Exercises: CGFun, CGBlock, CG_RET terminator with no value.

((fun %hi
   :signature (void)
   :entry %b0
   (block %b0
     :term (ret))))
```

**Expected LLVM IR shape**:

```llvm
define void @hi() {
entry:
  ret void
}
```

**Expected C shape**:

```c
void hi() {
  return;
}
```

---

### Test 02 — fn returning a constant

```cgir
;; Test 02: constant payload.
;; Exercises: CGV_CONSTANT scope, ImmValue (int), CG_RET with value.

((const %c42 (int 42) :type int64)
 (fun %answer
   :signature (int64)
   :entry %b0
   (block %b0
     :term (ret %c42))))
```

**Expected LLVM IR**:

```llvm
define i64 @answer() {
entry:
  ret i64 42
}
```

---

### Test 03 — fn returning its arg (identity)

```cgir
;; Test 03: formal argument as a value.
;; Exercises: CGV_FORMAL scope, def-use across a single block.

((fun %id
   :signature (int64 int64)
   :entry %b0
   :formals (%x)
   (value %x :type int64 :scope formal)
   (block %b0
     :term (ret %x))))
```

**Expected LLVM IR**:

```llvm
define i64 @id(i64 %x) {
entry:
  ret i64 %x
}
```

---

### Test 04 — fn with a local + arithmetic

```cgir
;; Test 04: local value + CG_BINOP.
;; sum = a + b
;; return sum
;;
;; Exercises: CG_BINOP with sub-kind add, CGV_LOCAL scope,
;; def-use chain through a local.

((fun %sum2
   :signature (int64 int64 int64)
   :entry %b0
   :formals (%a %b)
   (value %a :type int64 :scope formal)
   (value %b :type int64 :scope formal)
   (value %s :type int64 :scope local)
   (block %b0
     (inst %i0 binop add %a %b => %s)
     :term (ret %s))))
```

**Expected LLVM IR**:

```llvm
define i64 @sum2(i64 %a, i64 %b) {
entry:
  %s = add i64 %a, %b
  ret i64 %s
}
```

---

### Test 05 — fn with comparison + conditional branch

```cgir
;; Test 05: sign(x) = -1 if x<0 else 1.
;; Exercises: CG_BINOP lt, CG_COND_BR, multi-block CFG,
;; multiple constant payloads.

((const %neg1 (int -1) :type int64)
 (const %one  (int  1) :type int64)
 (const %zero (int  0) :type int64)

 (fun %sign
   :signature (int64 int64)
   :entry %b0
   :formals (%x)
   (value %x :type int64 :scope formal)
   (value %t0 :type bool :scope local)

   (block %b0
     :label entry
     (inst %i0 binop lt %x %zero => %t0)
     :term (cond_br %t0 %b1 %b2))

   (block %b1
     :label if_neg
     :preds (%b0)
     :term (ret %neg1))

   (block %b2
     :label if_pos
     :preds (%b0)
     :term (ret %one))))
```

**Expected LLVM IR**:

```llvm
define i64 @sign(i64 %x) {
entry:
  %t0 = icmp slt i64 %x, 0
  br i1 %t0, label %if_neg, label %if_pos
if_neg:
  ret i64 -1
if_pos:
  ret i64 1
}
```

---

### Test 06 — loop with phi MOVE

```cgir
;; Test 06: count to N, returning the count.
;; Exercises: phi MOVE on a back-edge; the first phi-using test.
;;
;; Loop pseudocode:
;;   i = 0
;;   while i < n: i = i + 1
;;   return i

((const %zero (int 0) :type int64)
 (const %one  (int 1) :type int64)

 (fun %count_to
   :signature (int64 int64)
   :entry %b_entry
   :formals (%n)
   (value %n        :type int64 :scope formal)
   (value %i_entry  :type int64 :scope local)   ; initial i = 0
   (value %i_loop   :type int64 :scope local)   ; i at loop head
   (value %i_next   :type int64 :scope local)   ; i + 1 at loop body
   (value %cond     :type bool  :scope local)

   ;; entry: %i_entry = 0; goto loop_head
   (block %b_entry
     (inst %i0 move %zero => %i_entry)
     :term (br %b_head))

   ;; loop_head: %i_loop is a phi (from entry: %i_entry; from body: %i_next)
   (block %b_head
     :preds (%b_entry %b_body)
     :phi_by_pred
       ((%b_entry ((move %i_entry => %i_loop)))
        (%b_body  ((move %i_next  => %i_loop))))
     (inst %i1 binop lt %i_loop %n => %cond)
     :term (cond_br %cond %b_body %b_exit))

   ;; loop_body: %i_next = %i_loop + 1; back to loop_head
   (block %b_body
     :preds (%b_head)
     (inst %i2 binop add %i_loop %one => %i_next)
     :term (br %b_head))

   ;; exit: return %i_loop
   (block %b_exit
     :preds (%b_head)
     :term (ret %i_loop))))
```

**Expected LLVM IR** (alloca + load + store model — pyc's
convention — equivalent to a phi):

```llvm
define i64 @count_to(i64 %n) {
entry:
  %i = alloca i64
  store i64 0, ptr %i
  br label %head
head:
  %i_v = load i64, ptr %i
  %cond = icmp slt i64 %i_v, %n
  br i1 %cond, label %body, label %exit
body:
  %i_next = add i64 %i_v, 1
  store i64 %i_next, ptr %i
  br label %head
exit:
  %ret = load i64, ptr %i
  ret i64 %ret
}
```

The textual form's `phi_by_pred` makes the MOVE placement
explicit. The emitter chooses alloca-store or true phi based
on backend convention (pyc uses alloca-store).

---

### Test 07 — calling another fn

```cgir
;; Test 07: caller of a previously-defined fn.
;; main() { return id(7); }
;;
;; Exercises: CG_CALL via CGV_FUN_REF, cross-fun reference,
;; multiple top-level FunDecls.

((const %c7 (int 7) :type int64)

 (fun %id
   :signature (int64 int64)
   :entry %b0
   :formals (%x)
   (value %x :type int64 :scope formal)
   (block %b0
     :term (ret %x)))

 (value %id_ref :type fun_ptr :scope fun_ref)
 ;; Note: %id_ref is bound to the function %id by the parser
 ;; via its name. Phase 4's implementation may use a
 ;; `:target` keyword on `value` declarations of scope fun_ref.

 (fun %main
   :signature (int64)
   :main true
   :entry %b0
   (value %r :type int64 :scope local)
   (block %b0
     (inst %i0 call %id_ref %c7 => %r)
     :term (ret %r))))
```

**Expected LLVM IR**:

```llvm
define i64 @id(i64 %x) {
entry:
  ret i64 %x
}
define i64 @main() {
entry:
  %r = call i64 @id(i64 7)
  ret i64 %r
}
```

---

### Test 08 — allocate a struct + field access

```cgir
;; Test 08: struct allocation + field store + field load.
;;
;; struct Point { x: int64; y: int64 }
;; def make_pt():
;;   p = Point()
;;   p.x = 3
;;   return p.x
;;
;; Exercises: CGType (struct, is_heap_aggregate true),
;; CG_ALLOC, CG_FIELD_STORE, CG_FIELD_LOAD with pre-resolved
;; field_idx.

((type %Point
   :kind struct
   :is_heap_aggregate true
   :fields ((%x :type int64 :idx 0)
            (%y :type int64 :idx 1)))

 (const %c3 (int 3) :type int64)

 (fun %make_pt
   :signature (int64)
   :entry %b0
   (value %p   :type ptr  :scope local)  ; ptr-to-Point on heap
   (value %v   :type int64 :scope local)
   (block %b0
     (inst %i0 alloc :type %Point => %p)
     (inst %i1 field_store :field_idx 0 %p %c3)
     (inst %i2 field_load  :field_idx 0 %p => %v)
     :term (ret %v))))
```

**Expected LLVM IR** (simplified):

```llvm
%Point = type { i64, i64 }
define i64 @make_pt() {
entry:
  %p = call ptr @GC_malloc(i64 16)
  %p_x = getelementptr %Point, ptr %p, i32 0, i32 0
  store i64 3, ptr %p_x
  %v_ptr = getelementptr %Point, ptr %p, i32 0, i32 0
  %v = load i64, ptr %v_ptr
  ret i64 %v
}
```

---

### Test 09 — global variable + binding from construction

```cgir
;; Test 09: store malloc result into a global.
;; THE STRUCTURAL ISSUE-014 / 017 TEST.
;;
;; struct Box { val: int64 }
;; box: Box*  (global)
;; def init_box():
;;   box = Box()
;;   box.val = 42
;;
;; def get_val():
;;   return box.val
;;
;; Exercises: CGV_GLOBAL, CG_MOVE binding malloc result to
;; global, field access through global. The cross-function
;; flow (init_box stores into @box; get_val reads from @box)
;; was the issue 017 trigger.

((type %Box
   :kind struct
   :is_heap_aggregate true
   :fields ((%val :type int64 :idx 0)))

 (const %c42 (int 42) :type int64)

 (global %box :type ptr :scope global)

 (fun %init_box
   :signature (void)
   :entry %b0
   (value %tmp :type ptr :scope local)
   (block %b0
     (inst %i0 alloc :type %Box => %tmp)
     (inst %i1 move  %tmp => %box)           ; store to global
     (inst %i2 field_store :field_idx 0 %box %c42)
     :term (ret)))

 (fun %get_val
   :signature (int64)
   :entry %b0
   (value %v :type int64 :scope local)
   (block %b0
     (inst %i0 field_load :field_idx 0 %box => %v)
     :term (ret %v))))
```

**Expected LLVM IR**:

```llvm
%Box = type { i64 }
@box = global ptr null

define void @init_box() {
entry:
  %tmp = call ptr @GC_malloc(i64 8)
  store ptr %tmp, ptr @box                ; THE binding store
  %box_v = load ptr, ptr @box
  %box_val = getelementptr %Box, ptr %box_v, i32 0, i32 0
  store i64 42, ptr %box_val
  ret void
}

define i64 @get_val() {
entry:
  %box_v = load ptr, ptr @box
  %box_val = getelementptr %Box, ptr %box_v, i32 0, i32 0
  %v = load i64, ptr %box_val
  ret i64 %v
}
```

**Why this test matters**: under v1, this is where issue 017
manifested. Under v2, the per-CGFun value cache means
`init_box`'s emission of `%tmp` doesn't leak to `get_val`'s
emission. Each function's value cache is independent. The
`CG_MOVE %tmp => %box` makes the construction-flow store
explicit; the emitter resolves it via the value's scope
(global) and emits the right LLVM store.

---

### Test 10 — vector indexing

```cgir
;; Test 10: vector / array indexing.
;; def sum_first_two(v):
;;   return v[0] + v[1]
;;
;; Exercises: CG_INDEX_LOAD, integer index, vector type.

((type %IntVec
   :kind ptr            ; opaque pointer; element type matters
   :element int64)

 (const %zero (int 0) :type int64)
 (const %one  (int 1) :type int64)

 (fun %sum_first_two
   :signature (int64 %IntVec)
   :entry %b0
   :formals (%v)
   (value %v   :type %IntVec :scope formal)
   (value %a   :type int64   :scope local)
   (value %b   :type int64   :scope local)
   (value %s   :type int64   :scope local)
   (block %b0
     (inst %i0 index_load %v %zero => %a)
     (inst %i1 index_load %v %one  => %b)
     (inst %i2 binop add  %a %b   => %s)
     :term (ret %s))))
```

**Expected LLVM IR**:

```llvm
define i64 @sum_first_two(ptr %v) {
entry:
  %a_p = getelementptr i64, ptr %v, i64 0
  %a   = load i64, ptr %a_p
  %b_p = getelementptr i64, ptr %v, i64 1
  %b   = load i64, ptr %b_p
  %s   = add i64 %a, %b
  ret i64 %s
}
```

---

### Test 11 — primitive via escape hatch (print)

```cgir
;; Test 11: invoke a runtime primitive.
;; def shout(n):
;;   print(n)
;;
;; Exercises: CG_PRIM with the deliberate escape hatch.
;; The backend's per-prim emitter handles the actual
;; LLVM/C emission via the `prim` field.
;;
;; Note: the textual form references prims by NAME (resolved
;; to Prim* at parse time via the runtime's prim table). This
;; keeps the test corpus stable as the prim table evolves.

((fun %shout
   :signature (void int64)
   :entry %b0
   :formals (%n)
   (value %n :type int64 :scope formal)
   (block %b0
     (inst %i0 prim :name "write" %n)
     (inst %i1 prim :name "writeln")
     :term (ret))))
```

**Expected LLVM IR** (after per-prim emitter dispatch):

```llvm
define void @shout(i64 %n) {
entry:
  %s = call ptr @GC_malloc(i64 64)
  call i32 @snprintf(ptr %s, i64 64, ptr @.str.tostr, i64 %n)
  call i32 (ptr, ...) @printf(ptr @.str.write, ptr %s)
  call i32 (ptr, ...) @printf(ptr @.str.writeln)
  ret void
}
```

---

### Test 12 — recursion + early return

```cgir
;; Test 12: recursive factorial.
;; def fact(n):
;;   if n <= 1: return 1
;;   return n * fact(n - 1)
;;
;; Exercises: self-call (recursive CGV_FUN_REF), conditional
;; branch with both branches reaching a return, multiplication.

((const %one  (int 1) :type int64)

 (fun %fact
   :signature (int64 int64)
   :entry %b0
   :formals (%n)
   (value %n        :type int64 :scope formal)
   (value %is_base  :type bool  :scope local)
   (value %nm1      :type int64 :scope local)
   (value %sub_res  :type int64 :scope local)
   (value %prod     :type int64 :scope local)

   ;; fact_ref binds to %fact itself for the recursive call.
   (value %fact_ref :type fun_ptr :scope fun_ref)

   (block %b0
     :label entry
     (inst %i0 binop le %n %one => %is_base)
     :term (cond_br %is_base %b_base %b_rec))

   (block %b_base
     :preds (%b0)
     :term (ret %one))

   (block %b_rec
     :preds (%b0)
     (inst %i1 binop sub %n %one          => %nm1)
     (inst %i2 call %fact_ref %nm1        => %sub_res)
     (inst %i3 binop mul %n %sub_res      => %prod)
     :term (ret %prod))))
```

**Expected LLVM IR**:

```llvm
define i64 @fact(i64 %n) {
entry:
  %is_base = icmp sle i64 %n, 1
  br i1 %is_base, label %base, label %rec
base:
  ret i64 1
rec:
  %nm1 = sub i64 %n, 1
  %sub_res = call i64 @fact(i64 %nm1)
  %prod = mul i64 %n, %sub_res
  ret i64 %prod
}
```

---

### Test 13 — small realistic program (multi-concept)

```cgir
;; Test 13: combines struct, loop, branch, recursion-of-mind.
;;
;; struct Counter { value: int64 }
;; def reset(c): c.value = 0
;; def bump(c):  c.value = c.value + 1
;; def count_up_to(n):
;;   c = Counter()
;;   reset(c)
;;   while c.value < n: bump(c)
;;   return c.value
;;
;; Exercises: struct allocation, multiple fns, mutation across
;; calls, loop with phi, recursive-cum-iterative flow.

((type %Counter
   :kind struct
   :is_heap_aggregate true
   :fields ((%value :type int64 :idx 0)))

 (const %zero (int 0) :type int64)
 (const %one  (int 1) :type int64)

 (fun %reset
   :signature (void ptr)
   :entry %b0
   :formals (%c)
   (value %c :type ptr :scope formal)
   (block %b0
     (inst %i0 field_store :field_idx 0 %c %zero)
     :term (ret)))

 (fun %bump
   :signature (void ptr)
   :entry %b0
   :formals (%c)
   (value %c    :type ptr   :scope formal)
   (value %v0   :type int64 :scope local)
   (value %v1   :type int64 :scope local)
   (block %b0
     (inst %i0 field_load  :field_idx 0 %c => %v0)
     (inst %i1 binop add %v0 %one => %v1)
     (inst %i2 field_store :field_idx 0 %c %v1)
     :term (ret)))

 (value %reset_ref :type fun_ptr :scope fun_ref)
 (value %bump_ref  :type fun_ptr :scope fun_ref)

 (fun %count_up_to
   :signature (int64 int64)
   :main true
   :entry %b_entry
   :formals (%n)
   (value %n        :type int64 :scope formal)
   (value %c        :type ptr   :scope local)
   (value %cv       :type int64 :scope local)  ; current value at loop head
   (value %cond     :type bool  :scope local)

   (block %b_entry
     (inst %i0 alloc :type %Counter => %c)
     (inst %i1 call %reset_ref %c)
     :term (br %b_head))

   (block %b_head
     :preds (%b_entry %b_body)
     (inst %i2 field_load :field_idx 0 %c => %cv)
     (inst %i3 binop lt %cv %n => %cond)
     :term (cond_br %cond %b_body %b_exit))

   (block %b_body
     :preds (%b_head)
     (inst %i4 call %bump_ref %c)
     :term (br %b_head))

   (block %b_exit
     :preds (%b_head)
     :term (ret %cv))))
```

Note: in this program the "loop variable" is reread from the
struct field each iteration (no phi needed in CG_IR because
the struct's mutation is visible across the loop). This
matches pyc's actual emission pattern.

---

## What the corpus covers

| Concept | Test(s) | Notes |
|---|---|---|
| Empty fn / void return | 01 | trivial |
| CG_RET with value | 02 | constant payload |
| CGV_CONSTANT | 02, 05, 07, 09, 10, 12, 13 | program-scope |
| CGV_FORMAL | 03, 04, 05, 07, 10, 11, 12, 13 | function args |
| CGV_LOCAL | 04+ | computed values |
| CGV_GLOBAL | 09 | module-level |
| CGV_FUN_REF | 07, 12, 13 | cross-fun calls |
| CG_BINOP (lt, le, add, sub, mul) | 04, 05, 06, 10, 12, 13 | arithmetic |
| CG_COND_BR | 05, 06, 12, 13 | conditional |
| CG_BR | 06, 13 | unconditional |
| Multi-block CFG | 05, 06, 12, 13 | preds list populated |
| Phi MOVE on back-edge | 06 | explicit `phi_by_pred` |
| CG_CALL | 07, 12, 13 | inter-fun |
| CG_ALLOC + struct type | 08, 09, 13 | with `is_heap_aggregate` |
| CG_FIELD_STORE / CG_FIELD_LOAD | 08, 09, 13 | with pre-resolved field_idx |
| CG_MOVE (binding to global) | 09 | the issue-017 fix |
| CG_INDEX_LOAD | 10 | vector access |
| CG_PRIM (escape hatch) | 11 | per-prim dispatch |
| Recursion | 12 | self-call via CGV_FUN_REF |
| Multiple fns + state | 09, 13 | full program |

Every concept from Phase 1's "8 must expose" list is touched
by at least one test.

**What's deliberately NOT tested in this v0 corpus**:

- Closures (subsidiary question SQ3)
- SUM tag discriminator (subsidiary question SQ2)
- Undef as a value (subsidiary question SQ1)
- Generic dispatch (clone-and-specialize) — handled upstream by
  IF1 before CG_IR
- Debug info / source locations — present in grammar, omitted
  in corpus for clarity

These belong in v1 of the corpus when Phase 5's semantics doc
formalizes them.

---

## Round-trip property

The textual form is the **source of truth** for cg-normalize
goldens going forward. The round-trip property:

```
text → parse → CGProgram → print → text
```

must be identity (modulo whitespace and comment removal).

Phase 4's parser must satisfy:

- Every test 01–13 round-trips.
- Every existing cg-normalize golden round-trips (after a
  one-time port to the new format).

---

## Phase 3 exit criteria — checked

- [x] Grammar defined (S-expression, ~100 BNF lines)
- [x] 13 hand-written test programs of escalating complexity
- [x] Every Phase 1 concept exercised by at least one test
- [x] Each test has a predictable expected LLVM IR shape
- [x] Round-trip property stated

---

## Recommended next phase

Phase 4 — incremental implementation. The plan:

1. Implement the parser (~100-150 LOC).
2. Implement a minimal LLVM emitter for test 01. Land it.
3. Implement just enough emitter logic to pass test 02. Land
   it.
4. Continue through tests 03–13, ONE AT A TIME, with each
   landing being a small, reviewable, revertable commit.
5. Each test passing is the verification (synthetic-test-as-
   contract). The pyc-suite is the regression catcher.

Each landing must:

- Add new tests that pass.
- Not regress any existing tests.
- Be small (one file changed, ideally; few hundred LOC max).

If a landing requires more than 2 attempts, stop and write
up why before retrying (per the revert-pattern reckoning).

Estimated Phase 4 scope: 8–12 commits, each landing 1–2 tests.
Total ~1500–2500 LOC across parser, emitter, and tests.

The pyc-suite stays at ≥ 38/37 throughout — no changes to
production code paths until Phase 5's full migration.

---

## Open items surfaced during Phase 3

1. **Symbol vs `%name` ambiguity**: `%fact_ref` in test 12 is
   bound to the function `%fact` by name. The grammar should
   make this explicit, perhaps via `:target %fact` on the
   value declaration. Phase 4 implementation will resolve.

2. **Phi placement under alloca-store**: pyc's convention is
   to use alloca + load/store rather than LLVM `phi`
   instructions (Test 06's expected IR shows this). The
   emitter chooses; the IR's `phi_by_pred` is abstract.

3. **Inst id `%i0`** vs result value id `%v`: instructions
   have ids for diagnostic linking; result values have
   separate ids for cross-block reference. The grammar
   handles both; the parser may make these the same id in
   the common case (one-result inst).

4. **CG_PRIM `:name`** field — does it reference by string
   (today) or by id? String is more readable; the parser
   resolves at parse time.

These don't block Phase 4; they're notes for Phase 5's
formal grammar pass.
