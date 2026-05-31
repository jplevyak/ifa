# `.ir` Text Format Specification

A textual representation of IF1 designed for human authoring and
diff-friendly machine output. Uses **symbolic names** for every
reference so raw IDs never appear in tests.

Owns the input side of the testing pipeline. See
[TESTING.md](../TESTING.md) and [TEST_RUNNER.md](TEST_RUNNER.md) for
context.

---

## 1. Design goals

- **Hand-writable.** A small test fits in 10-30 lines.
- **Lossless round-trip** for the subset of IF1 a test needs to
  describe. Not every IF1 feature must be expressible at first.
- **Symbolic, never numeric.** References are by name.
- **Single-pass parseable.** Forward references resolved with a
  symbol table; no need to backtrack.
- **Mirrors `if1_write` output.** A maintainer who has read
  [IR.md](../IR.md) §11 already knows the basic shape.

---

## 2. Lexical structure

S-expression. Whitespace is insignificant outside string literals.
Comments start with `;` and run to end of line.

```
file        := decl*
decl        := '(' head item* ')'
item        := atom | decl | string
atom        := ref | keyword | numeric | bareword
ref         := '%' ident   ; user reference  (e.g. %add)
              | '@' ident   ; builtin       (e.g. @int32)
              | '#' ident   ; symbol Sym    (e.g. #foo)
keyword     := ':' ident   ; field name     (e.g. :type)
string      := '"' chars '"'
numeric     := int | float
ident       := [A-Za-z_][A-Za-z0-9_]*
```

### 2.1 Reference namespaces

Three namespaces, distinguished by sigil:

| Sigil | Namespace | Resolves to |
|---|---|---|
| `%name` | user-defined | a `Sym *` created in this file, OR a `Var *` (per context) |
| `@name` | builtin | a Sym from `sym_*` (e.g. `@int32` → `sym_int32`) |
| `#name` | atom/symbol | a `Sym` with `is_symbol = 1` (e.g. `#foo` for selector dispatch) |

`%name` resolves bottom-up:
1. Local block scope (function body locals).
2. Module scope (top-level decls).
Forward references are allowed; the parser does a two-pass resolve.

Anonymous slots (e.g. compiler-generated temporaries) can be named
explicitly or left as `_` (auto-numbered `%t0`, `%t1`, ...). Two
underscores `__` is a different anonymous each time it appears.

### 2.2 Reserved bareword keywords

Used as enum-like values, not references. Listed per declaration kind
below.

```
RECORD SUM FUN REF TAGGED PRIMITIVE ALIAS UNKNOWN APPLICATION VARIABLE
IN INOUT OUT
true false
```

---

## 3. Top-level declarations

### 3.1 `(sym NAME ...attrs)`

Declares a `Sym` registered with IF1. Required.

```
(sym %x
  :type @int32
  :is_local true)
```

Attributes (kebab-case in source = `is_X` flag on Sym):

```
:name STRING          ; user name (defaults to NAME without %)
:type REF             ; Sym for the type
:in REF               ; containing scope sym
:is-local             ; flag (no value needed; presence = true)
:is-fun
:is-constant
:is-lvalue
:is-symbol
:is-pattern
:is-external
:is-this
:is-fake
:intent IN|INOUT|OUT
:nesting-depth N      ; integer (-1 for LOCALLY_NESTED)
:constant STRING      ; constant value as string
:immediate (...)      ; (kind value); see §3.2
:size N
:alignment N
```

### 3.2 Immediates

```
:immediate (int32 42)
:immediate (uint8 255)
:immediate (float64 3.14)
:immediate (bool true)
:immediate (string "hello")
:immediate (symbol "foo")
```

### 3.3 `(type NAME :kind K ...attrs)`

Convenience for a type Sym. Equivalent to `(sym NAME :type_kind K
:has [...] ...)`.

```
(type %Point
  :kind RECORD
  :has %x %y)              ; field Syms
```

Type kinds: `RECORD SUM FUN REF TAGGED PRIMITIVE ALIAS UNKNOWN
APPLICATION VARIABLE`. See [IR.md](../IR.md) §3.3.

### 3.4 `(fun NAME :args (...) :rets (...) :body BODY)`

Convenience for an `is_fun` Sym + its code body.

```
(fun %add
  :args (%a %b)
  :rets (%r)
  :body
    (send :prim :+ %a %b => %r)
    (reply %r))
```

The args become `Sym::has`; rets become `Sym::ret`. The body is a
sequence of code forms (§4). The parser ends by calling
`if1_closure` to register.

### 3.5 `(entry %name)`

Names the closure that's the program entry point. Resolves to
`if1->top` after `if1_finalize`.

```
(entry %__main__)
```

### 3.6 `(import @builtin-name as %local)`

Map a builtin Sym into a local alias. Useful when a test wants to
write `%add` for `prim_add`:

```
(import @prim_add as %add)
```

### 3.7 `(scope %name :in %parent ...decls)`

Optional scope block; declarations inside resolve `%name`s only
within the scope. Most tests won't need this — flat top-level is
fine.

---

## 4. Code forms (inside `:body`)

Each form is a separate line for diff readability.

### 4.1 Move

```
(move %src %dst)
```

### 4.2 Send

```
(send :prim %f %a1 %a2 => %r1 %r2)
(send :prim @sym_operator %lhs @sym_add %rhs => %result)
(send :keyword %f %a1 :kw %a2 => %r)   ; with keyword arg
```

`:prim` indicates this SEND should resolve to a `Prim *` via
`Primitives::find`. Without it, the SEND is a user-function call.

### 4.3 Control flow

```
(label %L1)
(goto %L1)
(if %cond %L_true %L_false)
```

Labels are declared by `(label %L)` and referenced by `%L`. The parser
allocates a fresh `Label *` per name.

### 4.4 Higher-level constructors

These are convenience wrappers around `if1_if` / `if1_loop` builders
([IR.md](../IR.md) §4.1). They emit the full label/goto skeleton.

```
(if-then-else %cond
  :then ((move %a %result))
  :else ((move %b %result)))

(loop :cond %cond_var
  :before ((move %0 %cond_var))
  :body ((send :prim @next %iter => %cond_var))
  :continue %cont_label
  :break %break_label)
```

Bodies are inline lists. Continue/break labels are optional names; if
omitted, anonymous labels are generated.

### 4.5 Group nodes

Usually emitted by builders. Direct authoring:

```
(seq
  (move %a %b)
  (move %b %c))

(sub ...)     ; uncommon; the default body is SUB
```

---

## 5. Full example

A complete `.ir` file. Test: a single-function add that returns
`a + b`.

```
;; tests/ir/finalize/simple_add.ir
;; Exercises: SEND resolves to prim_add at if1_finalize.

(import @int32 as %int)

(sym %a :type %int :is-local)
(sym %b :type %int :is-local)
(sym %r :type %int :is-local)
(sym %cont)
(sym %ret :type %int)

(fun %add
  :args (%a %b)
  :rets (%r)
  :body
    (send :prim @sym_operator %a @sym_add %b => %r)
    (send :prim @sym_primitive @sym_reply %cont %r))

(entry %add)
```

The parser:
1. Creates `Sym` objects for `%a`, `%b`, `%r`, `%cont`, `%ret`,
   `%add` (each via `if1_register_sym`).
2. Sets `%a->type = sym_int32`, etc.
3. Records `%add->is_fun = 1`, `%add->has = [%a, %b]`,
   `%add->ret = %ret`, `%add->cont = %cont`.
4. Builds a `Code` tree for the body via `if1_send` / `if1_move`.
5. Calls `if1_closure(if1, sym_add, code, 2, &args[0])` to register
   as a closure.
6. Sets `if1->top` from `(entry %add)`.

After parsing, the in-memory IF1 looks exactly as if a frontend had
built it.

---

## 6. Parser implementation

Location: `ifa/testing/parse_ir.{cc,h}` (new files).

Public API:

```c
// Parse a .ir file. On success, returns 0 and the in-memory IF1
// (singleton `if1`) is populated. Caller is responsible for
// ifa_init() before, and for running the IFA pipeline after.
//
// Errors print to stderr with file:line and return -1.
int parse_ir_file(cchar *filename);

// Same but from a buffer (for inline tests).
int parse_ir_string(cchar *source, cchar *fake_filename);

// Look up a symbolic name (for tests that need to reference syms
// after parsing). Returns NULL if not found.
Sym *parse_ir_lookup(cchar *name);
```

### 6.1 Parser approach

Hand-rolled recursive-descent. Reuse the `if1_*` builders from
[IR.md](../IR.md) §4.1 — the parser is a thin layer that translates
`.ir` forms into `if1_*` calls.

DParser was considered (already in the build) but:
- adds a build dependency to the test infra;
- s-expression grammar is trivial enough that ~300 lines of C++
  suffices;
- error messages are easier to control with a hand-rolled parser.

### 6.2 Symbol table

Single hash map `name → Sym *`. Two-pass:
1. **Declaration pass:** create empty Sym objects for every
   `(sym NAME ...)`, `(type NAME ...)`, `(fun NAME ...)`. Records
   in the name map.
2. **Resolution pass:** fill in fields (`type`, `has`, `body`, etc.)
   using the name map. Forward references resolve here.

Builtins (`@name`) are resolved by lookup in `if1->builtins`. Symbol
atoms (`#name`) are auto-created via `if1_make_symbol`.

### 6.3 Error handling

Errors are reported with `file:line:col: message`. Examples:

```
foo.ir:5:12: unknown symbol %unrl (did you mean %url?)
foo.ir:8:1: type RECORD requires :has
foo.ir:12:9: forward reference %later resolved to no declaration
```

No `fail()` — the parser sets a global error count and returns -1;
the caller decides what to do.

---

## 7. Writer implementation

Companion to the parser: produces `.ir` text from in-memory IF1.

Used for:
- Round-trip testing (parse → write → diff against input).
- Bootstrapping new tests from existing fixtures (`ifa-test
  --dump-ir <fixture>` extracts an `.ir`).
- Inter-phase checkpointing (the IF1 state at the boundary of a
  phase is itself a valid `.ir` file).

Location: `ifa/testing/write_ir.{cc,h}`.

Public API:

```c
// Write the entire IF1 state to fp as a .ir file.
void write_ir(FILE *fp, IF1 *p);

// Same but only include Syms reachable from `roots`.
// Useful for extracting a minimal repro.
void write_ir_subset(FILE *fp, IF1 *p, Vec<Sym *> &roots);
```

### 7.1 Name assignment

Every printable Sym needs a name. Order of preference:
1. `Sym::name` (user-given).
2. Pre-existing assignment from a parse (the writer's state map).
3. Auto-assigned `%t<n>` where `<n>` is the Sym's position in
   `IF1::allsyms` (stable across multiple runs of the same
   compilation).

For builtins, emit `@name`. For atoms (`is_symbol`), emit `#name`.

### 7.2 Order

Top-level decls are emitted in dependency order:
1. All `(import @... as %...)` lines.
2. Type-defining Syms first (`type_kind != Type_NONE`).
3. Sym declarations next (sorted by name).
4. Function decls last (sorted by name).
5. `(entry ...)` at end.

Within `:has`, members are emitted in declaration order
(`Sym::has` Vec order). Bodies are emitted in PNode order if
post-CFG, else in Code-tree DFS order.

---

## 8. Round-trip test

A bootstrapping test: `tests/ir/format/roundtrip.ir` is read,
written, re-read, and the second in-memory state is compared to the
first. Differences are bugs in either the parser or writer.

Implemented in `ifa/testing/roundtrip_test.cc`, registered with the
`UnitTest` framework so `ifa --test` exercises it.

---

## 9. What's NOT in v1

Defer to later versions to keep v1 small:

- **Pattern declarations.** Complex pattern syntax can wait —
  test functions that don't need pattern dispatch.
- **Generic types.** Same — most analysis tests are concrete.
- **PNode `creates` field.** Set only during cloning; not needed
  for hand-authored IR.
- **Display chains for nested closures.** Tests that need nesting
  use real nested syntax via `(fun %inner :in %outer ...)`; the
  display is computed at `Fun::Fun(Sym*)` time.
- **`MType` / dispatch tables.** Built from declared Funs; not
  authored.

The format is extensible — new keyword arguments can be added without
breaking existing files. Reserved sigils (`%`, `@`, `#`, `:`,
`$`, `!`) are kept for future use.

---

## 10. Open design questions

1. Should `:has` accept inline syms (e.g. `:has (sym %x :type @int))`
   to flatten test files, or always require pre-declared refs?
   *Tentative:* require pre-declared. Easier to reason about.
2. Should the format support multi-module tests (one `.ir` per
   module)? *Tentative:* not in v1. Real multi-module testing
   goes through the existing pyc suite.
3. Should the writer emit comments documenting the test's intent?
   *Tentative:* no — comments belong in the input file, not the
   written output.

Decisions should land in this file before the parser is coded.
