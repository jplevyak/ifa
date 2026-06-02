# Phase 09 — Phase A.2: `ast_to_if1.cc` lowering patterns

Annotated index of the IF1-construction shapes V's lowering
demonstrates. Reference for the IR generator's API design (Phase B).

Source: `ifa/frontend/ast_to_if1.cc` (1,876 LOC).

## API surface used

`if1_*` builder calls (all from `ifa/if1/if1.{h,cc}`):

| API | Purpose | Used in |
|-----|---------|---------|
| `if1_register_sym(p, s, name)` | add Sym to `allsyms`, set name | every Sym creation |
| `if1_make_symbol(p, name)` | get-or-create symbol Sym | constants, dispatch tags |
| `if1_get_builtin(p, name)` | lookup pre-registered builtin | sym_primitive, sym_reply, etc. |
| `if1_set_builtin(p, s, name)` | mark a Sym as builtin | initialization |
| `if1_const(p, type, str, imm, asym)` | constant Sym | literals |
| `if1_cannonicalize_string(p, s)` | intern a string | sym names |
| `if1_set_primitive_types(p, ...)` | hook primitive type metadata | one-time init |
| `if1_alloc_label(p)` | new Label | jump targets |
| `if1_label(p, &code, ast, lbl)` | emit label into code stream | loop heads, if-joins, fun entry |
| `if1_goto(p, &code, lbl)` | emit unconditional branch | loop back-edge, if-tail |
| `if1_if(p, &code, c_code, c_val, then_code, then_val, else_code, else_val, res, ast)` | emit if/else with optional value | if statements & expressions |
| `if1_loop(p, &code, top_lbl, end_lbl, cond_val, before, cond_code, after, body, ast)` | emit while/for-shaped loop | for, while |
| `if1_move(p, &code, src, dst, ast)` | emit MOVE | assignments, phi pseudo-moves |
| `if1_send(p, &code, nrvals, nlvals, …)` | emit SEND with positional rvals + lvals | method calls, primitive ops, reply |
| `if1_add_send_arg(p, send, sym)` | append rval to existing SEND | variadic builds |
| `if1_add_send_result(p, send, sym)` | append lval to existing SEND | tuple destructure |
| `if1_gen(p, &code, sub_code)` | splice a Code subtree into current | expression code merging |
| `if1_closure(p, fn, body, n_args, args[])` | finalize a Sym as a closure with body and formals | every fun definition |

Constructors used directly (without builder helpers):
- `new Sym` — when V wants to bypass the callback (rare; just twice).
- `new Label` — through `if1_alloc_label` only.

## Lowering patterns observed

Cataloged by AST shape → IF1 emission. The "API recipe" column is
the sequence of calls a generator would replicate.

### Function definition (`gen_fun`, line 1122)

V source:
```
f a : ...body...
```

Recipe:
```
body = NULL
if1_gen(if1, &body, expr->code)         # splice body code
if1_move(if1, &body, expr->rval, fn->ret, ast)  # set return val
if1_label(if1, &body, ast, fn_label)    # exit label
if1_send(if1, &body, 4, 0, sym_primitive, sym_reply, fn->cont, fn->ret)
# build args array: dispatch sym + receiver + formals
args[0] = if1_make_symbol(if1, fn_name)
args[1] = self_sym               # if method
args[2..] = formal_syms
if1_closure(if1, fn, body, n_args, args)
```

**Key insight for generator API**: `if1_closure` takes a body Code
tree + args. The body is built incrementally via `if1_gen` /
`if1_move` / `if1_send`. The reply at the end is a SEND, not a
return statement.

### Control flow: loop (`gen_loop`, line 1289)

Recipe:
```
if1_loop(if1, &code,
         top_label, end_label,
         cond_rval,
         before_code,      # optional init expression
         cond_code,        # cond evaluation
         NULL,             # after-iteration (V's for syntax)
         body_code,
         ast)
```

Single API call for the whole loop — `if1_loop` internally emits
the label/goto/if dance. Generator can use this directly.

### Control flow: if (`gen_if`, line 1299)

Recipe:
```
result_sym = new_sym(...)            # only if used as expression
if1_if(if1, &code,
       cond_code, cond_val,
       then_code, then_val,
       else_code, else_val,           # optional
       is_expr ? result_sym : NULL,
       ast)
```

Also a single-call helper.

### Type definition (`gen_type`, line 1536)

V source: `Point = record (x: int, y: int)`.

Recipe (per type):
```
sym->type_kind = Type_RECORD
sym->has = [field_syms...]    # set by earlier passes
# Generate the __init function:
fn = sym->init  # pre-created
for inherited in sym->includes:
  tself = new_sym; tself->aspect = inherited
  if1_move(self, tself)
  if1_send(2, 1, inherited.init, tself, rval)
# generate field initializers
if1_gen(if1, &body, init_record_code)
if1_label / reply / if1_closure
```

**Generator simplification**: when building synthetic types, the
inheritance and init-fn generation can be skipped — the generator
constructs the type Sym directly and provides a minimal init.

### Method call (in `gen_apply_op` / `gen_op`, line 1221+)

V source: `obj.method(arg1, arg2)`.

Recipe:
```
# Evaluate args; results in rvals
if1_send(if1, &code, n_args + 2, 1,    # nrvals = method_sym + receiver + args
         method_sym, obj, arg1, arg2, ...,
         result)
```

So a method send is `(send method_sym receiver args... => result)`.
The first rval is the dispatched-on symbol (= method name as a
symbol Sym).

### Primitive operator (also `gen_op`)

V source: `a + b`.

Recipe:
```
if1_send(if1, &code, 4, 1,
         sym_operator,         # dispatch tag
         a, sym_plus, b,        # the op symbol between operands
         result)
```

For unary: 3 rvals: `sym_operator, sym_op, operand`.

### Constructor call (`gen_constructor`, line 1309)

V source: `Point(3, 4)`.

Recipe:
```
# Args evaluated first, then:
if1_send(n_args + 2, 1,
         sym_primitive, sym_new,    # primitive: allocate of type
         type_sym, arg1, arg2, ...,  # actually goes through type's __init
         result)
```

For a generator constructing instances, the simpler form is just:
```
if1_send(3, 1, sym_primitive, sym_new, type_sym, result)
# Then per-field assignment:
if1_send(4, 1, sym_operator, result, sym_setter, sym_field_name, value, result_unused)
```

### Field access (`gen_op` with `.` op)

Recipe:
```
if1_send(4, 1,
         sym_operator, obj, sym_period, sym_field_name,
         result)
```

### Field assignment

Recipe:
```
if1_send(5, 1,
         sym_operator, obj, sym_setter, sym_field_name, value,
         result_unused)
```

### Top-level module body (`build_init`, line 1728)

Recipe:
```
sym___main__->scope = new Scope(...)
body = NULL
# For each module being initialized:
if1_send(if1, &body, 1, 1, module->init, rval)
sym___main__->cont = new_sym(...)
sym___main__->ret = sym_void
if1_send(if1, &body, 4, 0, sym_primitive, sym_reply, cont, ret)
if1_closure(if1, sym___main__, body, 1, &main_sym)
```

This is the pattern the test harness's `fa_setup.cc` already
replicates for synthetic main.

## What V's lowering does that the generator doesn't need

V's `ast_to_if1.cc` has 1,876 LOC, but a huge chunk is V-specific
plumbing the generator skips entirely:

| V machinery | LOC est. | Why generator doesn't need it |
|-------------|----------|-------------------------------|
| Scope resolution (`Scope`, `get_local`, `add_dynamic`) | ~200 | Generator names everything explicitly |
| Identifier resolution (`gen_def_ident`, AST_qualified_ident) | ~150 | Generator passes Sym pointers directly |
| Constant folding (`fold_constant`) | ~100 | Generator emits final values directly |
| Pattern definition (`scope_pattern`, pattern walks) | ~150 | Generator constructs patterns directly via `if1_set_pattern` (if needed) |
| Multi-pass scope building (top-down + bottom-up) | ~300 | Generator builds in one pass |
| AST node housekeeping (ast_print, html, graph) | ~200 | Generator has no AST |
| Constraint resolution (`scope_inherits`, `scope_constraints`) | ~150 | Generator declares constraints directly |
| Module / file management | ~100 | Generator works on one in-memory IF1 |

Conservative estimate: **~500 LOC is the actually-reusable
"IF1-construction patterns" subset** out of 1,876. The rest is V's
parser/scope/AST infrastructure that the generator skips.

## Inferred generator API layers (preview for Phase B)

Reading the patterns above, three layers naturally emerge:

### Layer 1 — Thin wrappers around `if1_*`

Maybe nothing to wrap. The `if1_*` APIs are already the right
abstraction level for "emit a single IF1 thing." The generator
calls them directly.

### Layer 2 — Code-builder helpers

Small composites that V's `gen_*` functions effectively are:

```cpp
struct CodeBuilder {
  Code *code = nullptr;
  void send_method(Sym *method, Sym *recv, Vec<Sym*> args, Sym *result);
  void send_primitive(Sym *prim_sym, Vec<Sym*> args, Vec<Sym*> results);
  void send_reply(Sym *cont, Sym *ret);
  void move(Sym *src, Sym *dst);
  void label(Label *l);
  void goto_(Label *l);
  void if_(Sym *cond, std::function<void(CodeBuilder&)> then,
                       std::function<void(CodeBuilder&)> els);
  void loop(Sym *cond, std::function<void(CodeBuilder&)> body);
};
```

These map roughly 1:1 onto V's `gen_op`, `gen_if`, `gen_loop`
function shapes.

### Layer 3 — Whole-program shape generators (the synthetic fixtures)

```cpp
namespace IRShape {
  // Builds a complete IF1 program (closures, types, main).
  void polymorphic_container(int n_types, int n_callsites);
  void cascading_dispatch(int depth);
  // ...
}
```

Each `IRShape::*` uses Layer 2 to construct closures, builds a
main that calls them in the right pattern to trigger the target
splitter stage, and leaves the IF1 ready for `ifa_analyze`.

## Recommendations for Phase B

1. **Build Layer 2 first.** Mirrors V's `gen_*` functions but
   without the AST/scope baggage. Probably 200-300 LOC. This is
   the bottom-up portion the user suggested.
2. **Build Layer 3 incrementally.** One shape at a time, each
   justified by which splitter stage it triggers (Phase A.3's
   output).
3. **Use V's `gen_*` functions as templates.** When building a
   shape that involves field-access patterns, look at how
   `gen_op` does it. When building closures, look at `gen_fun`.
   The shapes don't change just because the source language is gone.
4. **Keep a parallel `ast_to_if1.cc` reference open during
   construction.** Phase 09 plan already says delete in group 3
   only after generator parity — this catalog supports that.
5. **The shared infrastructure (`if1_*`, `Sym`, `Code`, `Label`,
   `Fun`) is already battle-tested.** Layer 1 doesn't need new
   abstractions; Layer 2 is mostly ergonomic conveniences.

Next: Phase A.3 — catalog splitter-stage trigger preconditions, so
Layer 3's shape list has concrete coverage targets.
