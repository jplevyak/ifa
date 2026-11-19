# 212 — exported functions: code outside the program calling in

**Status:** open. The IR, FA, DCE and both backends landed 2026-11-18, with
IF1 fixtures. The first user, pyc's `CFUNCTYPE` callbacks
([issues/188](../../issues/188-static-ctypes-subset.md) item 1, Part B),
landed the same day and found the four defects listed under "Fixed by the
first user".

## What it is

An exported function is one that code outside the program calls under a
fixed C name: a callback handed to a C library, a library entry point, a JIT
lookup by name. The frontend says so on the function's `Sym`:

```
Sym::export_name   the C name
Sym::export_args   one declared type per formal after the closure
Sym::export_ret    the declared result type (null: none)
```

A declared type is ifa's own: a number type (`int64`, `float64`, ...) is that
type, any other type is an object, an instance of that class or a subclass,
which reaches C as an opaque pointer. Nothing here knows about Python; the
IF1 text format spells it

```
(fun %cb
  :args (%cb %x %y)
  :export "cb_add"
  :export-args (@int64 @int64)
  :export-ret @int64
  ...)
```

## How it works

- **FA: one root per exported function** (`make_export_edge`,
  `refresh_export_edge` in `analysis/fa.cc`), beside the top edge. The edge
  passes the closure itself and one actual per declared type:
  - a number type: the type's abstract value, "some `int64`", the fact a
    value C returns gives;
  - an object: every CreationSet of the class or a subclass. `Sym::creators`
    grows during a pass, so the formal is fed where each one is made
    (`feed_export_formals`, called at the end of `creation_point`), on every
    pass. Not the class's prototype (`Sym::self`, its class-attribute
    holder), which is a CreationSet of the class but not an instance: fed
    to pyc's first callback trampoline, it made `cb.fn` a read of a field the
    prototype does not have. C hands back whichever instance the program gave it, and the
    analysis does not claim to know which: this keys on TYPE, so it is not
    provenance.
  - Re-seeded every pass with the top edge, since every pass re-derives from
    bottom. Every walk that starts at the top edge (backedge marking, the
    live-contour walk, the edge worklist) starts from every root
    (`root_entry_sets`).
- **DCE** (`optimize/dead.cc`): an exported function is a root of
  `mark_live_funs`, and its result is live (the outside caller reads it).
  That marking runs after every function's reset: a later function's reset
  clears the instance variables of the types it holds, which undid a field
  marked live (14_export_object's `bv`, found while writing it).
- **One clone.** C calls one entry point. `cg_exported_funs`
  (`codegen/codegen_common.cc`) returns the one live clone of each exported
  function, and fails naming the function if there are two: FA split it on a
  demand nobody can satisfy, and codegen does not pick one.
- **Exceptions are the frontend's.** An exception cannot cross into C, so
  the frontend that exports a function catches everything in it (pyc's
  callback trampoline does). ifa does not check it: `Fun::can_raise` means
  "reaches a raise", handled or not, so it cannot say whether one escapes.
  A check on it refused pyc's first trampoline, whose `try` catches
  everything; it was removed.
- **The entry point, both backends.** The clone keeps its own name and its
  live-argument signature. Beside it, `<name>(<every declared argument>)`
  forwards to the clone's live formals, each converted to the clone's type:
  - C: `write_c_export` (`codegen/cg.cc`), `extern "C"` under C++. It is
    written to its own buffer, which ifa/121's reference scan treats as a
    root, so the clone's body is kept.
  - LLVM: `emit_llvm_exports` (`codegen/llvm.cc`). The names stay external
    when the module is internalized for LTO, as `main` does.

## Fixed by the first user (pyc callbacks, 2026-11-18)

- **The display.** pyc gives a module-level function nesting depth 1, so its
  contour needs a display, which `update_display` built from the edge's
  sending contour. A root edge has none: it crashed. A root edge now takes
  the top's contour, and an exported function nested deeper is an error
  (its C caller cannot supply the enclosing variables).
- **The prototype.** A class's prototype (`Sym::self`) is a CreationSet of
  the class: it reached the object formal and made `cb.fn` a read of a
  field the prototype does not have. Excluded at the feed.
- **The empty first argument.** `analyze_edge` skips an edge whose argument
  is empty, as the object formal is until the first instance is made, and
  nothing retried it: the exported function was never analyzed. It worked
  in the fixtures only because the actuals were GLOBAL AVars, which keep
  the previous pass's types (and so kept a CreationSet a split had emptied).
  The actuals are now contoured on the root's EntrySet (reset every pass),
  and feeding an object formal re-queues its root edge.
- **The exception check**, which refused every trampoline (below).

## Tests

IF1 fixtures, `ifa/tests/ir/`:

- `codegen-c/13_export.ir` (and `codegen-llvm/`): number formals, one of
  them dead. The clone takes `a1` only; `cb_add` takes both and forwards one.
- `codegen-c/14_export_object.ir` (and `codegen-llvm/`): an object formal.
  The instance `%top` makes reaches it, so the field read is typed `int64`,
  and `get_bv` takes a `void *`.
- `clone/13_export.ir`, `clone/14_export_object.ir`: one EntrySet each.
- `testing/roundtrip_test.cc`: the text format writes and re-reads `:export`.

A program that exports nothing takes none of these paths. `make test`
leaves every golden unchanged, and the corpus `check` sweep
`check__default__77ec9889+ca9a81bc` is row-for-row identical to
`check__default__4c3220b7+ce9d4a40` (the last pre-212 compiler change): the
same compile, warning, run and stdout verdicts and the same EntrySet and
CreationSet counts on all 77 programs. (`othello2`'s stdout differs in
both.)

## Not done

- **No fixture for the two-clone error.** It needs an exported function that
  the program also calls with other types, which demands a split. Whether
  that should be an error or a reason to emit a second, internal clone is
  open until a frontend wants it.
- **The C side of the ABI is the C compiler's.** A declared `int64` is
  `_CG_int64`; the frontend's shim narrows to the real C types (pyc's ctypes
  shim does that already for foreign calls).
- **The JIT** does not look exported names up yet (pyc's `--jit` refuses a
  program with foreign calls, issues/188 item 6).
- **A library build** (`.so` with exported entry points and no `main`) is
  the other use, and needs only a frontend spelling and a link mode.
