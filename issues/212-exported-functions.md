# 212 — exported functions: code outside the program calling in

**Status:** open. The IR, FA, DCE and both backends landed 2026-11-18, with
IF1 fixtures; the first user is pyc's `CFUNCTYPE` callbacks
([issues/188](../../issues/188-static-ctypes-subset.md) item 1, Part B), not
yet written.

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
    pass. C hands back whichever instance the program gave it, and the
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
  demand nobody can satisfy, and codegen does not pick one. Also fails if
  the clone can raise (`Fun::can_raise`): an exception cannot cross into C,
  so the frontend that exports a function must catch everything in it.
- **The entry point, both backends.** The clone keeps its own name and its
  live-argument signature. Beside it, `<name>(<every declared argument>)`
  forwards to the clone's live formals, each converted to the clone's type:
  - C: `write_c_export` (`codegen/cg.cc`), `extern "C"` under C++. It is
    written to its own buffer, which ifa/121's reference scan treats as a
    root, so the clone's body is kept.
  - LLVM: `emit_llvm_exports` (`codegen/llvm.cc`). The names stay external
    when the module is internalized for LTO, as `main` does.

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
