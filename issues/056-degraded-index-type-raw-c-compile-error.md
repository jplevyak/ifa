# 056 — A salvage-degraded (non-integer) index Var produces a raw C compile error instead of a runtime-error guard

**Status:** open, found 2026-07-19 while digging into the shedskin
corpus's `RUN_FAIL` bucket (triage requested as a followup to
[053](closed/053-tuple-unpack-target-heterogeneous-arity-segfault.md)/[055](055-set-dunder-method-triggers-fa-nonconvergence-on-plcfrs.md)).
Not fixed — filed rather than fixed because the fix site
(`P_prim_index_object`/`P_prim_set_index_object` in
`ifa/codegen/cg.cc`) is a hot, heavily-shared codegen path for *all*
list/string/tuple indexing across the whole corpus, and a correct fix
needs to touch both the get and set emission (and check whether the
LLVM backend's `emit_send_index_load`/`emit_send_index_store`,
`ifa/codegen/cg_emit_llvm.cc`, has the same gap) with full regression
verification — more than this triage pass had budget for.
**Affects:** `ifa/codegen/cg.cc`'s `P_prim_index_object` (~line 471)
and `P_prim_set_index_object` (~line 559) — the single-index,
non-constant, non-record branch that emits `_CG_norm_idx(%s, ...)`
for the index argument.

## Symptom

Found via `shedskin_examples/loop/loop.py` (specifically exposed by
[the same-day dict.items()/.keys()/.values() fix](../../issues/025-shedskin-examples-coverage.md)
progressing `loop.py` further than before — not caused by that fix,
just newly visible once `loop.py`'s earlier blocker, at line 250,
stopped masking it). Once FA salvages some earlier violation to
`void`/`_CG_any` (`runtime_errors` default), that degraded value can
flow into an index-argument position feeding `_CG_norm_idx`, which
has a concrete `int32` C parameter. Codegen emits the index Var
verbatim (`cg_get_string(n->rvals[i])`) without checking its resolved
C type first, producing invalid C:

```
loop.py.c:6696:36: error: no matching function for call to '_CG_norm_idx'
 6696 |   ((_CG_int64*)(_CG_list_ptr(t0)))[_CG_norm_idx(t1,(int32)_CG_prim_len(0,t0))-0] = t2;
      |                                    ^~~~~~~~~~~~
pyc_c_runtime.h:858:14: note: candidate function not viable: cannot
convert argument of incomplete type '_CG_any' (aka 'void *') to
'int32' (aka 'int') for 1st argument
```

This is a genuine `pyc`-produced C compile error (not a runtime
crash) — the compiler considers the program to type (with salvage),
writes `.c`, and only `clang` on the generated code catches the
mismatch. That's the same *class* of bug already found and fixed
twice today in the sibling constant-field-getter path
([053](closed/053-tuple-unpack-target-heterogeneous-arity-segfault.md)'s
`resolve_uniform_size` + destination-cast fix, and the earlier
"minpng's and plcfrs's C-compile-error bugs" entry in
[../../issues/025](../../issues/025-shedskin-examples-coverage.md))
— established convention in this codebase is that every
salvage-reachable codegen site should degrade to a runtime
`assert(!"runtime error: ...")` guard (matching `P_prim_period`'s
`"getter not resolved"`, the constant-field-getter's `"bad getter"`,
`P_prim_call`'s `"matching function not found"`, etc.), never a raw,
unsalvageable C compile error. This specific site was missed when
`_CG_norm_idx` was introduced (issues/025, negative-index
normalization, earlier today) — that fix assumed the index argument
would always resolve to an integer type and didn't add the same
"is this actually usable" guard the constant-index branch already
has (`if (fruntime_errors && t->type_kind == Type_RECORD &&
!t->has.n)`).

## What a fix would look like

Before emitting the `_CG_norm_idx(...)`-wrapped index expression,
check the index Var's resolved type (`n->rvals[i]->type`) is a real
integer primitive (mirroring the `== sym_int64` check pattern already
used elsewhere in `cg.cc`, e.g. line 99). If not, emit
`assert(!"runtime error: index has no type");` (or reuse an existing
wording) instead of the malformed subscript expression — likely
needs restructuring the emission to decide *before* starting to write
the enclosing `%s = ((elem_type*)...)[...]` statement, similar to how
the constant-index `Type_RECORD` branch already special-cases
`!t->has.n` up front rather than partway through. Must cover both
`P_prim_index_object` (get) and `P_prim_set_index_object` (set), and
should be checked against the LLVM backend's independent emission for
parity (recent history in this codebase — the tuple-list-header bug,
053 — shows the two backends drift on exactly this kind of guard if
not deliberately kept in sync).

## Repro

`shedskin_examples/loop/loop.py`, compiled with dict.items()/.keys()/
.values() support present (see
[../../issues/025](../../issues/025-shedskin-examples-coverage.md)'s
2026-07-19 entry) — `./pyc -D . loop.py` produces `loop.py.c` with the
compile error above at (approximately) line 6696. A minimal synthetic
repro wasn't isolated in this triage pass; whoever picks this up
should be able to construct one directly from the fix description
above (any list `__setitem__`/`__getitem__` call site whose index
expression is FA-salvaged to void/any).

## What this unblocks

`loop.py` itself (currently `PYC_FAIL`) plus, more broadly, any
corpus program that reaches this exact salvage shape at an index
site — likely more than just this one example, since the underlying
cause (an unguarded primitive-type assumption in `_CG_norm_idx`'s
call sites) is structural, not `loop.py`-specific.
