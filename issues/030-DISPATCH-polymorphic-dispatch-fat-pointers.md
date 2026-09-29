# 030 — polymorphic dispatch is an if/else chain on the classtag; high fan-out has no table

**Status:** open, performance only. Classtag dispatch has been correct on
both backends since 2026-07-04, and closure-carrier mixing since
2026-08-06. Rewritten 2026-09-28; the mechanism write-up and root causes
are in git:
`git show 3f36072b:ifa/issues/030-DISPATCH-polymorphic-dispatch-fat-pointers.md`.

## How dispatch works (for orientation)

Every class-like record has a `__pyc_tag` at offset 0, pointing at its
`_CG_type_<name>`. Tuples, list-backed tuples and vectors are
deliberately untagged: their layouts are bare element arrays. At a call
site with several candidate `Fun`s, codegen emits: a nil test (if nil is
in the union), then one tag compare per class, each casting to that class
and calling its stored method slot (or calling a closure carrier's
implementation directly), then `else assert(!"…no branch matched")`. The
shared resolver is `poly_dispatch_classtag_targets` /
`poly_dispatch_directly_owned` / `poly_dispatch_is_nil_receiver`
(`codegen_common.cc`). Both backends call it and must not reimplement it.

Two backend divergences are known, with no failing repro: the
classtag-eligibility filter (`cg.cc` vs `cg_emit_llvm.cc`), and unnamed
(lambda) candidates, where LLVM bails the whole call site.

## What is open

**Table dispatch for high fan-out.** Every polymorphic site is an
O(branches) chain. It is correct up to 11-way
(`tests/poly_dispatch_shared_method_extra_args.py`). If profiling ever
shows it matters, index a per-class method table by the tag that already
exists (a fat pointer is not needed). After ifa/123's slot elision, only
genuinely polymorphic sites still dispatch (closed/126), so the win is
bounded.

**Deferred, with a safe degrade:** two distinct closure-carrier syms at
one site collide on the reserved name `__closure__`, and fall to the
runtime assert rather than mis-dispatching.

**Not this issue:** a union member with no implementation at all
([079](079-DISPATCH-single-candidate-dispatch-unchecked-cast.md)), and a
`{list, tuple}` union with no tag
([102](102-corpus-programs-compile-then-abort-at-runtime.md),
`tests/list_tuple_union_method.py`).
