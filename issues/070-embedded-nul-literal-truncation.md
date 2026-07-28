# 070 — compile-time str/bytes literals containing an embedded NUL are silently truncated

**Status:** fixed 2026-07-27, verification passed (both backends'
`test_pyc.py`, `shedskin_sweep.sh` no-regression, and the direct
repros below all match `python3` reference output). Pending commit;
move to `closed/` with the commit ref once committed. Found while
adding a `bytes` type to the pyc frontend (`__pyc__/01b_bytes.py`,
`sym_bytes` in `ifa/if1/ast.cc`) and testing it against realistic
binary-data literals. Pre-existing, predates that work — confirmed on
plain `str` too (the code path is identical), just never noticed
because ordinary Python source text rarely embeds a raw `\x00` in a
string literal the way binary-format `bytes` literals routinely do.

The fix landed in two passes. The first pass (stages 1-4 below) fixed
`len()`/content round-tripping but missed two further NUL-terminated
touch points, both caught by this doc's own verification plan (the
`b"a\x00b" != b"a\x00c"` equality check) and fixed in a second pass —
see "Two more touch points found during implementation" below.

## Symptom

```python
s = "Hello\x00World"
print(len(s))     # pyc: 5   CPython: 11
b = b"\x00\x01\x05\x06\x07\x08"
print(len(b))     # pyc: 0   CPython: 6
```

Any compile-time literal (`"..."` or `b"..."`) whose decoded bytes
include a `\x00`/`\0` escape (or a raw embedded NUL) is truncated at
that byte — the rest of the literal is silently dropped. This is
**only** a compile-time-literal problem: runtime-produced data (a
file read via `open(f, 'rb').read()`, a `bytes([...])` built from a
list of ints, string concatenation at runtime) is unaffected — `str`
is already a length-prefixed buffer
(`_CG_string`/`_CG_string_len`, `pyc_c_runtime.h:301,368-377`), not
NUL-terminated for length purposes, so arbitrary bytes survive
storage/round-trip correctly once they're *in* that representation.
The bug is entirely in how a literal gets *into* that representation
in the first place.

## Root cause: four NUL-terminated-string touch points in the literal pipeline

A literal's journey from Python source text to a runtime value passes
through four stages; **every one** of them currently loses the true
length and falls back to `strlen()`/NUL-terminated iteration:

1. **Decode** — `decode_string_content` (`python_ifa_build_if1.cc:341`)
   decodes escape sequences into a heap buffer, correctly writing a
   real `\0` byte for a `\x00`/`\0` escape at the right position — but
   it returns only `char *`, discarding the length it already
   computed internally (`op - out` at the point it NUL-terminates and
   returns). Every caller downstream must rediscover "how long is
   this" via `strlen()`.
2. **Intern** — `make_string`/`make_bytes` (`python_ifa_build_syms.cc`)
   pass that bare `char *` into `if1_const` (`ifa/if1/if1.cc:42`),
   which interns it via the **single-argument** overload of
   `if1_cannonicalize_string(p, constant)`
   (`ifa/if1/if1.h:72`/`if1.cc:659`). That single-arg overload exists
   purely as a convenience wrapper —
   `StringChainHash::canonicalize(cchar *s) { return canonicalize(s, s
   + strlen(s)); }` (`ifa/common/map.h:182`) — around an
   **already-length-aware** two-argument
   `canonicalize(cchar *s, cchar *e)`
   (`ifa/common/map.h:181`/`:678`). The infrastructure to intern by
   explicit length already exists; nothing in the current call chain
   uses it — a full-repo grep found zero call sites passing the `e`
   argument.
3. **Escape into generated C source** — `escape_string`
   (`ifa/common/misc.cc:129`) walks the constant with `for (; *s; s++)`
   to produce the escaped C string-literal text emitted into the
   `.c`/`.cc` output (`cg.cc`'s constant-materialization sites,
   `cg.cc:204`/`:1999`). Even if stages 1-2 above preserved the true
   length internally, this function stops at the first NUL when
   producing the *text* of the literal, emitting `"Hello"` instead of
   `"Hello\x00World"`.
4. **Materialize at program startup** — `_CG_String(const void *x)`
   (`pyc_c_runtime.h:383`) computes `len = strlen((char*)x)` on the
   compiled-in C string literal to build the runtime length-prefixed
   buffer. **This stage cannot be fixed by fixing 1-3 alone**: even a
   perfectly-escaped C source literal `"Hello\x00World"` is, at the C
   language level, still just a NUL-terminated `const char*` once
   compiled into the binary's static data — `strlen()` on it can only
   ever see "Hello" (5 bytes), regardless of what follows the escaped
   NUL in the source text. Only an explicit length passed alongside
   the buffer (an integer argument, or an array-initializer emission
   instead of a string literal) can carry the true extent through this
   final stage.

Because of stage 4, **fixing stages 1-3 is necessary but not
sufficient** — the codegen emission itself must change to pass an
explicit length, and the runtime needs a length-taking materialization
function.

One consumer that does *not* need to change: `ImmHashFns::hash`/`equal`
(`ifa/if1/num.h:245-252`) hash/compare the raw bytes of the `Immediate`
struct itself, which stores `v_string` as a single interned *pointer*
(`ifa/if1/num.h:53`) — not the string content. Correctness here is
entirely inherited from stage 2's interning being correct; once two
literals with identical true content intern to the same pointer (and
two with different content that merely share a truncated prefix do
NOT), the existing hash/equal is fine as-is. **This turned out to be
true only once stage 2's interning was ACTUALLY correct** — see
touch point 6 below, where it wasn't.

## Two more touch points found during implementation

Fixing stages 1-4 made `len()` and content printing correct, but
running this doc's own verification-plan equality check
(`b"a\x00b" != b"a\x00c"`) still failed: pyc printed `True` for
`b1 == b2` where CPython prints `False`. Two further NUL-terminated
touch points were responsible, neither anticipated by the original
four-stage analysis:

5. **Runtime equality/ordering/hash** — `_CG_str_eq`/`_CG_str_ne`/
   `_CG_str_lt`/`_CG_str_le`/`_CG_str_gt`/`_CG_str_ge`/`_CG_str_hash`
   (`pyc_c_runtime.h`, ~line 1233) used `strcmp()`/a `for (; *s; s++)`
   scan — NUL-terminated, despite `str`/`bytes` being length-prefixed
   buffers with the true length available via `_CG_string_len`
   (already used correctly by `_CG_strcat`/`_CG_string_mult`/
   `_CG_string_getslice` elsewhere in the same file). Fixed by
   rewriting all of these in terms of `_CG_string_len` +
   `memcmp`/explicit-length iteration (`_CG_str_cmp` helper added for
   the four ordering ops).
6. **Constant interning's own dedup fallback** —
   `StringChainHash<F,A>::canonicalize(s,e)`
   (`ifa/common/map.h:677`) *looks* length-aware (its initial bucket
   lookup hashes/scans the explicit `[s,e)` range), but when that
   lookup doesn't find a match it falls through to
   `ChainHash<cchar*,F,A>::put(s)` (`map.h:475`), which — for the
   default `F = StringHashFns` (`map.h:122`) — hashes with
   `while (*s) ...` and compares with `strcmp()`. Two *different*
   buffers that share a prefix up to an embedded NUL (`"a\x00b"` and
   `"a\x00c"`, both decoded/dupstr'd with a real trailing terminator
   at their true end) hash equal and `strcmp`-equal there, so `put()`
   silently returns the *other* literal's pointer — `if1_const` for
   `b"a\x00c"` ends up with `imm->v_string` pointing at `b"a\x00b"`'s
   buffer. This is the same NUL-terminated-comparison bug pattern as
   touch point 5, just in the shared string-interning table rather
   than the runtime. Confirmed via the generated C: before the fix,
   `nuleq.py`'s `b1`/`b2`/`s1`/`s2` all emitted the *identical*
   (and wrong on two counts) `_CG_String_n("a\x00\x00", 3)`.

   `StringChainHash` is also used for many *other* purposes across
   the compiler (symbol names, etc.) that never carry an embedded
   NUL, so `StringHashFns`/`ChainHash` themselves were left
   unchanged — full-table length tracking (storing a length
   alongside every interned entry) would be the "real" general fix
   but is a much larger structural change than this bug warrants.
   Instead, `canonicalize` now special-cases the embedded-NUL case:
   if `[s,e)` contains a `\0` before `e`, skip both the (already
   ineffective — the stored-string's own embedded NUL similarly
   defeats the lookup scan for genuine duplicates too) lookup and the
   unsafe `ChainHash::put` fallback, and just return a fresh
   `_dupstr<A>(s, e)` copy directly. This sacrifices deduping for
   literals with embedded NULs (each occurrence gets its own buffer)
   but is never wrong — correctness at the interning layer does not
   depend on pointer-uniqueness (the constants cache is a separate
   layer above this, keyed on `Immediate` content), only on distinct
   values never sharing a pointer, which this guarantees.

Also note: a `make clean && make` was required in `ifa/` to pick up
the `map.h` fix — its `.d` dependency file (`ifa/if1/if1.d`) predates
several of `if1.h`'s current includes and doesn't list `map.h` (or
even `num.h`) at all, so `make`'s header-change detection silently
no-ops on a touch of `map.h` alone. Same gotcha already recorded from
the Phase 8 CPython-removal work — see project memory.

## What this unblocks

Any corpus/user program with a compile-time literal (`str` or `bytes`)
that embeds a real NUL byte — most concretely, `bytes` literals for
binary file formats, which routinely use the full 0-255 byte range and
have no reason to avoid `\x00`. Not currently known to block any
specific shedskin corpus example (the ones found during the `bytes`
work all avoided literal embedded NULs once noticed), but it is a
correctness trap for exactly the kind of code a real `bytes` type
exists to support, and silently produces a *wrong, shorter* value
rather than an error — the worst failure mode.

## Proposed fix

Thread an explicit length alongside the buffer through all four
stages, then use it instead of `strlen()` at each:

1. `decode_string_content`: add an `int *out_len` out-parameter (or
   return a small `{char*, int}` pair) capturing `op - out` before the
   final NUL write. Update its two callers
   (`eval_string_pyda`/`make_bytes_pyda`, both in
   `python_ifa_build_if1.cc`).
2. `make_string`/`make_bytes` (`python_ifa_build_syms.cc`): accept an
   explicit length, build the `Immediate` (`imm.v_string = s`) same as
   today, but call `if1_const` with enough information to reach the
   two-arg intern path — simplest is to have `if1_const`
   (`ifa/if1/if1.cc:42`) itself accept an optional explicit length and
   call `if1_cannonicalize_string(p, constant, constant + len)` when
   given one, falling back to the current `strlen`-based single-arg
   call otherwise (keeps every other `if1_const` caller — numbers,
   symbols, non-literal callers — unchanged).
3. `escape_string`: add a length-aware overload (`escape_string(cchar
   *s, int len)`) iterating `for (int i = 0; i < len; i++)` instead of
   `for (; *s; s++)`; keep the existing NUL-terminated version for
   callers that don't have (or need) an explicit length. Update
   `cg.cc`'s two constant-materialization call sites
   (`cg.cc:204`/`:1999`, the same two sites touched for
   `IF1_CONST_KIND_BYTES` support) to use it, sourcing the length from
   the same place they already get `s->constant`.
4. `pyc_c_runtime.h`: add `_CG_String_n(const void *x, size_t len)`
   (drop-in sibling of `_CG_String`, `pyc_c_runtime.h:383`, minus the
   `strlen()` call) and switch `cg.cc`'s emission to call it with an
   explicit integer-literal length argument instead of `_CG_String`.
   Mirror in the LLVM backend's `materialize_pyc_string`
   (`ifa/codegen/cg_emit_llvm.cc`) — check whether it already carries
   a length (it takes `cs->imm.v_string`, a bare pointer, at the two
   call sites added for bytes support) or needs the same treatment.
5. Leave `ImmHashFns`, the `Immediate` struct layout, and
   `if1_cannonicalize_string`'s existing two-arg `canonicalize`
   untouched — per the Root cause section, they're either already
   correct or not on the truncation path once stages 1-4 carry a real
   length.

This is scoped to the *literal-constant* path only — no change to
`_CG_string`'s runtime representation, no change to `str`/`bytes`'s
buffer layout, and no change to any runtime-data code path (file I/O,
`bytes()`/`list()` construction, concatenation) since those don't go
through `escape_string`/`_CG_String` at all.

**Rejected alternative:** switching the compiler's internal string
storage (`Immediate::v_string`, decode/intern) to `std::string`
throughout. `std::string` naturally carries its own length, which
would fix stages 1-2, but two things make it a worse fit than the
explicit-length approach above: (a) `Immediate::v_string`
(`ifa/if1/num.h:53`) lives inside a plain C-style `union` with
`v_int64`/`v_float64`/etc. — `std::string` has non-trivial
constructors/destructors and cannot be a union member without a
tagged-variant restructuring of `Immediate` itself, a much larger
change than this bug warrants; (b) it does nothing for stage 4 — the
generated C source's `"..."` literal syntax is fundamentally
NUL-terminated regardless of how the compiler stored the value
internally, so an explicit length still has to be threaded into
codegen either way. The full generated/runtime code is already
compiled as C++23 (`Makefile.cg`: `CC = clang++`, `-std=c++23`,
despite the `.c` extension), so `std::string` is *available* if a
future need arises, it's just not the right tool for this specific
gap.

## Verification plan (all passed 2026-07-27)

- The two repros above (`"Hello\x00World"`, len 11;
  `b"\x00\x01\x05\x06\x07\x08"`, len 6) compile and run with the
  correct length and byte content (compare against `python3` output,
  including indexing individual bytes past the embedded NUL to
  confirm nothing after it was lost). **Passed** on both backends for
  `str`; `bytes` on the LLVM backend is blocked by an unrelated,
  pre-existing gap (`_CG_int_from_string` undefined at link time for
  `bytes` indexing under `-b`, from the earlier `bytes`-type work,
  not this issue) — worked around by testing `bytes` on the C backend
  only and `str` on both.
- A str/bytes literal `==` comparison across two values that share a
  truncatable prefix but differ after an embedded NUL
  (`b"a\x00b" != b"a\x00c"`) is correctly `False`/`True`. **This is
  what caught touch points 5-6** — before those fixes, pyc printed
  `True, False, True, False, True` for the full repro below where
  `python3` prints `False, True, False, True, True`; after, they
  match exactly:
  ```python
  b1 = b"a\x00b"; b2 = b"a\x00c"
  print(b1 == b2); print(b1 != b2)
  s1 = "a\x00b"; s2 = "a\x00c"
  print(s1 == s2); print(s1 != s2)
  print(b1 == b"a\x00b")
  ```
- Both backends (`./test_pyc.py` and `PYC_FLAGS=-b ./test_pyc.py`)
  stay green: 228 passed / 6 expected fails / 1 pre-existing unrelated
  failure (`cross_type_method.py`) / 4 skipped, on both — the exact
  pre-existing baseline, no regressions.
- `./shedskin_sweep.sh` shows no regressions — 52/77 compiled
  (22 clean + 30 with warnings), consistent with the post-`bytes`-work
  baseline (no embedded-NUL literals in the current corpus, so no
  compile-rate change expected either way).
