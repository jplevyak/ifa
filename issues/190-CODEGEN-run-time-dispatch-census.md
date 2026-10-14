# 190 — CODEGEN: run-time dispatch census, toward zero

**Status:** open. Nullable receivers fixed, both tagged and untagged
(2026-10-13). What remains is classified below.

## Goal

**Author's directive, 2026-10-13: minimal run-time dispatch, targeting
zero.** A dispatch that cannot resolve statically is a demand
([146](146-remove-all-arbitrary-splitting.md); `SETTER_OF_SETTER` asks it).
`PYC_DBG_CALLS=1` counts call sites; `=3` names each dynamic one with its
candidates and their source lines.

## Fixed: a nullable receiver is not a dispatch

chull's 15 dynamic calls were all one shape. The receiver is one class or
None: `fv.vertex[i] != e.endpts[0]`, `"%s" % self.vertex[k]`, and
`if e.newface:`. The None is real in CPython. `Face(edge=[...])` takes the
default `vertex=[None, None, None]` and fills it in later, so one object
holds None and then a Vertex over time. No contour split can separate
that, which is why `SETTER_OF_SETTER`'s dispatch demand did not reduce
these calls.

Two defects, both in codegen:

1. **A crash.** `poly_dispatch_classtag_targets` scanned every formal for one
   whose type has the method. `__pyc_None_type__.__eq__(self, x)`'s `self`
   has no `__eq__`, so the scan went on to `x : V` and took the None method
   for a V-tag candidate. No null test was emitted, and `None == v` read a
   class tag through NULL. On a method send it now asks only the receiver,
   the first formal after the selector. `tests/nil_receiver_method_send.py`
   segfaulted before the fix, on both backends.
2. **Not dynamic.** After the null test every remaining receiver calls the
   one non-None candidate. `nil_tested_target` (codegen_common.cc) names
   that pair, and both the C emitter and the census use it. The C arm is now
   a direct call instead of a tag compare and a slot call. The census reports
   `niltest=N` apart from `dynamic`, and only when N > 0. The LLVM emitter
   still tag-compares that arm; it is correct, just not shortened.

chull: `dynamic=15` → `dynamic=0 niltest=15`. Corpus `check -R`
(`check__default__4697b7c4+a77008dd` against `+3981dab9`): verdicts,
warnings and contours identical on all 77.

## Fixed: None plus an untagged builtin

28 more call sites in user code were None plus a `str`, `bytes`, `list`,
`tuple` or function value. The emitter already decided each of these with
a null test and a direct call, through its untagged-direct route or the
arity plan, but `nil_tested_target` required a classtag, so the census
counted them as dynamic. Counting with the same predicate in the runtime
library added 19 more. The predicate now accepts any receiver whose
members are all pointers: all tagged or all untagged, and none `None` or
a number. A scalar shares 0 with None, so a null test cannot decide it.

A temporary probe in the C emitter checked that every site the census
calls `niltest` is emitted as a null test plus one direct call. It found no
mismatches on 76 programs. One shape reaches that result by another
route: `{2-tuple, None}` goes through the arity plan, which keeps its
`len == 2` check after the null test. That check is a guard, not a choice
between functions, because only one candidate is left.

This changed only the census: emission is unchanged and no golden moved.

## Corpus census (76 programs; othello3 does not compile)

direct 37,236, **niltest 177, dynamic 95**. Before this issue: niltest 0,
dynamic 272.

`PYC_DBG_CALLS=3` names each candidate's receiver type and the receivers'
common user-defined ancestor. The ancestor walk goes through
`Sym::specializes` from each clone's ORIGINAL class, which is
`creators[0]->sym`. A clone's `specializes` names the original parents, so
comparing clones directly reported `Chrome(Material)` as unrelated to
`Material`.

### The 95, classified (2026-10-13)

| calls | class | programs |
| --- | --- | --- |
| **71** | **distinct classes with a common user ancestor**: polymorphism, which is legitimate. shedskin emits these as virtual calls; hoist, do not split (AGENTS.md) | pygasus 39 (`_exec` over `AddrMode`, `Instruction`, `MMCread/Write/Load`), quameon 11 (`sto_base`, `base_jastrow`), pygmy 7, bh 7 (`Node`), yopyra 3, tarsalzp 2, richards 1 (`Task`), path_tracing 1 (`Material`) |
| **4** | **one class, two clones** at one call site | quameon 2 (`coulomb_pot.compute_en_value` / `compute_nn_value`), fysphun 1 (`Link.applyme`), softrender 1 (`Vector4.add`) |
| **7** | `__pyc_unhandled_exception__`: `e.__str__()` over the exception classes a program raises, plus None | one per program that raises two classes with their own `__str__` |
| **13** | builtin receivers: tuple shapes (`__eq__` / `__str__`, amaze 8, 4 of them with None), `{bytes, bytearray}.__getitem__` (softrender 4), tuple `__eq__` (sunfish 1) | |

**No dynamic call has receivers of unrelated user classes.** Every
user-class dispatch in the corpus is either a hierarchy or clones of one
class.

### What is left to do

1. **The 4 one-class-two-clones calls are a demand.** Two CreationSets of
   one class, whose member contents differ, meet at one call site, and the
   slot dispatch picks the clone. quameon's case is the one ifa/132
   describes, where an int-list `charges` and a float-list `charges` refuse
   the clone collapse. `dispatch_named`/`per_cs_formals` in fa.cc already
   splits the calling EntrySet per receiver CreationSet, but only when the
   ambiguous receiver sym is UNTAGGED (`type_kind != Type_RECORD`), because
   that case used to be a codegen failure. For a tagged class it is "only" a
   slot call, so no demand fires. Under the zero-dispatch goal it should.
2. **The 71 hierarchy calls stay dynamic** unless a call site's receivers
   come from separable sources. Measuring that is a different question:
   does an ES split on the receiver's creation points make the site
   monomorphic?
3. The exception printer (7): an `isinstance` chain on the raised class,
   or a direct call per class, would remove it. It runs once, at exit.
4. The builtin shapes (13), one at a time.
