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
| **4** | **one class, two clones** at one call site: each is a value merge no contour split can separate (see below) | quameon 2 (`coulomb_pot.compute_en_value` / `compute_nn_value`), fysphun 1 (`Link.applyme`), softrender 1 (`Vector4.add`) |
| **7** | `__pyc_unhandled_exception__`: `e.__str__()` over the exception classes a program raises, plus None | one per program; 4 removed 2026-10-14 (item 3) |
| **13** | builtin receivers: tuple shapes (`__eq__` / `__str__`, amaze 8, 4 of them with None), `{bytes, bytearray}.__getitem__` (softrender 4: on the C backend these were `matching function not found` STUBS, not dispatches; gone since 2026-10-14, when `len` of a constant began to fold, see [102](102-corpus-programs-compile-then-abort-at-runtime.md)), tuple `__eq__` (sunfish 1) | |

**No dynamic call has receivers of unrelated user classes.** Every
user-class dispatch in the corpus is either a hierarchy or clones of one
class.

### What is left to do

1. **The 4 one-class-two-clones calls: none can be separated by a contour.**
   Planned: widen the receiver split (`per_cs_formals`, which fires only for
   untagged receivers) to tagged classes. Stop condition, set before
   measuring: if the two receiver CreationSets meet in one value that no
   contour split can separate, the split cannot make the site monomorphic,
   and widening it only adds contours. All four meet that condition:
   - **fysphun `Link.applyme`: one container.** `addlinki` stores an int
     `dist` and `addlinkrr` a float from `hypot`, and both go into the one
     global `links`, so `links[0]` may be either.
   - **softrender `Vector4.add`: a flow merge inside one contour.**
     `havenormal` is `pos_normal[key]` (a float Vector4) or
     `Vector4(0, 0, 0, 0)` (ints). The two meet at one variable inside one
     function.
   - **quameon `coulomb_pot` (2 calls): one field over time.**
     `wave_func.__init__` writes `self.pot = coulomb_pot(npos)`, whose
     default charges are float; quameon.py then overwrites `wavef.pot`
     with `coulomb_pot(npos, charges)`, whose charges are int. This is
     temporal, the same shape as bh's (AGENTS.md).

   In each, two value types legitimately reach one call, because CPython
   keeps the int an int. Only coercing to float would remove the dispatch,
   and that is a semantic deviation. The dispatch is the correct code, and
   the widening was not built.
2. **The 71 hierarchy calls stay dynamic** unless a call site's receivers
   come from separable sources. Measuring that is a different question:
   does an ES split on the receiver's creation points make the site
   monomorphic?
3. **The exception printer (7): 4 removed, 3 left (2026-10-14).** The
   candidates were None's `__str__` plus two IDENTICAL bodies,
   `Exception.__str__` and `BaseException.__str__`. They were two because
   `__pyc__/08_exception.py` derived `Exception` from `object` with its own
   copy of both methods, so the builtin hierarchy was flat. That was also a
   CPython bug: `except BaseException` did not catch a `ValueError`, and
   the program died with "Unhandled exception". `Exception` now derives
   from `BaseException` and inherits both methods, so a program whose
   exceptions all use the builtin `__str__` has one non-None target, and
   the call is a null test plus a direct call. Test:
   `tests/exception_hierarchy_base.py` (which fails on the old tree).
   - bh 8 → 7, neural1 1 → 0, pygasus 40 → 39, kanoodle 1 → 0.
   - **Left: voronoi2, rdb and msp_ss, 1 each.** The extra candidate is
     `GetoptError.__str__` (`pyc_lib/getopt.py`), a real override that
     CPython's getopt also has, with `BaseException` as the common
     ancestor. All three programs catch `GetoptError` around the
     `getopt` call, so it never reaches the printer. The analysis sees it
     anyway because the pending slot `__pyc_exc__` is one global: every
     raise stores to it, and the printer reads the union of every class
     ever raised, caught or not. Removing these 3 needs the printer to see
     only what can ESCAPE uncaught, which is a change to the exception
     model (threading the pending exception along the propagation path
     instead of through one flow-insensitive global), not to splitting.
     It is the same confluence as
     [issues/172](../../issues/172-generator-send-channel-and-stopiteration-value.md).
   - Corpus check -R `check__default__c3ea9eb3+3c8fdc66` against
     `check__default__4697b7c4+a77008dd`: every verdict and warning count
     identical on all 77; CreationSets -3 on every program (-228), and one
     EntrySet fewer on each of the 7 printer programs.
4. The builtin shapes (13), one at a time.

### Can a split remove any of them? The soft-demand inventory (2026-10-14)

The next step after [102](102-corpus-programs-compile-then-abort-at-runtime.md)'s
refusal work was a SOFT dispatch demand: treat every dispatch that has a
plan as a demand, so the splitter tries to make the site monomorphic.
Stop condition, set before measuring: if no dispatching send's receiver
union forms at a point a contour split can separate, the demand has no
beneficiary and is not built.

`IFA_DBG_DISPATCHORIGIN` (`report_dispatch_origin`, `fa_debug.cc`) walks
back from the receiver of every send whose non-None receivers reach two or
more target EntrySets, to the points where its classes first meet, and
classifies each: an ES formal (an ES split could separate), a CreationSet
member/element with two or more creation points (a CS split could), the
same with ONE creation point (one container: none can), a merge inside one
contour, or a module global. A send is separable only if EVERY meeting
point is. Two details mattered: a call result reaches the callee through
`lvalue`, not `backward`; and a predecessor on a cycle back through the
node (a list extended from its own contents) does not "carry" the union.

| program | codegen dynamic | sends | meeting points | separable |
| --- | --- | --- | --- | --- |
| pygasus | 39 | 44 | one container (every one) | 0 |
| quameon | 13 | 17 | one container | 0 |
| pygmy | 7 | 12 | one container, local merges | 0 |
| bh | 7 | 8 | one container (`Cell.subp`); 3 also meet at a recursive formal | 0 |
| yopyra | 3 | 4 | one container, local | 0 |
| tarsalzp | 2 | 2 | local | 0 |
| richards | 1 | 1 | one container | 0 |
| path_tracing | 1 | 1 | local | 0 |
| voronoi2, rdb, msp_ss | 1 each | 1 each | local + the `__pyc_exc__` global | 0 |

(fysphun, softrender, amaze and sunfish have dynamic calls the probe does
not count: one class with two clones, or builtin tuples, both measured
unseparable above.)

**0 of 92.** Every dispatching receiver's union forms in a single container
(the instruction table, the scene's object list, a tree node's children),
in a merge inside one function, or in the exception slot. No EntrySet or
CreationSet split can make any of these sites monomorphic, so a soft
dispatch demand would only add contours. Not built. These dispatches are
the program's polymorphism, as shedskin's virtual calls are.

Found on the way: SETTER_OF_SETTER's dispatch clause (`sos_poly_recvs`,
ifa/146) counts target FUNCTIONS. richards' `t.runTask()` reaches four
EntrySets of the one `Task.runTask`, which it does not see. Counting
EntrySets would widen it, but by the table above to no effect.

### The one-class-two-clones calls choose by argument (2026-10-14)

ifa/121's dispatch-arm plans found that these calls are not only
unseparable but unprovable: one receiver object reaches two clones that
differ in behaviour, chosen by an ARGUMENT's type, and both share the
class's tag, so the emitted code calls whatever the object's slot holds.
That is right only when the argument has the type that clone expects. It
is now warned at each site (fysphun `Link.applyme`, softrender
`Vector4.add` twice, `sub`, `transform`) and refused under `--strict`.
The way out is the same as before: these unions are real under CPython
(int and float fields), so only coercion would remove them.
