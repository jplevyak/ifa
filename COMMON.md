# COMMON — The plib Utility Layer

A working reference for `ifa/common/` — the hand-rolled C++ utility
library every other module depends on. It's a slimmed-down
template-heavy "plib" (John Plevyak's library) that predates
`<vector>` / `<unordered_map>` / etc.

Most of the time you don't need to read these headers — just use the
types. But the conventions (dual-mode `Vec`, set-vs-vector semantics,
sentinel-aware iteration, GC-aware allocation) are pervasive enough
to be load-bearing in every other deep doc, so this is the place
they're written down.

Sister docs: all of [IR.md](IR.md), [IFA.md](IFA.md),
[CLONE.md](CLONE.md), etc. — they all use these primitives.

---

## 1. In one paragraph

The library provides: `Vec<T>` (dual-mode dense-array/hash-set),
`Map<K,V>` and friends (`ChainHash`, `HashMap`, `BlockHash`,
`Env`), `Accum<T>` (set-with-iteration-order), linked lists with
intrusive links, hand-rolled argument parser (`ArgumentState`),
config file system (`int_config` / `string_config`), logging
(`alog` / `elog`), assertion-and-fail (`fail()` / `ASSERT()`),
timer, HTML output, service-lifecycle (`Service::start_all`), and a
unit-test framework (`UnitTest`). Everything inherits `gc`
(Boehm GC). Strings are interned via `IF1::strings` (a separate
chain hash in `if1.cc`). The header `ifadefs.h` provides shared
type aliases (`cchar`, `uint`, `int8`...`int64`, `uintptr_t`),
allocator macros (`MALLOC`, `FREE`), and the `gc` base class.

---

## 2. File map

```
ifa/common/
├── common.h          Top-level umbrella include (pulls everything in).
├── defalloc.h        DefaultAlloc (MALLOC/FREE wrappers).
├── vec.h / vec.cc    Vec<T>: dynamic array + hash set.
├── map.h             Map<K,V>, ChainHash, ChainHashMap, BlockHash, HashMap, Env.
├── list.h            SLL/DLL intrusive-link lists, Queue, Accum.
├── util.h            Misc inline helpers.
├── misc.h / misc.cc  buf_read, fail, error, trim, dupstr, str2bool, etc.
├── arg.h / arg.cc    ArgumentState, ArgumentDescription, process_args.
├── config.h / config.cc  Config file lookup (~/.ifarc, ifa.init).
├── log.h / log.cc    alog / elog / log_flags_arg.
├── fail.h / fail.cc  fail(), ASSERT(), myassert.
├── timer.h           Timer (high-precision CPU clock).
├── html.h / html.cc  HTML output for IFA visualisation.
├── service.h / service.cc  Service lifecycle (priority-ordered start/stop).
├── unit.h / unit.cc  UnitTest framework.
├── ifa_version.h / ifa_version.cc  Compile-time version string.
├── c_runtime.h       Shared between codegen and runtime (V).
└── libruntime.h      Library runtime helpers.
```

`ifadefs.h` is in `ifa/` proper (not `common/`) but acts as the
umbrella for everything — it includes `common.h` plus the IFA-specific
type aliases.

---

## 3. The `Vec<T>` (`vec.h` / `vec.cc`)

The single most-used type in the codebase.

```c
template <class C, class A = DefaultAlloc, int S = 2>
class Vec : public gc {
public:
  enum InitType { SET, MOVE };
  int n;       // current size
  int i;       // size index for sets, reserve for vectors
  C *v;        // backing storage (= &e[0] for small Vecs)
  C e[4];      // inline storage to avoid alloc for small Vecs

  // Vector mode:
  C &operator[](int i);
  C get(int i);
  void add(C a);
  C pop();
  void reset();
  void clear();
  void append(Vec &v);
  void copy(Vec &v);
  void move(Vec &v);     // steal v's storage; v becomes empty
  void insert(int i, C);
  void remove(int i);    // O(n)
  void remove_index(int i);
  void qsort(...);
  int index(C);          // linear search
  bool in(C);            // ditto

  // Set mode (hash-set):
  C *set_add(C);              // returns existing if dup, else inserts
  void set_remove(C);         // expensive
  int set_in(C);              // membership
  int set_union(Vec &v);
  int set_intersection(Vec &v);
  void set_intersection(Vec &v, Vec &result);
  void set_disjunction(Vec &v, Vec &result);
  void set_difference(Vec &v, Vec &result);
  int some_intersection(Vec &v);   // bool: any in common?
  int some_disjunction(Vec &v);    // bool: any not in common?
  int some_difference(Vec &v);     // bool: any in this but not other?
  int set_count();
  void set_clear();
  void set_to_vec();          // convert set-mode → dense-vector mode (compacts)

  C first_in_set();           // first non-NULL element in set mode
  C *end();
};
```

### 3.1 Dual mode

A `Vec` is used as either:

- **Dense array**: `add`/`pop`/`[i]`. Push and iterate normally.
  `n` is the number of elements; `v[0..n-1]` is the data.
- **Hash set**: `set_add`/`set_in`/`set_to_vec`. The Vec is sized
  to a prime/power-of-2 hash table, and `v[i]` may be NULL.
  Iteration must skip NULLs. After `set_to_vec`, the Vec is compacted
  to dense mode.

**Which mode is in use is by convention, not enforced.** The same
Vec object can be in set mode at one point and dense mode at another.
The caller is responsible for not mixing operations.

### 3.2 Iteration patterns

For dense Vecs:

```c
for (C *x : v) { ... }                  // range-for
for (int i = 0; i < v.n; i++) { v[i]; } // index-for
```

For set-mode Vecs (or potentially-set ones):

```c
for (C *x : v) if (x) { ... }           // skip NULLs
```

The convention `if (x)` is everywhere in the codebase — it's the
single most important idiom to recognise. Pointer fields can also
hold sentinel values (e.g., `PycSymbol*` sentinels in pyc's scope
maps; see [PYTHON_FRONTEND.md](../PYTHON_FRONTEND.md) §4.6) — that's
a different convention layered on top.

### 3.3 `set_to_vec`

After set operations are done, `set_to_vec()` compacts the Vec by
removing NULL slots. After this, the Vec is in dense mode and can
be iterated without `if (x)` checks. *Most code paths do this
implicitly* when they finish building a set.

If you forget `set_to_vec` and the consumer expects dense semantics,
you'll get NULL dereferences or wrong counts.

### 3.4 `move`

`v.move(other)` transfers `other`'s storage to `v`, clearing
`other`. O(1). Preferred over `copy(other); other.clear()` when you
own the source.

### 3.5 `Vec<C>(C)` constructor

`Vec<int>(5)` creates a single-element Vec containing `5`. Easy to
trip on if you meant "Vec of size 5" — that's `Vec<int> v; v.fill(5);`
instead.

### 3.6 Common Vec operations cross-referenced

| Operation | Use case |
|---|---|
| `add(x)` | Append to dense array. |
| `set_add(x)` | Add to set; returns existing element if dup. |
| `set_in(x)` | Set membership test. |
| `set_union(other)` | Merge other into this (set-mode). |
| `set_intersection(other, result)` | Compute intersection into result. |
| `some_disjunction(other)` | Bool: "are these sets non-equal?" — used heavily in CLONE.md. |
| `move(other)` | Steal storage; O(1). |
| `copy(other)` | Deep copy storage; O(n). |
| `append(other)` | Add all of other's elements to this. |
| `pop()` | Remove and return last; standard stack op. |
| `insert(i, x)` / `remove(i)` | Positional insert/remove; O(n). |
| `index(x)` | Linear search; returns position or -1. |
| `in(x)` | `index(x) >= 0`. |
| `qsort(cmp)` | In-place sort via libc qsort. |

---

## 4. Map and friends (`map.h`)

Three layers of hash table:

### 4.1 `MapElem<K, V>`

The kv-pair element. `MapElem::key` is 0 means "slot is empty"
(this is the standard NULL-key convention).

### 4.2 `Map<K, V>` (`map.h:48`)

A `Vec<MapElem<K, V>>` with hash-table operations layered on top.
Open addressing.

```c
ME *put(K key, V value);     // returns the slot
V  get(K key);               // 0 if missing
ME *get_internal(K key);     // returns the MapElem*
void copy(Map &m);
void move(Map &m);
void clear();
void map_union(Map &m);
int some_disjunction(Map &m);
void get_keys(Vec<K> &keys);
void get_values(Vec<V> &values);
```

Iteration uses the `form_Map` macro (defined per-type):

```c
form_Map(MapElem<K,V>, e, m) {
  if (e->key) { ... e->key, e->value ... }
}
```

There's a `form_*` macro per Map specialisation in the headers (e.g.,
`form_AVarMapElem`, `form_MPositionAVar`, `form_MPositionAType`).
These are just `for` loops with the right cast.

### 4.3 `HashMap<K, FN, V>`

Map with a custom hash function class `FN` providing
`static uint hash(K)` and `static int equal(K, K)`. Used in `fa.cc`
for `AType` interning (key is `AType *` with content-based hash via
`ATypeChainHashFns`).

### 4.4 `BlockHash<K, FN>` / `ChainHash<K, FN>` /
`ChainHashMap<K, FN, V>`

Variants:
- `BlockHash` — block-allocated hash, supports cheap remove. Used in
  `PNode::live_vars`.
- `ChainHash` — chained hash, no remove. Used for interning
  (`AType`, `Setters`, `MPosition`, etc.).
- `ChainHashMap` — chained hash with values. Used for
  `Primitives::registered_prims`.

### 4.5 `Env<K, V>` (declared in `map.h`)

A stack-of-Maps for lexical scopes. Used by SSU's rename pass
(`ssu.cc`). API:

```c
Env<K, V> env;
env.push();              // enter scope
env.put(k, v);           // bind in current scope
V x = env.get(k);        // walk down stack
env.pop();               // exit scope
```

---

## 5. Lists (`list.h`)

Intrusive linked lists. The macros define link classes that let one
object live in multiple lists:

```c
class MyClass {
  ...
  SLINK(MyClass, foo_link);   // singly linked list, field name "foo_link"
  LINK(MyClass,  bar_link);   // doubly linked list
};

Que(MyClass, foo_link) queue;
queue.enqueue(x);
MyClass *y = queue.pop();
```

`Que` is a FIFO queue using the link. `SLL` and `DLL` are stack-style
push/pop.

Used heavily in `fa.cc`:

```c
static Que(AEdge,     edge_worklist_link) edge_worklist;
static Que(AVar,      send_worklist_link) send_worklist;
static Que(EntrySet, es_worklist_link)    es_worklist;
```

### 5.1 `Accum<T>` (also in `list.h`)

A set-with-iteration-order: `add(x)` is `set_add(x)` but also
appends to an `asvec` vector preserving insertion order.

```c
Accum<Foo *> acc;
acc.add(x);                  // also acc.asvec.add(x) if new
for (Foo *y : acc.asvec) { ... }  // iterate in insertion order
acc.asset.set_in(z);         // test membership in O(1)
```

This is the structure of choice when you need both:
- "Have I seen this before?" O(1) lookup, and
- "Iterate in insertion order".

Used extensively in IFA's worklist-style traversals.

---

## 6. Allocation (`defalloc.h` + `ifadefs.h`)

```c
class DefaultAlloc {
public:
  static void *alloc(int s) { return MALLOC(s); }
  static void free(void *p) { FREE(p); }
};
```

`MALLOC` / `FREE` are macros in `ifadefs.h`:
- With `USE_GC=1` (default): `MALLOC` → `GC_malloc`, `FREE` → no-op.
- Without: standard `malloc` / `free`.

The `gc` class (also from Boehm) is the base of every GC-tracked
object. Inheriting `gc` arranges for the object's pointers to be
scanned by the collector.

**Convention:** every IFA class inherits `gc`. Don't `delete`.

---

## 7. Argument parsing (`arg.h` / `arg.cc`)

Declarative CLI parser. Each program declares an
`ArgumentDescription[]` table:

```c
static ArgumentDescription arg_desc[] = {
  {"verbose", 'v', "Verbosity", "+", &verbose_level, "PYC_VERBOSE", NULL},
  ...
  {0}
};
static ArgumentState arg_state("pyc", arg_desc);
```

Then in `main`:

```c
process_args(&arg_state, argc, argv);
// access via arg_state.nfile_arguments and arg_state.file_argument[]
```

Type codes (in the 4th field):
- `"F"` — bool flag (sets to true).
- `"f"` — bool flag (sets to false).
- `"T"` — bool toggle.
- `"+"` — int that increments.
- `"I"` / `"L"` / `"D"` — int / long / double.
- `"S<N>"` — string of length N.
- NULL — custom function handler in the 7th field.

The 6th field is the env var; if set, overrides the default.

`usage(state, NULL)` prints the help; `process_args` calls it on
parse errors.

---

## 8. Config files (`config.h` / `config.cc`)

`init_config()` reads `~/.ifarc` and `./ifa.init` (or whatever
`config_filenames` is set to). Then `get_int_config(name)`,
`get_int64_config(name, def)`, `get_string_config(...)`, etc.
return values.

Names support hierarchical query — `get_int_config("alog.test.fa")`
matches a line like `alog.test.fa = 1` in the config file.

`replace_config(fn)` switches to a different file mid-run.
`config_callback(pfn, ptr)` registers callbacks fired on config
changes.

Used for tunables that don't deserve a CLI flag (e.g.,
`alog.test.fa` toggles internal test logging in the analysis).

---

## 9. Logging (`log.h` / `log.cc`)

Two layers:

### 9.1 The fast log (`alog`)

For critical-path logging:

```c
DEF_ALOG(connection, new);                              // at file scope
ALOG(connection, new)("New connection: %d\n", id);     // call site
alog_MAIN_new_connection("...", id);                   // fast variant
```

`ALOG` caches the config flag in a hidden static; first call pays
for the lookup, subsequent calls are nearly free.

### 9.2 The slow log (`log`)

For diagnostic output:

```c
log(LOG_DISPATCH, "...");
log(LOG_SPLITTING, "...");
```

`LOG_*` are bitmask channels. Enabled via the CLI `-l` flag or
`IFA_LOG_FLAGS` env var, which sets the global `log_flags`.
`log_flags_arg(argstate, arg)` parses the flag value (a
comma-separated list of channel names).

`log_fp(channel)` returns the FILE* to write to (default stderr, but
redirectable via `IFA_LOG_DIR`).

### 9.3 `elog` — error log

Always-on error reporting. Goes to stderr.

---

## 10. Failures (`fail.h` / `fail.cc`)

```c
void fail(cchar *fmt, ...);          // print and exit(1)
int  show_error(cchar *fmt, IFAAST*, ...);
int  show_error(cchar *fmt, Var*, ...);
int  myassert(cchar *file, int line, cchar *str);
#define ASSERT(_x) ((_x) || myassert(__FILE__, __LINE__, #_x))
```

`fail` is the fatal-error escape hatch — use for invariant violations
that can't be recovered. `show_error` is for diagnosable user errors
that include source location (extracted from the IFAAST or Var).

`ASSERT` is the variant of `assert` that returns int (for use in
boolean expressions). Standard `assert` from `<assert.h>` also works
but is compiled out under `NDEBUG`.

---

## 11. Timer (`timer.h`)

```c
class Timer {
  Timer t;
  t.start();
  // ... work ...
  t.stop();
  t.time;          // seconds elapsed since last start
  t.accumulator[i] // accumulated time, slot i
  t.accumulate();  // add t.time to accumulator[0]
  t.restart();     // stop + clear + start
};
```

Uses `clock_gettime(CLOCK_REALTIME, ...)`. `fa.cc` uses three:
`pass_timer`, `match_timer`, `extend_timer`, with the per-pass
summary printed every iteration.

---

## 12. HTML output (`html.h` / `html.cc`)

Pretty-printing for `ifa -t` / `pyc --html`. Walks the analysis
state and emits a hyperlinked HTML view of every Fun, Var, Sym,
AVar, AType. Uses `mktree.js` / `mktree.css` (in the install dir)
for collapsible tree widgets.

Mostly opaque to deep-doc readers; treat it as "the
visualisation backend, see `--html`" and move on.

---

## 13. Service lifecycle (`service.h` / `service.cc`)

A tiny dependency-ordered init/start/stop framework:

```c
class MyService : public Service {
  void init() { ... }
  void start() { ... }
  void stop() { ... }
};
static MyService my_service;   // priority 0; runs first
```

```c
Service::start_all();   // calls init() then start() in priority order
Service::stop_all();    // calls stop() in reverse
```

Used by `pyc.cc` and `ifa/main.cc` at startup/shutdown. Most code
doesn't register services; this is for global resources (loggers,
config, network).

---

## 14. Unit tests (`unit.h` / `unit.cc`)

```c
int my_test() { return ... ? 0 : 1; }
UNIT_TEST_FUN(my_test);

// Or class-based:
class MyUT : public UnitTest {
  int run() { return ... ? 0 : 1; }
  MyUT() : UnitTest("my test") {}
} my_ut;

// Run all:
UnitTest::run_all();
```

`./ifa --test` invokes `UnitTest::run_all`. Used by
`common/vec_test.cc` and a handful of others. Most of IFA's testing
is end-to-end via test files (`tests/*.v`, `tests/*.py`), not unit
tests.

---

## 15. `ifadefs.h`

Top-level definitions. Pulls in `common.h` plus:
- `cchar` (alias for `const char`).
- `uint`, `uint8/16/32/64`, `int8/16/32/64`.
- `MALLOC` / `FREE` macros (GC-aware).
- `gc` base class.
- `STRCAT` / `STRCPYZ` / similar string macros.
- `open_hash_primes[]` — array of primes used by every hash-cons
  table.
- `numberof(arr)` — array-size macro.

Anything that includes `ifadefs.h` gets everything else for free.

---

## 16. Common gotchas (idioms that bite)

### 16.1 Set-mode iteration without `if (x)`
The pattern is `for (C *x : v) if (x) { ... }`. Forgetting the
`if (x)` causes NULL dereferences. Many bugs in IFA history have
been this.

### 16.2 `set_to_vec()` before consumption
A Vec built via `set_add` is "set-mode" — has NULL slots, count
includes them, iteration must filter. After all sets are added,
call `set_to_vec()` to compact into dense mode. Consumers that
expect dense semantics break otherwise.

### 16.3 `Vec::move()` invalidates the source
`v.move(other)` leaves `other` empty. Don't keep iterators/pointers
into `other` after.

### 16.4 `MapElem::key == 0` means empty slot
Always check `if (e->key)` before using a Map element. This is
*especially* important when iterating via `form_Map`.

### 16.5 `Accum<T>` is two structures in one
`Accum::asvec` is the insertion-order vector; `Accum::asset` is the
membership-test set. Mutate via `add()` which updates both.
Don't bypass into one or the other directly.

### 16.6 `Que` is a linked list, not a Vec
`Que(C, link_field)` requires `C` to have a `link_field` field of
the right type. The `LINK` macro defines the field. You can't put
a class in two `Que`s without two separate link fields.

### 16.7 `gc` base must come first
Multiple inheritance with `gc` requires `gc` to be listed first to
keep the vtable layout consistent with Boehm's scanning. Most IFA
classes use single inheritance from `gc`, which avoids the issue.

### 16.8 Hash functions must be deterministic across runs
Pointer-based hash functions (most of IFA) produce different
results across runs because `malloc` returns different addresses.
This is fine for correctness — the lookup is consistent within one
run — but breaks if you serialise hash-keyed state to disk and
expect to reload. The CDB (see [IFA.md](IFA.md) §11.5) is dormant
partly because of this.

### 16.9 `fail()` calls `exit(1)`
Use it only for genuinely unrecoverable errors. For user-facing
diagnostics use `show_error` (returns int, lets caller decide).

### 16.10 `ALOG` static init isn't thread-safe
`ALOG`'s hidden static for caching the channel state isn't
thread-safe per C++ standards — `magic statics` are guaranteed
thread-safe in C++11+ but the hidden conditional adds overhead per
call. Single-threaded use (which is what IFA does) is fine.

### 16.11 `arg.cc`'s string lifetime
`ArgumentDescription::location` for `"S<N>"` types points to a
fixed-size buffer the caller allocates. The parsed value is copied
in. Don't pass a stack buffer that goes out of scope.

### 16.12 Linear `Vec::remove(i)` is O(n)
`Vec::set_remove(x)` is also O(n). For high-churn sets, use
`BlockHash<K, FN>` which has O(1) removal.

---

## 17. When to use which container

A quick decision guide:

| Need | Use |
|---|---|
| Stack push/pop | `Vec<T>` (dense mode). |
| Membership test only | `Vec<T>` (set mode, `set_add` + `set_in`). |
| Insertion-order iteration + membership | `Accum<T>`. |
| Worklist with reentry-prevention | `Que(T, link)` + `in_*_worklist` bit. |
| Key-value map | `Map<K, V>` (open addressing). |
| Key-value with custom hash | `HashMap<K, FN, V>`. |
| Hash-cons / interning | `ChainHash<T, FN>` or `ChainHashMap<K, FN, V>`. |
| Need cheap remove | `BlockHash<T, FN>`. |
| Lexical scope | `Env<K, V>`. |

---

## 18. References

- `ifa/common/*.{h,cc}` — implementation.
- `ifa/ifadefs.h` — umbrella include + type aliases.
- Used everywhere; see any other deep doc for examples.

This is a low-churn area. Most IFA changes don't touch
`common/`. When they do, treat as load-bearing infrastructure
work — every other file depends on these contracts.
