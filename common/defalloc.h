#ifndef _defalloc_H_
#define _defalloc_H_

// Backing store for Vec, Map/Set and List. A large block is allocated with
// GC_MALLOC_IGNORE_OFF_PAGE: Boehm otherwise needs every page of it free of
// blacklisted addresses (pages a false pointer from a conservative scan may
// hit), and a Vec that doubles each pass past 64 KiB kept landing on them.
// The collector then printed "Repeated allocation of very large block" on
// stderr, nondeterministically and depending on heap layout:
// split_css_by_defs's growth did it in 14 of 160 parallel compiles of
// tests/collections_more.py, enough that adding a two-line method to
// __pyc__ failed make test. IGNORE_OFF_PAGE asks only that a pointer into
// the block's first page be held while it is live, which holds here: every
// container keeps the block's START in its storage field (Vec::v, Map::v).
#define DEFALLOC_LARGE (16 * 1024)

class DefaultAlloc {
 public:
  static void *alloc(int s) { return s >= DEFALLOC_LARGE ? GC_MALLOC_IGNORE_OFF_PAGE(s) : MALLOC(s); }
  static void free(void *p) { FREE(p); }
};

#endif
