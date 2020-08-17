/* -*-Mode: c++;-*-
   Copyright (c) 2003-2008 John Plevyak, All Rights Reserved
*/
#ifndef _pattern_H_
#define _pattern_H_

#include <sys/types.h>
#include "code.h"

class Sym;
class FA;
class Fun;
class AType;
class AVar;
class CreationSet;

#define MAX_ARGS 1000
#define CANNONICAL_MPOSITION ((MPosition *)(intptr_t)(-1))

#define int2Position(_i) ((void *)((intptr_t) - (_i)))
#define Position2int(_p) (-((intptr_t)_p))
#define is_intPosition(_p) (((uint32)Position2int(_p)) < MAX_ARGS)

class MPosition : public gc {
 public:
  Vec<const void *> pos;
  MPosition *cp, *up, *next, *down;
  void copy(MPosition &p);
  void set_top(const void *p) {
    pos[pos.n - 1] = p;
    cp = 0;
  }
  void push(int i) {
    pos.add(int2Position(i));
    if (cp && i == 1)
      cp = cp->down;
    else
      cp = 0;
  }
  void push(const void *p) {
    pos.add(p);
    cp = 0;
  }
  void pop() {
    pos.pop();
    if (cp)
      cp = cp->up;
    else
      cp = 0;
  }
  void inc() {
    pos[pos.n - 1] = int2Position(Position2int(pos[pos.n - 1]) + 1);
    if (cp)
      cp = cp->next;
    else
      cp = 0;
  }
  const void *last() { return pos[pos.n - 1]; }
  int is_positional() {
    for (int i = 0; i < pos.n; i++)
      if (!is_intPosition(pos[i])) return 0;
    return 1;
  }
  int last_is_positional() { return is_intPosition(last()); }
  int prefix_to_last(MPosition &p);
  MPosition() : cp(0), up(0), next(0), down(0) {}
  MPosition(MPosition &p);
};
#define forv_MPosition(_p, _v) forv_Vec(MPosition, _p, _v)

inline int MPosition::prefix_to_last(MPosition &p) {
  if (pos.n != p.pos.n + 1) return 0;
  if (memcmp(pos.v, p.pos.v, p.pos.n * sizeof(void *))) return 0;
  return 1;
}

class MPositionHashFuns : public gc {
 public:
  static uint hash(MPosition *x) {
    uint h = 1;
    for (int i = 0; i < x->pos.n; i++) h += ((uintptr_t)x->pos[i]) * open_hash_primes[i];
    if (!h) h = 1;
    return h;
  }
  static int equal(MPosition *x, MPosition *y) {
    if (x->pos.n != y->pos.n) return 0;
    for (int i = 0; i < x->pos.n; i++)
      if (x->pos[i] != y->pos[i]) return 0;
    return 1;
  }
};

class MType : public gc {
 public:
  Map<MPosition *, Vec<Fun *> *> funs;
};

class Patterns : public gc {
 public:
  Vec<Sym *> types;
  Vec<Sym *> types_set;
  Vec<MType *> mtypes;
};

class Match : public gc {
 public:
  Fun *fun;
  Map<MPosition *, AType *> formal_filters;  // formal -> type, positional-only
                                             // and takes into account all
                                             // arguments
  Vec<PNode *> visibility_points;
  unsigned int is_partial : 1;

  void merge(Match *m);  // merge in formal_filters and visibility_points

  Match(Fun *afun) : fun(afun), is_partial(0) { assert(afun); }
  Match(Match &m) : fun(m.fun), is_partial(m.is_partial) {
    formal_filters.copy(m.formal_filters);
    visibility_points.copy(m.visibility_points);
  }
};
#define forv_Match(_p, _v) forv_Vec(Match, _p, _v)

typedef Map<MPosition *, AType *> MapMPositionAType;
typedef MapElem<MPosition *, AType *> MapMPositionATypeElem;
#define form_MPositionAType(_p, _v) form_Map(MapMPositionATypeElem, _p, _v)
typedef MapElem<MPosition *, MPosition *> MapMPositionMPosition;
#define form_MPositionMPosition(_p, _v) form_Map(MapMPositionMPosition, _p, _v)
typedef MapElem<Sym *, Sym *> MapSymSym;
#define form_SymSym(_p, _v) form_Map(MapSymSym, _p, _v)
typedef MapElem<MPosition *, Sym *> MapMPositionSym;
#define form_MPositionSym(_p, _v) form_Map(MapMPositionSym, _p, _v)

void build_patterns(FA *fa);
void add_patterns(FA *fa, Fun *f);
void build_arg_positions(FA *fa);
int positional_to_named(PNode *pn, CreationSet *cs, MPosition &p, MPosition *result_p);
int pattern_match(Vec<AVar *> &args, Vec<cchar *> &names, AVar *send, int is_closure, Partial_kind partial,
                  PNode *visibility_point, Vec<Match *> &matches);
MPosition *cannonicalize_mposition(MPosition &p);
MPosition *build_arg_positions(Fun *f, MPosition *up = 0);

extern int pattern_match_hits, pattern_match_complete, pattern_matches;

#endif
