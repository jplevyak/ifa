
/*
  Simple list classes including ones using internal links and ones using external cons cells.

  NOTE: SLL/DLL/Queue do not support use with certain types of multiple-inheritance.
    if you get from g++:
      warning: invalid access to non-static data member `A::link' of NULL object
      warning: (perhaps the `offsetof' macro was used incorrectly)
    use -Wno-invalid-offsetof with version 3.3+ of GCC
*/

#ifndef _list_H_
#define _list_H_

#include "defalloc.h"

#include <stddef.h>
#if !defined(__FreeBSD__) || (__FreeBSD_version >= 500000)
#else
#include <stdint.h>
#endif

template <class C>
struct SLink {
  C *next;
  SLink() : next(NULL){};
};
#define SLINK(_c, _f)                                   \
  class Link##_##_f : public SLink<_c> {                \
   public:                                              \
    static _c *&next_link(_c *c) { return c->_f.next; } \
  };                                                    \
  SLink<_c> _f
#define SLINKM(_c, _m, _f)                                 \
  class Link##_##_m##_##_f : public SLink<_c> {            \
   public:                                                 \
    static _c *&next_link(_c *c) { return c->_m._f.next; } \
  };

template <class C>
struct Link : SLink<C> {
  C *prev;
  Link() : prev(NULL) {}
};
#define LINK(_c, _f)                                    \
  class Link##_##_f : public Link<_c> {                 \
   public:                                              \
    static _c *&next_link(_c *c) { return c->_f.next; } \
    static _c *&prev_link(_c *c) { return c->_f.prev; } \
  };                                                    \
  Link<_c> _f
#define LINKM(_c, _m, _f)                                  \
  class Link##_##_m##_##_f : public Link<_c> {             \
   public:                                                 \
    static _c *&next_link(_c *c) { return c->_m._f.next; } \
    static _c *&prev_link(_c *c) { return c->_m._f.prev; } \
  };
#define LINK_FORWARD_DECLARATION(_c, _f)       \
  class Link##_##_c##_##_f : public Link<_c> { \
   public:                                     \
    static _c *&next_link(_c *c);              \
    static _c *&prev_link(_c *c);              \
  };
#define LINK_DEFINITION(_c, _f)                                           \
  inline _c *&Link##_##_c##_##_f::next_link(_c *c) { return c->_f.next; } \
  inline _c *&Link##_##_c##_##_f::prev_link(_c *c) { return c->_f.prev; }

template <class C, class L = typename C::Link_link>
struct SLL : public gc {
  C *head;
  inline void push(C *e);
  inline C *pop();
  void clear() { head = NULL; }
  C *&next(C *e) { return L::next_link(e); }

  SLL() : head(NULL) {}
};
#define SList(_c, _f) SLL<_c, _c::Link##_##_f>
#define SListM(_c, _m, _ml, _l) SLL<_c, _c::Link##_##_ml##_##_l>
#define forl_LL(_c, _p, _l) for (_c *_p = (_l).head; _p; _p = (_l).next(_p))

template <class C, class L = typename C::Link_link>
struct DLL {
  C *head;
  inline void push(C *e);
  inline C *pop();
  inline void remove(C *e);
  inline void insert(C *e, C *after);
  bool in(C *e) { return head == e || next(e) || prev(e); }
  void clear() { head = NULL; }
  C *&next(C *e) { return *(C **)&L::next_link(e); }
  C *&prev(C *e) { return *(C **)&L::prev_link(e); }

  DLL() : head(NULL) {}
};
#define DList(_c, _f) DLL<_c, _c::Link##_##_f>
#define DListM(_c, _m, _ml, _l) DLL<_c, _c::Link##_##_ml##_##_l>

template <class C, class L = typename C::Link_link>
struct Queue : public DLL<C, L> {
  using DLL<C, L>::head;
  C *tail;
  inline void push(C *e);
  inline C *pop();
  inline void enqueue(C *e);
  inline C *dequeue();
  inline void remove(C *e);
  inline void insert(C *e, C *after);
  inline void append(Queue<C, L> &q);
  void clear() {
    head = NULL;
    tail = NULL;
  }

  Queue() : tail(NULL) {}
};
#define Que(_c, _f) Queue<_c, _c::Link##_##_f>
#define QueM(_c, _m, _mf, _f) Queue<_c, _c::Link##_##_mf##_##_f>

template <class C, class L = typename C::Link_link>
struct CountQueue : public Queue<C, L> {
  int size;
  inline CountQueue(void) : size(0) {}
  inline void push(C *e);
  inline C *pop();
  inline void enqueue(C *e);
  inline C *dequeue();
  inline void remove(C *e);
  inline void insert(C *e, C *after);
  inline void append(CountQueue<C, L> &q);
  inline void append_clear(CountQueue<C, L> &q);
};
#define CountQue(_c, _f) CountQueue<_c, _c::Link##_##_f>
#define CountQueM(_c, _m, _mf, _f) CountQueue<_c, _c::Link##_##_mf##_##_f>

template <class C, class A = DefaultAlloc>
struct ConsCell : public gc {
  C car;
  ConsCell *cdr;
  ConsCell(C acar, ConsCell *acdr) : car(acar), cdr(acdr) {}
  ConsCell(C acar) : car(acar), cdr(NULL) {}
  ConsCell(ConsCell *acdr) : cdr(acdr) {}
  static void *operator new(size_t size) { return A::alloc(size); }
  static void operator delete(void *p, size_t size) { A::free(p); }
};

template <class C, class A = DefaultAlloc>
struct List : public gc {
  ConsCell<C, A> *head;
  C first() {
    if (head)
      return head->car;
    else
      return 0;
  }
  C car() { return first(); }
  ConsCell<C, A> *rest() {
    if (head)
      return head->cdr;
    else
      return 0;
  }
  ConsCell<C, A> *cdr() { return rest(); }
  void push(C a) { head = new ConsCell<C, A>(a, head); }
  void push() { head = new ConsCell<C, A>(head); }
  C pop() {
    C a = car();
    head = cdr();
    return a;
  }
  void clear() { head = NULL; }
  void reverse();
  List(C acar) : head(new ConsCell<C, A>(acar)) {}
  List(C a, C b) : head(new ConsCell<C, A>(a, new ConsCell<C, A>(b))) {}
  List(C a, C b, C c) : head(new ConsCell<C, A>(a, new ConsCell<C, A>(b, new ConsCell<C, A>(c)))) {}
  List() : head(0) {}
};
#define forc_List(_c, _p, _l) \
  if ((_l).head)              \
    for (_c *_p = (_l).head; _p; _p = _p->cdr)

/* IMPLEMENTATION */

template <class C, class L>
inline void SLL<C, L>::push(C *e) {
  next(e) = head;
  head = e;
}

template <class C, class L>
inline C *SLL<C, L>::pop() {
  C *ret = head;
  if (head) {
    head = next(head);
    next(ret) = NULL;
  }
  return ret;
}

template <class C, class L>
inline void DLL<C, L>::push(C *e) {
  if (head) prev(head) = e;
  next(e) = head;
  head = e;
}

template <class C, class L>
inline C *DLL<C, L>::pop() {
  C *ret = head;
  if (ret) {
    head = next(ret);
    if (head) prev(head) = NULL;
    next(ret) = NULL;
    return ret;
  } else
    return NULL;
}

template <class C, class L>
inline void DLL<C, L>::remove(C *e) {
  if (!head) return;
  if (e == head) head = next(e);
  if (prev(e)) next(prev(e)) = next(e);
  if (next(e)) prev(next(e)) = prev(e);
  prev(e) = NULL;
  next(e) = NULL;
}

template <class C, class L>
inline void DLL<C, L>::insert(C *e, C *after) {
  if (!after) {
    push(e);
    return;
  }
  prev(e) = after;
  next(e) = next(after);
  next(after) = e;
  if (next(e)) prev(next(e)) = e;
}

template <class C, class L>
inline void Queue<C, L>::push(C *e) {
  DLL<C, L>::push(e);
  if (!tail) tail = head;
}

template <class C, class L>
inline C *Queue<C, L>::pop() {
  C *ret = DLL<C, L>::pop();
  if (!head) tail = NULL;
  return ret;
}

template <class C, class L>
inline void Queue<C, L>::insert(C *e, C *after) {
  DLL<C, L>::insert(e, after);
  if (!tail)
    tail = head;
  else if (tail == after)
    tail = e;
}

template <class C, class L>
inline void Queue<C, L>::remove(C *e) {
  if (tail == e) tail = prev(e);
  DLL<C, L>::remove(e);
}

template <class C, class L>
inline void Queue<C, L>::append(Queue<C, L> &q) {
  if (!head) {
    head = q.head;
    tail = q.tail;
  } else {
    if (q.head) {
      this->next(tail) = q.head;
      this->prev(q.head) = tail;
      tail = q.tail;
    }
  }
}

template <class C, class L>
inline void Queue<C, L>::enqueue(C *e) {
  if (tail)
    insert(e, tail);
  else
    push(e);
}

template <class C, class L>
inline C *Queue<C, L>::dequeue() {
  return pop();
}

template <class C, class L>
inline void CountQueue<C, L>::push(C *e) {
  Queue<C, L>::push(e);
  size++;
}

template <class C, class L>
inline C *CountQueue<C, L>::pop() {
  C *ret = Queue<C, L>::pop();
  if (ret) size--;
  return ret;
}

template <class C, class L>
inline void CountQueue<C, L>::remove(C *e) {
  Queue<C, L>::remove(e);
  size--;
}

template <class C, class L>
inline void CountQueue<C, L>::enqueue(C *e) {
  Queue<C, L>::enqueue(e);
  size++;
}

template <class C, class L>
inline C *CountQueue<C, L>::dequeue() {
  return pop();
}

template <class C, class L>
inline void CountQueue<C, L>::insert(C *e, C *after) {
  Queue<C, L>::insert(e, after);
  size++;
}

template <class C, class L>
inline void CountQueue<C, L>::append(CountQueue<C, L> &q) {
  Queue<C, L>::append(q);
  size += q.size;
};

template <class C, class L>
inline void CountQueue<C, L>::append_clear(CountQueue<C, L> &q) {
  append(q);
  q.head = q.tail = 0;
  q.size = 0;
}

template <class C, class A>
void List<C, A>::reverse() {
  ConsCell<C, A> *n, *t;
  for (ConsCell<C, A> *p = head; p; p = n) {
    n = p->cdr;
    p->cdr = t;
    t = p;
  }
  head = t;
}

void test_list();

#endif
