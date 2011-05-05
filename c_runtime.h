#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>

#include "gc.h"
#define MALLOC GC_MALLOC
#define REALLOC GC_REALLOC
#define FREE(_x)
#define MEM_INIT() GC_INIT()

typedef char int8;
typedef unsigned char uint8;
typedef int int32;
typedef unsigned int uint32;
typedef long long int64;
typedef unsigned long long uint64;
typedef short int16;
typedef unsigned short uint16;
#ifdef __APPLE__
typedef uint32 uint;
#endif
typedef float float32;
typedef double float64;
typedef struct { float32 r; float32 i; } complex32;
typedef struct { float64 r; float64 i; } complex64;

typedef void *_CG_symbol;
typedef void *_CG_function;
typedef void *_CG_tuple;
typedef void *_CG_list;
typedef void *_CG_vector;
typedef void *_CG_continuation;
typedef void *_CG_any;
typedef void *_CG_null;
typedef void *_CG_void;
typedef void *_CG_void_type;
typedef void *_CG_object;
typedef int _CG_int;
typedef uint8 _CG_bool;
typedef uint8 _CG_uint8;
typedef uint16 _CG_uint16;
typedef uint32 _CG_uint32;
typedef uint64 _CG_uint64;
typedef int8 _CG_int8;
typedef int16 _CG_int16;
typedef int32 _CG_int32;
typedef int64 _CG_int64;
typedef float32 _CG_float32;
typedef float64 _CG_float64;
typedef complex32 _CG_complex32;
typedef complex64 _CG_complex64;
typedef char * _CG_string;
typedef void *_CG_ref;
typedef void *_CG_fun;
typedef void *_CG_nil_type;
#define _CG_reply _CG_symbol
#define _CG_primitive _CG_symbol
#define _CG_make_tuple _CG_symbol
#define _CG_Symbol(_x, _y) ((void*)(uintptr_t)_x)
#define _CG_String(_x) ((char*)_x)
#define null ((void*)0)
#define bool int
#define True 1
#define False 0
#define __init ((void*)0)
#define nil_type 0

struct _CG_list_struct {
  uint32 total_len;
  uint32 len;
  void *ptr;
  char data[4]; // preallocated space
};
#define SIZEOF_LIST_HEADER (sizeof(void*)+8)
  
/*
  Lists and Tuples      

  Tuples are stored as a pointer directly to the structure
  containing the tuple elements.

  Lists are stored as _CG_list_struct and a _CG_list 
  is a pointer to &((_CG_list_struct*)list)->x[0]
  
  This makes immutable/constant Lists and Tuples compatible
  and puts the elements of such lists in the same cache line as
  the list information.
*/

#define _CG_TUPLE_TO_LIST_FUN(_s, _n) \
static inline _CG_list _CG_to_list(_CG_ps##_s p) { \
  _CG_list x = _CG_ptr_to_list(MALLOC(SIZEOF_LIST_HEADER+sizeof(_CG_s##_s))); \
  _CG_list_len(x) = _n; \
  _CG_list_total_len(0,x) = _n;    \
  _CG_list_ptr(x) = _CG_list_data(x); \
  memcpy(x, p, sizeof(_CG_s##_s)); \
  return x; \
}

static inline char * _CG_format_string(char *str, ...) {
  int l = strlen(str) + 24;
  char *s = 0;
  va_list ap;
  while (1) {
    va_start(ap, str);
    s = (char*)GC_MALLOC(l);
    int ll = vsnprintf(s, l, str, ap);
    if (ll < l-1) break;
    l = l * 2;
  }
  return s;
}

static inline void * _CG_prim_primitive_clone(void *p, size_t s) {
  void *x = GC_MALLOC(s);
  memcpy(x, p, s);
  return x;
}

static inline char * _CG_strcat(char *a, char *b) {
  int la = strlen(a), lb = strlen(b);
  char *x = (char*)GC_MALLOC(la + lb + 1);
  memcpy(x, a, la);
  memcpy(x + la, b, lb);
  x[la + lb] = 0;
  return x;
}

static inline char * _CG_char_from_string(void *s, int i) {
  char *x = (char*)GC_MALLOC(2);
  x[0] = ((char*)s)[i];
  x[1] = 0;
  return x;
}

static inline char *_CG_prim_primitive_to_string(double d) {
  char s[100], *p = s;
  snprintf(s, 100, "%.17g", d);
  while (*p) if (*p < '0' || *p > '9') break; else p++;
  if (!*p) {
    *p++ = '.';
    *p++ = '0';
  } else 
    while (*p) p++;
  char *r = (char*)MALLOC(p-s);
  memcpy(r, s, p-s);
  return r;
}

static inline char *_CG_prim_primitive_to_string(int i) {
  char s[100];
  snprintf(s, 100, "%d", i);
  int l = strlen(s);
  char *r = (char*)MALLOC(l+1);
  memcpy(r, s, l+1);
  return r;
}

static inline int _CG_float_printf(double d, bool ln) {
  char *s = _CG_prim_primitive_to_string(d);
  fputs(s, stdout);
  if (ln) fputs("\n", stdout);
}

#define _CG_list_to_struct(_l) ((_CG_list_struct*)(((char*)(_l))-SIZEOF_LIST_HEADER))
#define _CG_list_len(_l) (_CG_list_to_struct(_l)->len)
#define _CG_list_total_len(_c, _l) (_CG_list_to_struct(_l)->total_len)
#define _CG_list_ptr(_l) (_CG_list_to_struct(_l)->ptr)
#define _CG_list_data(_l) (&_CG_list_to_struct(_l)->data[0])
#define _CG_prim_len(_c, _l) ((_l)?_CG_list_len(_l):0)
#define _CG_ptr_to_list(_l) ((_CG_list)(((char*)(_l))+SIZEOF_LIST_HEADER))
static inline _CG_list _CG_to_list(_CG_list l) { return l; }

static inline _CG_list _CG_list_add_internal(_CG_list l1, _CG_list l2, uint32 size1, uint32 size2) {
  uint32 s1 = _CG_prim_len(0,l1), s2 = _CG_prim_len(0,l2);
  uint32 size = size1 ? size1 : size2;
  _CG_list x = (_CG_list)MALLOC(size * s1 * s2);
  if (s1)
    memcpy(x, _CG_list_ptr(l1), s1 * size);
  if (s2)
    memcpy(((char*)x) + s1 * size, _CG_list_ptr(l2), s2 * size);
  _CG_list_len(l1) = s1 + s2;
  _CG_list_total_len(0,l1) = s1 + s2;
  _CG_list_ptr(l1) = x;
  return l1;
}

static inline _CG_list _CG_list_resize_internal(_CG_list l1, uint32 size1, uint32 new_len) {
  uint32 s1 = _CG_prim_len(0,l1);
  _CG_list x = new_len ? (_CG_list)MALLOC(size1 * new_len) : 0;
  uint32 y = s1 < new_len ? s1 : new_len;
  if (y)
    memcpy(x, _CG_list_ptr(l1), s1 * size1);
  if (new_len > s1)
    memset(((char*)x) + s1 * size1, 0, (new_len - s1) * size1);
  _CG_list_len(l1) = new_len;
  _CG_list_total_len(0,l1) = new_len;
  _CG_list_ptr(l1) = x;
  return l1;
}

static inline _CG_list _CG_list_mult_internal(_CG_list l1, uint32 l, uint32 size) {
  if (!l) return 0;
  uint32 s1 = _CG_prim_len(0,l1);
  _CG_list x = _CG_ptr_to_list((_CG_list)MALLOC(size * s1 * l + SIZEOF_LIST_HEADER));
  _CG_list_len(x) = s1 * l;
  _CG_list_total_len(0,x) = s1 * l;
  _CG_list_ptr(x) = x;
  for (int i = 0; i < l; i++)
    memcpy(((char*)x) + (i * size * s1), _CG_list_ptr(l1), s1 * size);
  return x;
}

static inline _CG_list _CG_list_getslice_internal(_CG_list v, uint32 size, int32 l, int32 h, int32 s) {
  uint32 len = _CG_prim_len(0,v);
  if (l > len) l = len;
  if (l < 0) { 
    l = len + l;
    if (l < 0) l = 0;
  }
  if (h > len) h = len;
  if (h < 0) {
    h = len + h;
    if (h < 0) h = 0;
  }
  if (l > h) h = l; 
  int n = h - l;
  n = n / s;
  _CG_list x = _CG_ptr_to_list((_CG_list)MALLOC(size * n + SIZEOF_LIST_HEADER));
  _CG_list_len(x) = n;
  _CG_list_total_len(0,x) = n;
  _CG_list_ptr(x) = x;
  if (n) {
    if (s == 1)
      memcpy(x, ((char*)_CG_list_ptr(v)) + l * size, n * size);
    else
      for (int i = 0; i < n; i++)
        memcpy(((char*)x) + i * size, ((char*)_CG_list_ptr(v)) + (l + i) * size, size);
  }
  return x;
}

static inline _CG_list _CG_list_setslice_internal(_CG_list l1, uint32 size, int32 l, int32 h, _CG_list l2) {
  uint32 len1 = _CG_prim_len(0,l1), len2 = _CG_prim_len(0,l2);
  if (l > len1) l = len1;
  if (l < 0) {
    l = len1 + l;
    if (l < 0) l = 0;
  }
  if (h > len1) h = len1;
  if (h < 0) {
    h = len1 + h;
    if (h < 0) h = 0;
  }
  if (l > h) h = l; 
  int s = h - l; // size to delete
  s = len1 - s; // size to save
  int new_s = s + len2; // new size
  _CG_list p1 = _CG_list_ptr(l1);
  _CG_list x = (_CG_list)MALLOC(size * new_s);
  _CG_list_len(l1) = new_s;
  _CG_list_total_len(0,l1) = new_s;
  _CG_list_ptr(l1) = x;
  char *p = (char*)x;
  if (l) {
    memcpy(p, ((char*)p1), l * size);
    p += l * size;
  }
  if (len2) {
    memcpy(p, _CG_list_ptr(l2), len2 * size);
    p += len2 * size;
  }
  int sh = len1 - h;
  if (sh) {
    memcpy(p, ((char*)p1) + h * size, sh * size);
    p += sh * size;
  }
  return l1;
}

static inline void *_CG_prim_tuple_list_internal(uint s, uint n) {
  _CG_list x = _CG_ptr_to_list(GC_MALLOC(s + SIZEOF_LIST_HEADER));
  _CG_list_len(x) = n;
  _CG_list_total_len(0,x) = n;
  _CG_list_ptr(x) = x;
  return x;
}

#define _CG_string_len(_s) strlen(_s)
#define _CG_prim_tuple_list(_c, _n) (_c)(_CG_prim_tuple_list_internal(sizeof(*((_c)0)), _n))
#define _CG_prim_list(_e, _n) _CG_prim_tuple_list_internal(sizeof(_e), _n)
#define _CG_prim_tuple(_c, _n) (_c)GC_MALLOC(sizeof(*((_c)0)))
#define _CG_list_add(_l1, _l2, _s1, _s2) (_CG_list_add_internal(_CG_to_list(_l1), _CG_to_list(_l2), _s1, _s2))
#define _CG_list_resize(_l1, _s1, _new_len) (_CG_list_resize_internal(_CG_to_list(_l1), _s1, _new_len))
#define _CG_list_mult(_l1, _l, _s) (_CG_list_mult_internal(_CG_to_list(_l1), _l, _s))
#define _CG_list_getslice(_l, _s, _lower, _upper, _step) (_CG_list_getslice_internal(_CG_to_list(_l), _s, _lower, _upper, _step))
#define _CG_list_setslice(_l1, _s, _lower, _upper, _l2) (_CG_list_setslice_internal(_l1, _s, _lower, _upper, _CG_to_list(_l2)))
#define _CG_prim_coerce(_t, _v) ((_t)_v)
#define _CG_prim_closure(_c) (_c)GC_MALLOC(sizeof(*((_c)0)))
#define _CG_prim_vector(_c, _n) (void*)GC_MALLOC(sizeof(_c*) * _n)
#define _CG_prim_new(_c) (_c) GC_MALLOC(sizeof(*((_c)0)))
#define _CG_prim_clone(_c) _CG_prim_primitive_clone(_c, sizeof(*(_c)))
#define _CG_prim_reply(_s, _c, _r) return _r
#define _CG_prim_primitive(_p, _x) printf("%d\n", (unsigned int)(uintptr_t)_x);
#define _CG_prim_add(_a, _op, _b) ((_a) + (_b))
#define _CG_prim_subtract(_a, _op, _b) ((_a) - (_b))
#define _CG_prim_rsh(_a, _op, _b) ((_a) >> (_b))
#define _CG_prim_lsh(_a, _op, _b) ((_a) << (_b))
#define _CG_prim_mult(_a, _op, _b) ((_a) * (_b))
#define _CG_prim_mod(_a, _op, _b) ((_a) % (_b))
#define _CG_prim_pow(_a, _op, _b) (pow((_a),(_b)))
#define _CG_prim_div(_a, _op, _b) ((_a) / (_b))
#define _CG_prim_and(_a, _op, _b) ((_a) & (_b))
#define _CG_prim_xor(_a, _op, _b) ((_a) ^ (_b))
#define _CG_prim_or(_a, _op, _b) ((_a) | (_b))
#define _CG_prim_lor(_a, _op, _b) ((_a) || (_b))
#define _CG_prim_land(_a, _op, _b) ((_a) && (_b))
#define _CG_prim_lnot(_op, _a) (!(_a))
#define _CG_prim_less(_a, _op, _b) ((_a) < (_b))
#define _CG_prim_lessorequal(_a, _op, _b) ((_a) <= (_b))
#define _CG_prim_greater(_a, _op, _b) ((_a) > (_b))
#define _CG_prim_greaterorequal(_a, _op, _b) ((_a) >= (_b))
#define _CG_prim_equal(_a, _op, _b) ((_a) == (_b))
#define _CG_prim_notequal(_a, _op, _b) ((_a) != (_b))
#define _CG_prim_paren(_f, _a) ((*(_f))((_f), (_a)))
#define _CG_prim_set(_a, _b) (_a) = (_b)
#define _CG_prim_minus(_op, _a) (- (_a))
#define _CG_prim_not(_op, _a) (~ (_a))
#define _CG_prim_strcat(_a, _op, _b) (_CG_strcat(_a, _b))
#define _CG_prim_apply(_a, _b) ((*(_a)->e0)((_a)->e1))
#define _CG_make_apply(_r, _s, _f, _a) do {     \
  _r = (_s)GC_MALLOC(sizeof(*((_s)0)));         \
  _r->e0 = _f;                                  \
  _r->e1 = _a;                                  \
} while (0)
static inline char* _CG_chr(int x) {
  unsigned char *s = (unsigned char*)MALLOC(2);
  s[0] = x;
  s[1] = 0;
  return (char*)s;
}
static inline int _CG_ord(char *x) {
  if (x)
    return *(unsigned char*)x;
  else
    return 0;
}
