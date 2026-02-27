
#ifndef _ifa_common_common_H_
#define _ifa_common_common

#ifdef LEAK_DETECT
#define GC_DEBUG
#include "gc.h"
#define MEM_INIT() GC_INIT()
#define MALLOC(_n) GC_MALLOC(_n)
#define REALLOC(_p, _n) GC_REALLOC((_p), (_n))
#define MEMALIGN(_p, _n, _a) _p = GC_MALLOC(_n)
#define CHECK_LEAKS() GC_gcollect()
#define FREE(_p) GC_FREE(_p)
#define DELETE(_x)
#else
#ifdef USE_GC
#include "gc_cpp.h"
#define MEM_INIT() GC_INIT()
#define MALLOC(_n) GC_MALLOC(_n)
#define REALLOC(_p, _n) GC_REALLOC((_p), (_n))
#define MEMALIGN(_p, _n, _a) _p = GC_MALLOC(_n)
#define FREE(_x) (void)(_x)
#define DELETE(_x) (void)(_x)
#else
#define MEM_INIT()
#define MALLOC ::malloc
#define REALLOC ::realloc
#define MEMALIGN(_p, _a, _n) ::posix_memalign((void **)&(_p), (_a), (_n))
#define FREE ::free
#define DELETE(_x) delete _x
class gc {};
#endif
#endif

#define round2(_x, _n) ((_x + ((_n)-1)) & ~((_n)-1))
#define tohex1(_x) ((((_x)&15) > 9) ? (((_x)&15) - 10 + 'A') : (((_x)&15) + '0'))
#define tohex2(_x) ((((_x) >> 4) > 9) ? (((_x) >> 4) - 10 + 'A') : (((_x) >> 4) + '0'))
#define numberof(_x) ((sizeof(_x)) / (sizeof((_x)[0])))

#ifdef EXTERN
#define EXTERN_INIT(_x) = _x
#define EXTERN_ARGS(_x) _x
#else
#define EXTERN_INIT(_x)
#define EXTERN_ARGS(_x)
#define EXTERN extern
#endif

#define PERROR(_s) \
  do {             \
    perror(_s);    \
    exit(1);       \
  } while (0)
#define RETURN(_x) \
  do {             \
    result = _x;   \
    goto Lreturn;  \
  } while (0)

#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS 1
#endif

#define _GNU_SOURCE 1

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <limits.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/uio.h>
#include <unistd.h>
#include <fcntl.h>
#include <time.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <dirent.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <float.h>
#include <inttypes.h>

typedef int8_t int8;
typedef uint8_t uint8;
typedef uint8 byte;
typedef int32_t int32;
typedef uint32_t uint32;
typedef int16_t int16;
typedef uint16_t uint16;
typedef int64_t int64;
typedef uint64_t uint64;
typedef __int128_t int128;
typedef __uint128_t uint128;
/* typedef uint32 uint; * already part of most systems */

typedef const char cchar;
typedef char *charptr_t;
typedef const char *ccharptr_t;

#include "arg.h"
#include "config.h"
#include "defalloc.h"
#include "list.h"
#include "log.h"
#include "map.h"
#include "misc.h"
#include "service.h"
#include "timer.h"
#include "unit.h"
#include "util.h"
#include "vec.h"

#endif
