#ifndef _util_h_
#define _util_h_

#define HRTIME_NSEC 1ULL
#define HRTIME_USEC (1000 * HRTIME_NSEC)
#define HRTIME_MSEC (1000 * HRTIME_USEC)
#define HRTIME_SEC (1000 * HRTIME_MSEC)
#define HRTIME_MIN (60 * HRTIME_SEC)
#define HRTIME_HOUR (60 * HRTIME_MIN)
#define HRTIME_DAY (24 * HRTIME_HOUR)

#define STREQ(_s, _const_string) \
  ((strlen(_s) == sizeof(_const_string) - 1) && !strncmp(_s, _const_string "", sizeof(_const_string) - 1))
#define STRSUFFIX(_s, _const_string)            \
  ((strlen(_s) >= sizeof(_const_string) - 1) && \
   !strncmp(_s + strlen(_s) - (sizeof(_const_string) - 1), _const_string "", sizeof(_const_string) - 1))
#define STRPREFIX(_s, _const_string) \
  ((strlen(_s) >= sizeof(_const_string) - 1) && !strncmp(_s, _const_string "", sizeof(_const_string) - 1))
#define STRCMP(_s, _const_string) strncmp(_s, _const_string "", sizeof(_const_string) - 1)
#define STRCPY(_s, _const_string) \
  (memcpy((void *)(_s), _const_string "", sizeof(_const_string) - 1), sizeof(_const_string) - 1)
#define STRCPYZ(_s, _const_string) \
  (memcpy((void *)(_s), (_const_string ""), sizeof(_const_string)), sizeof(_const_string))
#define STRCASECMP(_s, _const_string) strncasecmp(_s, _const_string "", sizeof(_const_string) - 1)
#define MEMMEM(_s, _e, _const_string) memmem(_s, _e, _const_string "", sizeof(_const_string) - 1)
#define STRCAT(_s, _x) (strcat((char *)(_s), _x))

typedef uint64 hrtime_t;

int bind_port(int port, int protocol = SOCK_STREAM, bool nolinger = false);
int accept_socket(int socket, bool nolinger = false, bool tcp_nodelay = true, int set_buf_size = 0);
int connect_socket(in_addr_t addr, int port, int client_buf_size = 0, bool client_nodelay = 1,
                   bool client_nolinger = 0);
pthread_t create_thread(void *(*fn)(void *), void *data = 0, int stack_size = 0);
uint get_inet_addr(char *hostname);
char *memmem(char *m, char *e, char *p, int l);
hrtime_t hrtime();
void hrtime_to_ts(hrtime_t t, struct timespec *ts);
static inline hrtime_t ts_to_hrtime(struct timespec &ts) { return ts.tv_sec * HRTIME_SEC + ts.tv_nsec * HRTIME_NSEC; }
static inline double hrtime_to_sec(hrtime_t t) { return ((double)t) / ((double)HRTIME_SEC); }
static inline double hrtime_sec() { return hrtime_to_sec(hrtime()); }
void wait_for(hrtime_t t);
void wait_till(hrtime_t t);
void free_in(void *, hrtime_t);
void free_in(Vec<void *> &, hrtime_t);

// INLINE FUNCTIONS

static inline char *scpy(char *d, cchar *s) {
  while (*s) *d++ = *s++;
  return d;
}

// copy within buffer and null terminate with optional max len to copy
// return number copied not including null
static inline int strcpyn(char *ad, cchar *s, int buflen, int len = -1) {
  char *d = ad;
  if (len == 0 || buflen <= 0) return 0;
  if (len > 0 && len < buflen) buflen = len + 1;
  while (*s && d - ad < buflen - 1) *d++ = *s++;
  *d++ = 0;
  return d - ad - 1;
}

inline void init_recursive_mutex(pthread_mutex_t *mutex) {
  pthread_mutexattr_t mutexattr;
  pthread_mutexattr_init(&mutexattr);
  pthread_mutexattr_settype(&mutexattr, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(mutex, &mutexattr);
}

class xrange {
 public:
  class iterator_type {
   public:
    typedef int value_type;
    typedef int difference_type;
    typedef const value_type *pointer;
    typedef const value_type &reference;

    iterator_type(value_type value, difference_type increment) : value(value), increment(increment) {}
    explicit iterator_type(value_type value) : value(value) {}

    bool operator!=(const iterator_type &rhs) const { return value != rhs.value; }
    value_type operator++() { return value += increment; }
    operator pointer() const { return &value; }

   private:
    value_type value = 0;
    const difference_type increment = 0;
  };

  typedef iterator_type iterator;
  typedef const iterator_type const_iterator;

  explicit xrange(int last) : last_(last) {}
  xrange(int first, int last) : first_(first), last_(last) {}
  xrange(int first, int last, int increment) : first_(first), last_(last), increment_(increment) {}

  iterator begin() const { return iterator(first_, increment_); }
  iterator end() const { return iterator(last_); }

 private:
  int first_ = 0;
  int last_ = 0;
  int increment_ = 1;
};

#endif
