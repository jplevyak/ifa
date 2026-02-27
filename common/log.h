#ifndef _log_H
#define _log_H

/*
  Provide application and error logging.  Optionally, logging points can be turned
  on or off with config variables.

  Fast version (for the critical path)

  1. At the top of a source file or as EXTERN in a header, declare a LogTag for
       each log entry type:
    DEF_ALOG(connection, new);

  2. At each point where you want to use the log, call the defined object:
    alog_MAIN_new_connection("New connection open: %d.%d.%d.%d", ip[0], ip[1], ip[2], ip[3]);

  Slower version (for off the critical path)

  1. Cache the configuration value using a hidden static variable

    ALOG(connection, new)("New connection open: %d.%d.%d.%d", ip[0], ip[1], ip[2], ip[3]);

  NOTE: the C++ standard requires that static variables including the one
    hidden by the alog macro be initialized the first time the function is called.
    This results in an extra hidden conditional at the head of the function which
    is additional overhead for that function each time it is called.

  Application Log
    alog("new connection open");
  Error logs
    elog("bad error log");
*/

enum LogLevel {
  LOG_LEVEL_DEBUG,
  LOG_LEVEL_INFO,
  LOG_LEVEL_NOTICE,
  LOG_LEVEL_WARNING,
  LOG_LEVEL_ERROR,
  LOG_LEVEL_CRITICAL,
  LOG_LEVEL_ALERT,
  LOG_LEVEL_EMERGENCY
};

#define LOG_LEVEL_DEFAULT LOG_LEVEL_DEBUG

class Log {
 public:
  cchar *name;
  FILE *fp;
  LogLevel level;
  int default_active;
  void reset();
  int vlog(LogLevel l, cchar *format, va_list ap);
  int operator()(cchar *format, ...);
  int operator()(LogLevel l, cchar *format, ...);
  Log(cchar *aname, int adefault_active = 0);
};

class LocalLogTag {
 public:
  int active;
  Log *log;
  LocalLogTag(Log *alog, cchar *t1, cchar *t2 = 0, cchar *t3 = 0);
  int operator()(cchar *format, ...);
  int operator()(LogLevel l, cchar *format, ...);
};

class GlobalLogTag {
 public:
  int active;
  Log *log;
  cchar *t1, *t2, *t3;
  GlobalLogTag(Log *alog, cchar *t1, cchar *t2 = 0, cchar *t3 = 0);
  int operator()(cchar *format, ...);
  int operator()(LogLevel l, cchar *format, ...);
};

EXTERN Log alog EXTERN_ARGS(("alog", 0));
EXTERN Log elog EXTERN_ARGS(("elog", 1));

#define DEF_ALOG(_t1, _t2) GlobalLogTag alog_##_t1##_##_t2(&alog, #_t1, #_t2)
#define ALOG(_t1, _t2)                                              \
  static LocalLogTag ALOG_LOCAL__##_t1##__##_t2(&alog, #_t1, #_t2); \
  ALOG_LOCAL__##_t1##__##_t2

void global_tag_startup(void *data);

inline LocalLogTag::LocalLogTag(Log *alog, cchar *t1, cchar *t2, cchar *t3) : active(0), log(alog) {
  int_config(DYNAMIC_CONFIG, &active, log->default_active, t1, t2, t3);
}

inline int LocalLogTag::operator()(cchar *format, ...) {
  if (!active) return 0;
  va_list ap;
  va_start(ap, format);
  return log->vlog(LOG_LEVEL_DEFAULT, format, ap);
}

inline int LocalLogTag::operator()(LogLevel l, cchar *format, ...) {
  if (!active) return 0;
  va_list ap;
  va_start(ap, format);
  return log->vlog(l, format, ap);
}

inline GlobalLogTag::GlobalLogTag(Log *alog, cchar *at1, cchar *at2, cchar *at3)
    : active(0), log(alog), t1(at1), t2(at2), t3(at3) {
  config_callback(global_tag_startup, (void *)this);
}

inline int GlobalLogTag::operator()(cchar *format, ...) {
  if (!active) return 0;
  va_list ap;
  va_start(ap, format);
  return log->vlog(LOG_LEVEL_DEFAULT, format, ap);
}

inline int GlobalLogTag::operator()(LogLevel l, cchar *format, ...) {
  if (!active) return 0;
  va_list ap;
  va_start(ap, format);
  return log->vlog(l, format, ap);
}

inline int Log::vlog(LogLevel l, cchar *format, va_list ap) {
  if (l < level) return 0;
  char *t = (char*)malloc(strlen(format) + 4);
  strcpy(t, format);
  strcat(t, "\n");
  int result = vfprintf(fp, t, ap);
  va_end(ap);
  free(t);
  return result;
}

inline int Log::operator()(LogLevel l, cchar *format, ...) {
  va_list ap;
  va_start(ap, format);
  return vlog(l, format, ap);
}

inline int Log::operator()(cchar *format, ...) {
  va_list ap;
  va_start(ap, format);
  return vlog(LOG_LEVEL_DEFAULT, format, ap);
}

#endif
