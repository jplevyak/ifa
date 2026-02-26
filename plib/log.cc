#include "plib.h"

static cchar *log_level_names[] = {"debug", "info", "notice", "warning", "error", "critical", "alert", "emergency"};

void global_tag_startup(void *data) {
  GlobalLogTag *t = (GlobalLogTag *)data;
  int_config(DYNAMIC_CONFIG, &t->active, t->log->default_active, t->t1, t->t2, t->t3);
}

Log::Log(cchar *aname, int adefault_active)
    : name(aname), fp(stderr), level(LOG_LEVEL_DEBUG), default_active(adefault_active) {}

static class LogService : public Service {
  void reinit() {
    alog.reset();
    elog.reset();
  }
} log_service;

void Log::reset() {
  cchar *fn = 0, *ll = 0;
  string_config(GET_CONFIG, &fn, "stderr", name, "log", "filename");
  if (!fn || !STRCMP(fn, "stderr"))
    fp = stderr;
  else if (!STRCMP(fn, "stdout"))
    fp = stdout;
  else {
    fp = fopen(fn, "a");
    if (!fp) {
      fp = stderr;
      elog("unable to open log '%s' filename '%s'", name, fn);
    }
  }
  string_config(GET_CONFIG, &ll, "DEBUG", name, "log", "level");
  if (ll) {
    for (int i = 0; i < (int)numberof(log_level_names); i++)
      if (!strcasecmp(ll, log_level_names[i])) {
        level = (LogLevel)i;
        goto Lfound;
      }
    elog("unknown log level '%s' for log '%s'\n", ll, name);
  }
Lfound:;
}

static DEF_ALOG(log, test);

static class ALogUnitTest : UnitTest {
 public:
  int run() {
    ALOG(log, test)("testing %d, %d, %d...", 1, 2, 3);
    alog_log_test("testing %d, %d, %d...", 4, 5, 6);
    ALOG(log, test3)(LOG_LEVEL_ERROR, "testing debug...");
    return 0;
  }
  ALogUnitTest() : UnitTest("alog unit test") {}
} alog_unit_test;
