#include "common.h"

UnitTest *UnitTest::registered = 0;

UnitTest::UnitTest(cchar *aname, unit_test_fn_t afn) {
  name = aname;
  fn = afn;
  this->next = registered;
  registered = this;
}

int UnitTest::err(cchar *format, ...) {
  va_list va;
  va_start(va, format);
  vprintf(format, va);
  return 1;
}

int UnitTest::run_all() {
  int passed = 0, failed = 0;
  for (UnitTest *s = registered; s; s = s->next)
    if (!s->run()) {
      passed++;
      printf("PASSED %s\n", s->name);
    } else {
      failed++;
      printf("FAILED %s\n", s->name);
    }
  printf("UnitTest: %d PASSED %d FAILED, %s\n", passed, failed, !failed ? "SUCCESS" : "FAILURE");
  fflush(stdout);
  return failed;
}

static int my_test() {
  return 0;  // success
}

UNIT_TEST_FUN(my_test);

static class MyUnitTest : public UnitTest {
 public:
  int run() { return 0; }
  MyUnitTest() : UnitTest("my unit test") {}
} my_unit_test;
