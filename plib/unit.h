#ifndef _unit_h_
#define _unit_h_

#include <stdarg.h>

#define UNIT_TEST_FUN(_f) static UnitTest StaticUnitTest_##_f(#_f, _f)

/*
  Inherit from UnitTest

    static class MyUnitTest : public UnitTest { public:
      int run() {
        if ...
          return 0;
        else
          return err("failed!\n");
      }
      MyUnitTest() : UnitTest("my test") {}
    } my_unit_test;

  Use a function pointer

    int my_test() {
        if ...
          return 0;
        else {
          printf("failed!\n");
          return 1;
        }
    }
    UNIT_TEST_FUN(my_test);
*/

typedef int (*unit_test_fn_t)();

class UnitTest {
 public:
  cchar *name;
  unit_test_fn_t fn;
  virtual int run() { return fn(); }  // 0 is success, otherwise failure
  int err(cchar *format, ...);

  static UnitTest *registered;
  static int run_all();
  UnitTest *next;
  UnitTest(cchar *aname, unit_test_fn_t fn = 0);
};

#endif
