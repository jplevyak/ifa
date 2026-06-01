// SPDX-License-Identifier: BSD-3-Clause
#include "ifadefs.h"
#include "if1.h"
#include "sym.h"
#include "testing/test_callbacks.h"

Sym *IRCallbacks::new_Sym(cchar *name) {
  Sym *s = new Sym();
  if1_register_sym(if1, s, name);
  return s;
}
