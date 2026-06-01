// SPDX-License-Identifier: BSD-3-Clause
// Minimal IFACallbacks for the IFA test harness. No frontend
// semantics — just `new_Sym` + the required pure virtuals. Tests can
// instantiate via `ifa_init(new IRCallbacks)`.
#pragma once
#include "ifa.h"

class IRCallbacks : public IFACallbacks {
 public:
  Sym *new_Sym(cchar *name) override;
};
