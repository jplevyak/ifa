#ifndef _clone_h_
#define _clone_h_

#include "ifadefs.h"

class FA;
class EntrySet;
class AType;

int clone(FA *fa);
Sym *basic_type(FA *fa, AType *t, Sym *fail);
void log_test_fa(FA *fa);
Sym *to_basic_type(Sym *);
Sym *concrete_type_set_to_type(Vec<Sym *> &t);
// ifa/120: the union-Sym ownership audit (IFA_DBG_SUMAUDIT). Reports every
// field of a clone-made Type_SUM Sym that changed since it was handed out,
// attributed to the pipeline phase named here.
void sum_audit_check(cchar *phase);

#endif
