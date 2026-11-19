// SPDX-License-Identifier: BSD-3-Clause
//
// ifa/167 step 3: the FA environment levers, and the measurement behind
// each one.
//
// Every accessor here is the same five lines -- read one variable once,
// cache it, return it. What makes them worth collecting is the COMMENT on
// each: the corpus numbers that decided its default, and for several of
// them the record of what was tried and removed. Scattered through 14k
// lines at each lever's first use, that prose could not be read as a set.
// It can be here, which is what ifa/146's audit needs -- its running list
// of what has been removed and what is left is a list about this file.
//
// The rule for adding one: a lever must be able to say what it is FOR and
// what measurement sets its default. A lever that cannot is the arbitrary
// splitting ifa/146 exists to delete, and defaulting it off is not the
// remedy -- removing it is.
#ifndef _fa_flags_H_
#define _fa_flags_H_

int eslineage_enabled();
int csparentfirst_enabled();
int nilarg_enabled();
int strictviol_enabled();
int hard_reuse_enabled();
int csmold_enabled();
int csdcpa1_enabled();
int routecycle_enabled();
int routestable_enabled();
int selfprod_enabled();
int nilstore_enabled();
int cskey_enabled();
int csdefsplit_enabled();
int csladder_enabled();
int cscontent_enabled();
int esblock_enabled();
int loadbt_enabled();
int csbacktrack_enabled();

#endif
