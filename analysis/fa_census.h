// SPDX-License-Identifier: BSD-3-Clause
//
// ifa/167 step 1: the FA census -- the diagnostic counters the analysis
// increments at its decision points and prints somewhere else entirely.
//
// These were 101 file-scope statics in fa.cc. They are not analysis state:
// nothing in this struct is read to DECIDE anything, only to report what
// was decided. (The seven statics that ARE read to decide -- cur_split_stage,
// ifa_selective, cur_split_type_only, fa_selective_armed,
// and the two bt_noms_* -- stayed in fa.cc.)
//
// They live here for two reasons. The printers that consume them can now
// move out of fa.cc without dragging the algorithm along, which is ifa/167
// step 2. And a counter that nothing increments, or that nothing reads, is
// visible in a 150-line file in a way it was not in a 16.8k-line one --
// seven such counters were found dead in the survey that preceded this
// (`ed_split` printed a permanent 0; the four `stage2_*_time` accumulators
// made an entire diagnostic unreachable).
//
// Increment freely; this is a probe, not a contract. Nothing here may gain
// a reader that changes what the analysis does -- at that point it is
// analysis state and belongs back in fa.cc.
#ifndef _fa_census_H_
#define _fa_census_H_

#include "ifadefs.h"

// ifa/133 probe: where do CreationSets actually come from? One counter per
// creation_point route, dumped by IFA_DBG_CSROUTES at convergence.
enum CsRoute { kR_cs_map, kR_dcpa1, kR_split_parent, kR_csmold, kR_MINT, kR_count };
extern cchar *cs_route_name[kR_count];

struct FACensus {
  int cs_route_count[kR_count] = {};
  long work_edges = 0, work_sends = 0, work_escons = 0;
  int esl_hit = 0, esl_walk = 0; // ifa/133: split-parent route hits / chain walks
  int mint_in_child = 0, mint_child_novar = 0, mint_child_cmc = 0;
  int esl_reached = 0, esl_decline = 0;
  int route_saw_split = -3, route_saw_origin = -3;
  int grp_total = 0, grp_scattered = 0;
  int ck_single = 0, ck_irrep = 0, ck_samesym = 0, ck_repr = 0;
  long fa_cap_strips = 0; // ifa/131 step 1, probe only
  long ic_arg = 0, ic_ret = 0, ic_retn = 0;
  long tc_formal = 0, tc_return = 0;
  int ld_dup_es = 0, ld_dup_cs = 0, ld_churn = 0;
  long tc_seen = 0, tc_skip_rval = 0, tc_skip_lval = 0, tc_skip_cs = 0, tc_dec = 0, tc_defer = 0;
  int cl_unflagged = 0, cl_flagged = 0;
  int seed_reported = 0;
  long aes_apply_split = 0, aes_apply_split_nogrowth = 0;
  int stage_aes0 = 0, stage_acs0 = 0;
  long rc_ret_differ = 0, rc_ret_same = 0, rc_ret_one = 0, rc_local = 0, rc_other = 0;
  long rc_differ_1fun = 0, rc_differ_nfun = 0;
  long rv_formal = 0, rv_local = 0, rv_cs = 0, rv_none = 0, rv_noedge = 0;
  long wk_formal = 0, wk_cs = 0, wk_join = 0, wk_cap = 0;
  long wk_hops_formal = 0, wk_hops_cs = 0;
  long dk_static = 0, dk_funvar = 0, dk_recv = 0, dk_arg = 0, dk_nonarrow = 0, dk_noedge = 0;
  long dk_funvar_syms = 0, dk_recv_syms = 0, dk_recv_related = 0;
  long rd_formal = 0, rd_cs = 0, rd_declined_related = 0, rd_none = 0;
  long rd_cs_1def = 0, rd_cs_ndef = 0;
  long ed_seen = 0, ed_formal = 0, ed_no_formal = 0, ed_not_demanded = 0;
  long ed_had_formals = 0;
  long kd_stable = 0, kd_grew = 0, kd_shrank = 0, kd_new = 0, kd_flip = 0;
};

extern FACensus census;

#endif
