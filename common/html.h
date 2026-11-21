#ifndef _ifa_html_H_
#define _ifa_html_H_

#include "common.h"
#include <stdio.h>
#include "vec.h"

class Sym;
class Fun;
class FA;
class PNode;
class EntrySet;
class CreationSet;
class AEdge;
class MPosition;
class ATypeViolation;
class AVar;
class AType;

extern int ifa_dump_html;
extern int ifa_open_html;
extern int ifa_html_max_size_mb;
extern int ifa_html_pass_limit_mb;
extern int ifa_dump_json;

// True when any explorer output was asked for. Every recording hook in
// ifa/analysis tests this and nothing else, so with no output requested
// the hooks cost one branch each.
inline bool html_telemetry_on() { return ifa_dump_html || ifa_dump_json || ifa_open_html; }

// Write <out_base>.html and/or <out_base>.json. `source_fn` is the program
// as the user named it (read for the source view); `out_base` is where
// codegen wrote <out_base>.c / .ll -- they differ under pyc -x, where the
// outputs live in the cache entry.
void dump_html(FA *fa, cchar *source_fn, cchar *out_base);
void html_record_fa_pass(FA *fa, int pass);
void html_finalize_fa_pass(FA *fa, int pass);
void html_record_es_split(FA *fa, int stage, cchar *sub_kind, EntrySet *es, AVar *trigger_av, MPosition *avpos,
                          int fsetters, int fmark, const Vec<AEdge *> &before_edges,
                          const Vec<EntrySet *> &before_to, int aes_before,
                          CreationSet *demand_cs = nullptr, ATypeViolation *viol = nullptr,
                          cchar *extra_note = nullptr);
void html_record_cs_split(FA *fa, int stage, cchar *sub_kind, CreationSet *cs, CreationSet *new_cs, bool is_new_cs,
                          const Vec<AVar *> &moved_defs, cchar *sig_or_reason = nullptr,
                          CreationSet *demand_cs = nullptr, AVar *trigger_av = nullptr);
void html_record_const_demand(FA *fa, int stage, EntrySet *es, MPosition *pos, const Vec<CreationSet *> &const_keys,
                              bool has_nonconst, ATypeViolation *why);
void html_record_arity_demote(FA *fa, const Vec<CreationSet *> &group, Sym *common_basic);

void dump_sym_name(FILE *fp, Sym *s);
void dump_fun_list(FILE *fp, Vec<Fun *> &funs);

#endif
