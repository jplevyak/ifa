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

void dump_html(FA *fa, cchar *fn, cchar *mktree_dir);
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
