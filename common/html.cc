#include "ifadefs.h"
#include "common.h"
#include "html.h"
#include "ast.h"
#include "builtin.h"
#include "clone.h"
#include "dead.h"
#include "dom.h"
#include "fa.h"
#include "fail.h"
#include "fun.h"
#include "if1.h"
#include "inline.h"
#include "log.h"
#include "loop.h"
#include "num.h"
#include "pattern.h"
#include "pdb.h"
#include "pnode.h"
#include "prim.h"
#include "sym.h"
#include "var.h"

#include <algorithm>
#include <fcntl.h>
#include <fstream>
#include <iomanip>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <sys/wait.h>
#include <unistd.h>
#include <vector>
#include <unordered_set>
#include <unordered_map>

int ifa_dump_html = 0;
int ifa_open_html = 0;
int ifa_html_max_size_mb = 100;
int ifa_html_pass_limit_mb = 2;
int ifa_dump_json = 0;

static const char *fa_stage_names[FA::kNumFAPassStages] = {
    "TYPE_CONFL", "MARK_TYPE", "SETTER", "SETTER_OF_SETTER", "MARK_SETTER",
    "MARK_SET_OF_SET", "VIOLATION", "PER_CS_RECV", "CSM_ELEM_CS", "CPA",
    "CS_DEF_PART", "CONST_DEMAND"};

static const char *violation_kind_name(ATypeViolation_kind k) {
  switch (k) {
    case ATypeViolation_kind::PRIMITIVE_ARGUMENT: return "PRIMITIVE_ARGUMENT";
    case ATypeViolation_kind::SEND_ARGUMENT: return "SEND_ARGUMENT";
    case ATypeViolation_kind::DISPATCH_AMBIGUITY: return "DISPATCH_AMBIGUITY";
    case ATypeViolation_kind::MEMBER: return "MEMBER";
    case ATypeViolation_kind::MATCH: return "MATCH";
    case ATypeViolation_kind::NOTYPE: return "NOTYPE";
    case ATypeViolation_kind::BOXING: return "BOXING";
    case ATypeViolation_kind::CLOSURE_RECURSION: return "CLOSURE_RECURSION";
    case ATypeViolation_kind::MAYBE_UNBOUND: return "MAYBE_UNBOUND";
    case ATypeViolation_kind::DEFINITELY_UNBOUND: return "DEFINITELY_UNBOUND";
    default: return "OTHER";
  }
}

static const char *inline_kind_name(InlineEventKind k) {
  switch (k) {
    case INLINE_SINGLE_SEND: return "SINGLE_SEND";
    case INLINE_IDENTITY: return "IDENTITY";
    case INLINE_CLOSURE: return "CLOSURE";
    case INLINE_PRIM_CHAIN: return "PRIM_CHAIN";
    default: return "OTHER";
  }
}

static std::string json_escape(const std::string &s) {
  std::ostringstream o;
  for (size_t i = 0; i < s.size(); i++) {
    char c = s[i];
    switch (c) {
      case '"': o << "\\\""; break;
      case '\\': o << "\\\\"; break;
      case '/':
        // Escape "</" as "<\/" so embedded <script type="application/json"> cannot be closed prematurely
        if (i > 0 && s[i - 1] == '<') o << "\\/";
        else o << '/';
        break;
      case '\b': o << "\\b"; break;
      case '\f': o << "\\f"; break;
      case '\n': o << "\\n"; break;
      case '\r': o << "\\r"; break;
      case '\t': o << "\\t"; break;
      default:
        if ('\x00' <= c && c <= '\x1f') {
          o << "\\u" << std::hex << std::setw(4) << std::setfill('0') << (int)(unsigned char)c;
        } else {
          o << c;
        }
    }
  }
  return o.str();
}

static std::string json_escape(cchar *s) {
  if (!s) return "";
  return json_escape(std::string(s));
}

static std::string html_escape_str(cchar *s) {
  if (!s) return "";
  std::ostringstream o;
  for (const char *p = s; *p; p++) {
    switch (*p) {
      case '&': o << "&amp;"; break;
      case '<': o << "&lt;"; break;
      case '>': o << "&gt;"; break;
      case '"': o << "&quot;"; break;
      default: o << *p; break;
    }
  }
  return o.str();
}

static cchar *resolve_fun_filename(Fun *f, cchar *fallback_fn) {
  if (!f) return "";
  if (f->ast && f->filename() && *f->filename() && strcmp(f->filename(), "<unknown>") != 0) {
    return f->filename();
  }
  if (f->sym && f->sym->ast && f->sym->filename() && *f->sym->filename() && strcmp(f->sym->filename(), "<unknown>") != 0) {
    return f->sym->filename();
  }
  if (f->sym && f->sym->fun && f->sym->fun != f && f->sym->fun->ast && f->sym->fun->filename() &&
      *f->sym->fun->filename() && strcmp(f->sym->fun->filename(), "<unknown>") != 0) {
    return f->sym->fun->filename();
  }
  if (f->sym && f->sym->name && strcmp(f->sym->name, "__main__") == 0 && fallback_fn && *fallback_fn) {
    cchar *slash = strrchr(fallback_fn, '/');
    return slash ? slash + 1 : fallback_fn;
  }
  return "";
}

static int resolve_fun_line(Fun *f) {
  if (!f) return 0;
  if (f->ast && f->line() > 0) return f->line();
  if (f->sym && f->sym->source_line() > 0) return f->sym->source_line();
  if (f->sym && f->sym->line() > 0) return f->sym->line();
  if (f->sym && f->sym->fun && f->sym->fun != f && f->sym->fun->ast && f->sym->fun->line() > 0) {
    return f->sym->fun->line();
  }
  if (f->sym && f->sym->name && strcmp(f->sym->name, "__main__") == 0) return 1;
  return 0;
}

static void serialize_atype(std::ostringstream &os, AType *t, std::unordered_set<int> *referenced_cs = nullptr) {
  if (!t) {
    os << "[]";
    return;
  }
  os << "[";
  bool first = true;
  for (CreationSet *cs : *t) {
    if (!cs || !cs->sym) continue;
    if (referenced_cs) referenced_cs->insert(cs->id);
    if (!first) os << ",";
    first = false;
    os << "{\"cs\":" << cs->id;
    os << ",\"sym\":\"" << json_escape(cs->sym->name ? cs->sym->name : "*anon*") << "\"";
    if (cs->sym->is_constant) {
      os << ",\"is_const\":true";
      if (cs->sym->constant) {
        os << ",\"val\":\"" << json_escape(cs->sym->constant) << "\"";
      }
    }
    if (cs->static_arity >= 0) {
      os << ",\"arity\":" << cs->static_arity;
    }
    os << "}";
  }
  os << "]";
}

static std::string format_atype_simple(const Vec<CreationSet *> &css) {
  if (css.n == 0) return "void";
  std::vector<std::string> parts;
  for (int i = 0; i < css.n; i++) {
    CreationSet *cs = css[i];
    if (!cs || !cs->sym) continue;
    std::string s = cs->sym->name ? cs->sym->name : "*anon*";
    if (cs->sym->is_constant && cs->sym->constant) {
      s += "=" + std::string(cs->sym->constant);
    }
    bool exists = false;
    for (const auto &p : parts) {
      if (p == s) { exists = true; break; }
    }
    if (!exists) parts.push_back(s);
  }
  if (parts.empty()) return "void";
  std::string res;
  for (size_t i = 0; i < parts.size(); i++) {
    if (i > 0) res += " | ";
    res += parts[i];
  }
  return res;
}

static std::string format_atype_simple(AType *t) {
  if (!t) return "void";
  Vec<CreationSet *> css;
  for (CreationSet *cs : *t) css.add(cs);
  return format_atype_simple(css);
}

struct RecordedArg {
  int pos;
  int av_id;
  std::string var_name;
  std::string type_str;
  std::vector<int> cs_ids;
};

struct RecordedRet {
  int av_id;
  std::string type_str;
  std::vector<int> cs_ids;
};

struct RecordedEdge {
  int id;
  int to_es;
  int pnode_id;
  std::string callee_fun;
  Fun *callee;
  EntrySet *to_es_ptr;
  PNode *pnode;
};

struct RecordedES {
  int id;
  EntrySet *es;
  Fun *fun;
  int fun_id;
  std::string fun_name;
  bool can_raise;
  bool returns;
  bool raises;
  int split_origin;
  std::vector<RecordedArg> args;
  std::vector<RecordedRet> rets;
  std::vector<RecordedEdge> out_edges;
};

struct RecordedField {
  int index;
  int av_id;
  std::string name;
  std::string type_str;
};

struct RecordedCS {
  int id;
  CreationSet *cs;
  Sym *type;
  std::string sym;
  int static_arity;
  bool no_static_arity;
  int split_origin;
  std::vector<RecordedField> fields;
};

struct RecordedViol {
  std::string kind;
  int av_id;
  int send_id;
  int contour_id;
  int fun_id;
  EntrySet *es;
  Fun *fun;
  std::string var_name;
  std::string fun_name;
  std::string type_str;
  Var *var;
};

struct RecordedAVar {
  int id;
  int var_id;
  std::string var_name;
  std::string fun_name;
  int contour_id;
  int fun_id;
  AVar *av;
  EntrySet *es_contour;
  Fun *fun;
  Var *var;
  std::string type_str;
  std::vector<int> cs_ids;
  std::vector<int> forward;
  std::vector<int> backward;
};

struct RecordedEvent {
  std::string stage;
  int splits;
  int ess_before;
  int ess_after;
  int css_before;
  int css_after;
  int viol_before;
  int viol_after;
};

struct RecordedSplitProduct {
  int id;
  bool is_new;
  int moved_count;
  std::string sig;
  EntrySet *es_ptr;
  CreationSet *cs_ptr;
};

struct RecordedSplitEdge {
  int edge_id;
  int from_es;
  int from_fun_id;
  Fun *from_fun;
  std::string from_fun_name;
  int pnode_id;
  int line;
  int old_to_es;
  int new_to_es;
  std::string arg_summary;
};

struct RecordedSplitDef {
  int av_id;
  int var_id;
  std::string var_name;
  int contour_id;
  int fun_id;
  Fun *fun;
  std::string fun_name;
  int pnode_id;
  int line;
  int old_cs;
  int new_cs;
  std::string detail;
};

struct RecordedSplit {
  int id;
  int pass_num;
  std::string stage;
  std::string sub_kind;
  std::string kind;
  int target_id;
  int target_fun_id;
  Fun *target_fun;
  std::string target_name;
  int trigger_av_id;
  int trigger_var_id;
  std::string trigger_var_name;
  std::string trigger_role;
  int trigger_contour_id;
  int trigger_fun_id;
  Fun *trigger_fun;
  std::string trigger_fun_name;
  std::string trigger_type_str;
  std::string viol_kind;
  int demand_cs_id;
  std::string demand_cs_sym;
  std::string reason;
  int stayed_count;
  int moved_count;
  std::vector<RecordedSplitProduct> products;
  std::vector<RecordedSplitEdge> affected_edges;
  std::vector<RecordedSplitDef> affected_defs;
};

struct RecordedPass {
  int pass_num;
  int fa_pass;
  int events_start_idx;
  int dup_splits;
  int cs_dup_splits;
  std::vector<RecordedEvent> events;
  std::vector<RecordedSplit> splits;
  std::vector<RecordedES> entry_sets;
  std::vector<RecordedCS> creation_sets;
  std::vector<RecordedViol> violations;
  std::vector<RecordedAVar> avars;
};

static std::vector<RecordedPass> g_recorded_passes;

static void check_env_config() {
  static bool env_checked = false;
  if (!env_checked) {
    env_checked = true;
    if (cchar *v = getenv("PYC_HTML_MAX_SIZE")) ifa_html_max_size_mb = atoi(v);
    if (cchar *v = getenv("IFA_HTML_MAX_SIZE")) ifa_html_max_size_mb = atoi(v);
    if (cchar *v = getenv("PYC_HTML_PASS_LIMIT")) ifa_html_pass_limit_mb = atoi(v);
    if (cchar *v = getenv("IFA_HTML_PASS_LIMIT")) ifa_html_pass_limit_mb = atoi(v);
    if (cchar *v = getenv("PYC_DUMP_JSON")) ifa_dump_json = atoi(v);
    if (cchar *v = getenv("IFA_DUMP_JSON")) ifa_dump_json = atoi(v);
    if (cchar *v = getenv("PYC_HTML")) ifa_dump_html = atoi(v);
    if (cchar *v = getenv("IFA_HTML")) ifa_dump_html = atoi(v);
    if (cchar *v = getenv("PYC_BROWSE")) ifa_open_html = atoi(v);
    if (cchar *v = getenv("IFA_BROWSE")) ifa_open_html = atoi(v);
    if (cchar *v = getenv("PYC_HTML_OPEN")) ifa_open_html = atoi(v);
    if (cchar *v = getenv("IFA_HTML_OPEN")) ifa_open_html = atoi(v);
  }
  if (ifa_open_html) ifa_dump_html = 1;
}

static void open_html_in_browser(const char *path) {
  if (!path || !*path) return;
  const char *browser = getenv("BROWSER");
  pid_t pid = fork();
  if (pid == 0) {
#ifdef __APPLE__
    if (!browser || !*browser) {
      execlp("open", "open", path, (char *)nullptr);
      _exit(127);
    }
#endif
    pid_t pid2 = fork();
    if (pid2 == 0) {
      int devnull = open("/dev/null", O_RDWR);
      if (devnull >= 0) {
        dup2(devnull, STDIN_FILENO);
        dup2(devnull, STDOUT_FILENO);
        dup2(devnull, STDERR_FILENO);
        if (devnull > 2) close(devnull);
      }
      if (browser && *browser) {
        execlp(browser, browser, path, (char *)nullptr);
      }
#ifdef __APPLE__
      execlp("open", "open", path, (char *)nullptr);
#else
      execlp("xdg-open", "xdg-open", path, (char *)nullptr);
#endif
      _exit(127);
    }
    _exit(0);
  } else if (pid > 0) {
    int status = 0;
    (void)waitpid(pid, &status, 0);
  }
}

static bool is_creation_set_live(CreationSet *cs, const std::unordered_set<int> &live_ref_cs_ids) {
  if (!cs) return false;
  if (cs->type && cs->type->type_live) return true;
  for (AVar *d : cs->defs) {
    if (d && d->var && d->var->live) return true;
  }
  for (AVar *iv : cs->vars) {
    if (iv && iv->var && iv->var->live) return true;
  }
  if (live_ref_cs_ids.count(cs->id)) {
    if (!cs->sym || !cs->sym->is_fun) return true;
  }
  return false;
}

void html_record_fa_pass(FA *fa, int pass) {
  if (!ifa_dump_html && !ifa_dump_json && !ifa_open_html) return;
  if (!fa) return;
  check_env_config();

  RecordedPass rp;
  rp.pass_num = (int)g_recorded_passes.size() + 1;
  rp.fa_pass = pass;
  rp.events_start_idx = fa_events_get().n;
  rp.dup_splits = fa->dup_split_attempts;
  rp.cs_dup_splits = fa->cs_dup_split_attempts;

  // Split events for this pass (updated at end of pass in html_finalize_fa_pass)
  for (FAPassEvent *e : fa_events_get()) {
    if (!e || e->pass != pass) continue;
    cchar *st_name = ((int)e->stage >= 0 && (int)e->stage < FA::kNumFAPassStages) ? fa_stage_names[(int)e->stage] : "UNKNOWN";
    RecordedEvent ev;
    ev.stage = st_name;
    ev.splits = e->splits;
    ev.ess_before = e->ess_before;
    ev.ess_after = e->ess_after;
    ev.css_before = e->css_before;
    ev.css_after = e->css_after;
    ev.viol_before = e->violations_before;
    ev.viol_after = e->violations_after;
    rp.events.push_back(ev);
  }

  // EntrySets
  for (int i = 0; i < fa->ess.n; i++) {
    EntrySet *es = fa->ess[i];
    if (!es) continue;
    RecordedES res;
    res.id = es->id;
    res.es = es;
    res.fun = es->fun;
    res.fun_id = es->fun ? es->fun->id : -1;
    res.fun_name = (es->fun && es->fun->sym && es->fun->sym->name) ? es->fun->sym->name : "*anon*";
    res.can_raise = es->can_raise;
    res.returns = es->returns;
    res.raises = es->raises;
    res.split_origin = es->split ? es->split->id : (es->split_origin ? es->split_origin->id : -1);

    for (int ai = 0; ai < es->args.n; ai++) {
      if (es->args[ai].key && es->args[ai].value) {
        AVar *av = es->args[ai].value;
        RecordedArg arg;
        arg.pos = ai;
        arg.av_id = av->id;
        arg.var_name = (av->var && av->var->sym && av->var->sym->name) ? av->var->sym->name : "";
        std::ostringstream os;
        std::unordered_set<int> cids;
        serialize_atype(os, av->out, &cids);
        arg.type_str = os.str();
        arg.cs_ids.assign(cids.begin(), cids.end());
        res.args.push_back(arg);
      }
    }

    for (int ri = 0; ri < es->rets.n; ri++) {
      AVar *av = es->rets[ri];
      if (!av) continue;
      RecordedRet ret;
      ret.av_id = av->id;
      std::ostringstream os;
      std::unordered_set<int> cids;
      serialize_atype(os, av->out, &cids);
      ret.type_str = os.str();
      ret.cs_ids.assign(cids.begin(), cids.end());
      res.rets.push_back(ret);
    }

    for (int oi = 0; oi < es->out_edges.n; oi++) {
      AEdge *ae = es->out_edges[oi];
      if (!ae) continue;
      RecordedEdge edge;
      edge.id = ae->id;
      edge.to_es = ae->to ? ae->to->id : -1;
      edge.to_es_ptr = ae->to;
      edge.pnode_id = ae->pnode ? ae->pnode->id : -1;
      edge.callee_fun = (ae->fun && ae->fun->sym && ae->fun->sym->name) ? ae->fun->sym->name : "";
      edge.callee = ae->fun;
      edge.pnode = ae->pnode;
      res.out_edges.push_back(edge);
    }
    rp.entry_sets.push_back(res);
  }

  // CreationSets
  for (int i = 0; i < fa->css.n; i++) {
    CreationSet *cs = fa->css[i];
    if (!cs) continue;
    RecordedCS rcs;
    rcs.id = cs->id;
    rcs.cs = cs;
    rcs.type = cs->type;
    rcs.sym = (cs->sym && cs->sym->name) ? cs->sym->name : "*anon*";
    rcs.static_arity = cs->static_arity;
    rcs.no_static_arity = cs->no_static_arity;
    rcs.split_origin = cs->split ? cs->split->id : (cs->split_origin ? cs->split_origin->id : -1);

    for (int fi = 0; fi < cs->vars.n; fi++) {
      AVar *fav = cs->vars[fi];
      if (!fav) continue;
      RecordedField fld;
      fld.index = fi;
      fld.av_id = fav->id;
      fld.name = (fav->var && fav->var->sym && fav->var->sym->name) ? fav->var->sym->name : "";
      std::ostringstream os;
      serialize_atype(os, fav->out);
      fld.type_str = os.str();
      rcs.fields.push_back(fld);
    }
    rp.creation_sets.push_back(rcs);
  }

  // Violations
  for (ATypeViolation *v : fa->type_violations) {
    if (!v) continue;
    RecordedViol rviol;
    rviol.kind = violation_kind_name(v->kind);
    rviol.av_id = v->av ? v->av->id : -1;
    rviol.send_id = v->send ? v->send->id : -1;
    EntrySet *ves = (v->av && v->av->contour_is_entry_set) ? (EntrySet *)v->av->contour : nullptr;
    rviol.es = ves;
    rviol.contour_id = ves ? ves->id : -1;
    rviol.fun = ves ? ves->fun : nullptr;
    rviol.fun_id = (ves && ves->fun) ? ves->fun->id : -1;
    rviol.var_name = (v->av && v->av->var && v->av->var->sym && v->av->var->sym->name) ? v->av->var->sym->name : "";
    if (ves && ves->fun && ves->fun->sym && ves->fun->sym->name) {
      rviol.fun_name = ves->fun->sym->name;
    } else if (v->av && v->av->var && v->av->var->sym && v->av->var->sym->in && v->av->var->sym->in->name) {
      rviol.fun_name = v->av->var->sym->in->name;
    }
    rviol.type_str = v->av ? format_atype_simple(v->av->out) : "";
    rviol.var = v->av ? v->av->var : (v->send ? v->send->var : nullptr);
    rp.violations.push_back(rviol);
  }

  // Active AVars & Dataflow
  for (Fun *f : fa->funs) {
    if (!f) continue;
    for (Var *v : f->fa_all_Vars) {
      if (!v) continue;
      for (int vi = 0; vi < v->avars.n; vi++) {
        if (!v->avars[vi].key || !v->avars[vi].value) continue;
        AVar *av = v->avars[vi].value;
        if (!av) continue;
        if (av->out || av->in || av->forward.n || av->backward.n) {
          RecordedAVar rav;
          rav.id = av->id;
          rav.var_id = v->id;
          rav.var_name = (v->sym && v->sym->name) ? v->sym->name : "";
          EntrySet *es_contour = av->contour_is_entry_set ? (EntrySet *)av->contour : nullptr;
          rav.es_contour = es_contour;
          rav.av = av;
          rav.contour_id = es_contour ? es_contour->id : -1;
          rav.fun_id = f->id;
          rav.fun_name = (f->sym && f->sym->name) ? f->sym->name : "";
          rav.fun = f;
          rav.var = v;
          std::ostringstream os;
          std::unordered_set<int> cids;
          serialize_atype(os, av->out, &cids);
          rav.type_str = os.str();
          rav.cs_ids.assign(cids.begin(), cids.end());
          for (int fwi = 0; fwi < av->forward.n; fwi++) {
            if (av->forward[fwi]) rav.forward.push_back(av->forward[fwi]->id);
          }
          for (int bwi = 0; bwi < av->backward.n; bwi++) {
            if (av->backward[bwi]) rav.backward.push_back(av->backward[bwi]->id);
          }
          rp.avars.push_back(rav);
        }
      }
    }
  }

  g_recorded_passes.push_back(rp);
}

void html_finalize_fa_pass(FA *fa, int pass) {
  (void)pass;
  if (!ifa_dump_html && !ifa_dump_json && !ifa_open_html) return;
  if (!fa || g_recorded_passes.empty()) return;
  RecordedPass &rp = g_recorded_passes.back();
  rp.dup_splits = fa->dup_split_attempts;
  rp.cs_dup_splits = fa->cs_dup_split_attempts;
  rp.events.clear();
  const Vec<FAPassEvent *> &evs = fa_events_get();
  for (int i = rp.events_start_idx; i < evs.n; i++) {
    FAPassEvent *e = evs[i];
    if (!e) continue;
    cchar *st_name = ((int)e->stage >= 0 && (int)e->stage < FA::kNumFAPassStages) ? fa_stage_names[(int)e->stage] : "UNKNOWN";
    RecordedEvent ev;
    ev.stage = st_name;
    ev.splits = e->splits;
    ev.ess_before = e->ess_before;
    ev.ess_after = e->ess_after;
    ev.css_before = e->css_before;
    ev.css_after = e->css_after;
    ev.viol_before = e->violations_before;
    ev.viol_after = e->violations_after;
    rp.events.push_back(ev);
  }
  for (size_t si = 0; si < rp.splits.size(); si++) {
    const RecordedSplit &rs = rp.splits[si];
    bool found = false;
    for (size_t ei = 0; ei < rp.events.size(); ei++) {
      if (rp.events[ei].stage == rs.stage) {
        found = true;
        break;
      }
    }
    if (!found) {
      int cnt = 0;
      for (size_t sj = 0; sj < rp.splits.size(); sj++) {
        if (rp.splits[sj].stage == rs.stage) cnt++;
      }
      RecordedEvent ev;
      ev.stage = rs.stage;
      ev.splits = cnt;
      ev.ess_before = (int)rp.entry_sets.size();
      ev.ess_after = (int)rp.entry_sets.size();
      ev.css_before = (int)rp.creation_sets.size();
      ev.css_after = (int)rp.creation_sets.size();
      ev.viol_before = (int)rp.violations.size();
      ev.viol_after = (int)rp.violations.size();
      rp.events.push_back(ev);
    }
  }
  for (size_t ei = 0; ei < rp.events.size(); ei++) {
    int cnt = 0;
    for (size_t sj = 0; sj < rp.splits.size(); sj++) {
      if (rp.splits[sj].stage == rp.events[ei].stage) cnt++;
    }
    if (cnt > rp.events[ei].splits) rp.events[ei].splits = cnt;
  }
}

static std::string format_atype_with_cs_ids(AType *t) {
  if (!t || !t->n) return "void";
  std::vector<std::string> parts;
  for (CreationSet *cs : t->sorted) {
    if (!cs || !cs->sym) continue;
    cchar *raw_name = cs->sym->name ? cs->sym->name
                                    : ((cs->sym->is_constant && cs->sym->type && cs->sym->type->name)
                                           ? cs->sym->type->name
                                           : "*anon*");
    std::string s = raw_name;
    if (cs->sym->is_constant && cs->sym->constant) {
      s += "=" + std::string(cs->sym->constant);
    } else if (cs->sym->type_kind == Type_RECORD || cs->sym->element) {
      s += "#" + std::to_string(cs->id);
    }
    if (std::find(parts.begin(), parts.end(), s) == parts.end()) {
      parts.push_back(s);
    }
  }
  if (parts.empty()) return "void";
  std::string res;
  for (size_t i = 0; i < parts.size(); i++) {
    if (i > 0) res += " | ";
    if (i >= 6) {
      res += "... (+" + std::to_string(parts.size() - i) + ")";
      break;
    }
    res += parts[i];
  }
  return res;
}

static std::string format_cs_content_summary(CreationSet *cs) {
  if (!cs || !cs->sym) return "";
  std::vector<std::string> items;
  if (cs->added_element_var && cs->sym->element && cs->sym->element->var) {
    AVar *eav = cs->sym->element->var->avars.get(cs);
    if (eav && eav->out && eav->out->n) {
      items.push_back("elem: " + format_atype_with_cs_ids(eav->out));
    }
  }
  for (int i = 0; i < cs->vars.n && items.size() < 4; i++) {
    AVar *fav = cs->vars[i];
    if (!fav || !fav->out || !fav->out->n) continue;
    cchar *fname = (fav->var && fav->var->sym && fav->var->sym->name) ? fav->var->sym->name : nullptr;
    std::string label = fname ? std::string(fname) : ("slot[" + std::to_string(i) + "]");
    items.push_back(label + ": " + format_atype_with_cs_ids(fav->out));
  }
  std::string res;
  for (size_t i = 0; i < items.size(); i++) {
    if (i > 0) res += "; ";
    res += items[i];
  }
  return res;
}

static std::string format_edge_arg_summary(AEdge *ee, EntrySet *es, MPosition *focus_pos) {
  if (!ee) return "";
  if (focus_pos) {
    AVar *fa = ee->filtered_args.get(focus_pos);
    AVar *a = fa ? fa : ee->args.get(focus_pos);
    if (a && a->out && a->out->n) {
      std::string vname;
      if (es) {
        AVar *formal_av = es->args.get(focus_pos);
        if (formal_av && formal_av->var && formal_av->var->sym && formal_av->var->sym->name) {
          vname = std::string(formal_av->var->sym->name) + "=";
        }
      }
      return vname + format_atype_with_cs_ids(a->out);
    }
  }
  if (!es || !es->fun) return "";
  std::vector<std::string> parts;
  for (int i = 1; i < es->fun->positional_arg_positions.n && parts.size() < 3; i++) {
    MPosition *mp = es->fun->positional_arg_positions[i];
    if (!mp) continue;
    AVar *fa = ee->filtered_args.get(mp);
    AVar *a = fa ? fa : ee->args.get(mp);
    if (!a || !a->out || !a->out->n) continue;
    AVar *formal_av = es->args.get(mp);
    std::string vname = (formal_av && formal_av->var && formal_av->var->sym && formal_av->var->sym->name)
                            ? (std::string(formal_av->var->sym->name) + "=")
                            : ("arg" + std::to_string(i) + "=");
    parts.push_back(vname + format_atype_with_cs_ids(a->out));
  }
  std::string res;
  for (size_t i = 0; i < parts.size(); i++) {
    if (i > 0) res += ", ";
    res += parts[i];
  }
  return res;
}

static std::string format_es_product_sig(EntrySet *prod, MPosition *focus_pos, const std::vector<AEdge *> &edges_to_prod) {
  if (!prod) return "";
  if (focus_pos) {
    if (AType *flt = prod->filters.get(focus_pos)) {
      return "filter: " + format_atype_with_cs_ids(flt);
    }
  }
  if (prod->filters.n > 0) {
    for (int i = 0; i < prod->filters.n; i++) {
      if (prod->filters[i].key && prod->filters[i].value) {
        return "filter: " + format_atype_with_cs_ids(prod->filters[i].value);
      }
    }
  }
  if (!edges_to_prod.empty()) {
    std::string s = format_edge_arg_summary(edges_to_prod[0], prod, focus_pos);
    if (!s.empty()) return s;
  }
  return "";
}

void html_record_es_split(FA *fa, int stage, cchar *sub_kind, EntrySet *es, AVar *trigger_av, MPosition *avpos,
                          int fsetters, int fmark, const Vec<AEdge *> &before_edges,
                          const Vec<EntrySet *> &before_to, int aes_before,
                          CreationSet *demand_cs, ATypeViolation *viol,
                          cchar *extra_note) {
  (void)fsetters;
  (void)fmark;
  if (!ifa_dump_html && !ifa_dump_json && !ifa_open_html) return;
  if (!fa || !es || g_recorded_passes.empty()) return;

  RecordedPass &rp = g_recorded_passes.back();
  RecordedSplit rs;
  rs.id = (int)rp.splits.size() + 1;
  rs.pass_num = rp.pass_num;
  rs.stage = (stage >= 0 && stage < FA::kNumFAPassStages) ? fa_stage_names[stage] : "POST_PASS";
  rs.sub_kind = sub_kind ? sub_kind : "TYPE_COMPAT";
  rs.kind = "EntrySet";
  rs.target_id = es->id;
  rs.target_fun = es->fun;
  rs.target_fun_id = es->fun ? es->fun->id : -1;
  rs.target_name = (es->fun && es->fun->sym && es->fun->sym->name) ? es->fun->sym->name : "*anon*";

  rs.trigger_av_id = trigger_av ? trigger_av->id : -1;
  rs.trigger_var_id = (trigger_av && trigger_av->var) ? trigger_av->var->id : -1;
  rs.trigger_var_name = (trigger_av && trigger_av->var && trigger_av->var->sym && trigger_av->var->sym->name)
                            ? trigger_av->var->sym->name
                            : "";
  rs.trigger_type_str = (trigger_av && trigger_av->out) ? format_atype_with_cs_ids(trigger_av->out) : "";

  if (trigger_av && trigger_av->is_lvalue) {
    rs.trigger_role = "return lvalue";
  } else if (avpos && avpos->pos.n > 0 && avpos->is_positional()) {
    rs.trigger_role = "formal arg #" + std::to_string((int)Position2int(avpos->last()));
  } else if (trigger_av && trigger_av->var && trigger_av->var->is_formal) {
    rs.trigger_role = "formal parameter";
  } else if (trigger_av && !trigger_av->contour_is_entry_set && trigger_av->contour != GLOBAL_CONTOUR) {
    rs.trigger_role = "CS element/field demand";
  } else {
    rs.trigger_role = "confluence AVar";
  }

  rs.trigger_contour_id = -1;
  rs.trigger_fun = nullptr;
  rs.trigger_fun_id = -1;
  if (trigger_av && trigger_av->contour_is_entry_set && trigger_av->contour != GLOBAL_CONTOUR) {
    EntrySet *tes = (EntrySet *)trigger_av->contour;
    rs.trigger_contour_id = tes->id;
    rs.trigger_fun = tes->fun;
    rs.trigger_fun_id = tes->fun ? tes->fun->id : -1;
    rs.trigger_fun_name = (tes->fun && tes->fun->sym && tes->fun->sym->name) ? tes->fun->sym->name : "";
  } else if (trigger_av && !trigger_av->contour_is_entry_set && trigger_av->contour != GLOBAL_CONTOUR) {
    CreationSet *tcs = (CreationSet *)trigger_av->contour;
    rs.trigger_contour_id = tcs->id;
    rs.trigger_fun_name = (tcs->sym && tcs->sym->name) ? (std::string("CS ") + tcs->sym->name + "#" + std::to_string(tcs->id)) : "";
  }

  ATypeViolation *matched_viol = viol;
  if (!matched_viol && (stage == (int)FAPassStage::VIOLATION)) {
    for (ATypeViolation *v : fa->type_violations) {
      if (!v || !v->av) continue;
      if (v->av == trigger_av || (v->av->contour_is_entry_set && (EntrySet *)v->av->contour == es)) {
        matched_viol = v;
        break;
      }
    }
  }
  rs.viol_kind = matched_viol ? violation_kind_name(matched_viol->kind) : "";
  rs.demand_cs_id = demand_cs ? demand_cs->id : -1;
  rs.demand_cs_sym = (demand_cs && demand_cs->sym && demand_cs->sym->name) ? demand_cs->sym->name : "";

  std::unordered_set<EntrySet *> newly_minted_es;
  for (int i = aes_before; i < fa->all_entry_sets.n; i++) {
    if (fa->all_entry_sets[i]) newly_minted_es.insert(fa->all_entry_sets[i]);
  }

  rs.stayed_count = 0;
  rs.moved_count = 0;
  std::map<int, EntrySet *> prod_map;
  std::map<int, std::vector<AEdge *>> prod_edges;

  for (int i = 0; i < before_edges.n; i++) {
    AEdge *ee = before_edges[i];
    if (!ee) continue;
    EntrySet *old_to = (i < before_to.n) ? before_to[i] : es;
    EntrySet *new_to = ee->to;
    if (new_to == es) {
      rs.stayed_count++;
    } else if (new_to && new_to != old_to) {
      rs.moved_count++;
      prod_map[new_to->id] = new_to;
      prod_edges[new_to->id].push_back(ee);

      RecordedSplitEdge rse;
      rse.edge_id = ee->id;
      rse.from_es = ee->from ? ee->from->id : -1;
      rse.from_fun = ee->from ? ee->from->fun : nullptr;
      rse.from_fun_id = (ee->from && ee->from->fun) ? ee->from->fun->id : -1;
      rse.from_fun_name = (ee->from && ee->from->fun && ee->from->fun->sym && ee->from->fun->sym->name)
                              ? ee->from->fun->sym->name
                              : "*anon*";
      rse.pnode_id = ee->pnode ? ee->pnode->id : -1;
      rse.line = (ee->pnode && ee->pnode->code) ? ee->pnode->code->line() : 0;
      rse.old_to_es = old_to ? old_to->id : es->id;
      rse.new_to_es = new_to->id;
      rse.arg_summary = format_edge_arg_summary(ee, es, avpos);
      rs.affected_edges.push_back(rse);
    }
  }

  for (auto &kv : prod_map) {
    EntrySet *prod = kv.second;
    RecordedSplitProduct rsp;
    rsp.id = prod->id;
    rsp.is_new = newly_minted_es.count(prod) > 0;
    rsp.moved_count = (int)prod_edges[prod->id].size();
    rsp.sig = format_es_product_sig(prod, avpos, prod_edges[prod->id]);
    rsp.es_ptr = prod;
    rsp.cs_ptr = nullptr;
    rs.products.push_back(rsp);
  }

  std::string trig_label = !rs.trigger_var_name.empty()
                               ? ("'" + rs.trigger_var_name + "' (" + rs.trigger_role + ")")
                               : (rs.trigger_av_id >= 0 ? ("AVar #" + std::to_string(rs.trigger_av_id) + " (" + rs.trigger_role + ")")
                                                        : rs.trigger_role);

  if (rs.sub_kind == "TYPE_COMPAT") {
    if (stage == (int)FAPassStage::VIOLATION) {
      rs.reason = "Type violation" + (rs.viol_kind.empty() ? "" : (" (" + rs.viol_kind + ")")) + " traced to " +
                  trig_label + " holding union [" + rs.trigger_type_str +
                  "]; partitioning incoming call edges of ES#" + std::to_string(es->id) + " (" + rs.target_name +
                  ") by argument type compatibility";
    } else {
      rs.reason = "Type confluence on " + trig_label + " holding union [" + rs.trigger_type_str +
                  "]; partitioning incoming call edges of ES#" + std::to_string(es->id) + " (" + rs.target_name +
                  ") by argument type compatibility";
    }
  } else if (rs.sub_kind == "SETTER_COMPAT") {
    rs.reason = "Setter confluence on " + trig_label + " (stage " + rs.stage +
                "); separating incoming call edges of ES#" + std::to_string(es->id) + " (" + rs.target_name +
                ") that carry distinct setter equivalence classes";
  } else if (rs.sub_kind == "MARK_SETTER_COMPAT") {
    rs.reason = "Mark-setter confluence on " + trig_label + " (stage " + rs.stage +
                "); separating incoming call edges of ES#" + std::to_string(es->id) + " (" + rs.target_name +
                ") by mark-distance setter compatibility";
  } else if (rs.sub_kind == "SPLIT_EDGES_DYNAMIC") {
    rs.reason = "Dynamic dispatch split on " + trig_label + " holding [" + rs.trigger_type_str + "]" +
                (rs.viol_kind.empty() ? "" : (" for " + rs.viol_kind + " violation")) +
                "; filtering incoming call edges per CreationSet";
  } else if (rs.sub_kind == "PER_CS_RECEIVER") {
    rs.reason = "Per-CS method receiver split on " + rs.target_name + " (" + trig_label + " holds [" +
                rs.trigger_type_str + "]); separating method contour per receiver CreationSet";
  } else if (rs.sub_kind == "CSM_ELEMENT_CS") {
    rs.reason = "Container method " + rs.target_name + " received same-class containers [" + rs.trigger_type_str +
                "] with divergent element types; separating method contour per container CreationSet";
  } else if (rs.sub_kind == "UNTAGGED_DISPATCH") {
    rs.reason = "Untagged container dispatch ambiguity in ES#" + std::to_string(es->id) + " (" + rs.target_name +
                "); splitting calling contour per receiver CreationSet on " + trig_label + " [" + rs.trigger_type_str + "]";
  } else if (rs.sub_kind == "BOXING_RECEIVER" || rs.sub_kind == "ARG_VIOL_RECEIVER") {
    rs.reason = "Receiver split on ES#" + std::to_string(es->id) + " (" + rs.target_name + ") driven by " +
                (rs.viol_kind.empty() ? "BOXING" : rs.viol_kind) +
                " violation: partitioning callers into 2 groups by which offending element/field types each caller's receiver carries";
  } else if (rs.sub_kind == "BLOCKING_ES") {
    rs.reason = "Shared contour ES#" + std::to_string(es->id) + " (" + rs.target_name +
                ") blocked creation-point separation of demanded CreationSet CS#" + std::to_string(rs.demand_cs_id) +
                " (" + rs.demand_cs_sym + "); splitting in-edges into 2 groups by reachable creation points";
  } else if (rs.sub_kind == "ES_BY_ARG_TYPES") {
    rs.reason = "Demanded CreationSet CS#" + std::to_string(rs.demand_cs_id) + " (" + rs.demand_cs_sym +
                ") has a single allocation site (defs=1) inside ES#" + std::to_string(es->id) + " (" + rs.target_name +
                "); splitting contour by caller argument types to duplicate the creation point";
  } else if (rs.sub_kind == "ES_BY_CALL_SITE") {
    rs.reason = "Demanded CreationSet CS#" + std::to_string(rs.demand_cs_id) + " (" + rs.demand_cs_sym +
                ") has defs=1 with irrepresentable element [" + rs.trigger_type_str +
                "]; partitioning callers of ES#" + std::to_string(es->id) + " (" + rs.target_name +
                ") by returned assign-set reachability";
  } else if (rs.sub_kind == "NUMERIC_SEED_SPLIT") {
    rs.reason = "Formal " + trig_label + " of ES#" + std::to_string(es->id) + " (" + rs.target_name +
                ") holds pure numeric mix [" + rs.trigger_type_str +
                "]; backtracking callers to pure numeric seeds (keeping {" + (extra_note ? extra_note : "") + "})";
  } else {
    rs.reason = "Split ES#" + std::to_string(es->id) + " (" + rs.target_name + ") on " + trig_label;
  }

  rp.splits.push_back(rs);
}

void html_record_cs_split(FA *fa, int stage, cchar *sub_kind, CreationSet *cs, CreationSet *new_cs, bool is_new_cs,
                          const Vec<AVar *> &moved_defs, cchar *sig_or_reason,
                          CreationSet *demand_cs, AVar *trigger_av) {
  if (!ifa_dump_html && !ifa_dump_json && !ifa_open_html) return;
  if (!fa || !cs || !new_cs || g_recorded_passes.empty()) return;

  RecordedPass &rp = g_recorded_passes.back();
  std::string st_name = (stage >= 0 && stage < FA::kNumFAPassStages) ? fa_stage_names[stage] : "POST_PASS";
  std::string sk = sub_kind ? sub_kind : "CS_SPLIT";

  // Check if we can merge into an existing RecordedSplit for the same (stage, sub_kind, cs->id) in this pass
  RecordedSplit *target_rs = nullptr;
  for (size_t i = 0; i < rp.splits.size(); i++) {
    RecordedSplit &existing = rp.splits[i];
    if (existing.kind == "CreationSet" && existing.stage == st_name && existing.sub_kind == sk &&
        existing.target_id == cs->id) {
      target_rs = &existing;
      break;
    }
  }

  int moved_n = 0;
  for (AVar *d : moved_defs) if (d) moved_n++;

  if (!target_rs) {
    RecordedSplit rs;
    rs.id = (int)rp.splits.size() + 1;
    rs.pass_num = rp.pass_num;
    rs.stage = st_name;
    rs.sub_kind = sk;
    rs.kind = "CreationSet";
    rs.target_id = cs->id;
    rs.target_fun = nullptr;
    rs.target_fun_id = -1;
    rs.target_name = (cs->sym && cs->sym->name) ? cs->sym->name : "*anon*";

    rs.trigger_av_id = trigger_av ? trigger_av->id : -1;
    rs.trigger_var_id = (trigger_av && trigger_av->var) ? trigger_av->var->id : -1;
    rs.trigger_var_name = (trigger_av && trigger_av->var && trigger_av->var->sym && trigger_av->var->sym->name)
                              ? trigger_av->var->sym->name
                              : "";
    rs.trigger_role = trigger_av ? "creation point / starter" : "CS content union";
    rs.trigger_contour_id = -1;
    rs.trigger_fun = nullptr;
    rs.trigger_fun_id = -1;
    if (trigger_av && trigger_av->contour_is_entry_set && trigger_av->contour != GLOBAL_CONTOUR) {
      EntrySet *tes = (EntrySet *)trigger_av->contour;
      rs.trigger_contour_id = tes->id;
      rs.trigger_fun = tes->fun;
      rs.trigger_fun_id = tes->fun ? tes->fun->id : -1;
      rs.trigger_fun_name = (tes->fun && tes->fun->sym && tes->fun->sym->name) ? tes->fun->sym->name : "";
    }

    rs.trigger_type_str = format_cs_content_summary(demand_cs ? demand_cs : cs);
    rs.viol_kind = "";
    rs.demand_cs_id = demand_cs ? demand_cs->id : cs->id;
    rs.demand_cs_sym = (demand_cs && demand_cs->sym && demand_cs->sym->name) ? demand_cs->sym->name : rs.target_name;
    rs.stayed_count = cs->defs.set_count();
    rs.moved_count = 0;

    std::string content_desc = rs.trigger_type_str.empty() ? "" : (" [" + rs.trigger_type_str + "]");
    if (sk == "SETTER_EQ_CLASS") {
      rs.reason = "Creation points of " + rs.target_name + " (CS#" + std::to_string(cs->id) + ")" + content_desc +
                  " reach distinct setter equivalence classes; separating creation points by setter class compatibility";
    } else if (sk == "ELEM_SETTER_CLASS") {
      rs.reason = "Container " + rs.target_name + " (CS#" + std::to_string(cs->id) +
                  ") has irrepresentable element union" + content_desc +
                  "; partitioning creation points by setter equivalence classes";
    } else if (sk == "SETTER_REPR_KEY") {
      rs.reason = "Creation points of " + rs.target_name + " (CS#" + std::to_string(cs->id) + ")" + content_desc +
                  " reach setters with distinct basic representation types; partitioning by setter representation key";
    } else if (sk == "LADDER_NO_CONFUSION") {
      rs.reason = "Shedskin Ladder Route 1 (no_confusion): peeling unconfused creation points of " + rs.target_name +
                  " (CS#" + std::to_string(cs->id) + ")" + content_desc + " that feed a single element assign-set";
    } else if (sk == "LADDER_PATH_PARTITION") {
      rs.reason = "Shedskin Ladder Route 3 (path_partition): partitioning creation points of " + rs.target_name +
                  " (CS#" + std::to_string(cs->id) + ")" + content_desc +
                  " by the union of element assign-sets they reach";
    } else if (sk == "CS_ASSIGN_SET_KEY") {
      rs.reason = "Route 4 (CS_DEF_PARTITION by assign-set key): partitioning creation points of " + rs.target_name +
                  " (CS#" + std::to_string(cs->id) + ")" + content_desc +
                  " by assign-set reachability signature";
    } else if (sk == "CS_MEMBER_KEY") {
      rs.reason = "Route 4 (CS_DEF_PARTITION by member/slot key): partitioning creation points of " + rs.target_name +
                  " (CS#" + std::to_string(cs->id) + ")" + content_desc +
                  " by downstream class member / construction-slot signature";
    } else if (sk == "OWNER_OF_DEMANDED") {
      rs.reason = "Lifted demand from inner container CS#" + std::to_string(rs.demand_cs_id) + " (" + rs.demand_cs_sym +
                  ")" + content_desc + " to enclosing owner " + rs.target_name + " (CS#" + std::to_string(cs->id) +
                  "); partitioning owner creation points by inner assign-set reachability";
    } else if (sk == "PARENT_FIRST_JOINSPLIT") {
      rs.reason = "Undoing parent-first CS merge on demand: " + rs.target_name + " (CS#" + std::to_string(cs->id) +
                  ") has irrepresentable element" + content_desc +
                  " across split contours of the same allocation site";
    } else if (sk == "CSELEM_RESPLIT") {
      rs.reason = "Post-pass element-shape resplit: creation points of " + rs.target_name + " (CS#" +
                  std::to_string(cs->id) + ") diverged in element shape";
    } else if (sk == "CSELEM_REJOIN") {
      rs.reason = "Post-pass element-shape rejoin: creation point of " + rs.target_name + " (CS#" +
                  std::to_string(cs->id) + ") with resolved shape joined canonical CS#" + std::to_string(new_cs->id);
    } else {
      rs.reason = "Partitioned CreationSet CS#" + std::to_string(cs->id) + " (" + rs.target_name + ")" + content_desc;
    }

    rp.splits.push_back(rs);
    target_rs = &rp.splits.back();
  }

  target_rs->stayed_count = cs->defs.set_count();
  target_rs->moved_count += moved_n;

  RecordedSplitProduct rsp;
  rsp.id = new_cs->id;
  rsp.is_new = is_new_cs;
  rsp.moved_count = moved_n;
  rsp.sig = sig_or_reason ? sig_or_reason : "";
  rsp.es_ptr = nullptr;
  rsp.cs_ptr = new_cs;
  target_rs->products.push_back(rsp);

  for (AVar *d : moved_defs) {
    if (!d) continue;
    RecordedSplitDef rsd;
    rsd.av_id = d->id;
    rsd.var_id = d->var ? d->var->id : -1;
    rsd.var_name = (d->var && d->var->sym && d->var->sym->name) ? d->var->sym->name : "";
    EntrySet *des = (d->contour_is_entry_set && d->contour != GLOBAL_CONTOUR) ? (EntrySet *)d->contour : nullptr;
    rsd.contour_id = des ? des->id : -1;
    rsd.fun = des ? des->fun : nullptr;
    rsd.fun_id = (des && des->fun) ? des->fun->id : -1;
    rsd.fun_name = (des && des->fun && des->fun->sym && des->fun->sym->name)
                       ? des->fun->sym->name
                       : (d->contour == GLOBAL_CONTOUR ? "__main__" : "");
    PNode *def_pn = d->var ? d->var->def : nullptr;
    rsd.pnode_id = def_pn ? def_pn->id : -1;
    rsd.line = (def_pn && def_pn->code) ? def_pn->code->line() : 0;
    rsd.old_cs = cs->id;
    rsd.new_cs = new_cs->id;

    if (sig_or_reason && *sig_or_reason) {
      rsd.detail = std::string("sig: ") + sig_or_reason;
    } else if (d->setters) {
      std::vector<std::string> sparts;
      for (AVar *s : *d->setters) {
        if (!s || !s->out || !s->out->n) continue;
        std::string st = format_atype_with_cs_ids(s->out);
        if (std::find(sparts.begin(), sparts.end(), st) == sparts.end()) {
          sparts.push_back(st);
          if (sparts.size() >= 3) break;
        }
      }
      if (!sparts.empty()) {
        rsd.detail = "writes: ";
        for (size_t k = 0; k < sparts.size(); k++) {
          if (k > 0) rsd.detail += ", ";
          rsd.detail += sparts[k];
        }
      }
    }
    target_rs->affected_defs.push_back(rsd);
  }
}

void html_record_const_demand(FA *fa, int stage, EntrySet *es, MPosition *pos, const Vec<CreationSet *> &const_keys,
                              bool has_nonconst, ATypeViolation *why) {
  if (!ifa_dump_html && !ifa_dump_json && !ifa_open_html) return;
  if (!fa || !es || g_recorded_passes.empty()) return;

  RecordedPass &rp = g_recorded_passes.back();
  RecordedSplit rs;
  rs.id = (int)rp.splits.size() + 1;
  rs.pass_num = rp.pass_num;
  rs.stage = (stage >= 0 && stage < FA::kNumFAPassStages) ? fa_stage_names[stage] : "CONST_DEMAND";
  rs.sub_kind = "CONST_POSITION_NOMINATED";
  rs.kind = "ConstDemand";
  rs.target_id = es->id;
  rs.target_fun = es->fun;
  rs.target_fun_id = es->fun ? es->fun->id : -1;
  rs.target_name = (es->fun && es->fun->sym && es->fun->sym->name) ? es->fun->sym->name : "*anon*";

  AVar *fav = pos ? es->args.get(pos) : nullptr;
  rs.trigger_av_id = fav ? fav->id : (why && why->av ? why->av->id : -1);
  rs.trigger_var_id = (fav && fav->var) ? fav->var->id : -1;
  rs.trigger_var_name = (fav && fav->var && fav->var->sym && fav->var->sym->name) ? fav->var->sym->name : "";
  rs.trigger_role = (pos && pos->pos.n > 0 && pos->is_positional())
                        ? ("formal arg #" + std::to_string((int)Position2int(pos->last())))
                        : "formal parameter";
  rs.trigger_contour_id = es->id;
  rs.trigger_fun = es->fun;
  rs.trigger_fun_id = rs.target_fun_id;
  rs.trigger_fun_name = rs.target_name;

  std::string consts_str;
  for (int i = 0; i < const_keys.n; i++) {
    CreationSet *ck = const_keys[i];
    if (!ck || !ck->sym) continue;
    if (!consts_str.empty()) consts_str += " | ";
    cchar *raw_cname = ck->sym->name ? ck->sym->name
                                     : ((ck->sym->is_constant && ck->sym->type && ck->sym->type->name)
                                            ? ck->sym->type->name
                                            : "const");
    std::string cname = raw_cname;
    if (ck->sym->constant) cname += "=" + std::string(ck->sym->constant);
    consts_str += cname;
  }
  if (has_nonconst) {
    if (!consts_str.empty()) consts_str += " | ";
    consts_str += "<non-const>";
  }
  rs.trigger_type_str = consts_str;
  rs.viol_kind = why ? violation_kind_name(why->kind) : "";
  rs.demand_cs_id = -1;
  rs.stayed_count = 0;
  rs.moved_count = const_keys.n + (has_nonconst ? 1 : 0);

  int viol_av_id = (why && why->av) ? why->av->id : -1;
  rs.reason = "Violation" + (rs.viol_kind.empty() ? "" : (" (" + rs.viol_kind + ")")) +
              (viol_av_id >= 0 ? (" on AVar #" + std::to_string(viol_av_id)) : "") +
              " control/data-depends on formal '" + rs.trigger_var_name + "' of ES#" + std::to_string(es->id) + " (" +
              rs.target_name + "), where callers pass distinct constants [" + consts_str +
              "]; nominating formal for per-constant specialization on next pass";

  for (AEdge *ee : es->edges) {
    if (!ee || !ee->args.n) continue;
    AVar *ac = pos ? ee->args.get(pos) : nullptr;
    if (!ac || !ac->out || !ac->out->n) continue;
    RecordedSplitEdge rse;
    rse.edge_id = ee->id;
    rse.from_es = ee->from ? ee->from->id : -1;
    rse.from_fun = ee->from ? ee->from->fun : nullptr;
    rse.from_fun_id = (ee->from && ee->from->fun) ? ee->from->fun->id : -1;
    rse.from_fun_name = (ee->from && ee->from->fun && ee->from->fun->sym && ee->from->fun->sym->name)
                            ? ee->from->fun->sym->name
                            : "*anon*";
    rse.pnode_id = ee->pnode ? ee->pnode->id : -1;
    rse.line = (ee->pnode && ee->pnode->code) ? ee->pnode->code->line() : 0;
    rse.old_to_es = es->id;
    rse.new_to_es = es->id;
    rse.arg_summary = (!rs.trigger_var_name.empty() ? (rs.trigger_var_name + "=") : "") + format_atype_with_cs_ids(ac->out);
    rs.affected_edges.push_back(rse);
  }

  rp.splits.push_back(rs);
}

void html_record_arity_demote(FA *fa, const Vec<CreationSet *> &group, Sym *common_basic) {
  if (!ifa_dump_html && !ifa_dump_json && !ifa_open_html) return;
  if (!fa || !group.n || g_recorded_passes.empty()) return;

  RecordedPass &rp = g_recorded_passes.back();
  CreationSet *first = group[0];
  RecordedSplit rs;
  rs.id = (int)rp.splits.size() + 1;
  rs.pass_num = rp.pass_num;
  rs.stage = "SLOT_ARITY";
  rs.sub_kind = "MIXED_ARITY_DEMOTE";
  rs.kind = "ArityDemote";
  rs.target_id = first ? first->id : -1;
  rs.target_fun = nullptr;
  rs.target_fun_id = -1;
  rs.target_name = (first && first->sym && first->sym->name) ? first->sym->name : "tuple";
  rs.trigger_av_id = -1;
  rs.trigger_var_id = -1;
  rs.trigger_role = "homogeneous mixed-arity union";
  rs.trigger_contour_id = -1;
  rs.trigger_fun = nullptr;
  rs.trigger_fun_id = -1;
  std::string elem_name = (common_basic && common_basic->name) ? common_basic->name : "homogeneous";
  rs.trigger_type_str = "elem: " + elem_name;
  rs.demand_cs_id = -1;
  rs.stayed_count = 0;
  rs.moved_count = group.n;
  rs.reason = "Connected CreationSets of '" + rs.target_name + "' with differing static arities meet in a shared AVar "
              "and agree on homogeneous element type '" + elem_name +
              "'; demoting static record layout to dynamic list layout (no_static_arity=1)";

  for (int i = 0; i < group.n; i++) {
    CreationSet *g = group[i];
    if (!g) continue;
    RecordedSplitProduct rsp;
    rsp.id = g->id;
    rsp.is_new = false;
    rsp.moved_count = g->defs.set_count();
    rsp.sig = "arity " + std::to_string(g->vars.n) + " -> dynamic";
    rsp.es_ptr = nullptr;
    rsp.cs_ptr = g;
    rs.products.push_back(rsp);
  }

  rp.splits.push_back(rs);
}

static bool is_recorded_es_live(const RecordedES &res) {
  if (res.es && res.es->fun && res.es->fun->live) return true;
  if (res.fun && res.fun->live) return true;
  return false;
}

static bool is_recorded_avar_live(const RecordedAVar &rav) {
  Fun *cur_fun = (rav.es_contour && rav.es_contour->fun) ? rav.es_contour->fun : rav.fun;
  bool fun_live = (cur_fun && cur_fun->live) || (rav.fun && rav.fun->live);
  if (!fun_live) return false;
  Var *cur_var = (rav.av && rav.av->var) ? rav.av->var : rav.var;
  bool var_live = (cur_var && cur_var->live) || (rav.var && rav.var->live);
  return var_live;
}

static void format_recorded_pass(const RecordedPass &rp, std::string &summary_json, std::string &full_json) {
  std::unordered_set<int> live_ref_cs_ids;
  std::vector<size_t> live_es_indices;
  for (size_t i = 0; i < rp.entry_sets.size(); i++) {
    if (is_recorded_es_live(rp.entry_sets[i])) {
      live_es_indices.push_back(i);
      for (const auto &a : rp.entry_sets[i].args) {
        for (int cid : a.cs_ids) live_ref_cs_ids.insert(cid);
      }
      for (const auto &r : rp.entry_sets[i].rets) {
        for (int cid : r.cs_ids) live_ref_cs_ids.insert(cid);
      }
    }
  }

  std::unordered_set<int> live_avar_ids;
  for (const RecordedAVar &rav : rp.avars) {
    if (!is_recorded_avar_live(rav)) continue;
    live_avar_ids.insert(rav.id);
    for (int cid : rav.cs_ids) live_ref_cs_ids.insert(cid);
  }

  std::vector<size_t> live_cs_indices;
  for (size_t i = 0; i < rp.creation_sets.size(); i++) {
    if (is_creation_set_live(rp.creation_sets[i].cs, live_ref_cs_ids)) {
      live_cs_indices.push_back(i);
    }
  }

  // Keep all violations recorded in the pass (FA only creates violations on active expressions)
  std::vector<size_t> live_viol_indices;
  for (size_t i = 0; i < rp.violations.size(); i++) {
    live_viol_indices.push_back(i);
  }

  std::ostringstream splits_os;
  splits_os << "[";
  for (size_t si = 0; si < rp.splits.size(); si++) {
    if (si > 0) splits_os << ",";
    const RecordedSplit &rs = rp.splits[si];
    int eff_target_fun_id = (rs.target_fun && rs.target_fun->live) ? rs.target_fun->id : rs.target_fun_id;
    int eff_trig_fun_id = (rs.trigger_fun && rs.trigger_fun->live) ? rs.trigger_fun->id : rs.trigger_fun_id;
    splits_os << "{\"id\":" << rs.id
              << ",\"pass\":" << rs.pass_num
              << ",\"stage\":\"" << json_escape(rs.stage) << "\""
              << ",\"sub_kind\":\"" << json_escape(rs.sub_kind) << "\""
              << ",\"kind\":\"" << json_escape(rs.kind) << "\""
              << ",\"target_id\":" << rs.target_id
              << ",\"target_fun_id\":" << eff_target_fun_id
              << ",\"target_name\":\"" << json_escape(rs.target_name) << "\""
              << ",\"trigger_av_id\":" << rs.trigger_av_id
              << ",\"trigger_var_id\":" << rs.trigger_var_id
              << ",\"trigger_var_name\":\"" << json_escape(rs.trigger_var_name) << "\""
              << ",\"trigger_role\":\"" << json_escape(rs.trigger_role) << "\""
              << ",\"trigger_contour_id\":" << rs.trigger_contour_id
              << ",\"trigger_fun_id\":" << eff_trig_fun_id
              << ",\"trigger_fun_name\":\"" << json_escape(rs.trigger_fun_name) << "\""
              << ",\"trigger_type_str\":\"" << json_escape(rs.trigger_type_str) << "\""
              << ",\"viol_kind\":\"" << json_escape(rs.viol_kind) << "\""
              << ",\"demand_cs_id\":" << rs.demand_cs_id
              << ",\"demand_cs_sym\":\"" << json_escape(rs.demand_cs_sym) << "\""
              << ",\"reason\":\"" << json_escape(rs.reason) << "\""
              << ",\"stayed_count\":" << rs.stayed_count
              << ",\"moved_count\":" << rs.moved_count;

    splits_os << ",\"products\":[";
    for (size_t pi = 0; pi < rs.products.size(); pi++) {
      if (pi > 0) splits_os << ",";
      const RecordedSplitProduct &p = rs.products[pi];
      int p_fun_id = (p.es_ptr && p.es_ptr->fun && p.es_ptr->fun->live) ? p.es_ptr->fun->id : eff_target_fun_id;
      splits_os << "{\"id\":" << p.id
                << ",\"is_new\":" << (p.is_new ? "true" : "false")
                << ",\"moved_count\":" << p.moved_count
                << ",\"fun_id\":" << p_fun_id
                << ",\"sig\":\"" << json_escape(p.sig) << "\""
                << "}";
    }
    splits_os << "]";

    splits_os << ",\"affected_edges\":[";
    for (size_t ei = 0; ei < rs.affected_edges.size(); ei++) {
      if (ei > 0) splits_os << ",";
      const RecordedSplitEdge &e = rs.affected_edges[ei];
      int eff_from_fun_id = (e.from_fun && e.from_fun->live) ? e.from_fun->id : e.from_fun_id;
      splits_os << "{\"edge_id\":" << e.edge_id
                << ",\"from_es\":" << e.from_es
                << ",\"from_fun_id\":" << eff_from_fun_id
                << ",\"from_fun_name\":\"" << json_escape(e.from_fun_name) << "\""
                << ",\"pnode_id\":" << e.pnode_id
                << ",\"line\":" << e.line
                << ",\"old_to_es\":" << e.old_to_es
                << ",\"new_to_es\":" << e.new_to_es
                << ",\"arg_summary\":\"" << json_escape(e.arg_summary) << "\""
                << "}";
    }
    splits_os << "]";

    splits_os << ",\"affected_defs\":[";
    for (size_t di = 0; di < rs.affected_defs.size(); di++) {
      if (di > 0) splits_os << ",";
      const RecordedSplitDef &d = rs.affected_defs[di];
      int eff_def_fun_id = (d.fun && d.fun->live) ? d.fun->id : d.fun_id;
      splits_os << "{\"av_id\":" << d.av_id
                << ",\"var_id\":" << d.var_id
                << ",\"var_name\":\"" << json_escape(d.var_name) << "\""
                << ",\"contour_id\":" << d.contour_id
                << ",\"fun_id\":" << eff_def_fun_id
                << ",\"fun_name\":\"" << json_escape(d.fun_name) << "\""
                << ",\"pnode_id\":" << d.pnode_id
                << ",\"line\":" << d.line
                << ",\"old_cs\":" << d.old_cs
                << ",\"new_cs\":" << d.new_cs
                << ",\"detail\":\"" << json_escape(d.detail) << "\""
                << "}";
    }
    splits_os << "]";

    splits_os << "}";
  }
  splits_os << "]";
  std::string splits_json = splits_os.str();

  std::ostringstream sum_os;
  sum_os << "{\"pass\":" << rp.pass_num
         << ",\"ess_count\":" << live_es_indices.size()
         << ",\"css_count\":" << live_cs_indices.size()
         << ",\"violations_count\":" << live_viol_indices.size()
         << ",\"splits_count\":" << rp.splits.size()
         << ",\"dup_splits\":" << rp.dup_splits
         << ",\"cs_dup_splits\":" << rp.cs_dup_splits
         << ",\"events\":[";
  for (size_t ei = 0; ei < rp.events.size(); ei++) {
    if (ei > 0) sum_os << ",";
    const RecordedEvent &ev = rp.events[ei];
    sum_os << "{\"stage\":\"" << ev.stage << "\""
           << ",\"splits\":" << ev.splits
           << ",\"ess_before\":" << ev.ess_before << ",\"ess_after\":" << ev.ess_after
           << ",\"css_before\":" << ev.css_before << ",\"css_after\":" << ev.css_after
           << ",\"viol_before\":" << ev.viol_before << ",\"viol_after\":" << ev.viol_after
           << "}";
  }
  sum_os << "],\"splits\":" << splits_json << "}";
  summary_json = sum_os.str();

  std::ostringstream full_os;
  full_os << "{\"pass\":" << rp.pass_num
          << ",\"summary\":" << summary_json
          << ",\"splits\":" << splits_json;

  // Live EntrySets
  full_os << ",\"entry_sets\":[";
  for (size_t li = 0; li < live_es_indices.size(); li++) {
    if (li > 0) full_os << ",";
    const RecordedES &res = rp.entry_sets[live_es_indices[li]];
    Fun *cur_fun = (res.es && res.es->fun && res.es->fun->live) ? res.es->fun : res.fun;
    int eff_fun_id = cur_fun ? cur_fun->id : res.fun_id;
    full_os << "{\"id\":" << res.id
            << ",\"fun_id\":" << eff_fun_id
            << ",\"orig_fun_id\":" << res.fun_id
            << ",\"fun_name\":\"" << json_escape(res.fun_name) << "\""
            << ",\"can_raise\":" << (res.can_raise ? "true" : "false")
            << ",\"returns\":" << (res.returns ? "true" : "false")
            << ",\"raises\":" << (res.raises ? "true" : "false")
            << ",\"split_origin\":" << res.split_origin;

    full_os << ",\"args\":[";
    for (size_t ai = 0; ai < res.args.size(); ai++) {
      if (ai > 0) full_os << ",";
      full_os << "{\"pos\":" << res.args[ai].pos
              << ",\"av_id\":" << res.args[ai].av_id
              << ",\"var_name\":\"" << json_escape(res.args[ai].var_name) << "\""
              << ",\"type\":" << res.args[ai].type_str
              << "}";
    }
    full_os << "]";

    full_os << ",\"rets\":[";
    for (size_t ri = 0; ri < res.rets.size(); ri++) {
      if (ri > 0) full_os << ",";
      full_os << "{\"av_id\":" << res.rets[ri].av_id
              << ",\"type\":" << res.rets[ri].type_str
              << "}";
    }
    full_os << "]";

    full_os << ",\"out_edges\":[";
    bool first_oe = true;
    for (size_t oi = 0; oi < res.out_edges.size(); oi++) {
      const RecordedEdge &edge = res.out_edges[oi];
      Fun *callee_f = (edge.to_es_ptr && edge.to_es_ptr->fun) ? edge.to_es_ptr->fun : edge.callee;
      if (callee_f && !callee_f->live && (!edge.callee || !edge.callee->live)) continue;
      PNode *call_pn = edge.pnode;
      if (cur_fun && cur_fun->nmap && call_pn) {
        if (PNode *mapped = cur_fun->nmap->get(call_pn)) call_pn = mapped;
      }
      if (call_pn && !call_pn->live && (!edge.pnode || !edge.pnode->live)) continue;
      if (!first_oe) full_os << ",";
      first_oe = false;
      full_os << "{\"id\":" << edge.id
              << ",\"to_es\":" << edge.to_es
              << ",\"pnode_id\":" << (call_pn ? call_pn->id : edge.pnode_id)
              << ",\"callee_fun\":\"" << json_escape(edge.callee_fun) << "\""
              << "}";
    }
    full_os << "]";

    full_os << "}";
  }
  full_os << "]";

  // Live CreationSets
  full_os << ",\"creation_sets\":[";
  for (size_t li = 0; li < live_cs_indices.size(); li++) {
    if (li > 0) full_os << ",";
    const RecordedCS &rcs = rp.creation_sets[live_cs_indices[li]];
    full_os << "{\"id\":" << rcs.id
            << ",\"sym\":\"" << json_escape(rcs.sym) << "\""
            << ",\"static_arity\":" << rcs.static_arity
            << ",\"no_static_arity\":" << (rcs.no_static_arity ? "true" : "false")
            << ",\"split_origin\":" << rcs.split_origin;

    full_os << ",\"fields\":[";
    for (size_t fi = 0; fi < rcs.fields.size(); fi++) {
      if (fi > 0) full_os << ",";
      full_os << "{\"index\":" << rcs.fields[fi].index
              << ",\"av_id\":" << rcs.fields[fi].av_id
              << ",\"name\":\"" << json_escape(rcs.fields[fi].name) << "\""
              << ",\"type\":" << rcs.fields[fi].type_str
              << "}";
    }
    full_os << "]";

    full_os << "}";
  }
  full_os << "]";

  // Violations
  full_os << ",\"violations\":[";
  for (size_t li = 0; li < live_viol_indices.size(); li++) {
    if (li > 0) full_os << ",";
    const RecordedViol &rviol = rp.violations[live_viol_indices[li]];
    Fun *vfun = (rviol.es && rviol.es->fun && rviol.es->fun->live) ? rviol.es->fun : rviol.fun;
    int eff_fun_id = vfun ? vfun->id : rviol.fun_id;
    full_os << "{\"kind\":\"" << rviol.kind << "\""
            << ",\"av_id\":" << rviol.av_id
            << ",\"send_id\":" << rviol.send_id
            << ",\"contour_id\":" << rviol.contour_id
            << ",\"fun_id\":" << eff_fun_id
            << ",\"var_name\":\"" << json_escape(rviol.var_name) << "\""
            << ",\"fun_name\":\"" << json_escape(rviol.fun_name) << "\""
            << ",\"type_str\":\"" << json_escape(rviol.type_str) << "\""
            << "}";
  }
  full_os << "]";

  // Live Active AVars & Dataflow
  full_os << ",\"avars\":[";
  bool first_av = true;
  for (const RecordedAVar &rav : rp.avars) {
    if (!live_avar_ids.count(rav.id)) continue;
    if (!first_av) full_os << ",";
    first_av = false;
    Fun *cur_fun = (rav.es_contour && rav.es_contour->fun && rav.es_contour->fun->live) ? rav.es_contour->fun : rav.fun;
    Var *cur_var = (rav.av && rav.av->var && rav.av->var->live) ? rav.av->var : rav.var;
    int eff_fun_id = cur_fun ? cur_fun->id : rav.fun_id;
    int eff_var_id = cur_var ? cur_var->id : rav.var_id;
    full_os << "{\"id\":" << rav.id
            << ",\"var_id\":" << eff_var_id
            << ",\"orig_var_id\":" << rav.var_id
            << ",\"var_name\":\"" << json_escape(rav.var_name) << "\""
            << ",\"fun_name\":\"" << json_escape(rav.fun_name) << "\""
            << ",\"contour_id\":" << rav.contour_id
            << ",\"fun_id\":" << eff_fun_id
            << ",\"orig_fun_id\":" << rav.fun_id
            << ",\"type\":" << rav.type_str;

    full_os << ",\"forward\":[";
    bool first_fwd = true;
    for (int fwid : rav.forward) {
      if (!live_avar_ids.count(fwid)) continue;
      if (!first_fwd) full_os << ",";
      first_fwd = false;
      full_os << fwid;
    }
    full_os << "]";

    full_os << ",\"backward\":[";
    bool first_bwd = true;
    for (int bwid : rav.backward) {
      if (!live_avar_ids.count(bwid)) continue;
      if (!first_bwd) full_os << ",";
      first_bwd = false;
      full_os << bwid;
    }
    full_os << "]";

    full_os << "}";
  }
  full_os << "]";

  full_os << "}";
  full_json = full_os.str();
}

void dump_sym_name(FILE *fp, Sym *s) {
  if (!fp || !s) return;
  fprintf(fp, "<a href=\"#SYM_%d\">%s (%d)</a>", s->id, s->name ? s->name : "*anon*", s->id);
}

void dump_fun_list(FILE *fp, Vec<Fun *> &funs) {
  if (!fp) return;
  for (int i = 0; i < funs.n; i++) {
    Fun *f = funs[i];
    if (!f) continue;
    if (i) fprintf(fp, ", ");
    fprintf(fp, "<a href=\"#FUN_%d\">%s (%d)</a>", f->id, f->sym && f->sym->name ? f->sym->name : "*anon*", f->id);
  }
}

void dump_html(FA *fa, cchar *fn, cchar *out_base) {
  // `fn` is the program as named on the command line; `out_base` is where
  // codegen wrote its output, and where ours goes.
  check_env_config();

  // If no passes were recorded, record one now
  if (g_recorded_passes.empty() && fa) {
    html_record_fa_pass(fa, 0);
  }

  std::string base_fn(out_base ? out_base : fn);
  std::string html_fn = base_fn + ".html";
  std::string json_fn = base_fn + ".json";

  // Phase 1: Source Files Lines
  std::vector<std::string> source_lines;
  std::ifstream src_file(fn);
  if (src_file.is_open()) {
    std::string line;
    while (std::getline(src_file, line)) {
      source_lines.push_back(line);
    }
  }

  // Detect active backend at runtime and load generated C or LLVM IR lines if available
  bool is_llvm_backend = false;
  Vec<Fun *> *compiled_funs = (fa && fa->funs.n > 0) ? &fa->funs : (pdb ? &pdb->funs : nullptr);
  if (compiled_funs) {
    for (int fi = 0; fi < compiled_funs->n; fi++) {
      Fun *f = (*compiled_funs)[fi];
      if (f && f->llvm) {
        is_llvm_backend = true;
        break;
      }
    }
  }
  if (!is_llvm_backend && getenv("IFA_LLVM")) is_llvm_backend = true;

  std::string cg_fn;
  std::string cg_lang = is_llvm_backend ? "LLVM" : "C";
  if (is_llvm_backend) {
    std::string stem = base_fn;
    size_t dot = stem.find_last_of('.');
    if (dot != std::string::npos) stem = stem.substr(0, dot);
    cg_fn = stem + ".ll";
  } else {
    cg_fn = base_fn + ".c";
  }

  std::vector<std::string> cg_lines;
  {
    std::ifstream cg_file(cg_fn);
    if (cg_file.is_open()) {
      std::string line;
      while (std::getline(cg_file, line)) {
        cg_lines.push_back(line);
      }
    }
  }

  // Map each live Fun::id -> (start_line, end_line) (1-based) in cg_lines
  std::unordered_map<int, std::pair<int, int>> fun_cg_spans;
  if (compiled_funs && !cg_lines.empty()) {
    for (int fi = 0; fi < compiled_funs->n; fi++) {
      Fun *f = (*compiled_funs)[fi];
      if (!f || !f->live || !f->cg_string || !*f->cg_string) continue;
      std::string needle = std::string(f->cg_string) + "(";
      int start_ln = 0, end_ln = 0;
      for (size_t li = 0; li < cg_lines.size(); li++) {
        const std::string &ln = cg_lines[li];
        if (is_llvm_backend) {
          if (ln.rfind("define ", 0) == 0 && ln.find("@" + needle) != std::string::npos) {
            start_ln = (int)li + 1;
            for (size_t ej = li; ej < cg_lines.size(); ej++) {
              if (!cg_lines[ej].empty() && cg_lines[ej][0] == '}') {
                end_ln = (int)ej + 1;
                break;
              }
            }
            break;
          }
        } else {
          size_t pos = ln.find(needle);
          if (pos != std::string::npos && (pos == 0 || ln[pos - 1] == ' ' || ln[pos - 1] == '*')) {
            if (!ln.empty() && ln.back() != ';') {
              start_ln = (int)li + 1;
              if (ln.find("{ }") != std::string::npos) {
                end_ln = start_ln;
              } else {
                for (size_t ej = li + 1; ej < cg_lines.size(); ej++) {
                  if (!cg_lines[ej].empty() && cg_lines[ej][0] == '}') {
                    end_ln = (int)ej + 1;
                    break;
                  }
                }
              }
              break;
            }
          }
        }
      }
      if (start_ln > 0) {
        if (end_ln < start_ln) end_ln = start_ln;
        fun_cg_spans[f->id] = {start_ln, end_ln};
      }
    }
  }

  // Build Whole-Program Compilation JSON Document
  std::ostringstream root;
  root << "{\n";
  root << "  \"meta\": {\n";
  root << "    \"program\": \"" << json_escape(fn) << "\",\n";
  root << "    \"total_passes\": " << g_recorded_passes.size() << ",\n";
  root << "    \"max_size_mb\": " << ifa_html_max_size_mb << ",\n";
  root << "    \"pass_limit_mb\": " << ifa_html_pass_limit_mb << ",\n";
  root << "    \"backend\": \"" << cg_lang << "\"\n";
  root << "  },\n";

  // Phase 1: Source code lines
  root << "  \"phase1_source\": {\n";
  root << "    \"lines\": [\n";
  for (size_t i = 0; i < source_lines.size(); i++) {
    if (i > 0) root << ",\n";
    root << "      \"" << json_escape(source_lines[i]) << "\"";
  }
  root << "\n    ]\n  },\n";

  // Phase 2: CFG & SSU (Functions, Blocks, PNodes)
  struct RecordedContourCall {
    int edge_id;
    int from_es;
    int to_es;
    int callee_fun_id;
    std::string callee_name;
  };
  std::unordered_map<int, std::vector<RecordedContourCall>> pnode_contour_calls;
  if (fa) {
    for (int ei = 0; ei < fa->all_aedges.n; ei++) {
      AEdge *ae = fa->all_aedges[ei];
      if (!ae || !ae->pnode) continue;
      Fun *callee_f = (ae->to && ae->to->fun) ? ae->to->fun : ae->fun;
      if (!callee_f || !callee_f->live) continue;
      Fun *caller_f = (ae->from && ae->from->fun) ? ae->from->fun : nullptr;
      if (caller_f && !caller_f->live) continue;
      PNode *call_pn = ae->pnode;
      if (caller_f && caller_f->nmap) {
        if (PNode *mapped = caller_f->nmap->get(call_pn)) call_pn = mapped;
      }
      if (!call_pn) continue;
      RecordedContourCall cc;
      cc.edge_id = ae->id;
      cc.from_es = ae->from ? ae->from->id : -1;
      cc.to_es = ae->to ? ae->to->id : -1;
      cc.callee_fun_id = callee_f->id;
      cc.callee_name = (callee_f->sym && callee_f->sym->name) ? callee_f->sym->name : "*anon*";
      auto &vec = pnode_contour_calls[call_pn->id];
      bool dup = false;
      for (const auto &c : vec) {
        if (c.from_es == cc.from_es && c.to_es == cc.to_es && c.callee_fun_id == cc.callee_fun_id) {
          dup = true;
          break;
        }
      }
      if (!dup) vec.push_back(cc);
    }
  }
  if (!g_recorded_passes.empty()) {
    const RecordedPass &last_rp = g_recorded_passes.back();
    for (const RecordedES &res : last_rp.entry_sets) {
      Fun *caller_f = (res.es && res.es->fun) ? res.es->fun : res.fun;
      if (caller_f && !caller_f->live) continue;
      for (const RecordedEdge &re : res.out_edges) {
        if (re.pnode_id >= 0 && re.to_es >= 0) {
          Fun *callee_f = (re.to_es_ptr && re.to_es_ptr->fun) ? re.to_es_ptr->fun : re.callee;
          if (callee_f && !callee_f->live) continue;
          PNode *call_pn = re.pnode;
          if (caller_f && caller_f->nmap && call_pn) {
            if (PNode *mapped = caller_f->nmap->get(call_pn)) call_pn = mapped;
          }
          int pn_id = call_pn ? call_pn->id : re.pnode_id;
          auto &vec = pnode_contour_calls[pn_id];
          bool found = false;
          for (const auto &c : vec) {
            if (c.from_es == res.id && c.to_es == re.to_es) { found = true; break; }
          }
          if (!found) {
            RecordedContourCall cc;
            cc.edge_id = re.id;
            cc.from_es = res.id;
            cc.to_es = re.to_es;
            cc.callee_fun_id = callee_f ? callee_f->id : -1;
            cc.callee_name = re.callee_fun;
            vec.push_back(cc);
          }
        }
      }
    }
  }

  root << "  \"phase2_cfg\": {\n";
  root << "    \"functions\": [\n";
  if (compiled_funs) {
    bool first_fn = true;
    for (int fi = 0; fi < compiled_funs->n; fi++) {
      Fun *f = (*compiled_funs)[fi];
      if (!f || !f->live) continue;
      if (!first_fn) root << ",\n";
      first_fn = false;
      bool is_clone = (f->sym && f->sym->fun && f->sym->fun != f);
      int orig_id = (f->sym && f->sym->fun) ? f->sym->fun->id : f->id;
      auto span_it = fun_cg_spans.find(f->id);
      int cg_start = span_it != fun_cg_spans.end() ? span_it->second.first : 0;
      int cg_end = span_it != fun_cg_spans.end() ? span_it->second.second : 0;
      root << "      {\n";
      root << "        \"id\": " << f->id << ",\n";
      root << "        \"name\": \"" << json_escape(f->sym && f->sym->name ? f->sym->name : "*anon*") << "\",\n";
      root << "        \"cg_name\": \"" << json_escape(f->cg_string ? f->cg_string : "") << "\",\n";
      root << "        \"cg_start_line\": " << cg_start << ",\n";
      root << "        \"cg_end_line\": " << cg_end << ",\n";
      root << "        \"filename\": \"" << json_escape(resolve_fun_filename(f, fn)) << "\",\n";
      root << "        \"line\": " << resolve_fun_line(f) << ",\n";
      root << "        \"can_raise\": " << (f->can_raise ? "true" : "false") << ",\n";
      root << "        \"live\": true,\n";
      root << "        \"is_generic\": " << (f->is_generic ? "true" : "false") << ",\n";
      root << "        \"is_clone\": " << (is_clone ? "true" : "false") << ",\n";
      root << "        \"wraps_id\": " << orig_id << ",\n";
      root << "        \"entry_id\": " << (f->entry ? f->entry->id : -1) << ",\n";
      root << "        \"exit_id\": " << (f->exit ? f->exit->id : -1) << ",\n";
      root << "        \"entry_sets\": [";
      for (int ei = 0; ei < f->ess.n; ei++) {
        if (ei > 0) root << ",";
        root << (f->ess[ei] ? f->ess[ei]->id : -1);
      }
      root << "],\n";

      // Call edges: only to live callees from live call sites
      root << "        \"calls\": [";
      bool first_call = true;
      for (int ci = 0; ci < f->calls.n; ci++) {
        if (f->calls[ci].key && f->calls[ci].value) {
          PNode *caller_pn = f->calls[ci].key;
          if (!caller_pn || !caller_pn->live) continue;
          for (Fun *callee : *f->calls[ci].value) {
            if (!callee || !callee->live) continue;
            if (!first_call) root << ",";
            first_call = false;
            root << "{\"pnode_id\":" << caller_pn->id
                 << ",\"callee_id\":" << callee->id
                 << ",\"callee_name\":\"" << json_escape(callee->sym && callee->sym->name ? callee->sym->name : "*anon*") << "\"}";
          }
        }
      }
      root << "],\n";

      // Called by: only from live callers
      root << "        \"called_by\": [";
      bool first_cb = true;
      for (CallPoint *cp : f->called) {
        if (!cp || !cp->fun || !cp->fun->live) continue;
        if (!first_cb) root << ",";
        first_cb = false;
        root << "{\"caller_id\":" << cp->fun->id
             << ",\"caller_name\":\"" << json_escape(cp->fun->sym && cp->fun->sym->name ? cp->fun->sym->name : "*anon*") << "\"}";
      }
      root << "],\n";

      // Collect formal vars from positional_arg_positions
      std::unordered_set<int> formal_var_ids;
      for (MPosition *pos : f->positional_arg_positions) {
        Var *av_arg = f->args.get(pos);
        if (av_arg && (!av_arg->sym || av_arg->sym != f->sym)) {
          formal_var_ids.insert(av_arg->id);
        }
      }

      // Collect variables for function
      Vec<Var *> f_vars;
      for (int vi = 0; vi < f->fa_all_Vars.n; vi++) {
        Var *v = f->fa_all_Vars[vi];
        if (v) f_vars.set_add(v);
      }
      for (int pi = 0; pi < f->fa_all_PNodes.n; pi++) {
        PNode *pn = f->fa_all_PNodes[pi];
        if (!pn) continue;
        for (int li = 0; li < pn->lvals.n; li++) {
          Var *v = pn->lvals[li];
          if (v) f_vars.set_add(v);
        }
        for (int ri = 0; ri < pn->rvals.n; ri++) {
          Var *v = pn->rvals[ri];
          if (v) f_vars.set_add(v);
        }
      }
      f_vars.set_to_vec();

      std::unordered_map<int, std::string> var_type_map;
      for (Var *v : f_vars) {
        Vec<CreationSet *> unified_css;
        for (int avi = 0; avi < v->avars.n; avi++) {
          if (!v->avars[avi].key || !v->avars[avi].value) continue;
          AVar *av = v->avars[avi].value;
          if (!av || !av->out) continue;
          for (CreationSet *cs : *av->out) {
            if (cs) unified_css.set_add(cs);
          }
        }
        unified_css.set_to_vec();
        std::string v_type_str = format_atype_simple(unified_css);
        if (v_type_str == "void" || v_type_str.empty()) {
          if (v->constant && v->constant->is_constant && v->constant->constant) {
            v_type_str = std::string(v->constant->name ? v->constant->name : "*anon*") + "=" + v->constant->constant;
          } else if (v->constant && v->constant->name) {
            v_type_str = v->constant->name;
          } else if (v->type && v->type->name) {
            v_type_str = v->type->name;
          } else {
            v_type_str = "unknown";
          }
        }
        var_type_map[v->id] = v_type_str;
      }

      // Vars on function: cross links vars -> avars, contour mappings, type annotations
      root << "        \"vars\": [\n";
      bool first_fv = true;
      for (Var *v : f_vars) {
        if (!first_fv) root << ",\n";
        first_fv = false;
        std::string v_type = var_type_map[v->id];
        bool is_formal_v = v->is_formal || (formal_var_ids.count(v->id) > 0);
        root << "          {\"id\":" << v->id
             << ",\"name\":\"" << json_escape(v->sym && v->sym->name ? v->sym->name : "") << "\""
             << ",\"type_str\":\"" << json_escape(v_type) << "\""
             << ",\"is_formal\":" << (is_formal_v ? "true" : "false")
             << ",\"is_internal\":" << (v->is_internal ? "true" : "false")
             << ",\"is_live\":" << (v->live ? "true" : "false")
             << ",\"avars\":[";
        bool first_va = true;
        for (int avi = 0; avi < v->avars.n; avi++) {
          if (!v->avars[avi].key || !v->avars[avi].value) continue;
          AVar *av = v->avars[avi].value;
          if (!av) continue;
          EntrySet *es_contour = av->contour_is_entry_set ? (EntrySet *)av->contour : nullptr;
          if (es_contour && es_contour->fun && !es_contour->fun->live) continue;
          int cid = es_contour ? es_contour->id : -1;
          if (!first_va) root << ",";
          first_va = false;
          std::string av_type_str = format_atype_simple(av->out);
          std::ostringstream type_os;
          serialize_atype(type_os, av->out);
          root << "{\"av_id\":" << av->id
               << ",\"contour_id\":" << cid
               << ",\"type_str\":\"" << json_escape(av_type_str) << "\""
               << ",\"type\":" << type_os.str()
               << "}";
        }
        root << "]}";
      }
      root << "\n        ],\n";

      // PNodes in CFG: fa_all_PNodes is collected backwards from f->exit, so iterate in reverse
      // so that f->entry is first and f->exit is last.
      root << "        \"pnodes\": [\n";
      bool first_pn = true;
      for (int pi = f->fa_all_PNodes.n - 1; pi >= 0; pi--) {
        PNode *pn = f->fa_all_PNodes[pi];
        if (!pn) continue;
        if (!first_pn) root << ",\n";
        first_pn = false;
        root << "          {\n";
        root << "            \"id\": " << pn->id << ",\n";
        root << "            \"kind\": \"" << (pn->code ? code_string[pn->code->kind] : "UNKNOWN") << "\",\n";
        root << "            \"prim\": \"" << json_escape(pn->prim && pn->prim->name ? pn->prim->name : (pn->code && pn->code->prim && pn->code->prim->name ? pn->code->prim->name : "")) << "\",\n";
        root << "            \"line\": " << (pn->code ? pn->code->line() : 0) << ",\n";
        root << "            \"live\": " << (pn->live ? "true" : "false") << ",\n";
        root << "            \"fa_live\": " << (pn->fa_live ? "true" : "false") << ",\n";
        root << "            \"fa_noreturn\": " << (pn->fa_noreturn ? "true" : "false") << ",\n";
        root << "            \"fa_noreturn_raises\": " << (pn->fa_noreturn_raises ? "true" : "false") << ",\n";
        int dom_id = (pn->dom && pn->dom->idom) ? pn->dom->idom->id : -1;
        int label_id = (pn->code && pn->code->label[0]) ? pn->code->label[0]->id : -1;
        int label_else_id = (pn->code && pn->code->label[1]) ? pn->code->label[1]->id : -1;
        root << "            \"dom_id\": " << dom_id << ",\n";
        root << "            \"label_id\": " << label_id << ",\n";
        root << "            \"label_else_id\": " << label_else_id << ",\n";

        root << "            \"lvals\": [";
        bool first_lv = true;
        for (int li = 0; li < pn->lvals.n; li++) {
          Var *v = pn->lvals[li];
          if (!v) continue;
          if (!first_lv) root << ",";
          first_lv = false;
          std::string vtype = var_type_map.count(v->id) ? var_type_map[v->id] : "void";
          root << "{\"id\":" << v->id
               << ",\"name\":\"" << json_escape(v->sym && v->sym->name ? v->sym->name : "") << "\""
               << ",\"type\":\"" << json_escape(vtype) << "\""
               << ",\"live\":" << (v->live ? "true" : "false") << "}";
        }
        root << "],\n";

        root << "            \"rvals\": [";
        bool first_rv = true;
        for (int ri = 0; ri < pn->rvals.n; ri++) {
          Var *v = pn->rvals[ri];
          if (!v) continue;
          if (!first_rv) root << ",";
          first_rv = false;
          std::string vtype = var_type_map.count(v->id) ? var_type_map[v->id] : "void";
          root << "{\"id\":" << v->id
               << ",\"name\":\"" << json_escape(v->sym && v->sym->name ? v->sym->name : "") << "\""
               << ",\"type\":\"" << json_escape(vtype) << "\""
               << ",\"live\":" << (v->live ? "true" : "false") << "}";
        }
        root << "],\n";

        root << "            \"contour_calls\": [";
        auto it_cc = pnode_contour_calls.find(pn->id);
        if (it_cc != pnode_contour_calls.end()) {
          bool first_cc = true;
          for (const auto &cc : it_cc->second) {
            if (!first_cc) root << ",";
            first_cc = false;
            root << "{\"edge_id\":" << cc.edge_id
                 << ",\"from_es\":" << cc.from_es
                 << ",\"to_es\":" << cc.to_es
                 << ",\"callee_id\":" << cc.callee_fun_id
                 << ",\"callee_name\":\"" << json_escape(cc.callee_name) << "\"}";
          }
        }
        root << "],\n";

        root << "            \"cfg_succ\": [";
        bool first_succ = true;
        for (int si = 0; si < pn->cfg_succ.n; si++) {
          if (!pn->cfg_succ[si]) continue;
          if (!first_succ) root << ",";
          first_succ = false;
          root << pn->cfg_succ[si]->id;
        }
        root << "],\n";

        root << "            \"cfg_pred\": [";
        bool first_pred = true;
        for (int pdi = 0; pdi < pn->cfg_pred.n; pdi++) {
          if (!pn->cfg_pred[pdi]) continue;
          if (!first_pred) root << ",";
          first_pred = false;
          root << pn->cfg_pred[pdi]->id;
        }
        root << "]\n";

        root << "          }";
      }
      root << "\n        ]\n";

      root << "      }";
    }
  }
  root << "\n    ]\n  },\n";

  // Phase 3: Flow Analysis (IFA Passes & Companion Partitioning)
  size_t pass_threshold = (size_t)std::max(1, ifa_html_pass_limit_mb) * 1024ULL * 1024ULL;
  size_t max_total_bytes = (size_t)std::max(1, ifa_html_max_size_mb) * 1024ULL * 1024ULL;
  size_t cumulative_pass_bytes = 0;

  // Format all passes first so we can guarantee the final pass (and Pass 1) stay within max_total_bytes
  std::vector<std::string> pass_summaries(g_recorded_passes.size());
  std::vector<std::string> pass_fulls(g_recorded_passes.size());
  for (size_t pi = 0; pi < g_recorded_passes.size(); pi++) {
    format_recorded_pass(g_recorded_passes[pi], pass_summaries[pi], pass_fulls[pi]);
  }
  size_t reserved_last_bytes = g_recorded_passes.size() > 1 ? pass_fulls.back().size() : 0;

  root << "  \"phase3_ifa\": {\n";
  root << "    \"passes\": [\n";
  for (size_t pi = 0; pi < g_recorded_passes.size(); pi++) {
    const RecordedPass &rp = g_recorded_passes[pi];
    const std::string &summary_json = pass_summaries[pi];
    const std::string &full_json = pass_fulls[pi];
    bool is_first_or_last = (pi == 0) || (pi + 1 == g_recorded_passes.size());
    bool over_max_budget = !is_first_or_last &&
                           (cumulative_pass_bytes + full_json.size() + reserved_last_bytes > max_total_bytes);

    if (pi > 0) root << ",\n";
    if (over_max_budget) {
      root << "      {\n";
      root << "        \"pass\": " << rp.pass_num << ",\n";
      root << "        \"external\": false,\n";
      root << "        \"size_bytes\": " << summary_json.size() << ",\n";
      root << "        \"summary_only\": true,\n";
      root << "        \"summary\": " << summary_json << "\n";
      root << "      }";
      cumulative_pass_bytes += summary_json.size();
    } else if (full_json.size() > pass_threshold) {
      // Exceeds individual pass threshold: write companion file
      std::string pass_num_str = std::to_string(rp.pass_num);
      std::string companion_js_path = base_fn + "_pass_" + pass_num_str + ".js";
      std::string companion_json_path = base_fn + "_pass_" + pass_num_str + ".json";

      // Find relative filename for script tag
      std::string companion_rel_js = companion_js_path;
      size_t last_slash = companion_rel_js.find_last_of("/\\");
      if (last_slash != std::string::npos) {
        companion_rel_js = companion_rel_js.substr(last_slash + 1);
      }

      if (ifa_dump_html) {
        std::ofstream cjs(companion_js_path);
        if (cjs.is_open()) {
          cjs << "window.__PYC_PASS_DATA__ = window.__PYC_PASS_DATA__ || {};\n";
          cjs << "window.__PYC_PASS_DATA__[" << rp.pass_num << "] = " << full_json << ";\n";
        }
      }

      if (ifa_dump_json) {
        std::ofstream cjson(companion_json_path);
        if (cjson.is_open()) {
          cjson << full_json << "\n";
        }
      }

      root << "      {\n";
      root << "        \"pass\": " << rp.pass_num << ",\n";
      root << "        \"external\": true,\n";
      root << "        \"companion_file\": \"" << json_escape(companion_rel_js) << "\",\n";
      root << "        \"size_bytes\": " << full_json.size() << ",\n";
      root << "        \"summary_only\": false,\n";
      root << "        \"summary\": " << summary_json << "\n";
      root << "      }";
      cumulative_pass_bytes += full_json.size();
    } else {
      // Inlined in main document
      root << "      {\n";
      root << "        \"pass\": " << rp.pass_num << ",\n";
      root << "        \"external\": false,\n";
      root << "        \"size_bytes\": " << full_json.size() << ",\n";
      root << "        \"summary_only\": false,\n";
      root << "        \"data\": " << full_json << "\n";
      root << "      }";
      cumulative_pass_bytes += full_json.size();
    }
  }
  root << "\n    ]\n  },\n";

  // Phase 4: Cloning & Concrete Type Layouts
  root << "  \"phase4_cloning\": {\n";
  root << "    \"function_clones\": [\n";
  if (compiled_funs) {
    bool first_fc = true;
    for (int fi = 0; fi < compiled_funs->n; fi++) {
      Fun *f = (*compiled_funs)[fi];
      if (!f || !f->live) continue;
      if (!first_fc) root << ",\n";
      first_fc = false;
      bool is_clone = (f->sym && f->sym->fun && f->sym->fun != f);
      int orig_id = (f->sym && f->sym->fun) ? f->sym->fun->id : f->id;
      auto span_it = fun_cg_spans.find(f->id);
      int cg_start = span_it != fun_cg_spans.end() ? span_it->second.first : 0;
      int cg_end = span_it != fun_cg_spans.end() ? span_it->second.second : 0;
      root << "      {\n";
      root << "        \"id\": " << f->id << ",\n";
      root << "        \"name\": \"" << json_escape(f->sym && f->sym->name ? f->sym->name : "*anon*") << "\",\n";
      root << "        \"cg_name\": \"" << json_escape(f->cg_string ? f->cg_string : "") << "\",\n";
      root << "        \"cg_start_line\": " << cg_start << ",\n";
      root << "        \"cg_end_line\": " << cg_end << ",\n";
      root << "        \"filename\": \"" << json_escape(resolve_fun_filename(f, fn)) << "\",\n";
      root << "        \"line\": " << resolve_fun_line(f) << ",\n";
      root << "        \"can_raise\": " << (f->can_raise ? "true" : "false") << ",\n";
      root << "        \"wraps_id\": " << orig_id << ",\n";
      root << "        \"is_clone\": " << (is_clone ? "true" : "false") << ",\n";
      root << "        \"entry_sets\": [";
      for (int ei = 0; ei < f->ess.n; ei++) {
        if (ei > 0) root << ",";
        root << (f->ess[ei] ? f->ess[ei]->id : -1);
      }
      root << "],\n";

      // Formals (in true positional order via positional_arg_positions)
      root << "        \"formals\": [";
      bool first_fm = true;
      for (int pi = 0; pi < f->positional_arg_positions.n; pi++) {
        MPosition *pos = f->positional_arg_positions[pi];
        Var *v = pos ? f->args.get(pos) : nullptr;
        if (!v) continue;
        // Skip internal position-0 function-self argument
        if (pi == 0 && ((v->sym && f->sym && v->sym == f->sym) || (v->type && v->type->is_fun) ||
                        (!v->sym || !v->sym->name || *v->sym->name == '\0'))) {
          continue;
        }
        if (!first_fm) root << ",";
        first_fm = false;
        Vec<CreationSet *> unified_css;
        for (int avi = 0; avi < v->avars.n; avi++) {
          if (!v->avars[avi].key || !v->avars[avi].value) continue;
          AVar *av = v->avars[avi].value;
          if (!av || !av->out) continue;
          for (CreationSet *cs : *av->out) {
            if (cs) unified_css.set_add(cs);
          }
        }
        unified_css.set_to_vec();
        std::string v_type_str = format_atype_simple(unified_css);
        if (v_type_str == "void" || v_type_str.empty()) {
          if (v->constant && v->constant->is_constant && v->constant->constant) {
            v_type_str = std::string(v->constant->name ? v->constant->name : "*anon*") + "=" + v->constant->constant;
          } else if (v->constant && v->constant->name) {
            v_type_str = v->constant->name;
          } else if (v->type && v->type->name) {
            v_type_str = v->type->name;
          } else {
            v_type_str = "unknown";
          }
        }
        cchar *fm_name = (v->sym && v->sym->name && *v->sym->name) ? v->sym->name : "";
        if (!*fm_name && pos) {
          Sym *arg_s = f->arg_syms.get(pos);
          if (arg_s && arg_s->name) fm_name = arg_s->name;
        }
        root << "{\"name\":\"" << json_escape(fm_name) << "\""
             << ",\"type\":\"" << json_escape(v_type_str) << "\"}";
      }
      root << "],\n";

      // Returns
      root << "        \"returns\": [";
      bool first_ret = true;
      for (int ri = 0; ri < f->rets.n; ri++) {
        Var *v = f->rets[ri];
        if (!v) continue;
        if (!first_ret) root << ",";
        first_ret = false;
        Vec<CreationSet *> unified_css;
        for (int avi = 0; avi < v->avars.n; avi++) {
          if (!v->avars[avi].key || !v->avars[avi].value) continue;
          AVar *av = v->avars[avi].value;
          if (!av || !av->out) continue;
          for (CreationSet *cs : *av->out) {
            if (cs) unified_css.set_add(cs);
          }
        }
        unified_css.set_to_vec();
        std::string v_type_str = format_atype_simple(unified_css);
        if (v_type_str == "void" || v_type_str.empty()) {
          if (v->constant && v->constant->is_constant && v->constant->constant) {
            v_type_str = std::string(v->constant->name ? v->constant->name : "*anon*") + "=" + v->constant->constant;
          } else if (v->constant && v->constant->name) {
            v_type_str = v->constant->name;
          } else if (v->type && v->type->name) {
            v_type_str = v->type->name;
          } else {
            v_type_str = "void";
          }
        }
        root << "{\"name\":\"" << json_escape(v->sym && v->sym->name ? v->sym->name : "") << "\""
             << ",\"type\":\"" << json_escape(v_type_str) << "\"}";
      }
      root << "]\n";
      root << "      }";
    }
  }
  root << "\n    ],\n";

  root << "    \"concrete_types\": [\n";
  if (fa) {
    Vec<Sym *> concrete_types;
    for (CreationSet *cs : fa->css) {
      if (cs && cs->type && !cs->type->is_symbol && !cs->type->is_fun && cs->type->type_kind != Type_PRIMITIVE) {
        if (cs->type->type_live) {
          concrete_types.set_add(cs->type);
        }
      }
    }
    concrete_types.set_to_vec();
    bool first_ct = true;
    for (int ti = 0; ti < concrete_types.n; ti++) {
      Sym *t = concrete_types[ti];
      if (!t) continue;
      if (!first_ct) root << ",\n";
      first_ct = false;

      unsigned int max_align = t->alignment ? t->alignment : 8;
      unsigned int cur_offset = 0;
      struct EmittedField {
        std::string name;
        std::string type;
        unsigned int size;
        unsigned int offset;
        unsigned int align;
      };
      std::vector<EmittedField> emitted_fields;
      int elided_count = 0;

      for (int mi = 0; mi < t->has.n; mi++) {
        Sym *m = t->has[mi];
        if (!m) continue;
        // Skip elided/dead slots (matching cg_field_live)
        if (!m->type || (m->var && !m->var->live)) {
          elided_count++;
          continue;
        }

        std::string fld_name;
        if (m->name && strlen(m->name) > 0) {
          fld_name = m->name;
        } else if (mi < t->has_names.n && t->has_names[mi] && strlen(t->has_names[mi]) > 0) {
          fld_name = t->has_names[mi];
        } else {
          fld_name = "e" + std::to_string(mi);
        }

        std::string fld_type = (m->type && m->type->name) ? m->type->name : "ptr";
        unsigned int fld_size = m->size;
        if (fld_size == 0 && m->type) {
          fld_size = m->type->size;
        }
        if (fld_size == 0) fld_size = 8;
        unsigned int fld_align = m->alignment ? m->alignment : (m->type && m->type->alignment ? m->type->alignment : std::min(fld_size, 8u));
        if (fld_align == 0) fld_align = 8;
        if (fld_align > max_align) max_align = fld_align;

        if (fld_align > 1 && (cur_offset % fld_align) != 0) {
          cur_offset += fld_align - (cur_offset % fld_align);
        }
        emitted_fields.push_back({fld_name, fld_type, fld_size, cur_offset, fld_align});
        cur_offset += fld_size;
      }

      if (max_align > 1 && (cur_offset % max_align) != 0) {
        cur_offset += max_align - (cur_offset % max_align);
      }
      unsigned int total_size = t->size ? t->size : cur_offset;
      if (total_size < cur_offset) total_size = cur_offset;

      std::string elem_type_str;
      if (t->element && t->element->type && t->element->type->name) {
        elem_type_str = t->element->type->name;
      }

      root << "      {\n";
      root << "        \"id\": " << t->id << ",\n";
      root << "        \"name\": \"" << json_escape(t->name ? t->name : "*anon*") << "\",\n";
      root << "        \"type_kind\": \"" << (t->type_kind < 11 ? type_kind_string[t->type_kind] : "UNKNOWN") << "\",\n";
      root << "        \"size\": " << total_size << ",\n";
      root << "        \"alignment\": " << max_align << ",\n";
      root << "        \"elided_slots\": " << elided_count << ",\n";
      root << "        \"element_type\": \"" << json_escape(elem_type_str) << "\",\n";
      root << "        \"fields\": [\n";
      for (size_t fi = 0; fi < emitted_fields.size(); fi++) {
        if (fi > 0) root << ",\n";
        const auto &ef = emitted_fields[fi];
        root << "          {\"name\":\"" << json_escape(ef.name) << "\""
             << ",\"type\":\"" << json_escape(ef.type) << "\""
             << ",\"size\":" << ef.size
             << ",\"offset\":" << ef.offset
             << ",\"alignment\":" << ef.align
             << "}";
      }
      root << "\n        ]\n";
      root << "      }";
    }
  }
  root << "\n    ]\n  },\n";

  // Phase 5: Optimizations
  root << "  \"phase5_optimizations\": {\n";
  root << "    \"inlining_events\": [\n";
  const Vec<InlineEvent *> &inlines = inline_events_get();
  bool first_inl = true;
  for (int ii = 0; ii < inlines.n; ii++) {
    InlineEvent *ie = inlines[ii];
    if (!ie) continue;
    if (ie->caller && !ie->caller->live) continue;
    if (!first_inl) root << ",\n";
    first_inl = false;
    root << "      {\"kind\":\"" << inline_kind_name(ie->kind) << "\""
         << ",\"caller_id\":" << (ie->caller ? ie->caller->id : -1)
         << ",\"caller_name\":\"" << json_escape(ie->caller && ie->caller->sym && ie->caller->sym->name ? ie->caller->sym->name : "") << "\""
         << ",\"callee_id\":" << (ie->callee ? ie->callee->id : -1)
         << ",\"callee_name\":\"" << json_escape(ie->callee && ie->callee->sym && ie->callee->sym->name ? ie->callee->sym->name : "") << "\""
         << ",\"pnode_id\":" << (ie->pnode ? ie->pnode->id : -1)
         << "}";
  }
  root << "\n    ]\n";
  root << "  },\n";

  // Phase 6: Generated Code (C / LLVM IR)
  root << "  \"phase6_codegen\": {\n";
  root << "    \"language\": \"" << cg_lang << "\",\n";
  root << "    \"filename\": \"" << json_escape(cg_fn) << "\",\n";
  root << "    \"lines\": [\n";
  for (size_t i = 0; i < cg_lines.size(); i++) {
    if (i > 0) root << ",\n";
    root << "      \"" << json_escape(cg_lines[i]) << "\"";
  }
  root << "\n    ]\n";
  root << "  }\n";

  root << "}\n";
  std::string full_program_json = root.str();

  // If dump-json is on, write pure JSON file
  if (ifa_dump_json) {
    std::ofstream jf(json_fn);
    if (jf.is_open()) {
      jf << full_program_json;
      fprintf(stderr, "wrote %s\n", json_fn.c_str());
    } else {
      fprintf(stderr, "cannot write %s\n", json_fn.c_str());
    }
  }

  // Write Main Standalone HTML5 Application
  if (ifa_dump_html) {
    FILE *fp = fopen(html_fn.c_str(), "w");
    if (!fp) {
      fprintf(stderr, "cannot write %s\n", html_fn.c_str());
      g_recorded_passes.clear();
      inline_events_reset();
      return;
    }

  fprintf(fp, "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n");
  fprintf(fp, "<meta charset=\"UTF-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n");
  fprintf(fp, "<meta name=\"color-scheme\" content=\"dark\">\n");
  fprintf(fp, "<title>pyc Compiler Explorer - %s</title>\n", html_escape_str(fn).c_str());

  // Modern Embedded CSS
  fprintf(fp, "<style>\n");
  fputs(R"CSS(
:root {
  color-scheme: dark;
  --bg-primary: #0f172a;
  --bg-secondary: #1e293b;
  --bg-tertiary: #334155;
  --bg-card: #1e293b;
  --text-main: #f8fafc;
  --text-muted: #94a3b8;
  --text-dim: #64748b;
  --border: #334155;
  --primary: #38bdf8;
  --primary-hover: #0ea5e9;
  --accent-green: #22c55e;
  --accent-red: #ef4444;
  --accent-orange: #f97316;
  --accent-purple: #a855f7;
  --code-bg: #090d16;
  --font-mono: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
}
* { box-sizing: border-box; margin: 0; padding: 0; }
body {
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
  background-color: var(--bg-primary);
  color: var(--text-main);
  line-height: 1.5;
  display: flex;
  flex-direction: column;
  height: 100vh;
  overflow: hidden;
}
a, a:visited {
  color: var(--primary);
  text-decoration: none;
}
a:hover {
  color: var(--primary-hover);
  text-decoration: underline;
}
code, pre {
  font-family: var(--font-mono);
  color: #7dd3fc;
  background-color: rgba(56, 189, 248, 0.1);
  padding: 0.1rem 0.35rem;
  border-radius: 4px;
}
select, option, optgroup {
  background-color: var(--bg-secondary) !important;
  color: var(--text-main) !important;
}
input, textarea, select, button {
  font-family: inherit;
  color: var(--text-main);
}
input::placeholder {
  color: var(--text-dim);
  opacity: 1;
}
header {
  background-color: var(--bg-secondary);
  border-bottom: 1px solid var(--border);
  padding: 0.75rem 1.25rem;
  display: flex;
  justify-content: space-between;
  align-items: center;
  gap: 1rem;
}
.header-title {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  font-size: 1.15rem;
  font-weight: 700;
  color: var(--primary);
}
.header-badge {
  font-size: 0.75rem;
  padding: 0.2rem 0.5rem;
  border-radius: 9999px;
  background-color: var(--bg-tertiary);
  color: var(--text-muted);
  font-weight: 500;
}
.header-stats {
  display: flex;
  align-items: center;
  gap: 0.75rem;
}
.stat-pill {
  display: flex;
  align-items: center;
  gap: 0.35rem;
  font-size: 0.8rem;
  padding: 0.25rem 0.65rem;
  border-radius: 6px;
  background-color: var(--bg-tertiary);
}
.stat-pill.success { color: var(--accent-green); }
.stat-pill.warning { color: var(--accent-orange); }
.stat-pill.danger { color: var(--accent-red); }

nav.tab-bar {
  background-color: var(--bg-secondary);
  border-bottom: 1px solid var(--border);
  display: flex;
  gap: 0.25rem;
  padding: 0.25rem 1rem 0;
  overflow-x: auto;
}
.tab-btn {
  background: none;
  border: none;
  border-bottom: 2px solid transparent;
  padding: 0.6rem 0.9rem;
  color: var(--text-muted);
  font-size: 0.85rem;
  font-weight: 600;
  cursor: pointer;
  display: flex;
  align-items: center;
  gap: 0.4rem;
  white-space: nowrap;
}
.tab-btn:hover { color: var(--text-main); }
.tab-btn.active {
  color: var(--primary);
  border-bottom-color: var(--primary);
  background-color: rgba(56, 189, 248, 0.05);
}

main.content-area {
  flex: 1;
  overflow: hidden;
  position: relative;
  display: flex;
}
.view-panel {
  display: none;
  width: 100%;
  height: 100%;
  overflow-y: auto;
  padding: 1.25rem;
}
.view-panel.active { display: flex; flex-direction: column; }

/* Dashboard Cards */
.grid-cards {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1rem;
  margin-bottom: 1.5rem;
}
.card {
  background-color: var(--bg-card);
  border: 1px solid var(--border);
  border-radius: 8px;
  padding: 1rem;
}
.card-label { font-size: 0.8rem; color: var(--text-muted); text-transform: uppercase; letter-spacing: 0.05em; }
.card-value { font-size: 1.75rem; font-weight: 700; margin-top: 0.35rem; color: var(--text-main); }
.card-sub { font-size: 0.75rem; color: var(--text-dim); margin-top: 0.25rem; }

/* Controls bar */
.controls-bar {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1rem;
  gap: 1rem;
  flex-wrap: wrap;
}
.search-input {
  background-color: var(--bg-tertiary);
  border: 1px solid var(--border);
  color: var(--text-main);
  padding: 0.4rem 0.75rem;
  border-radius: 6px;
  font-size: 0.85rem;
  width: 250px;
}
.search-input:focus { outline: 1px solid var(--primary); }

/* Table styling */
.table-container {
  background-color: var(--bg-card);
  border: 1px solid var(--border);
  border-radius: 8px;
  overflow: auto;
  max-height: calc(100vh - 220px);
}
table.data-table {
  width: 100%;
  border-collapse: collapse;
  text-align: left;
  font-size: 0.85rem;
}
table.data-table th {
  background-color: var(--bg-secondary);
  color: var(--text-muted);
  padding: 0.65rem 0.85rem;
  border-bottom: 1px solid var(--border);
  position: sticky;
  top: 0;
  z-index: 10;
}
table.data-table td {
  padding: 0.65rem 0.85rem;
  border-bottom: 1px solid rgba(51, 65, 85, 0.4);
  color: var(--text-main);
}
table.data-table tr:hover { background-color: rgba(51, 65, 85, 0.3); }

/* SVG Canvas */
.canvas-container {
  flex: 1;
  background-color: var(--code-bg);
  border: 1px solid var(--border);
  border-radius: 8px;
  position: relative;
  overflow: hidden;
}
svg.interactive-canvas {
  width: 100%;
  height: 100%;
  cursor: grab;
}
svg.interactive-canvas:active { cursor: grabbing; }

/* Control buttons & dropdowns */
.btn-sm {
  background-color: var(--bg-tertiary);
  color: var(--text-main);
  border: 1px solid var(--border);
  padding: 0.35rem 0.65rem;
  border-radius: 6px;
  font-size: 0.8rem;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.15s ease;
}
.btn-sm:hover {
  background-color: var(--primary-hover);
  color: #ffffff;
}
.dropdown-control {
  background: var(--bg-tertiary);
  color: var(--text-main);
  border: 1px solid var(--border);
  padding: 0.35rem 0.65rem;
  border-radius: 6px;
  font-size: 0.8rem;
  cursor: pointer;
}
.dropdown-control:focus {
  outline: 1px solid var(--primary);
}

/* Call Graph Inspector Card */
.inspector-card {
  position: absolute;
  bottom: 1.5rem;
  right: 1.5rem;
  width: 300px;
  background-color: var(--bg-card);
  border: 1px solid var(--border);
  border-radius: 8px;
  padding: 1rem;
  box-shadow: 0 10px 25px -5px rgba(0, 0, 0, 0.5);
  z-index: 100;
  font-size: 0.85rem;
}
.inspector-card h4 {
  color: var(--primary);
  margin-bottom: 0.5rem;
  word-break: break-all;
}
.inspector-card .prop-row {
  display: flex;
  justify-content: space-between;
  margin-bottom: 0.25rem;
  color: var(--text-muted);
}
.inspector-card .prop-val {
  color: var(--text-main);
  font-weight: 600;
  font-family: var(--font-mono);
}

/* Loading Spinner */
.loading-spinner {
  width: 36px;
  height: 36px;
  border: 3px solid rgba(56, 189, 248, 0.2);
  border-top-color: var(--primary);
  border-radius: 50%;
  animation: cg-spin 0.8s linear infinite;
}
@keyframes cg-spin {
  to { transform: rotate(360deg); }
}

/* Pass Stepper Bar */
.pass-stepper {
  background-color: var(--bg-secondary);
  border: 1px solid var(--border);
  border-radius: 8px;
  padding: 0.75rem 1rem;
  margin-bottom: 1rem;
  display: flex;
  align-items: center;
  gap: 1rem;
}
.stepper-btn {
  background-color: var(--bg-tertiary);
  color: var(--text-main);
  border: 1px solid var(--border);
  padding: 0.4rem 0.8rem;
  border-radius: 6px;
  font-size: 0.85rem;
  cursor: pointer;
  font-weight: 600;
  transition: all 0.15s ease;
}
.stepper-btn:hover { background-color: var(--primary-hover); color: #ffffff; }
.stepper-slider { flex: 1; cursor: pointer; accent-color: var(--primary); }

/* Badges */
.badge {
  display: inline-block;
  padding: 0.15rem 0.45rem;
  border-radius: 4px;
  font-size: 0.75rem;
  font-weight: 600;
  font-family: var(--font-mono);
}
.badge-live { background-color: rgba(34, 197, 94, 0.2); color: #4ade80; border: 1px solid rgba(34, 197, 94, 0.3); }
.badge-dead { background-color: rgba(239, 68, 68, 0.2); color: #f87171; border: 1px solid rgba(239, 68, 68, 0.3); text-decoration: line-through; }
.badge-prim { background-color: rgba(56, 189, 248, 0.2); color: #7dd3fc; border: 1px solid rgba(56, 189, 248, 0.3); }
.badge-kind { background-color: rgba(168, 85, 247, 0.2); color: #c084fc; border: 1px solid rgba(168, 85, 247, 0.3); }
.badge-type { background-color: rgba(168, 85, 247, 0.2); color: #d8b4fe; border: 1px solid rgba(168, 85, 247, 0.3); font-family: var(--font-mono); }
.badge-var { background-color: rgba(34, 197, 94, 0.2); color: #86efac; border: 1px solid rgba(34, 197, 94, 0.3); font-family: var(--font-mono); cursor: pointer; }
.badge-var:hover { background-color: rgba(34, 197, 94, 0.35); }
.badge-contour { background-color: rgba(56, 189, 248, 0.2); color: #7dd3fc; border: 1px solid rgba(56, 189, 248, 0.3); font-family: var(--font-mono); cursor: pointer; }

/* Button Group */
.btn-group { display: inline-flex; border-radius: 6px; overflow: hidden; border: 1px solid var(--border); }
.btn-group .btn-sm { border-radius: 0; border: none; border-right: 1px solid var(--border); }
.btn-group .btn-sm:last-child { border-right: none; }
.btn-group .btn-sm.active { background-color: var(--primary-hover); color: #ffffff; font-weight: 700; }

/* Inspector sections */
.inspector-section { margin-top: 0.75rem; border-top: 1px solid var(--border); padding-top: 0.5rem; }
.inspector-section-title { font-size: 0.75rem; font-weight: 700; text-transform: uppercase; color: var(--text-muted); margin-bottom: 0.35rem; }
.inspector-table { width: 100%; border-collapse: collapse; font-size: 0.75rem; margin-top: 0.25rem; }
.inspector-table th { text-align: left; padding: 0.25rem; color: var(--text-muted); border-bottom: 1px solid var(--border); }
.inspector-table td { padding: 0.25rem; border-bottom: 1px solid rgba(51, 65, 85, 0.3); color: var(--text-main); }

/* Memory Box Visualizer */
.memory-bar {
  display: flex;
  width: 100%;
  height: 48px;
  border: 1px solid var(--border);
  border-radius: 6px;
  overflow: hidden;
  margin-top: 0.5rem;
}
.memory-chunk {
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  padding: 0.25rem;
  font-size: 0.75rem;
  font-family: var(--font-mono);
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  border-right: 1px solid rgba(0,0,0,0.3);
}
.memory-chunk.field { background-color: #0284c7; color: #fff; }
.memory-chunk.padding { background-color: #334155; color: #94a3b8; background-image: repeating-linear-gradient(45deg, transparent, transparent 5px, rgba(0,0,0,0.2) 5px, rgba(0,0,0,0.2) 10px); }

/* Code viewer */
.code-container {
  background-color: var(--code-bg);
  border: 1px solid var(--border);
  border-radius: 8px;
  padding: 1rem;
  font-family: var(--font-mono);
  font-size: 0.85rem;
  overflow: auto;
  line-height: 1.6;
}
.code-line { display: flex; gap: 1rem; padding: 0 0.25rem; border-radius: 3px; border-left: 3px solid transparent; }
.code-line.highlight-line { background-color: rgba(56, 189, 248, 0.2); outline: 1px solid rgba(56, 189, 248, 0.5); }
.code-line.fun-span-line { background-color: rgba(56, 189, 248, 0.07); border-left-color: rgba(56, 189, 248, 0.55); }
.code-line.pnode-match-line { background-color: rgba(34, 197, 94, 0.2); border-left-color: #22c55e; outline: 1px solid rgba(34, 197, 94, 0.45); }
.code-line.clickable-line { cursor: pointer; }
.code-line.clickable-line:hover { background-color: rgba(255, 255, 255, 0.06); }
.line-num { color: var(--text-dim); user-select: none; text-align: right; width: 44px; flex-shrink: 0; }
.line-text { color: var(--text-main); white-space: pre; }

/* CFG & Source Side-by-Side Split Workspace */
.cfg-workspace {
  display: flex;
  gap: 0.75rem;
  flex: 1;
  min-height: 520px;
  height: calc(100vh - 195px);
}
.cfg-main-pane {
  flex: 1;
  min-width: 0;
  display: flex;
  flex-direction: column;
  position: relative;
}
.cfg-split-dock {
  width: 46%;
  min-width: 360px;
  display: none;
  flex-direction: column;
  gap: 0.6rem;
  height: 100%;
}
.cfg-split-dock.active { display: flex; }
.dock-pane {
  flex: 1;
  min-height: 0;
  display: flex;
  flex-direction: column;
  background-color: var(--bg-secondary);
  border: 1px solid var(--border);
  border-radius: 8px;
  overflow: hidden;
}
.dock-pane-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0.4rem 0.75rem;
  background-color: var(--bg-tertiary);
  border-bottom: 1px solid var(--border);
  font-size: 0.78rem;
  font-weight: 600;
}
.dock-pane-body {
  flex: 1;
  min-height: 0;
  overflow: auto;
  padding: 0.5rem;
  font-family: var(--font-mono);
  font-size: 0.78rem;
  line-height: 1.5;
  background-color: var(--code-bg);
}
.source-split-grid {
  display: flex;
  gap: 0.75rem;
  height: calc(100vh - 195px);
}
.source-split-col {
  flex: 1;
  min-width: 0;
  display: flex;
  flex-direction: column;
  border: 1px solid var(--border);
  border-radius: 8px;
  overflow: hidden;
  background-color: var(--code-bg);
}
.source-split-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0.45rem 0.85rem;
  background-color: var(--bg-secondary);
  border-bottom: 1px solid var(--border);
  font-size: 0.82rem;
  font-weight: 600;
}

/* Pass-to-Pass Diff Mode Styles */
.diff-delta {
  display: inline-block;
  font-size: 0.8rem;
  font-weight: 600;
  padding: 0.1rem 0.4rem;
  border-radius: 4px;
  margin-left: 0.45rem;
  vertical-align: middle;
}
.diff-delta.plus { background: rgba(56, 189, 248, 0.18); color: #38bdf8; }
.diff-delta.minus { background: rgba(34, 197, 94, 0.18); color: #4ade80; }
.diff-delta.warn-plus { background: rgba(239, 68, 68, 0.2); color: #f87171; }
.diff-delta.zero { background: rgba(148, 163, 184, 0.12); color: var(--text-muted); }
.diff-row-added { background-color: rgba(34, 197, 94, 0.08) !important; border-left: 3px solid #22c55e; }
.diff-row-changed { background-color: rgba(234, 179, 8, 0.08) !important; border-left: 3px solid #eab308; }
.diff-badge-new { background-color: rgba(34, 197, 94, 0.22); color: #4ade80; border: 1px solid rgba(34, 197, 94, 0.4); }
.diff-badge-mod { background-color: rgba(234, 179, 8, 0.22); color: #facc15; border: 1px solid rgba(234, 179, 8, 0.4); }
.diff-badge-pers { background-color: rgba(249, 115, 22, 0.2); color: #fb923c; border: 1px solid rgba(249, 115, 22, 0.35); }
.diff-badge-resolved { background-color: rgba(34, 197, 94, 0.2); color: #4ade80; border: 1px solid rgba(34, 197, 94, 0.35); }
.diff-old { color: #f87171; text-decoration: line-through; opacity: 0.85; font-size: 0.78rem; }
.diff-new { color: #4ade80; font-weight: 600; }

/* Splits Deep-Dive Section Styles */
.splits-header-bar {
  display: flex;
  justify-content: space-between;
  align-items: center;
  flex-wrap: wrap;
  gap: 0.65rem;
  margin: 0.75rem 0 0.5rem;
  padding: 0.55rem 0.8rem;
  background-color: var(--bg-secondary);
  border: 1px solid var(--border);
  border-radius: 8px;
}
.split-stage-badge {
  display: inline-block;
  padding: 0.15rem 0.45rem;
  border-radius: 4px;
  font-size: 0.7rem;
  font-weight: 700;
  font-family: var(--font-mono);
  background: rgba(56, 189, 248, 0.18);
  color: #38bdf8;
  border: 1px solid rgba(56, 189, 248, 0.35);
}
.split-stage-badge.stage-viol {
  background: rgba(239, 68, 68, 0.2);
  color: #f87171;
  border-color: rgba(239, 68, 68, 0.4);
}
.split-stage-badge.stage-cs {
  background: rgba(168, 85, 247, 0.2);
  color: #c084fc;
  border-color: rgba(168, 85, 247, 0.4);
}
.split-subkind-badge {
  display: inline-block;
  padding: 0.12rem 0.4rem;
  border-radius: 4px;
  font-size: 0.68rem;
  font-family: var(--font-mono);
  background: rgba(148, 163, 184, 0.15);
  color: #cbd5e1;
  border: 1px solid rgba(148, 163, 184, 0.25);
}
.split-product-pill {
  display: inline-flex;
  align-items: center;
  gap: 0.35rem;
  padding: 0.2rem 0.45rem;
  margin: 0.15rem 0.25rem 0.15rem 0;
  background: rgba(15, 23, 42, 0.6);
  border: 1px solid var(--border);
  border-radius: 5px;
  font-size: 0.75rem;
}
.split-reason-box {
  font-size: 0.8rem;
  line-height: 1.45;
  color: var(--text-main);
  margin-bottom: 0.35rem;
}
.split-trigger-meta {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 0.35rem;
  font-size: 0.74rem;
  color: var(--text-muted);
}
.split-impact-list {
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  margin-top: 0.25rem;
}
.split-impact-item {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: 0.35rem;
  padding: 0.25rem 0.45rem;
  background: rgba(15, 23, 42, 0.55);
  border: 1px solid rgba(51, 65, 85, 0.6);
  border-radius: 5px;
  font-size: 0.74rem;
}
.stage-event-chip {
  display: inline-flex;
  align-items: center;
  gap: 0.35rem;
  padding: 0.25rem 0.55rem;
  border-radius: 6px;
  background: rgba(56, 189, 248, 0.14);
  border: 1px solid rgba(56, 189, 248, 0.35);
  color: var(--text-main);
  font-size: 0.76rem;
  cursor: pointer;
  transition: background 0.15s;
}
.stage-event-chip:hover, .stage-event-chip.active {
  background: rgba(56, 189, 248, 0.3);
  border-color: var(--primary);
}
)CSS", fp);
  fprintf(fp, "</style>\n</head>\n<body>\n");

  // Application Layout HTML
  fputs(R"HTML(
<header>
  <div class="header-title">
    <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="12" cy="12" r="10"/><path d="m10 15 5-3-5-3v6z"/></svg>
    <span>pyc Compiler Explorer</span>
    <span class="header-badge" id="hdr-program"></span>
  </div>
  <div class="header-stats">
    <div class="stat-pill" id="pill-backend"></div>
    <div class="stat-pill" id="pill-passes"></div>
    <div class="stat-pill" id="pill-contours"></div>
    <div class="stat-pill" id="pill-viol"></div>
  </div>
</header>

<nav class="tab-bar">
  <button class="tab-btn active" data-tab="dashboard">📊 Dashboard</button>
  <button class="tab-btn" data-tab="callgraph">🌐 Call Graph</button>
  <button class="tab-btn" data-tab="cfg">🔀 CFG & SSU</button>
  <button class="tab-btn" data-tab="passes">🔬 IFA Passes</button>
  <button class="tab-btn" data-tab="dataflow">🔄 Dataflow Tracer</button>
  <button class="tab-btn" data-tab="transfer">⚡ Transfer Functions</button>
  <button class="tab-btn" data-tab="memory">🧬 Clones & Memory</button>
  <button class="tab-btn" data-tab="source">📜 Source &amp; Codegen</button>
</nav>

<main class="content-area">
  <!-- View: Dashboard -->
  <div class="view-panel active" id="view-dashboard">
    <div class="grid-cards">
      <div class="card"><div class="card-label">Source Lines</div><div class="card-value" id="card-lines">0</div><div class="card-sub">Program Source</div></div>
      <div class="card"><div class="card-label">Functions</div><div class="card-value" id="card-funs">0</div><div class="card-sub">Compiled Bodies</div></div>
      <div class="card"><div class="card-label">IFA Passes</div><div class="card-value" id="card-passes">0</div><div class="card-sub">Flow Iterations</div></div>
      <div class="card"><div class="card-label">EntrySets (Contours)</div><div class="card-value" id="card-ess">0</div><div class="card-sub">Demand Function Contours</div></div>
      <div class="card"><div class="card-label">CreationSets (Data)</div><div class="card-value" id="card-css">0</div><div class="card-sub">Data Contours</div></div>
      <div class="card"><div class="card-label">Inlined Calls</div><div class="card-value" id="card-inlines">0</div><div class="card-sub">Inline Optimizations</div></div>
    </div>
    <div class="card" style="margin-top: 1rem;">
      <h3 style="margin-bottom: 0.75rem;">Analysis Convergence Trajectory <span style="font-size:0.75rem; font-weight:400; color:var(--text-muted);">(click a point to inspect pass)</span></h3>
      <div id="chart-trajectory" style="height: 260px; width: 100%;"></div>
    </div>
    <div class="card" id="dashboard-inlining-card" style="margin-top: 1rem; display: none;">
      <h3 style="margin-bottom: 0.75rem;">Phase 5 Inlining Optimizations</h3>
      <div class="table-container" style="max-height: 260px;">
        <table class="data-table" id="dashboard-inlines-table">
          <thead><tr><th>Caller</th><th>Inlined Callee</th><th>Call Site PNode</th><th>Reason</th></tr></thead>
          <tbody></tbody>
        </table>
      </div>
    </div>
  </div>

  <!-- View: Call Graph -->
  <div class="view-panel" id="view-callgraph">
    <div class="controls-bar">
      <div style="display:flex; align-items:center; gap:0.75rem;">
        <span style="font-weight: 600;">Call Graph</span>
        <span class="header-badge" id="cg-node-count">0 functions</span>
      </div>
      <div style="display:flex; align-items:center; gap:0.5rem; flex-wrap:wrap;">
        <label style="font-size:0.8rem; color:var(--text-muted);">Scope:</label>
        <select id="cg-scope-select" class="dropdown-control">
          <option value="all" selected>All Executable Functions</option>
          <option value="active">Connected Only</option>
          <option value="user">User Code Only</option>
        </select>

        <label style="font-size:0.8rem; color:var(--text-muted); margin-left:0.25rem;">Depth:</label>
        <select id="cg-depth-select" class="dropdown-control">
          <option value="all" selected>All Levels</option>
          <option value="1">Level 1 (Entry + Direct)</option>
          <option value="2">Level 2</option>
          <option value="3">Level 3</option>
        </select>

        <label style="font-size:0.8rem; color:var(--text-muted); margin-left:0.25rem;">Direction:</label>
        <select id="cg-direction-select" class="dropdown-control">
          <option value="DOWN" selected>Top-to-Bottom</option>
          <option value="RIGHT">Left-to-Right</option>
        </select>

        <input type="text" class="search-input" id="cg-search" placeholder="Search functions..." style="width:150px;">

        <div style="display:flex; gap:0.25rem;">
          <button class="btn-sm" id="cg-btn-expand-all" title="Expand All Levels">⊞ Expand All</button>
          <button class="btn-sm" id="cg-btn-collapse-root" title="Collapse to Entry Point">⊟ Collapse</button>
          <button class="btn-sm" id="cg-btn-zoom-in" title="Zoom In">+</button>
          <button class="btn-sm" id="cg-btn-zoom-out" title="Zoom Out">−</button>
          <button class="btn-sm" id="cg-btn-fit" title="Fit to View">⛶ Fit</button>
          <button class="btn-sm" id="cg-btn-reset" title="Reset Selection">↺ Reset</button>
        </div>
      </div>
    </div>
    <div class="canvas-container" style="position:relative; flex:1; min-height:500px;">
      <div id="cy-callgraph" style="width:100%; height:100%;"></div>
      <div id="cg-node-inspector" class="inspector-card" style="display:none;"></div>
      <div id="cg-loading-overlay" style="display:flex; flex-direction:column; align-items:center; justify-content:center; position:absolute; top:0; left:0; width:100%; height:100%; background:rgba(15,23,42,0.85); z-index:50;">
        <div class="loading-spinner"></div>
        <p id="cg-loading-status" style="margin-top:1rem; font-size:0.9rem; color:var(--primary); font-weight:600;">Loading graph engine...</p>
      </div>
      <div id="cg-offline-fallback" style="display:none; position:absolute; top:50%; left:50%; transform:translate(-50%, -50%); text-align:center; color:var(--text-muted); padding:2rem; background:var(--bg-card); border:1px solid var(--border); border-radius:8px; z-index:60;">
        <p style="font-weight:600; color:var(--accent-orange); margin-bottom:0.5rem;">Could not load graph engine</p>
        <p style="font-size:0.85rem;">Cytoscape / ELK could not be reached via CDN. Please check your internet connection.</p>
        <button class="btn-sm" style="margin-top:0.75rem;" onclick="loadCallGraphScripts(initCallGraph)">Retry</button>
      </div>
    </div>
  </div>

  <!-- View: CFG & SSU -->
  <div class="view-panel" id="view-cfg" style="position:relative;">
    <div class="controls-bar" style="margin-bottom:0.75rem;">
      <div style="display:flex; align-items:center; gap:0.55rem; flex-wrap:wrap;">
        <label style="font-weight:600; font-size:0.85rem;">Function:</label>
        <select id="cfg-fun-select" class="dropdown-control"></select>

        <label style="font-weight:600; font-size:0.85rem; margin-left:0.2rem;">Pass:</label>
        <select id="cfg-pass-select" class="dropdown-control"></select>

        <label style="font-weight:600; font-size:0.85rem; margin-left:0.2rem;">Contour:</label>
        <select id="cfg-contour-select" class="dropdown-control"></select>

        <div class="btn-group" style="margin-left:0.35rem;">
          <button id="btn-cfg-mode-graph" class="btn-sm active">📈 Visual Graph</button>
          <button id="btn-cfg-mode-table" class="btn-sm">📋 Instruction Table</button>
        </div>

        <label style="font-weight:600; font-size:0.8rem; color:var(--text-muted); margin-left:0.35rem;">Split Dock:</label>
        <div class="btn-group" title="Side-by-Side Source &amp; Generated C/LLVM Split View">
          <button id="btn-cfg-split-off" class="btn-sm active" onclick="setCfgSplitMode('off')">Off</button>
          <button id="btn-cfg-split-py" class="btn-sm" onclick="setCfgSplitMode('py')">📜 Python</button>
          <button id="btn-cfg-split-cg" class="btn-sm" onclick="setCfgSplitMode('cg')">⚙️ C/LLVM</button>
          <button id="btn-cfg-split-both" class="btn-sm" onclick="setCfgSplitMode('both')">⚡ Both</button>
        </div>
      </div>

      <div style="display:flex; align-items:center; gap:0.4rem;">
        <div id="cfg-graph-controls" style="display:flex; gap:0.25rem;">
          <button class="btn-sm" id="cfg-btn-zoom-in" title="Zoom In">+</button>
          <button class="btn-sm" id="cfg-btn-zoom-out" title="Zoom Out">−</button>
          <button class="btn-sm" id="cfg-btn-fit" title="Fit to View">⛶ Fit</button>
          <button class="btn-sm" id="cfg-btn-reset" title="Reset View">↺ Reset</button>
        </div>
        <div id="cfg-fun-meta" style="font-size:0.8rem; color:var(--text-muted); margin-left:0.5rem;"></div>
      </div>
    </div>

    <div class="cfg-workspace">
      <div class="cfg-main-pane">
        <!-- Container for Visual Graph -->
        <div id="cfg-graph-view" class="canvas-container" style="position:relative; flex:1; min-height:480px;">
          <div id="cy-cfg" style="width:100%; height:100%;"></div>
          <div id="cfg-loading-overlay" style="display:none; flex-direction:column; align-items:center; justify-content:center; position:absolute; top:0; left:0; width:100%; height:100%; background:rgba(15,23,42,0.85); z-index:50;">
            <div class="loading-spinner"></div>
            <p style="margin-top:1rem; font-size:0.9rem; color:var(--primary); font-weight:600;">Rendering CFG layout...</p>
          </div>
        </div>

        <!-- Container for Instruction Table -->
        <div id="cfg-table-view" class="table-container" style="display:none; flex:1; max-height:100%;">
          <table class="data-table" id="cfg-pnodes-table">
            <thead><tr><th>ID</th><th>Kind</th><th>Op / Prim</th><th>Line</th><th>Lvals (Defs &amp; Types)</th><th>Rvals (Uses &amp; Types)</th><th>Contour Calls</th><th>Successors</th><th>Predecessors</th></tr></thead>
            <tbody></tbody>
          </table>
        </div>

        <!-- CFG Inspector Card / Slide-out Panel -->
        <div id="cfg-inspector" class="inspector-card" style="display:none; width:360px; max-height:calc(100% - 2rem); overflow-y:auto; top:1rem; right:1rem; bottom:auto;">
        </div>
      </div>

      <!-- Side-by-Side Source + Target Code Split Dock -->
      <div id="cfg-split-dock" class="cfg-split-dock">
        <div id="cfg-dock-py-pane" class="dock-pane">
          <div class="dock-pane-header">
            <span id="cfg-dock-py-title">📜 Python Source (click line to select PNode)</span>
            <button class="btn-sm" style="padding:0.1rem 0.4rem; font-size:0.7rem;" onclick="openCurrentFunInSourceTab('py')">Open in Tab ↗</button>
          </div>
          <div id="cfg-dock-py-body" class="dock-pane-body"></div>
        </div>
        <div id="cfg-dock-cg-pane" class="dock-pane">
          <div class="dock-pane-header">
            <span id="cfg-dock-cg-title">⚙️ Generated Target Code</span>
            <button class="btn-sm" style="padding:0.1rem 0.4rem; font-size:0.7rem;" onclick="openCurrentFunInSourceTab('cg')">Open in Tab ↗</button>
          </div>
          <div id="cfg-dock-cg-body" class="dock-pane-body"></div>
        </div>
      </div>
    </div>
  </div>

  <!-- View: IFA Passes -->
  <div class="view-panel" id="view-passes">
    <div class="pass-stepper" style="flex-wrap:wrap;">
      <button class="stepper-btn" id="btn-prev-pass">&lt; Prev Pass</button>
      <input type="range" class="stepper-slider" id="pass-slider" min="1" max="1" value="1" style="min-width:140px;">
      <button class="stepper-btn" id="btn-next-pass">Next Pass &gt;</button>
      <span style="font-weight:700; min-width:95px; text-align:center;" id="pass-indicator">Pass 1 / 1</span>

      <div style="display:flex; align-items:center; gap:0.4rem; border-left:1px solid var(--border); padding-left:0.75rem;">
        <button class="btn-sm active" id="btn-pass-diff-toggle" title="Compare against another pass and highlight newly split contours, resolved violations, and widened AVars">🔀 Diff Mode: ON</button>
        <span style="font-size:0.78rem; color:var(--text-muted);">vs</span>
        <select id="pass-diff-base-select" class="dropdown-control" title="Base pass for comparison">
          <option value="prev" selected>Prev Pass (N−1)</option>
        </select>
        <button class="btn-sm" id="btn-pass-changed-only" title="Filter tables to show only contours and AVars that changed in this pass">⚡ Changed Only</button>
      </div>

      <input type="text" class="search-input" id="pass-filter-input" placeholder="Filter contours or types..." style="width:190px;">
    </div>
    <div id="pass-detail-container">
      <div class="grid-cards" style="margin-bottom:1rem;">
        <div class="card"><div class="card-label">Pass EntrySets</div><div class="card-value" id="pass-card-ess">0</div></div>
        <div class="card"><div class="card-label">Pass CreationSets</div><div class="card-value" id="pass-card-css">0</div></div>
        <div class="card"><div class="card-label">Splits in Pass</div><div class="card-value" id="pass-card-splits">0</div></div>
        <div class="card"><div class="card-label">Type Violations</div><div class="card-value" id="pass-card-viol">0</div></div>
        <div class="card"><div class="card-label">Dup Split Redo</div><div class="card-value" id="pass-card-dup">0</div></div>
      </div>
      <div id="pass-summary-only-banner" style="display:none; margin-bottom:1rem; padding:0.75rem 1rem; border-radius:6px; background:rgba(249,115,22,0.15); border:1px solid rgba(249,115,22,0.4); color:var(--accent-orange); font-size:0.85rem;">
        Detailed contour tables for this intermediate pass were omitted to respect <code>--html-max-size</code>. Summary metrics and split events are shown below.
      </div>
      <div id="pass-events-section" style="display:none; margin-bottom:1rem;">
        <h4 style="margin: 0.5rem 0 0.4rem;">Splitter Stage Events in this Pass <span style="font-size:0.74rem; font-weight:400; color:var(--text-muted); margin-left:0.4rem;">(click a stage to filter splits below)</span></h4>
        <div id="pass-events-badges" style="display:flex; gap:0.5rem; flex-wrap:wrap;"></div>
      </div>
      <div id="pass-splits-section" style="margin-bottom:1.25rem;">
        <div class="splits-header-bar">
          <div style="display:flex; align-items:center; gap:0.6rem; flex-wrap:wrap;">
            <h4 style="margin:0; font-size:0.92rem; color:var(--text-main);">🔬 Splits Deep Dive (Why &amp; What Was Affected)</h4>
            <span id="pass-splits-summary-label" style="font-size:0.78rem; color:var(--text-muted);"></span>
            <span id="pass-splits-stage-chip" style="display:none;"></span>
          </div>
          <div style="display:flex; align-items:center; gap:0.5rem; flex-wrap:wrap;">
            <span style="font-size:0.75rem; color:var(--text-muted);">Scope:</span>
            <div class="btn-group">
              <button class="btn-sm active" id="btn-splits-scope-decided" onclick="setSplitsScope('decided')" title="Splits decided during extend_analysis() at the end of this pass">🔨 Decided in Pass</button>
              <button class="btn-sm" id="btn-splits-scope-entering" onclick="setSplitsScope('entering')" title="Splits decided at the end of Pass N-1 that created the contours entering this pass">📥 Entering Pass</button>
              <button class="btn-sm" id="btn-splits-scope-all" onclick="setSplitsScope('all')" title="Chronological view of every split across all IFA passes">📚 All Passes</button>
            </div>
            <span style="font-size:0.75rem; color:var(--text-muted); margin-left:0.2rem;">Kind:</span>
            <div class="btn-group">
              <button class="btn-sm active" id="btn-splits-kind-all" onclick="setSplitsKindFilter('all')">All</button>
              <button class="btn-sm" id="btn-splits-kind-es" onclick="setSplitsKindFilter('EntrySet')">EntrySet</button>
              <button class="btn-sm" id="btn-splits-kind-cs" onclick="setSplitsKindFilter('CreationSet')">CreationSet</button>
              <button class="btn-sm" id="btn-splits-kind-other" onclick="setSplitsKindFilter('other')">Const / Arity</button>
            </div>
            <button class="btn-sm" id="btn-splits-expand-all" onclick="toggleSplitsExpandAll()" title="Expand or collapse all affected call edges and creation points">⊞ Expand All</button>
          </div>
        </div>
        <div id="pass-splits-context-banner" style="display:none; margin-bottom:0.5rem; padding:0.45rem 0.75rem; border-radius:6px; background:rgba(56,189,248,0.1); border:1px solid rgba(56,189,248,0.3); color:#7dd3fc; font-size:0.78rem;"></div>
        <div class="table-container" style="max-height: 440px;">
          <table class="data-table" id="pass-splits-table">
            <thead>
              <tr>
                <th style="width:76px;">Pass / #</th>
                <th style="width:155px;">Stage &amp; Mechanism</th>
                <th style="width:270px;">What Was Split (Parent &rarr; Products)</th>
                <th>Why It Split (Trigger, Demand &amp; Converged Union)</th>
                <th style="width:340px;">What Was Affected (Call Edges / Creation Points)</th>
              </tr>
            </thead>
            <tbody></tbody>
          </table>
        </div>
      </div>
      <div id="pass-violations-section" style="display:none; margin-bottom:1rem;">
        <h4 style="margin: 0.5rem 0 0.4rem; color: var(--accent-orange);">Type Violations Evaluated at Quiescence</h4>
        <div class="table-container" style="max-height: 200px;">
          <table class="data-table" id="pass-viol-table">
            <thead><tr><th>Status / Kind</th><th>AVar</th><th>Variable</th><th>Function</th><th>Contour</th><th>Conflicting Union Types</th></tr></thead>
            <tbody></tbody>
          </table>
        </div>
      </div>
      <div id="pass-resolved-viol-section" style="display:none; margin-bottom:1rem;">
        <h4 style="margin: 0.5rem 0 0.4rem; color: var(--accent-green);">✓ Violations Resolved Since Comparison Pass</h4>
        <div class="table-container" style="max-height: 170px;">
          <table class="data-table" id="pass-resolved-viol-table">
            <thead><tr><th>Status / Kind</th><th>AVar</th><th>Variable</th><th>Function</th><th>Contour</th><th>Previous Conflicting Types</th></tr></thead>
            <tbody></tbody>
          </table>
        </div>
      </div>
      <h4 style="margin: 0.75rem 0 0.5rem;">Contours (EntrySets) in this Pass <span id="pass-ess-diff-summary" style="font-size:0.78rem; font-weight:400; color:var(--text-muted); margin-left:0.5rem;"></span></h4>
      <div class="table-container" style="max-height: 280px; margin-bottom: 1rem;">
        <table class="data-table" id="pass-ess-table">
          <thead><tr><th>ID</th><th>Function</th><th>Formals &amp; Converged Types</th><th>Returns</th><th>Raises</th><th>Lineage / Diff</th></tr></thead>
          <tbody></tbody>
        </table>
      </div>
      <h4 style="margin: 0.75rem 0 0.5rem;">Data Contours (CreationSets) in this Pass <span id="pass-css-diff-summary" style="font-size:0.78rem; font-weight:400; color:var(--text-muted); margin-left:0.5rem;"></span></h4>
      <div class="table-container" style="max-height: 280px; margin-bottom: 1rem;">
        <table class="data-table" id="pass-css-table">
          <thead><tr><th>ID</th><th>Class / Sym</th><th>Static Arity</th><th>Fields &amp; Types</th><th>Split Origin / Diff</th></tr></thead>
          <tbody></tbody>
        </table>
      </div>
      <div id="pass-avar-diff-section" style="display:none; margin-bottom: 1rem;">
        <h4 style="margin: 0.75rem 0 0.5rem;">AVars with Converged Type Changes in this Pass <span id="pass-avar-diff-summary" style="font-size:0.78rem; font-weight:400; color:var(--text-muted); margin-left:0.5rem;"></span></h4>
        <div class="table-container" style="max-height: 280px;">
          <table class="data-table" id="pass-avar-diff-table">
            <thead><tr><th>AVar ID</th><th>Variable</th><th>Function</th><th>Contour</th><th>Change</th><th>Type Transition (Base &rarr; Current)</th></tr></thead>
            <tbody></tbody>
          </table>
        </div>
      </div>
    </div>
  </div>

  <!-- View: Dataflow Tracer -->
  <div class="view-panel" id="view-dataflow">
    <div class="controls-bar">
      <div style="display:flex; align-items:center; gap:0.75rem;">
        <span style="font-weight:600;">Bidirectional Dataflow Tracer</span>
        <select id="df-pass-select" class="dropdown-control"></select>
        <span id="df-count" style="font-size:0.8rem; color:var(--text-muted);"></span>
      </div>
      <input type="text" class="search-input" id="df-search" placeholder="Search Variable, Function, or AVar ID...">
    </div>
    <div class="table-container">
      <table class="data-table" id="df-table">
        <thead><tr><th>AVar ID</th><th>Variable</th><th>Function</th><th>Contour</th><th>Converged Type</th><th>Upstream (Backward Defs)</th><th>Downstream (Forward Uses)</th></tr></thead>
        <tbody></tbody>
      </table>
    </div>
  </div>

  <!-- View: Transfer Functions -->
  <div class="view-panel" id="view-transfer">
    <div class="controls-bar">
      <div style="display:flex; align-items:center; gap:0.75rem;">
        <span style="font-weight:600;">Primitive Operations &amp; Transfer Functions</span>
        <span id="tf-count" style="font-size:0.8rem; color:var(--text-muted);"></span>
      </div>
      <input type="text" class="search-input" id="tf-search" placeholder="Filter primitives, functions, or types...">
    </div>
    <div class="table-container">
      <table class="data-table" id="tf-table">
        <thead><tr><th>PNode</th><th>Function</th><th>Primitive</th><th>Input Types (Rvals)</th><th>Output Types (Lvals)</th><th>Status</th></tr></thead>
        <tbody></tbody>
      </table>
    </div>
  </div>

  <!-- View: Clones & Memory -->
  <div class="view-panel" id="view-memory">
    <div class="controls-bar">
      <div><span style="font-weight:600;">Function Clones &amp; Concrete Struct Layouts</span></div>
      <input type="text" class="search-input" id="memory-search" placeholder="Filter functions or types...">
    </div>
    <div style="margin-bottom:1.5rem;">
      <h3 style="margin-bottom:0.75rem; color:var(--text-main);">Function Clones &amp; Specializations</h3>
      <div class="table-container" style="max-height:360px;">
        <table class="data-table" id="clones-table">
          <thead>
            <tr>
              <th>ID</th>
              <th>Function</th>
              <th>Status</th>
              <th>Location</th>
              <th>EntrySet Contours</th>
              <th>Specialized Formals</th>
              <th>Return Type</th>
              <th>Actions</th>
            </tr>
          </thead>
          <tbody></tbody>
        </table>
      </div>
    </div>
    <div>
      <h3 style="margin-bottom:0.75rem; color:var(--text-main);">Concrete Struct Memory Layouts</h3>
      <div id="memory-layouts-container"></div>
    </div>
  </div>

  <!-- View: Source & Codegen -->
  <div class="view-panel" id="view-source">
    <div class="controls-bar">
      <div style="display:flex; align-items:center; gap:0.65rem; flex-wrap:wrap;">
        <span style="font-weight:600;">Source &amp; Target Code</span>
        <div class="btn-group">
          <button id="btn-src-mode-py" class="btn-sm" onclick="setSourceTabMode('py')">📜 Python Source</button>
          <button id="btn-src-mode-cg" class="btn-sm" onclick="setSourceTabMode('cg')">⚙️ Generated C / LLVM</button>
          <button id="btn-src-mode-both" class="btn-sm active" onclick="setSourceTabMode('both')">⚡ Side-by-Side Split</button>
        </div>
        <label style="font-size:0.8rem; color:var(--text-muted); margin-left:0.4rem;">Jump to Function:</label>
        <select id="src-fun-jump-select" class="dropdown-control">
          <option value="">-- Select Function --</option>
        </select>
      </div>
      <div id="src-codegen-meta" style="font-size:0.8rem; color:var(--text-muted);"></div>
    </div>
    <div class="source-split-grid" id="source-split-grid">
      <div class="source-split-col" id="source-py-col">
        <div class="source-split-header">
          <span id="source-py-header-title">📜 Python Source</span>
          <span style="font-size:0.75rem; color:var(--text-muted);">Click a line to inspect its CFG PNode</span>
        </div>
        <div class="code-container" id="source-code-viewer" style="border:none; border-radius:0; flex:1;"></div>
      </div>
      <div class="source-split-col" id="source-cg-col">
        <div class="source-split-header">
          <span id="source-cg-header-title">⚙️ Generated Target Code</span>
          <span id="source-cg-header-sub" style="font-size:0.75rem; color:var(--text-muted);"></span>
        </div>
        <div class="code-container" id="codegen-code-viewer" style="border:none; border-radius:0; flex:1;"></div>
      </div>
    </div>
  </div>
</main>
)HTML", fp);

  // Embed Compilation JSON Dataset
  fprintf(fp, "<script id=\"pyc-compiler-data\" type=\"application/json\">\n");
  fprintf(fp, "%s\n", full_program_json.c_str());
  fprintf(fp, "</script>\n");

  // Embedded Client-side Explorer UI Logic (Vanilla ES6)
  fprintf(fp, "<script>\n");
  fputs(R"JS(
window.__PYC_PASS_DATA__ = window.__PYC_PASS_DATA__ || {};

const DATA = JSON.parse(document.getElementById('pyc-compiler-data').textContent);

const totalPasses = (DATA.phase3_ifa && DATA.phase3_ifa.passes) ? DATA.phase3_ifa.passes.length : 0;
let currentPassIndex = 0;

function escapeHtml(str) {
  if (str === null || str === undefined) return '';
  return String(str).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
}

function getPassMeta(passNum) {
  const passes = (DATA.phase3_ifa && DATA.phase3_ifa.passes) || [];
  let pMeta = passes.find(p => p.pass === passNum);
  if (!pMeta) pMeta = passes.find(p => p.pass === (passNum - 1));
  if (!pMeta && passNum >= 1 && passNum <= passes.length) pMeta = passes[passNum - 1];
  if (!pMeta && passNum === 0 && passes.length > 0) pMeta = passes[0];
  return pMeta || null;
}

function ensurePassIndexed(passData) {
  if (!passData || passData._indexed) return passData;
  passData._indexed = true;
  passData._avarById = new Map();
  passData._avarsByVarId = new Map();
  passData._esById = new Map();
  (passData.avars || []).forEach(av => {
    passData._avarById.set(av.id, av);
    const addKey = (k) => {
      if (k === undefined || k === null || k < 0) return;
      let arr = passData._avarsByVarId.get(k);
      if (!arr) { arr = []; passData._avarsByVarId.set(k, arr); }
      if (!arr.includes(av)) arr.push(av);
    };
    addKey(av.var_id);
    addKey(av.orig_var_id);
  });
  (passData.entry_sets || []).forEach(es => {
    passData._esById.set(es.id, es);
  });
  return passData;
}

function ensureFunIndexed(f) {
  if (!f || f._indexed) return f;
  f._indexed = true;
  f._varById = new Map();
  (f.vars || []).forEach(v => f._varById.set(v.id, v));
  return f;
}

function getPassData(passNum) {
  const pMeta = getPassMeta(passNum);
  if (!pMeta) return null;
  const raw = (window.__PYC_PASS_DATA__ && (window.__PYC_PASS_DATA__[passNum] || window.__PYC_PASS_DATA__[pMeta.pass])) || (!pMeta.external ? pMeta.data : null);
  return ensurePassIndexed(raw);
}

// Init Header
document.getElementById('hdr-program').textContent = DATA.meta.program;
document.getElementById('pill-backend').textContent = 'Backend: ' + DATA.meta.backend;
document.getElementById('pill-passes').textContent = DATA.meta.total_passes + ' Passes';
const lastPass = DATA.phase3_ifa.passes.length > 0 ? DATA.phase3_ifa.passes[DATA.phase3_ifa.passes.length - 1] : null;
const lastPassSummary = lastPass ? (lastPass.external ? lastPass.summary : (lastPass.data ? lastPass.data.summary : lastPass.summary)) : null;

if (lastPassSummary) {
  document.getElementById('pill-contours').textContent = lastPassSummary.ess_count + ' Contours';
  const vPill = document.getElementById('pill-viol');
  vPill.textContent = lastPassSummary.violations_count + ' Violations';
  vPill.className = 'stat-pill ' + (lastPassSummary.violations_count === 0 ? 'success' : 'warning');
}

// Init Dashboard
document.getElementById('card-lines').textContent = DATA.phase1_source.lines.length;
document.getElementById('card-funs').textContent = DATA.phase2_cfg.functions.length;
document.getElementById('card-passes').textContent = DATA.meta.total_passes;
if (lastPassSummary) {
  document.getElementById('card-ess').textContent = lastPassSummary.ess_count;
  document.getElementById('card-css').textContent = lastPassSummary.css_count;
}
document.getElementById('card-inlines').textContent = DATA.phase5_optimizations.inlining_events.length;

function initDashboardInlining() {
  const events = (DATA.phase5_optimizations && DATA.phase5_optimizations.inlining_events) || [];
  const card = document.getElementById('dashboard-inlining-card');
  const tbody = document.querySelector('#dashboard-inlines-table tbody');
  if (!card || !tbody || events.length === 0) return;
  card.style.display = 'block';
  tbody.innerHTML = '';
  events.forEach(ev => {
    const tr = document.createElement('tr');
    const callerLink = ev.caller_id >= 0
      ? `<a href="#" onclick="jumpToCFGAndNode(${ev.caller_id}, ${ev.pnode_id}); return false;" style="color:var(--primary); font-weight:600; text-decoration:underline;">${escapeHtml(ev.caller_name)} (#${ev.caller_id})</a>`
      : escapeHtml(ev.caller_name);
    const calleeExists = (DATA.phase2_cfg.functions || []).some(f => f.id === ev.callee_id);
    const calleeLink = calleeExists
      ? `<a href="#" onclick="jumpToCFG(${ev.callee_id}); return false;" style="color:var(--text-main); text-decoration:underline;">${escapeHtml(ev.callee_name)} (#${ev.callee_id})</a>`
      : `${escapeHtml(ev.callee_name)} <span style="color:var(--text-dim); font-size:0.75rem;">(#${ev.callee_id} elided)</span>`;
    tr.innerHTML = `
      <td>${callerLink}</td>
      <td>${calleeLink}</td>
      <td><code>#${ev.pnode_id}</code></td>
      <td><span class="badge badge-prim">${escapeHtml(ev.reason)}</span></td>
    `;
    tbody.appendChild(tr);
  });
}
initDashboardInlining();

// Render Trajectory SVG
function renderTrajectoryChart() {
  const container = document.getElementById('chart-trajectory');
  const passes = DATA.phase3_ifa.passes;
  if (!container || !passes || passes.length === 0) return;
  const w = Math.max(360, container.clientWidth || 800);
  const h = 240;
  const padL = 48, padR = 32, padT = 36, padB = 34;

  let maxVal = 4;
  passes.forEach(p => {
    const s = p.external ? p.summary : (p.data ? p.data.summary : p.summary);
    if (s) {
      maxVal = Math.max(maxVal, s.ess_count || 0, s.css_count || 0, s.violations_count || 0);
    }
  });

  const getX = (idx) => passes.length === 1
    ? (padL + (w - padL - padR) / 2)
    : (padL + (idx / (passes.length - 1)) * (w - padL - padR));
  const getY = (val) => h - padB - (val / maxVal) * (h - padT - padB);

  let essPath = '', cssPath = '', violPath = '';
  let dotsAndLabels = '';
  const labelStride = Math.max(1, Math.ceil(passes.length / 16));

  passes.forEach((p, idx) => {
    const s = p.external ? p.summary : (p.data ? p.data.summary : p.summary);
    if (!s) return;
    const passNum = idx + 1;
    const x = getX(idx);
    const yEss = getY(s.ess_count || 0);
    const yCss = getY(s.css_count || 0);
    const yViol = getY(s.violations_count || 0);

    essPath += (idx === 0 ? 'M' : 'L') + `${x},${yEss}`;
    cssPath += (idx === 0 ? 'M' : 'L') + `${x},${yCss}`;
    violPath += (idx === 0 ? 'M' : 'L') + `${x},${yViol}`;

    const tip = `Pass ${passNum}: ${s.ess_count || 0} EntrySets, ${s.css_count || 0} CreationSets, ${s.violations_count || 0} Violations`;
    dotsAndLabels += `
      <circle cx="${x}" cy="${yEss}" r="4.5" fill="#38bdf8" style="cursor:pointer;" onclick="jumpToPassContour(${passNum})"><title>${tip}</title></circle>
      <circle cx="${x}" cy="${yCss}" r="4.5" fill="#a855f7" style="cursor:pointer;" onclick="jumpToPassContour(${passNum})"><title>${tip}</title></circle>
      <circle cx="${x}" cy="${yViol}" r="4" fill="#ef4444" style="cursor:pointer;" onclick="jumpToPassContour(${passNum})"><title>${tip}</title></circle>
    `;
    if (idx % labelStride === 0 || idx === passes.length - 1) {
      dotsAndLabels += `<text x="${x}" y="${h - 10}" fill="#64748b" font-size="11" text-anchor="middle">P${passNum}</text>`;
    }
  });

  container.innerHTML = `
    <svg width="${w}" height="${h}" style="background:var(--code-bg); border-radius:6px; display:block;">
      <!-- Grid lines & Y-axis labels -->
      <line x1="${padL}" y1="${getY(0)}" x2="${w-padR}" y2="${getY(0)}" stroke="#334155" stroke-width="1"/>
      <text x="${padL - 8}" y="${getY(0) + 4}" fill="#64748b" font-size="11" text-anchor="end">0</text>
      <line x1="${padL}" y1="${getY(Math.round(maxVal/2))}" x2="${w-padR}" y2="${getY(Math.round(maxVal/2))}" stroke="#1e293b" stroke-dasharray="4"/>
      <text x="${padL - 8}" y="${getY(Math.round(maxVal/2)) + 4}" fill="#64748b" font-size="11" text-anchor="end">${Math.round(maxVal/2)}</text>
      <line x1="${padL}" y1="${getY(maxVal)}" x2="${w-padR}" y2="${getY(maxVal)}" stroke="#1e293b" stroke-dasharray="4"/>
      <text x="${padL - 8}" y="${getY(maxVal) + 4}" fill="#64748b" font-size="11" text-anchor="end">${maxVal}</text>

      <!-- Paths -->
      <path d="${essPath}" fill="none" stroke="#38bdf8" stroke-width="2"/>
      <path d="${cssPath}" fill="none" stroke="#a855f7" stroke-width="2"/>
      <path d="${violPath}" fill="none" stroke="#ef4444" stroke-width="2"/>

      <!-- Interactive Data Points & X-axis labels -->
      ${dotsAndLabels}

      <!-- Legend -->
      <circle cx="${padL + 10}" cy="18" r="4" fill="#38bdf8"/><text x="${padL + 20}" y="22" fill="#94a3b8" font-size="12">EntrySets (ESS)</text>
      <circle cx="${padL + 150}" cy="18" r="4" fill="#a855f7"/><text x="${padL + 160}" y="22" fill="#94a3b8" font-size="12">CreationSets (CSS)</text>
      <circle cx="${padL + 300}" cy="18" r="4" fill="#ef4444"/><text x="${padL + 310}" y="22" fill="#94a3b8" font-size="12">Violations</text>
    </svg>
  `;
}
renderTrajectoryChart();
window.addEventListener('resize', () => {
  const dash = document.getElementById('view-dashboard');
  if (dash && dash.classList.contains('active')) renderTrajectoryChart();
});

function switchTab(tabName) {
  document.querySelectorAll('.tab-btn').forEach(b => {
    b.classList.toggle('active', b.dataset.tab === tabName);
  });
  document.querySelectorAll('.view-panel').forEach(p => {
    p.classList.toggle('active', p.id === 'view-' + tabName);
  });
}

// Tabs Navigation
document.querySelectorAll('.tab-btn').forEach(btn => {
  btn.addEventListener('click', () => {
    const tab = btn.dataset.tab;
    switchTab(tab);
    if (tab === 'dashboard') renderTrajectoryChart();
    if (tab === 'callgraph') renderCallGraph();
    if (tab === 'cfg') renderCFG();
    if (tab === 'dataflow') {
      if (!currentDfData) {
        loadPassData(currentDfPass, (d) => {
          currentDfData = d;
          renderDataflowTable();
        });
      } else {
        renderDataflowTable();
      }
    }
  });
});

// Tab: Source & Codegen Split Viewer
const cgData = DATA.phase6_codegen || { language: DATA.meta.backend || 'C', filename: '', lines: [] };
let currentSourceTabMode = (cgData.lines && cgData.lines.length > 0) ? 'both' : 'py';

const srcPyHeaderTitle = document.getElementById('source-py-header-title');
if (srcPyHeaderTitle) srcPyHeaderTitle.textContent = `📜 Python Source (${DATA.meta.program || 'program'})`;
const srcCgHeaderTitle = document.getElementById('source-cg-header-title');
if (srcCgHeaderTitle) srcCgHeaderTitle.textContent = `⚙️ Generated ${cgData.language || 'Target'} (${cgData.filename || 'not emitted'})`;
const srcCgHeaderSub = document.getElementById('source-cg-header-sub');
if (srcCgHeaderSub) srcCgHeaderSub.textContent = `${(cgData.lines || []).length} lines`;
const srcCodegenMeta = document.getElementById('src-codegen-meta');
if (srcCodegenMeta) {
  srcCodegenMeta.textContent = cgData.filename
    ? `Target: ${cgData.filename} (${(cgData.lines || []).length} lines of ${cgData.language})`
    : `Python Source: ${(DATA.phase1_source.lines || []).length} lines`;
}

// Build line -> {fun, pnode} lookup for Python source lines
const pyLineToFunPNode = new Map();
(DATA.phase2_cfg.functions || []).forEach(f => {
  (f.pnodes || []).forEach(pn => {
    if (pn.line > 0 && !pyLineToFunPNode.has(pn.line)) {
      pyLineToFunPNode.set(pn.line, { fun: f, pn: pn });
    }
  });
});

function getFunctionPythonSpan(f) {
  if (!f) return { start: 0, end: 0 };
  let minL = f.line > 0 ? f.line : 0;
  let maxL = minL;
  (f.pnodes || []).forEach(pn => {
    if (pn.line > 0) {
      if (minL === 0 || pn.line < minL) minL = pn.line;
      if (pn.line > maxL) maxL = pn.line;
    }
  });
  return { start: minL, end: maxL };
}

const srcViewer = document.getElementById('source-code-viewer');
(DATA.phase1_source.lines || []).forEach((line, idx) => {
  const lineNum = idx + 1;
  const d = document.createElement('div');
  const hit = pyLineToFunPNode.get(lineNum);
  d.className = 'code-line' + (hit ? ' clickable-line' : '');
  d.id = 'src-line-' + lineNum;
  if (hit) {
    d.title = `Function ${hit.fun.name} — PNode #${hit.pn.id} (${hit.pn.kind}). Click to sync target code; double-click to inspect in CFG.`;
    d.addEventListener('click', () => {
      highlightSourceAndCodegenForFunction(hit.fun, lineNum);
    });
    d.addEventListener('dblclick', () => {
      jumpToCFGAndNode(hit.fun.id, hit.pn.id);
    });
  }
  d.innerHTML = `<span class="line-num">${lineNum}</span><span class="line-text">${escapeHtml(line)}</span>`;
  srcViewer.appendChild(d);
});

const cgViewer = document.getElementById('codegen-code-viewer');
if (cgViewer) {
  if (!cgData.lines || cgData.lines.length === 0) {
    cgViewer.innerHTML = `<div style="padding:1.5rem; color:var(--text-muted); text-align:center;">No generated ${escapeHtml(cgData.language || 'C/LLVM')} file was available when HTML was emitted.</div>`;
  } else {
    const cgFrag = document.createDocumentFragment();
    cgData.lines.forEach((line, idx) => {
      const lineNum = idx + 1;
      const d = document.createElement('div');
      d.className = 'code-line';
      d.id = 'cg-line-' + lineNum;
      d.innerHTML = `<span class="line-num">${lineNum}</span><span class="line-text">${escapeHtml(line)}</span>`;
      cgFrag.appendChild(d);
    });
    cgViewer.appendChild(cgFrag);
  }
}

const srcFunJumpSelect = document.getElementById('src-fun-jump-select');
if (srcFunJumpSelect) {
  (DATA.phase2_cfg.functions || []).forEach(f => {
    if (f.line > 0 || (f.cg_start_line && f.cg_start_line > 0)) {
      const opt = document.createElement('option');
      opt.value = f.id;
      const cgTag = (f.cg_start_line > 0) ? ` → ${f.cg_name} (L${f.cg_start_line})` : '';
      opt.textContent = `${f.name} (#${f.id})${cgTag}`;
      srcFunJumpSelect.appendChild(opt);
    }
  });
  srcFunJumpSelect.addEventListener('change', () => {
    const fId = parseInt(srcFunJumpSelect.value, 10);
    const f = (DATA.phase2_cfg.functions || []).find(fn => fn.id === fId);
    if (f) highlightSourceAndCodegenForFunction(f, f.line);
  });
}

function setSourceTabMode(mode) {
  currentSourceTabMode = mode;
  ['py', 'cg', 'both'].forEach(m => {
    const btn = document.getElementById('btn-src-mode-' + m);
    if (btn) btn.classList.toggle('active', m === mode);
  });
  const pyCol = document.getElementById('source-py-col');
  const cgCol = document.getElementById('source-cg-col');
  if (pyCol) pyCol.style.display = (mode === 'py' || mode === 'both') ? 'flex' : 'none';
  if (cgCol) cgCol.style.display = (mode === 'cg' || mode === 'both') ? 'flex' : 'none';
}
setSourceTabMode(currentSourceTabMode);

function highlightSourceAndCodegenForFunction(f, focusPyLine) {
  if (!f) return;
  document.querySelectorAll('#source-code-viewer .code-line.highlight-line, #source-code-viewer .code-line.fun-span-line, #source-code-viewer .code-line.pnode-match-line').forEach(el => {
    el.classList.remove('highlight-line', 'fun-span-line', 'pnode-match-line');
  });
  document.querySelectorAll('#codegen-code-viewer .code-line.highlight-line, #codegen-code-viewer .code-line.fun-span-line, #codegen-code-viewer .code-line.pnode-match-line').forEach(el => {
    el.classList.remove('highlight-line', 'fun-span-line', 'pnode-match-line');
  });

  const pySpan = getFunctionPythonSpan(f);
  if (pySpan.start > 0) {
    for (let l = pySpan.start; l <= pySpan.end; l++) {
      const el = document.getElementById('src-line-' + l);
      if (el) el.classList.add('fun-span-line');
    }
  }
  const targetPyLine = focusPyLine || pySpan.start;
  if (targetPyLine > 0) {
    const lineEl = document.getElementById('src-line-' + targetPyLine);
    if (lineEl) {
      lineEl.classList.add('highlight-line');
      lineEl.scrollIntoView({ behavior: 'smooth', block: 'center' });
    }
  }

  if (f.cg_start_line > 0 && f.cg_end_line >= f.cg_start_line) {
    for (let l = f.cg_start_line; l <= f.cg_end_line; l++) {
      const el = document.getElementById('cg-line-' + l);
      if (el) el.classList.add('fun-span-line');
    }
    const cgStartEl = document.getElementById('cg-line-' + f.cg_start_line);
    if (cgStartEl) {
      cgStartEl.classList.add('highlight-line');
      cgStartEl.scrollIntoView({ behavior: 'smooth', block: 'center' });
    }
  }
}

function jumpToSource(lineNum, funId) {
  if (!lineNum || lineNum <= 0) return;
  switchTab('source');
  if (currentSourceTabMode === 'cg') setSourceTabMode((cgData.lines && cgData.lines.length > 0) ? 'both' : 'py');

  let f = funId !== undefined ? (DATA.phase2_cfg.functions || []).find(fn => fn.id === funId) : null;
  if (!f) {
    const hit = pyLineToFunPNode.get(lineNum);
    if (hit) f = hit.fun;
  }
  if (f) {
    if (srcFunJumpSelect) srcFunJumpSelect.value = f.id;
    highlightSourceAndCodegenForFunction(f, lineNum);
  } else {
    document.querySelectorAll('#source-code-viewer .code-line.highlight-line').forEach(el => el.classList.remove('highlight-line'));
    const lineEl = document.getElementById('src-line-' + lineNum);
    if (lineEl) {
      lineEl.classList.add('highlight-line');
      lineEl.scrollIntoView({ behavior: 'smooth', block: 'center' });
    }
  }
}

function jumpToCodegen(startLine, endLine, funId) {
  if (!startLine || startLine <= 0) return;
  switchTab('source');
  if (currentSourceTabMode === 'py') setSourceTabMode('both');

  const f = funId !== undefined
    ? (DATA.phase2_cfg.functions || []).find(fn => fn.id === funId)
    : (DATA.phase2_cfg.functions || []).find(fn => fn.cg_start_line === startLine);

  if (f) {
    if (srcFunJumpSelect) srcFunJumpSelect.value = f.id;
    highlightSourceAndCodegenForFunction(f, f.line);
  } else {
    document.querySelectorAll('#codegen-code-viewer .code-line.highlight-line, #codegen-code-viewer .code-line.fun-span-line').forEach(el => {
      el.classList.remove('highlight-line', 'fun-span-line');
    });
    const lastL = (endLine && endLine >= startLine) ? endLine : startLine;
    for (let l = startLine; l <= lastL; l++) {
      const el = document.getElementById('cg-line-' + l);
      if (el) el.classList.add('fun-span-line');
    }
    const startEl = document.getElementById('cg-line-' + startLine);
    if (startEl) {
      startEl.classList.add('highlight-line');
      startEl.scrollIntoView({ behavior: 'smooth', block: 'center' });
    }
  }
}

// Tab: Call Graph (Incremental Cytoscape + ELK Hierarchical Layout)
let cyInstance = null;
let currentCgScope = 'all';
let currentCgDepth = 'all';
let currentCgDirection = 'DOWN';
let expandedNodeIds = new Set();
let collapsedNodeIds = new Set();
let globalFunMap = new Map();
let currentScopedIdSet = new Set();
let currentInspectedFunId = null;

function getSubtreeDescendants(rootId) {
  const descendants = new Set();
  const queue = [rootId];
  while (queue.length > 0) {
    const currId = queue.shift();
    const f = globalFunMap.get(currId);
    if (!f || !f.calls) continue;
    f.calls.forEach(c => {
      const calleeId = c.callee_id;
      if (calleeId !== rootId && !descendants.has(calleeId)) {
        if (!currentScopedIdSet || currentScopedIdSet.has(calleeId)) {
          descendants.add(calleeId);
          queue.push(calleeId);
        }
      }
    });
  }
  return descendants;
}

function showCgNodeInspector(fId) {
  const f = globalFunMap.get(fId);
  if (!f || !cyInstance) return;
  const node = cyInstance.getElementById('fn_' + fId);
  if (!node || node.length === 0) return;

  currentInspectedFunId = fId;

  cyInstance.elements().removeClass('selected-focus caller-focus callee-focus caller-edge callee-edge dimmed');
  cyInstance.elements().addClass('dimmed');

  node.removeClass('dimmed').addClass('selected-focus');

  const inEdges = node.incomers('edge');
  const inNodes = node.incomers('node');
  inEdges.removeClass('dimmed').addClass('caller-edge');
  inNodes.removeClass('dimmed').addClass('caller-focus');

  const outEdges = node.outgoers('edge');
  const outNodes = node.outgoers('node');
  outEdges.removeClass('dimmed').addClass('callee-edge');
  outNodes.removeClass('dimmed').addClass('callee-focus');

  const desc = getSubtreeDescendants(f.id);
  const hiddenDescCount = node.data('hiddenDescCount') || 0;
  const hiddenCount = node.data('hiddenCount') || 0;
  const isSubtreeCollapsed = collapsedNodeIds.has(f.id);
  const visibleDescCount = Math.max(0, desc.size - hiddenDescCount);

  const srcLocHtml = f.line > 0
    ? `<a href="#" onclick="jumpToSource(${f.line}); return false;">${escapeHtml(f.filename || 'unknown')}:${f.line}</a>`
    : `${escapeHtml(f.filename || 'unknown')}`;

  const inspector = document.getElementById('cg-node-inspector');
  inspector.style.display = 'block';
  inspector.innerHTML = `
    <h4>${escapeHtml(f.name)}</h4>
    <div class="prop-row"><span>ID:</span><span class="prop-val">${f.id}${f.is_clone ? ` (clone of #${f.wraps_id})` : ''}</span></div>
    <div class="prop-row"><span>Source:</span><span class="prop-val">${srcLocHtml}</span></div>
    <div class="prop-row"><span>Depth Level:</span><span class="prop-val">${node.data('depth')}</span></div>
    <div class="prop-row"><span>Callers:</span><span class="prop-val">${inNodes.length}</span></div>
    <div class="prop-row"><span>Direct Callees:</span><span class="prop-val">${outNodes.length} visible ${hiddenCount > 0 ? '(' + hiddenCount + ' hidden)' : ''}</span></div>
    ${desc.size > 0 ? `<div class="prop-row"><span>Subtree Total:</span><span class="prop-val">${visibleDescCount} visible, ${hiddenDescCount} hidden</span></div>` : ''}
    <div style="margin-top:0.75rem; display:flex; flex-direction:column; gap:0.4rem;">
      ${hiddenDescCount > 0 || isSubtreeCollapsed ? `<button class="btn-sm" style="background:#059669; color:#fff; font-weight:600;" id="btn-uncollapse-subtree">⊞⊞ Uncollapse Subtree (+${hiddenDescCount})</button>` : ''}
      ${hiddenCount > 0 ? `<button class="btn-sm" style="background:#d97706; color:#fff; font-weight:600;" id="btn-expand-node">⊞ Expand Direct Callees (+${hiddenCount})</button>` : ''}
      ${desc.size > 0 && (visibleDescCount > 0 || outNodes.length > 0) ? `<button class="btn-sm" style="background:var(--bg-tertiary); color:var(--text-main); border:1px solid var(--border);" id="btn-collapse-subtree">⊟ Collapse Subtree</button>` : ''}
      <div style="display:flex; gap:0.4rem;">
        <button class="btn-sm" style="flex:1; background:var(--primary-hover); color:#fff; font-weight:600;" id="btn-jump-cfg">Inspect CFG &rarr;</button>
        <button class="btn-sm" style="background:var(--bg-tertiary); color:var(--text-main);" id="btn-close-inspector">✕</button>
      </div>
    </div>
  `;

  const uncollapseBtn = document.getElementById('btn-uncollapse-subtree');
  if (uncollapseBtn) {
    uncollapseBtn.addEventListener('click', () => {
      uncollapseSubtree(f.id);
    });
  }

  const expandBtn = document.getElementById('btn-expand-node');
  if (expandBtn) {
    expandBtn.addEventListener('click', () => {
      expandNodeCallees(f.id);
    });
  }

  const collapseBtn = document.getElementById('btn-collapse-subtree');
  if (collapseBtn) {
    collapseBtn.addEventListener('click', () => {
      collapseSubtree(f.id);
    });
  }

  document.getElementById('btn-jump-cfg').addEventListener('click', () => {
    jumpToCFG(f.id);
  });
  document.getElementById('btn-close-inspector').addEventListener('click', () => {
    inspector.style.display = 'none';
    currentInspectedFunId = null;
    cyInstance.elements().removeClass('selected-focus caller-focus callee-focus caller-edge callee-edge dimmed');
  });
}

function collapseSubtree(fId) {
  collapsedNodeIds.add(fId);
  const desc = getSubtreeDescendants(fId);
  expandedNodeIds.delete(fId);
  desc.forEach(dId => {
    expandedNodeIds.delete(dId);
    collapsedNodeIds.delete(dId);
  });
  rebuildCallGraph();
  showCgNodeInspector(fId);
}

function uncollapseSubtree(fId) {
  collapsedNodeIds.delete(fId);
  const desc = getSubtreeDescendants(fId);
  expandedNodeIds.add(fId);
  desc.forEach(dId => {
    collapsedNodeIds.delete(dId);
    expandedNodeIds.add(dId);
  });
  rebuildCallGraph();
  showCgNodeInspector(fId);
}

function expandNodeCallees(fId) {
  collapsedNodeIds.delete(fId);
  expandedNodeIds.add(fId);
  rebuildCallGraph();
  showCgNodeInspector(fId);
}

let cgScriptsLoading = false;
let cgScriptsLoaded = false;
let cgScriptCallbacks = [];

function loadScript(src, onSuccess, onError) {
  const s = document.createElement('script');
  s.src = src;
  s.onload = onSuccess;
  s.onerror = onError;
  document.head.appendChild(s);
}

function loadCallGraphScripts(callback) {
  if (typeof cytoscape !== 'undefined') {
    cgScriptsLoaded = true;
    if (callback) callback(true);
    return;
  }
  if (callback) cgScriptCallbacks.push(callback);
  if (cgScriptsLoading) return;
  cgScriptsLoading = true;

  const flushCallbacks = (ok) => {
    cgScriptsLoading = false;
    cgScriptsLoaded = ok;
    const cbs = cgScriptCallbacks.slice();
    cgScriptCallbacks = [];
    cbs.forEach(cb => { try { cb(ok); } catch (e) { console.error(e); } });
  };

  const overlay = document.getElementById('cg-loading-overlay');
  const statusEl = document.getElementById('cg-loading-status');
  if (overlay) overlay.style.display = 'flex';
  if (statusEl) statusEl.textContent = 'Loading Cytoscape engine...';

  loadScript('https://cdn.jsdelivr.net/npm/cytoscape@3.30.2/dist/cytoscape.min.js', () => {
    if (statusEl) statusEl.textContent = 'Loading ELK layout engine...';
    loadScript('https://cdn.jsdelivr.net/npm/elkjs@0.9.3/lib/elk.bundled.js', () => {
      if (statusEl) statusEl.textContent = 'Loading Cytoscape-ELK adapter...';
      loadScript('https://cdn.jsdelivr.net/npm/cytoscape-elk@2.2.0/dist/cytoscape-elk.min.js', () => {
        if (overlay) overlay.style.display = 'none';
        flushCallbacks(true);
      }, () => {
        if (overlay) overlay.style.display = 'none';
        flushCallbacks(true);
      });
    }, () => {
      if (overlay) overlay.style.display = 'none';
      flushCallbacks(true);
    });
  }, () => {
    if (overlay) overlay.style.display = 'none';
    const fallback = document.getElementById('cg-offline-fallback');
    if (fallback) fallback.style.display = 'block';
    flushCallbacks(false);
  });
}

function getCallGraphData() {
  const allFuns = DATA.phase2_cfg.functions || [];
  globalFunMap.clear();
  allFuns.forEach(f => globalFunMap.set(f.id, f));

  // 1. Filter by Scope
  let scopedFuns = allFuns;
  if (currentCgScope === 'active') {
    scopedFuns = allFuns.filter(f => (f.calls && f.calls.length > 0) || (f.called_by && f.called_by.length > 0));
    if (scopedFuns.length === 0) scopedFuns = allFuns;
  } else if (currentCgScope === 'user') {
    scopedFuns = allFuns.filter(f => {
      const fn = f.filename || '';
      return fn !== '' && !fn.startsWith('__pyc__') && !fn.startsWith('<builtin');
    });
    if (scopedFuns.length === 0) scopedFuns = allFuns;
  }
  const scopedIdSet = new Set(scopedFuns.map(f => f.id));
  currentScopedIdSet = scopedIdSet;

  // 2. Discover Root Functions (Entry Points)
  let roots = [];
  scopedFuns.forEach(f => {
    const inScopedCallers = (f.called_by || []).filter(c => scopedIdSet.has(c.caller_id));
    if (inScopedCallers.length === 0) {
      roots.push(f.id);
    }
  });

  if (roots.length === 0 && scopedFuns.length > 0) {
    const mainFn = scopedFuns.find(f => f.name === '__main__' || f.name === 'main');
    if (mainFn) roots.push(mainFn.id);
    else roots.push(scopedFuns[0].id);
  }

  // 3. BFS Depth Calculation from Roots
  const depths = new Map();
  const queue = [];
  roots.forEach(rId => {
    depths.set(rId, 0);
    queue.push(rId);
  });

  while (queue.length > 0) {
    const currId = queue.shift();
    const currD = depths.get(currId);
    const f = globalFunMap.get(currId);
    if (!f || !f.calls) continue;
    f.calls.forEach(c => {
      if (scopedIdSet.has(c.callee_id) && !depths.has(c.callee_id)) {
        depths.set(c.callee_id, currD + 1);
        queue.push(c.callee_id);
      }
    });
  }

  scopedFuns.forEach(f => {
    if (!depths.has(f.id)) {
      depths.set(f.id, 0);
      roots.push(f.id);
    }
  });

  // 4. Determine Visible Functions by Graph Traversal from Roots
  const maxDepth = currentCgDepth === 'all' ? 9999 : parseInt(currentCgDepth, 10);
  const visibleIds = new Set();
  const visQueue = [];

  roots.forEach(rId => {
    if (scopedIdSet.has(rId)) {
      visibleIds.add(rId);
      visQueue.push(rId);
    }
  });

  if (visibleIds.size === 0 && scopedFuns.length > 0) {
    visibleIds.add(scopedFuns[0].id);
    visQueue.push(scopedFuns[0].id);
  }

  // Also include explicitly expanded nodes
  expandedNodeIds.forEach(id => {
    if (scopedIdSet.has(id) && !visibleIds.has(id)) {
      visibleIds.add(id);
      visQueue.push(id);
    }
  });

  while (visQueue.length > 0) {
    const currId = visQueue.shift();
    // Collapsed nodes stay visible themselves, but do NOT propagate visibility to callees
    if (collapsedNodeIds.has(currId)) {
      continue;
    }

    const f = globalFunMap.get(currId);
    if (!f || !f.calls) continue;

    const currDepth = depths.get(currId) ?? 0;
    const isExplicitlyExpanded = expandedNodeIds.has(currId);

    f.calls.forEach(c => {
      const calleeId = c.callee_id;
      if (!scopedIdSet.has(calleeId)) return;

      const calleeDepth = depths.get(calleeId) ?? (currDepth + 1);
      const shouldBeVisible = isExplicitlyExpanded || (calleeDepth <= maxDepth);

      if (shouldBeVisible) {
        if (!visibleIds.has(calleeId)) {
          visibleIds.add(calleeId);
          visQueue.push(calleeId);
        }
      }
    });
  }

  // 5. Build Cytoscape Nodes and Edges
  const elements = [];
  visibleIds.forEach(fId => {
    const f = globalFunMap.get(fId);
    if (!f) return;

    const isUser = f.filename && !f.filename.startsWith('__pyc__') && !f.filename.startsWith('<builtin');
    const isEntry = roots.includes(f.id);

    const totalCallees = (f.calls || []).filter(c => scopedIdSet.has(c.callee_id));
    const hiddenCallees = totalCallees.filter(c => !visibleIds.has(c.callee_id)).length;
    const isSubtreeCollapsed = collapsedNodeIds.has(f.id);
    const desc = getSubtreeDescendants(f.id);
    const hiddenDescCount = Array.from(desc).filter(id => !visibleIds.has(id)).length;

    const classes = [];
    if (isEntry) classes.push('entry-node');
    else if (isUser) classes.push('user-node');
    else classes.push('runtime-node');

    if (f.can_raise) classes.push('can-raise');
    if (!f.live) classes.push('dead-node');
    if (hiddenCallees > 0 || isSubtreeCollapsed) classes.push('has-hidden');
    if (isSubtreeCollapsed) classes.push('collapsed-subtree');

    let baseLabel = f.is_clone ? `${f.name || '*anon*'} #${f.id}` : (f.name || '*anon*');
    let displayName = baseLabel;
    if (isSubtreeCollapsed && hiddenDescCount > 0) {
      displayName = `${baseLabel} [⊟ ${hiddenDescCount}]`;
    } else if (hiddenDescCount > 0) {
      displayName = `${baseLabel} (+${hiddenDescCount})`;
    }

    elements.push({
      group: 'nodes',
      data: {
        id: 'fn_' + f.id,
        funId: f.id,
        name: f.name || '*anon*',
        displayName: displayName,
        filename: f.filename || 'unknown',
        line: f.line || 0,
        can_raise: f.can_raise,
        live: f.live,
        depth: depths.get(f.id) ?? 0,
        callCount: totalCallees.length,
        hiddenCount: hiddenCallees,
        hiddenDescCount: hiddenDescCount,
        isSubtreeCollapsed: isSubtreeCollapsed,
        calledByCount: f.called_by ? f.called_by.length : 0
      },
      classes: classes.join(' ')
    });
  });

  const edgeSet = new Set();
  visibleIds.forEach(fId => {
    const f = globalFunMap.get(fId);
    if (!f || !f.calls) return;
    f.calls.forEach(c => {
      if (visibleIds.has(c.callee_id)) {
        const edgeId = `e_${f.id}_${c.callee_id}`;
        if (!edgeSet.has(edgeId)) {
          edgeSet.add(edgeId);
          elements.push({
            group: 'edges',
            data: {
              id: edgeId,
              source: 'fn_' + f.id,
              target: 'fn_' + c.callee_id,
              pnode_id: c.pnode_id
            }
          });
        }
      }
    });
  });

  return { elements, count: visibleIds.size, total: scopedFuns.length };
}

function initCallGraph() {
  const container = document.getElementById('cy-callgraph');
  if (!container) return;

  if (typeof cytoscape === 'undefined') {
    loadCallGraphScripts(ok => { if (ok) initCallGraph(); });
    return;
  }

  if (typeof cytoscapeElk !== 'undefined') {
    try { cytoscape.use(cytoscapeElk); } catch (e) {}
  }

  const { elements, count, total } = getCallGraphData();
  document.getElementById('cg-node-count').textContent = `${count} of ${total} functions`;

  const cyStyles = [
    {
      selector: 'node',
      style: {
        'label': 'data(displayName)',
        'font-family': 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace',
        'font-size': '11px',
        'color': '#f8fafc',
        'text-valign': 'center',
        'text-halign': 'center',
        'background-color': '#1e293b',
        'border-width': 2,
        'border-color': '#475569',
        'shape': 'round-rectangle',
        'padding': '8px',
        'width': 'label',
        'height': 'label',
        'text-max-width': '220px',
        'text-wrap': 'ellipsis',
        'transition-property': 'background-color, border-color, opacity, border-width',
        'transition-duration': '0.15s'
      }
    },
    {
      selector: 'node.user-node',
      style: {
        'background-color': '#0369a1',
        'border-color': '#38bdf8',
        'color': '#ffffff'
      }
    },
    {
      selector: 'node.entry-node',
      style: {
        'background-color': '#065f46',
        'border-color': '#34d399',
        'border-width': 3,
        'color': '#ecfdf5'
      }
    },
    {
      selector: 'node.runtime-node',
      style: {
        'background-color': '#1e293b',
        'border-color': '#64748b',
        'color': '#cbd5e1'
      }
    },
    {
      selector: 'node.can-raise',
      style: {
        'border-color': '#f97316'
      }
    },
    {
      selector: 'node.dead-node',
      style: {
        'opacity': 0.4,
        'border-style': 'dashed'
      }
    },
    {
      selector: 'node.has-hidden',
      style: {
        'border-color': '#f59e0b',
        'border-width': 2.5
      }
    },
    {
      selector: 'node.collapsed-subtree',
      style: {
        'border-color': '#f59e0b',
        'border-width': 3,
        'border-style': 'dashed'
      }
    },
    {
      selector: 'edge',
      style: {
        'width': 1.5,
        'line-color': '#475569',
        'target-arrow-color': '#475569',
        'target-arrow-shape': 'triangle',
        'arrow-scale': 1.0,
        'curve-style': 'bezier',
        'opacity': 0.7,
        'transition-property': 'line-color, target-arrow-color, width, opacity',
        'transition-duration': '0.15s'
      }
    },
    {
      selector: 'node.selected-focus',
      style: {
        'border-color': '#facc15',
        'border-width': 4,
        'background-color': '#ca8a04',
        'color': '#ffffff',
        'opacity': 1.0,
        'z-index': 999
      }
    },
    {
      selector: 'node.caller-focus',
      style: {
        'border-color': '#4ade80',
        'border-width': 3,
        'color': '#ffffff',
        'opacity': 1.0,
        'z-index': 990
      }
    },
    {
      selector: 'node.callee-focus',
      style: {
        'border-color': '#38bdf8',
        'border-width': 3,
        'color': '#ffffff',
        'opacity': 1.0,
        'z-index': 990
      }
    },
    {
      selector: 'edge.caller-edge',
      style: {
        'line-color': '#4ade80',
        'target-arrow-color': '#4ade80',
        'width': 3,
        'opacity': 1.0,
        'z-index': 995
      }
    },
    {
      selector: 'edge.callee-edge',
      style: {
        'line-color': '#38bdf8',
        'target-arrow-color': '#38bdf8',
        'width': 3,
        'opacity': 1.0,
        'z-index': 995
      }
    },
    {
      selector: '.dimmed',
      style: {
        'opacity': 0.12
      }
    }
  ];

  if (cyInstance) {
    cyInstance.destroy();
  }

  cyInstance = cytoscape({
    container: container,
    elements: elements,
    style: cyStyles,
    boxSelectionEnabled: false,
    wheelSensitivity: 0.25,
    minZoom: 0.1,
    maxZoom: 4.0
  });

  applyCallGraphLayout(currentCgDirection);

  // Interaction handlers
  cyInstance.on('tap', 'node', function(evt) {
    const fId = evt.target.data('funId');
    showCgNodeInspector(fId);
  });

  cyInstance.on('dbltap', 'node', function(evt) {
    const fId = evt.target.data('funId');
    const desc = getSubtreeDescendants(fId);
    const node = evt.target;
    const hiddenDescCount = node.data('hiddenDescCount') || 0;
    if (hiddenDescCount > 0 || collapsedNodeIds.has(fId)) {
      uncollapseSubtree(fId);
    } else if (desc.size > 0) {
      collapseSubtree(fId);
    } else {
      jumpToCFG(fId);
    }
  });

  cyInstance.on('cxttap', 'node', function(evt) {
    evt.preventDefault();
    const fId = evt.target.data('funId');
    const desc = getSubtreeDescendants(fId);
    const node = evt.target;
    const hiddenDescCount = node.data('hiddenDescCount') || 0;
    if (hiddenDescCount > 0 || collapsedNodeIds.has(fId)) {
      uncollapseSubtree(fId);
    } else if (desc.size > 0) {
      collapseSubtree(fId);
    }
  });

  cyInstance.on('tap', function(evt) {
    if (evt.target === cyInstance) {
      cyInstance.elements().removeClass('selected-focus caller-focus callee-focus caller-edge callee-edge dimmed');
      document.getElementById('cg-node-inspector').style.display = 'none';
      currentInspectedFunId = null;
    }
  });
}

function rebuildCallGraph() {
  if (!cyInstance) {
    initCallGraph();
    return;
  }
  const { elements, count, total } = getCallGraphData();
  document.getElementById('cg-node-count').textContent = `${count} of ${total} functions`;

  cyInstance.elements().remove();
  cyInstance.add(elements);
  applyCallGraphLayout(currentCgDirection);
}

function applyCallGraphLayout(direction) {
  if (!cyInstance) return;
  const dir = direction || 'DOWN';

  let layoutOptions = {
    name: 'elk',
    fit: true,
    padding: 35,
    animate: false,
    elk: {
      'algorithm': 'layered',
      'org.eclipse.elk.direction': dir,
      'org.eclipse.elk.layered.spacing.nodeNodeBetweenLayers': '50',
      'org.eclipse.elk.spacing.nodeNode': '30',
      'org.eclipse.elk.edgeRouting': 'POLYLINE'
    }
  };

  try {
    const l = cyInstance.layout(layoutOptions);
    l.run();
  } catch (err) {
    console.warn('ELK layout fallback to breadthfirst:', err);
    cyInstance.layout({
      name: 'breadthfirst',
      directed: true,
      spacingFactor: 1.5,
      padding: 35
    }).run();
  }

  if (currentInspectedFunId !== null) {
    setTimeout(() => {
      showCgNodeInspector(currentInspectedFunId);
    }, 50);
  }
}

function renderCallGraph() {
  if (typeof cytoscape === 'undefined') {
    loadCallGraphScripts((ok) => {
      if (ok) initCallGraph();
    });
  } else if (!cyInstance) {
    initCallGraph();
  } else {
    setTimeout(() => {
      cyInstance.resize();
      cyInstance.fit(undefined, 35);
    }, 20);
  }
}

// Scope dropdown handler
document.getElementById('cg-scope-select').addEventListener('change', (e) => {
  currentCgScope = e.target.value;
  expandedNodeIds.clear();
  collapsedNodeIds.clear();
  rebuildCallGraph();
});

// Depth dropdown handler
document.getElementById('cg-depth-select').addEventListener('change', (e) => {
  currentCgDepth = e.target.value;
  expandedNodeIds.clear();
  collapsedNodeIds.clear();
  rebuildCallGraph();
});

// Direction dropdown handler
document.getElementById('cg-direction-select').addEventListener('change', (e) => {
  currentCgDirection = e.target.value;
  applyCallGraphLayout(currentCgDirection);
});

// Expand All button
document.getElementById('cg-btn-expand-all').addEventListener('click', () => {
  currentCgDepth = 'all';
  document.getElementById('cg-depth-select').value = 'all';
  collapsedNodeIds.clear();
  rebuildCallGraph();
});

// Collapse to Root button
document.getElementById('cg-btn-collapse-root').addEventListener('click', () => {
  currentCgDepth = '1';
  document.getElementById('cg-depth-select').value = '1';
  expandedNodeIds.clear();
  collapsedNodeIds.clear();
  rebuildCallGraph();
});

// Search input handler
const searchInput = document.getElementById('cg-search');
searchInput.addEventListener('input', () => {
  if (!cyInstance) return;
  const q = searchInput.value.trim().toLowerCase();
  if (!q) {
    cyInstance.elements().removeClass('selected-focus caller-focus callee-focus caller-edge callee-edge dimmed');
    return;
  }
  cyInstance.elements().addClass('dimmed');
  const matches = cyInstance.nodes().filter(n => n.data('name').toLowerCase().includes(q));
  matches.removeClass('dimmed').addClass('selected-focus');
  if (matches.length === 1) {
    cyInstance.animate({
      center: { eles: matches },
      zoom: Math.max(cyInstance.zoom(), 1.0),
      duration: 300
    });
  }
});

// Zoom and reset buttons
document.getElementById('cg-btn-zoom-in').addEventListener('click', () => {
  if (cyInstance) cyInstance.zoom({ level: cyInstance.zoom() * 1.3, renderedPosition: { x: cyInstance.width() / 2, y: cyInstance.height() / 2 } });
});
document.getElementById('cg-btn-zoom-out').addEventListener('click', () => {
  if (cyInstance) cyInstance.zoom({ level: cyInstance.zoom() * 0.75, renderedPosition: { x: cyInstance.width() / 2, y: cyInstance.height() / 2 } });
});
document.getElementById('cg-btn-fit').addEventListener('click', () => {
  if (cyInstance) cyInstance.fit(undefined, 35);
});
document.getElementById('cg-btn-reset').addEventListener('click', () => {
  if (cyInstance) {
    searchInput.value = '';
    cyInstance.elements().removeClass('selected-focus caller-focus callee-focus caller-edge callee-edge dimmed');
    document.getElementById('cg-node-inspector').style.display = 'none';
    cyInstance.fit(undefined, 35);
  }
});

// Tab: CFG & SSU (Hierarchical Cytoscape + ELK Visual Graph & Instruction Table)
let cyCfgInstance = null;
let currentCfgFunId = DATA.phase2_cfg.functions.length > 0 ? DATA.phase2_cfg.functions[0].id : null;
let currentCfgPass = 'final';
let currentCfgContour = 'all';
let currentCfgMode = 'graph';

const cfgSelect = document.getElementById('cfg-fun-select');
DATA.phase2_cfg.functions.forEach(f => {
  const opt = document.createElement('option');
  opt.value = f.id;
  opt.textContent = f.is_clone ? `${f.name} (id: ${f.id}, clone of #${f.wraps_id})` : `${f.name} (id: ${f.id})`;
  cfgSelect.appendChild(opt);
});

const cfgPassSelect = document.getElementById('cfg-pass-select');
if (cfgPassSelect) {
  const finalOpt = document.createElement('option');
  finalOpt.value = 'final';
  finalOpt.textContent = `Final (Pass ${DATA.meta.total_passes} Quiescence)`;
  cfgPassSelect.appendChild(finalOpt);
  for (let pi = 1; pi <= DATA.meta.total_passes; pi++) {
    const opt = document.createElement('option');
    opt.value = pi;
    opt.textContent = `Pass ${pi}`;
    cfgPassSelect.appendChild(opt);
  }
}

const cfgContourSelect = document.getElementById('cfg-contour-select');

cfgSelect.addEventListener('change', () => {
  currentCfgFunId = parseInt(cfgSelect.value);
  updateCfgContours();
  renderCFGCurrent();
});

if (cfgPassSelect) {
  cfgPassSelect.addEventListener('change', () => {
    currentCfgPass = cfgPassSelect.value;
    updateCfgContours();
    renderCFGCurrent();
  });
}

if (cfgContourSelect) {
  cfgContourSelect.addEventListener('change', () => {
    currentCfgContour = cfgContourSelect.value;
    renderCFGCurrent();
  });
}

document.getElementById('btn-cfg-mode-graph').addEventListener('click', () => {
  currentCfgMode = 'graph';
  document.getElementById('btn-cfg-mode-graph').classList.add('active');
  document.getElementById('btn-cfg-mode-table').classList.remove('active');
  document.getElementById('cfg-graph-view').style.display = 'block';
  document.getElementById('cfg-table-view').style.display = 'none';
  document.getElementById('cfg-graph-controls').style.display = 'flex';
  renderCFGCurrent();
});

document.getElementById('btn-cfg-mode-table').addEventListener('click', () => {
  currentCfgMode = 'table';
  document.getElementById('btn-cfg-mode-table').classList.add('active');
  document.getElementById('btn-cfg-mode-graph').classList.remove('active');
  document.getElementById('cfg-graph-view').style.display = 'none';
  document.getElementById('cfg-table-view').style.display = 'block';
  document.getElementById('cfg-graph-controls').style.display = 'none';
  renderCFGCurrent();
});

document.getElementById('cfg-btn-zoom-in').addEventListener('click', () => {
  if (cyCfgInstance) cyCfgInstance.zoom({ level: cyCfgInstance.zoom() * 1.3, renderedPosition: { x: cyCfgInstance.width() / 2, y: cyCfgInstance.height() / 2 } });
});
document.getElementById('cfg-btn-zoom-out').addEventListener('click', () => {
  if (cyCfgInstance) cyCfgInstance.zoom({ level: cyCfgInstance.zoom() * 0.75, renderedPosition: { x: cyCfgInstance.width() / 2, y: cyCfgInstance.height() / 2 } });
});
document.getElementById('cfg-btn-fit').addEventListener('click', () => {
  if (cyCfgInstance) cyCfgInstance.fit(undefined, 35);
});
document.getElementById('cfg-btn-reset').addEventListener('click', () => {
  if (cyCfgInstance) {
    cyCfgInstance.elements().removeClass('pnode-selected pnode-neighbor edge-highlight');
    document.getElementById('cfg-inspector').style.display = 'none';
    cyCfgInstance.fit(undefined, 35);
  }
});

function updateCfgContours() {
  const sel = document.getElementById('cfg-contour-select');
  if (!sel) return;
  const oldVal = sel.value;
  sel.innerHTML = '<option value="all">All Contours (Unified)</option>';

  const f = DATA.phase2_cfg.functions.find(fn => fn.id === currentCfgFunId);
  const pNum = currentCfgPass === 'final' ? totalPasses : parseInt(currentCfgPass);
  const passData = getPassData(pNum);
  const seenEs = new Set();
  if (passData && passData.entry_sets) {
    passData.entry_sets.filter(es =>
      es.fun_id === currentCfgFunId ||
      (f && f.is_clone && es.orig_fun_id === f.wraps_id && (!f.entry_sets || f.entry_sets.length === 0 || f.entry_sets.includes(es.id)))
    ).forEach(es => {
      seenEs.add(es.id);
      const opt = document.createElement('option');
      opt.value = es.id;
      opt.textContent = `EntrySet #${es.id}`;
      sel.appendChild(opt);
    });
  }
  if (seenEs.size === 0 && f && f.entry_sets) {
    f.entry_sets.forEach(esId => {
      const opt = document.createElement('option');
      opt.value = esId;
      opt.textContent = `EntrySet #${esId}`;
      sel.appendChild(opt);
    });
  }
  if (oldVal && sel.querySelector(`option[value="${oldVal}"]`)) {
    sel.value = oldVal;
  } else {
    sel.value = 'all';
  }
  currentCfgContour = sel.value;
}

function getTypeForVar(f, vId, passChoice, contourChoice) {
  if (!f) return 'void';
  ensureFunIndexed(f);
  const v = f._varById.get(vId);
  const pNum = passChoice === 'final' ? totalPasses : parseInt(passChoice);
  const passData = getPassData(pNum);

  if (passData && passData._avarById) {
    let matchingAvars = [];
    if (v && v.avars && v.avars.length > 0) {
      v.avars.forEach(a => {
        const av = passData._avarById.get(a.av_id);
        if (av) matchingAvars.push(av);
      });
    }
    if (matchingAvars.length === 0 && passData._avarsByVarId) {
      const byVar = passData._avarsByVarId.get(vId) || [];
      matchingAvars = byVar.filter(av =>
        av.fun_id === f.id || av.fun_id === -1 || (f.is_clone && av.orig_fun_id === f.wraps_id)
      );
    }
    if (matchingAvars.length > 0) {
      if (contourChoice !== 'all') {
        const cId = parseInt(contourChoice);
        const specificAv = matchingAvars.find(av => av.contour_id === cId);
        if (specificAv) {
          const typeNames = (specificAv.type || []).map(t => t.sym + (t.is_const ? `=${t.val}` : ''));
          return typeNames.length > 0 ? Array.from(new Set(typeNames)).join(' | ') : (specificAv.type_str || 'void');
        }
      } else {
        const allTypes = new Set();
        matchingAvars.forEach(av => {
          (av.type || []).forEach(t => allTypes.add(t.sym + (t.is_const ? `=${t.val}` : '')));
        });
        if (allTypes.size > 0) return Array.from(allTypes).join(' | ');
      }
    }
  }

  if (v) {
    if (contourChoice !== 'all') {
      const cId = parseInt(contourChoice);
      const av = (v.avars || []).find(a => a.contour_id === cId);
      if (av && av.type_str) return av.type_str;
    }
    return v.type_str || 'void';
  }
  return 'void';
}

let currentCfgSplitMode = 'off';
let currentSelectedPNodeId = null;

function setCfgSplitMode(mode) {
  currentCfgSplitMode = mode;
  ['off', 'py', 'cg', 'both'].forEach(m => {
    const btn = document.getElementById('btn-cfg-split-' + m);
    if (btn) btn.classList.toggle('active', m === mode);
  });
  const dock = document.getElementById('cfg-split-dock');
  const pyPane = document.getElementById('cfg-dock-py-pane');
  const cgPane = document.getElementById('cfg-dock-cg-pane');
  if (dock) dock.classList.toggle('active', mode !== 'off');
  if (pyPane) pyPane.style.display = (mode === 'py' || mode === 'both') ? 'flex' : 'none';
  if (cgPane) cgPane.style.display = (mode === 'cg' || mode === 'both') ? 'flex' : 'none';

  if (mode !== 'off' && currentCfgFunId !== null) {
    syncCfgSplitDock(currentCfgFunId, currentSelectedPNodeId);
  }
  if (cyCfgInstance) {
    setTimeout(() => {
      cyCfgInstance.resize();
      cyCfgInstance.fit(undefined, 35);
    }, 30);
  }
}

function openCurrentFunInSourceTab(which) {
  const f = (DATA.phase2_cfg.functions || []).find(fn => fn.id === currentCfgFunId);
  if (!f) {
    switchTab('source');
    return;
  }
  if (which === 'cg' && f.cg_start_line > 0) {
    jumpToCodegen(f.cg_start_line, f.cg_end_line, f.id);
  } else if (f.line > 0) {
    jumpToSource(f.line, f.id);
  } else {
    switchTab('source');
  }
}

function syncCfgSplitDock(funId, selectedPNodeId) {
  if (currentCfgSplitMode === 'off') return;
  const f = (DATA.phase2_cfg.functions || []).find(fn => fn.id === funId);
  if (!f) return;
  const selectedPn = selectedPNodeId !== null && selectedPNodeId !== undefined
    ? (f.pnodes || []).find(p => p.id === selectedPNodeId)
    : null;

  // 1. Sync Python Source Dock Pane
  const pyBody = document.getElementById('cfg-dock-py-body');
  const pyTitle = document.getElementById('cfg-dock-py-title');
  if (pyBody && (currentCfgSplitMode === 'py' || currentCfgSplitMode === 'both')) {
    const pyLines = (DATA.phase1_source && DATA.phase1_source.lines) || [];
    const span = getFunctionPythonSpan(f);
    if (pyTitle) {
      pyTitle.textContent = span.start > 0
        ? `📜 Python: ${f.name} (L${span.start}–${span.end})`
        : `📜 Python: ${f.name}`;
    }
    pyBody.innerHTML = '';
    if (pyLines.length === 0 || span.start <= 0) {
      pyBody.innerHTML = `<div style="padding:1rem; color:var(--text-muted);">No user source lines mapped to ${escapeHtml(f.name)} (builtin / runtime helper).</div>`;
    } else {
      const winStart = Math.max(1, span.start - 5);
      const winEnd = Math.min(pyLines.length, Math.max(span.end + 8, span.start + 15));
      const lineToPNode = new Map();
      (f.pnodes || []).forEach(pn => {
        if (pn.line > 0 && !lineToPNode.has(pn.line)) lineToPNode.set(pn.line, pn);
      });
      const frag = document.createDocumentFragment();
      let scrollTarget = null;
      for (let l = winStart; l <= winEnd; l++) {
        const row = document.createElement('div');
        const inSpan = l >= span.start && l <= span.end;
        const isSelectedLine = selectedPn && selectedPn.line === l;
        const pnAtLine = lineToPNode.get(l);
        let cls = 'code-line';
        if (inSpan) cls += ' fun-span-line';
        if (isSelectedLine) cls += ' pnode-match-line';
        if (pnAtLine) cls += ' clickable-line';
        row.className = cls;
        if (pnAtLine) {
          row.title = `Click to select PNode #${pnAtLine.id} (${pnAtLine.kind})`;
          row.addEventListener('click', () => {
            showPNodeInspector(f.id, pnAtLine.id);
          });
        }
        row.innerHTML = `<span class="line-num">${l}</span><span class="line-text">${escapeHtml(pyLines[l - 1] || '')}</span>`;
        frag.appendChild(row);
        if (isSelectedLine || (!scrollTarget && l === span.start)) scrollTarget = row;
      }
      pyBody.appendChild(frag);
      if (scrollTarget) {
        setTimeout(() => scrollTarget.scrollIntoView({ behavior: 'smooth', block: 'center' }), 10);
      }
    }
  }

  // 2. Sync Generated C / LLVM Dock Pane
  const cgBody = document.getElementById('cfg-dock-cg-body');
  const cgTitle = document.getElementById('cfg-dock-cg-title');
  if (cgBody && (currentCfgSplitMode === 'cg' || currentCfgSplitMode === 'both')) {
    const cgLines = (cgData && cgData.lines) || [];
    const lang = (cgData && cgData.language) || DATA.meta.backend || 'C';
    if (cgTitle) {
      cgTitle.textContent = (f.cg_start_line > 0)
        ? `⚙️ ${lang}: ${f.cg_name} (L${f.cg_start_line}–${f.cg_end_line})`
        : `⚙️ ${lang}: ${f.cg_name || f.name}`;
    }
    cgBody.innerHTML = '';
    if (cgLines.length === 0 || !f.cg_start_line || f.cg_start_line <= 0) {
      cgBody.innerHTML = `<div style="padding:1rem; color:var(--text-muted);">No standalone ${escapeHtml(lang)} function emitted for <code>${escapeHtml(f.cg_name || f.name)}</code> (inlined or elided).</div>`;
    } else {
      const winStart = Math.max(1, f.cg_start_line - 1);
      const winEnd = Math.min(cgLines.length, f.cg_end_line + 1);

      // Compute tokens to highlight for selectedPn inside the emitted C/LLVM body
      const matchPatterns = [];
      if (selectedPn) {
        if (selectedPn.label_id > 0) {
          matchPatterns.push(`L${selectedPn.label_id}:`);
          matchPatterns.push(`goto L${selectedPn.label_id};`);
          matchPatterns.push(`label %L${selectedPn.label_id}`);
        }
        if (selectedPn.label_else_id > 0) {
          matchPatterns.push(`L${selectedPn.label_else_id}:`);
          matchPatterns.push(`goto L${selectedPn.label_else_id};`);
          matchPatterns.push(`label %L${selectedPn.label_else_id}`);
        }
        (selectedPn.contour_calls || []).forEach(cc => {
          const calleeFun = (DATA.phase2_cfg.functions || []).find(fn => fn.id === cc.callee_id || fn.wraps_id === cc.callee_id);
          if (calleeFun && calleeFun.cg_name) {
            matchPatterns.push(calleeFun.cg_name + '(');
          }
        });
        if (selectedPn.prim === 'prim_reply') {
          matchPatterns.push('return ');
          matchPatterns.push('return;');
          matchPatterns.push('co_return ');
          matchPatterns.push('ret ');
        }
      }

      const frag = document.createDocumentFragment();
      let scrollTarget = null;
      for (let l = winStart; l <= winEnd; l++) {
        const lineStr = cgLines[l - 1] || '';
        const inSpan = l >= f.cg_start_line && l <= f.cg_end_line;
        const isMatch = inSpan && matchPatterns.length > 0 && matchPatterns.some(pat => lineStr.includes(pat));
        let cls = 'code-line';
        if (inSpan) cls += ' fun-span-line';
        if (isMatch) cls += ' pnode-match-line';
        const row = document.createElement('div');
        row.className = cls;
        row.innerHTML = `<span class="line-num">${l}</span><span class="line-text">${escapeHtml(lineStr)}</span>`;
        frag.appendChild(row);
        if (isMatch && !scrollTarget) scrollTarget = row;
      }
      cgBody.appendChild(frag);
      if (scrollTarget) {
        setTimeout(() => scrollTarget.scrollIntoView({ behavior: 'smooth', block: 'center' }), 10);
      }
    }
  }
}

function renderCFGCurrent() {
  const pNum = currentCfgPass === 'final' ? totalPasses : parseInt(currentCfgPass);
  loadPassData(pNum, () => {
    updateCfgContours();
    renderCFGForFunction(currentCfgFunId);
  });
}

function renderCFGForFunction(funId) {
  const f = DATA.phase2_cfg.functions.find(fn => fn.id === funId);
  if (!f) return;
  ensureFunIndexed(f);
  if (currentCfgFunId !== funId) currentSelectedPNodeId = null;
  currentCfgFunId = funId;
  const cfgSelect = document.getElementById('cfg-fun-select');
  if (cfgSelect && cfgSelect.value != funId) cfgSelect.value = funId;

  const locHtml = f.line > 0
    ? `<a href="#" onclick="jumpToSource(${f.line}, ${f.id}); return false;">${escapeHtml(f.filename || 'unknown')}:${f.line}</a>`
    : escapeHtml(f.filename || 'unknown');
  const cgHtml = (f.cg_start_line && f.cg_start_line > 0)
    ? ` | Emit: <a href="#" onclick="jumpToCodegen(${f.cg_start_line}, ${f.cg_end_line}, ${f.id}); return false;" style="color:var(--primary);">${escapeHtml(f.cg_name)} (L${f.cg_start_line}–${f.cg_end_line})</a>`
    : '';
  document.getElementById('cfg-fun-meta').innerHTML = `File: ${locHtml}${cgHtml} | Can Raise: ${f.can_raise} | Live: ${f.live}`;

  syncCfgSplitDock(f.id, currentSelectedPNodeId);

  // 1. Render Table View
  const tbody = document.querySelector('#cfg-pnodes-table tbody');
  if (tbody) {
    tbody.innerHTML = '';
    f.pnodes.forEach(pn => {
      const tr = document.createElement('tr');
      const lvalsStr = (pn.lvals || []).map(v => {
        const t = getTypeForVar(f, v.id, currentCfgPass, currentCfgContour);
        return `<span class="badge badge-var" onclick="showVarInspector(${f.id}, ${v.id})">${escapeHtml(v.name || 'v'+v.id)}</span> <span class="badge badge-type">${escapeHtml(t)}</span>`;
      }).join(' ');

      const rvalsStr = (pn.rvals || []).map(v => {
        const t = getTypeForVar(f, v.id, currentCfgPass, currentCfgContour);
        return `<span class="badge badge-var" onclick="showVarInspector(${f.id}, ${v.id})">${escapeHtml(v.name || 'v'+v.id)}</span> <span class="badge badge-type">${escapeHtml(t)}</span>`;
      }).join(' ');

      const callsStr = (pn.contour_calls || []).map(c => {
        return `<button class="btn-sm" style="font-size:0.75rem; padding:0.15rem 0.35rem; margin:0.1rem;" onclick="jumpToCFG(${c.callee_id}, ${c.to_es}, currentCfgPass === 'final' ? totalPasses : parseInt(currentCfgPass))">${escapeHtml(c.callee_name)} (ES#${c.to_es})</button>`;
      }).join(' ');

      let statusBadge = '';
      if (!pn.live && !pn.fa_live) statusBadge = ' <span class="badge" style="background:#475569; color:#cbd5e1; font-size:0.65rem;">DEAD</span>';
      else if (!pn.live && pn.fa_live) statusBadge = ' <span class="badge" style="background:#334155; color:#94a3b8; font-size:0.65rem;">FOLDED</span>';

      const lineCell = pn.line > 0 ? `<a href="#" onclick="jumpToSource(${pn.line}); return false;">${pn.line}</a>` : '-';

      tr.innerHTML = `
        <td><a href="#" onclick="showPNodeInspector(${f.id}, ${pn.id}); return false;" style="color:var(--primary); font-weight:700;">#${pn.id}</a></td>
        <td><span class="badge badge-kind">${pn.kind}</span>${statusBadge}</td>
        <td>${pn.prim ? `<span class="badge badge-prim">${escapeHtml(pn.prim)}</span>` : '-'}</td>
        <td>${lineCell}</td>
        <td>${lvalsStr || '-'}</td>
        <td>${rvalsStr || '-'}</td>
        <td>${callsStr || '-'}</td>
        <td>${pn.cfg_succ.map(id => `<a href="#" onclick="showPNodeInspector(${f.id}, ${id}); return false;">#${id}</a>`).join(', ') || '-'}</td>
        <td>${pn.cfg_pred.map(id => `<a href="#" onclick="showPNodeInspector(${f.id}, ${id}); return false;">#${id}</a>`).join(', ') || '-'}</td>
      `;
      tbody.appendChild(tr);
    });
  }

  // 2. Render Visual Graph View only when CFG view is visible
  const cfgPanel = document.getElementById('view-cfg');
  if (currentCfgMode === 'graph' && cfgPanel && cfgPanel.classList.contains('active')) {
    renderCFGGraph(f);
  }
}

function renderCFGGraph(f) {
  if (typeof cytoscape === 'undefined') {
    loadCallGraphScripts((ok) => {
      if (ok && typeof cytoscape !== 'undefined') {
        renderCFGGraph(f);
      } else {
        // Offline fallback: switch CFG view to Instruction Table
        currentCfgMode = 'table';
        document.getElementById('btn-cfg-mode-table').classList.add('active');
        document.getElementById('btn-cfg-mode-graph').classList.remove('active');
        document.getElementById('cfg-graph-view').style.display = 'none';
        document.getElementById('cfg-table-view').style.display = 'block';
        document.getElementById('cfg-graph-controls').style.display = 'none';
      }
    });
    return;
  }

  const container = document.getElementById('cy-cfg');
  if (!container) return;

  if (typeof cytoscapeElk !== 'undefined') {
    try { cytoscape.use(cytoscapeElk); } catch (e) {}
  }

  const elements = [];
  const pnodeIdSet = new Set(f.pnodes.map(p => p.id));

  f.pnodes.forEach((pn, idx) => {
    let lines = [];
    let header = `#${pn.id} ${pn.kind}`;
    if (pn.prim) header += ` [${pn.prim}]`;
    if (pn.line) header += ` (L${pn.line})`;
    if (!pn.live && !pn.fa_live) header += ` [DEAD]`;
    else if (!pn.live && pn.fa_live) header += ` [FOLDED]`;
    lines.push(header);

    if (pn.lvals && pn.lvals.length > 0) {
      const defs = pn.lvals.map(v => `${v.name || 'v'+v.id}: ${getTypeForVar(f, v.id, currentCfgPass, currentCfgContour)}`).join(', ');
      lines.push(`▶ def: ${defs}`);
    }
    if (pn.rvals && pn.rvals.length > 0) {
      const uses = pn.rvals.map(v => `${v.name || 'v'+v.id}: ${getTypeForVar(f, v.id, currentCfgPass, currentCfgContour)}`).join(', ');
      lines.push(`◀ use: ${uses}`);
    }
    if (pn.contour_calls && pn.contour_calls.length > 0) {
      const calls = pn.contour_calls.map(c => `${c.callee_name} (ES#${c.to_es})`).join(', ');
      lines.push(`☎ to: ${calls}`);
    }

    const classes = ['pnode-box'];
    if (pn.kind === 'SEND') classes.push('pnode-send');
    else if (pn.kind === 'IF') classes.push('pnode-if');
    else if (pn.kind === 'GOTO') classes.push('pnode-goto');
    else if (pn.kind === 'LABEL') classes.push('pnode-label');
    else classes.push('pnode-normal');

    if (pn.id === f.entry_id || (f.entry_id < 0 && idx === 0)) classes.push('pnode-entry');
    if (pn.id === f.exit_id || pn.fa_noreturn || pn.fa_noreturn_raises || pn.prim === 'prim_reply') classes.push('pnode-exit');
    if (!pn.live && !pn.fa_live) classes.push('pnode-dead');

    elements.push({
      group: 'nodes',
      data: {
        id: 'pn_' + pn.id,
        pnodeId: pn.id,
        kind: pn.kind,
        prim: pn.prim,
        line: pn.line,
        label: lines.join('\n')
      },
      classes: classes.join(' ')
    });
  });

  f.pnodes.forEach(pn => {
    (pn.cfg_succ || []).forEach(succ_id => {
      if (pnodeIdSet.has(succ_id)) {
        let edgeLabel = '';
        let edgeClass = '';
        if (pn.kind === 'IF' && pn.cfg_succ.length >= 2) {
          if (succ_id === pn.cfg_succ[0]) {
            edgeLabel = 'then';
            edgeClass = 'edge-then';
          } else {
            edgeLabel = 'else';
            edgeClass = 'edge-else';
          }
        }
        elements.push({
          group: 'edges',
          data: {
            id: `cfg_e_${pn.id}_${succ_id}`,
            source: 'pn_' + pn.id,
            target: 'pn_' + succ_id,
            label: edgeLabel
          },
          classes: edgeClass
        });
      }
    });
  });

  if (!cyCfgInstance) {
    cyCfgInstance = cytoscape({
      container: container,
      elements: elements,
      style: [
        {
          selector: 'node',
          style: {
            'label': 'data(label)',
            'font-family': 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace',
            'font-size': '11px',
            'color': '#f8fafc',
            'text-valign': 'center',
            'text-halign': 'center',
            'background-color': '#1e293b',
            'border-width': 2,
            'border-color': '#475569',
            'shape': 'round-rectangle',
            'padding': '12px',
            'width': 'label',
            'height': 'label',
            'text-max-width': '320px',
            'text-wrap': 'wrap',
            'text-justification': 'left',
            'line-height': 1.4,
            'transition-property': 'background-color, border-color, border-width',
            'transition-duration': '0.15s'
          }
        },
        {
          selector: 'node.pnode-send',
          style: { 'background-color': '#2e1065', 'border-color': '#a855f7', 'color': '#f3e8ff' }
        },
        {
          selector: 'node.pnode-if',
          style: { 'background-color': '#431407', 'border-color': '#f97316', 'color': '#ffedd5' }
        },
        {
          selector: 'node.pnode-goto',
          style: { 'background-color': '#1e1b4b', 'border-color': '#6366f1', 'color': '#e0e7ff' }
        },
        {
          selector: 'node.pnode-label',
          style: { 'background-color': '#0f172a', 'border-color': '#334155', 'color': '#94a3b8' }
        },
        {
          selector: 'node.pnode-entry',
          style: { 'background-color': '#064e3b', 'border-color': '#10b981', 'color': '#d1fae5' }
        },
        {
          selector: 'node.pnode-exit',
          style: { 'background-color': '#450a0a', 'border-color': '#ef4444', 'color': '#fee2e2' }
        },
        {
          selector: 'node.pnode-dead',
          style: { 'opacity': 0.45, 'border-style': 'dashed' }
        },
        {
          selector: 'node.pnode-selected',
          style: {
            'border-color': '#38bdf8',
            'border-width': 3,
            'color': '#ffffff',
            'shadow-blur': 12,
            'shadow-color': '#38bdf8',
            'shadow-opacity': 0.8
          }
        },
        {
          selector: 'node.pnode-neighbor',
          style: {
            'border-color': '#22c55e',
            'border-width': 2,
            'color': '#ffffff'
          }
        },
        {
          selector: 'edge',
          style: {
            'width': 2,
            'line-color': '#475569',
            'target-arrow-color': '#38bdf8',
            'target-arrow-shape': 'triangle',
            'curve-style': 'bezier',
            'arrow-scale': 1.1,
            'label': 'data(label)',
            'font-size': '10px',
            'color': '#cbd5e1',
            'font-family': 'ui-monospace, monospace',
            'text-background-opacity': 0.85,
            'text-background-color': '#0f172a',
            'text-background-padding': '2px',
            'text-background-shape': 'roundrectangle'
          }
        },
        {
          selector: 'edge.edge-then',
          style: { 'line-color': '#22c55e', 'target-arrow-color': '#22c55e', 'color': '#4ade80' }
        },
        {
          selector: 'edge.edge-else',
          style: { 'line-color': '#f97316', 'target-arrow-color': '#f97316', 'color': '#fb923c' }
        },
        {
          selector: 'edge.edge-highlight',
          style: { 'line-color': '#38bdf8', 'target-arrow-color': '#38bdf8', 'width': 3 }
        }
      ]
    });

    cyCfgInstance.on('tap', 'node', function(evt) {
      const node = evt.target;
      const pnId = node.data('pnodeId');
      showPNodeInspector(currentCfgFunId, pnId);
    });

    cyCfgInstance.on('tap', function(evt) {
      if (evt.target === cyCfgInstance) {
        currentSelectedPNodeId = null;
        document.getElementById('cfg-inspector').style.display = 'none';
        cyCfgInstance.elements().removeClass('pnode-selected pnode-neighbor edge-highlight');
        syncCfgSplitDock(currentCfgFunId, null);
      }
    });
  } else {
    cyCfgInstance.elements().remove();
    cyCfgInstance.add(elements);
  }

  let layoutOptions = {
    name: 'elk',
    fit: true,
    padding: 35,
    animate: false,
    elk: {
      'algorithm': 'layered',
      'org.eclipse.elk.direction': 'DOWN',
      'org.eclipse.elk.layered.spacing.nodeNodeBetweenLayers': '50',
      'org.eclipse.elk.spacing.nodeNode': '30',
      'org.eclipse.elk.edgeRouting': 'POLYLINE'
    }
  };

  try {
    const l = cyCfgInstance.layout(layoutOptions);
    l.run();
  } catch (e) {
    cyCfgInstance.layout({
      name: 'breadthfirst',
      directed: true,
      spacingFactor: 1.5,
      padding: 35
    }).run();
  }
}

function showPNodeInspector(funId, pnodeId) {
  const f = DATA.phase2_cfg.functions.find(fn => fn.id === funId);
  if (!f) return;
  ensureFunIndexed(f);
  const pn = f.pnodes.find(p => p.id === pnodeId);
  if (!pn) return;

  currentSelectedPNodeId = pnodeId;
  syncCfgSplitDock(funId, pnodeId);

  const inspector = document.getElementById('cfg-inspector');
  if (!inspector) return;
  inspector.style.display = 'block';

  if (cyCfgInstance) {
    cyCfgInstance.elements().removeClass('pnode-selected pnode-neighbor edge-highlight');
    const node = cyCfgInstance.getElementById('pn_' + pnodeId);
    if (node && node.length > 0) {
      node.addClass('pnode-selected');
      node.incomers('edge').addClass('edge-highlight');
      node.outgoers('edge').addClass('edge-highlight');
      node.incomers('node').addClass('pnode-neighbor');
      node.outgoers('node').addClass('pnode-neighbor');
    }
  }

  const pNum = currentCfgPass === 'final' ? totalPasses : parseInt(currentCfgPass);
  const srcLineLink = pn.line > 0 ? `<a href="#" onclick="jumpToSource(${pn.line}); return false;">line ${pn.line}</a>` : '-';

  let html = `
    <div style="display:flex; justify-content:space-between; align-items:center;">
      <h4 style="margin:0;">PNode #${pn.id} (${pn.kind})</h4>
      <button class="btn-sm" onclick="document.getElementById('cfg-inspector').style.display='none'" style="padding:0.15rem 0.4rem;">✕</button>
    </div>
    <div class="prop-row" style="margin-top:0.4rem;"><span>Primitive:</span><span class="prop-val">${pn.prim ? `<span class="badge badge-prim">${escapeHtml(pn.prim)}</span>` : 'none'}</span></div>
    <div class="prop-row"><span>Source Line:</span><span class="prop-val">${srcLineLink}</span></div>
    <div class="prop-row"><span>Predecessors:</span><span class="prop-val">${pn.cfg_pred.map(id => `<a href="#" onclick="showPNodeInspector(${f.id}, ${id}); return false;" style="color:var(--primary); margin-left:0.25rem;">#${id}</a>`).join('') || 'entry'}</span></div>
    <div class="prop-row"><span>Successors:</span><span class="prop-val">${pn.cfg_succ.map(id => `<a href="#" onclick="showPNodeInspector(${f.id}, ${id}); return false;" style="color:var(--primary); margin-left:0.25rem;">#${id}</a>`).join('') || 'exit'}</span></div>
  `;

  if (pn.lvals && pn.lvals.length > 0) {
    html += `
      <div class="inspector-section">
        <div class="inspector-section-title">Defined Variables (Lvals)</div>
    `;
    pn.lvals.forEach(lv => {
      const v = f._varById.get(lv.id);
      const curType = getTypeForVar(f, lv.id, currentCfgPass, currentCfgContour);
      html += `
        <div style="margin-bottom:0.6rem; background:rgba(0,0,0,0.25); padding:0.4rem; border-radius:6px;">
          <div style="display:flex; justify-content:space-between; align-items:center;">
            <span><strong style="color:var(--accent-green);">${escapeHtml(lv.name || 'v'+lv.id)}</strong> <span style="font-size:0.75rem; color:var(--text-dim);">(id: ${lv.id})</span></span>
            <span class="badge badge-type">${escapeHtml(curType)}</span>
          </div>
      `;
      if (v && v.avars && v.avars.length > 0) {
        html += `
          <div style="font-size:0.75rem; color:var(--text-muted); margin-top:0.25rem;">Mapped AVars across contours:</div>
          <table class="inspector-table">
            <thead><tr><th>Contour</th><th>AVar</th><th>Contour Type</th><th>Actions</th></tr></thead>
            <tbody>
        `;
        v.avars.forEach(av => {
          html += `
            <tr>
              <td>ES#${av.contour_id}</td>
              <td>#${av.av_id}</td>
              <td><code>${escapeHtml(av.type_str)}</code></td>
              <td>
                <button class="btn-sm" style="padding:0.1rem 0.3rem; font-size:0.7rem;" onclick="jumpToDataflow(${av.av_id})">Trace</button>
                <button class="btn-sm" style="padding:0.1rem 0.3rem; font-size:0.7rem;" onclick="jumpToPassContour(${pNum}, ${av.contour_id})">Passes</button>
              </td>
            </tr>
          `;
        });
        html += `</tbody></table>`;
      }
      html += `</div>`;
    });
    html += `</div>`;
  }

  if (pn.rvals && pn.rvals.length > 0) {
    html += `
      <div class="inspector-section">
        <div class="inspector-section-title">Used Variables (Rvals)</div>
    `;
    pn.rvals.forEach(rv => {
      const v = f._varById.get(rv.id);
      const curType = getTypeForVar(f, rv.id, currentCfgPass, currentCfgContour);
      html += `
        <div style="margin-bottom:0.6rem; background:rgba(0,0,0,0.25); padding:0.4rem; border-radius:6px;">
          <div style="display:flex; justify-content:space-between; align-items:center;">
            <span><strong style="color:var(--primary);">${escapeHtml(rv.name || 'v'+rv.id)}</strong> <span style="font-size:0.75rem; color:var(--text-dim);">(id: ${rv.id})</span></span>
            <span class="badge badge-type">${escapeHtml(curType)}</span>
          </div>
      `;
      if (v && v.avars && v.avars.length > 0) {
        html += `
          <div style="font-size:0.75rem; color:var(--text-muted); margin-top:0.25rem;">Mapped AVars across contours:</div>
          <table class="inspector-table">
            <thead><tr><th>Contour</th><th>AVar</th><th>Contour Type</th><th>Actions</th></tr></thead>
            <tbody>
        `;
        v.avars.forEach(av => {
          html += `
            <tr>
              <td>ES#${av.contour_id}</td>
              <td>#${av.av_id}</td>
              <td><code>${escapeHtml(av.type_str)}</code></td>
              <td>
                <button class="btn-sm" style="padding:0.1rem 0.3rem; font-size:0.7rem;" onclick="jumpToDataflow(${av.av_id})">Trace</button>
                <button class="btn-sm" style="padding:0.1rem 0.3rem; font-size:0.7rem;" onclick="jumpToPassContour(${pNum}, ${av.contour_id})">Passes</button>
              </td>
            </tr>
          `;
        });
        html += `</tbody></table>`;
      }
      html += `</div>`;
    });
    html += `</div>`;
  }

  if (pn.contour_calls && pn.contour_calls.length > 0) {
    html += `
      <div class="inspector-section">
        <div class="inspector-section-title">Contour Calls & Dispatch Targets</div>
    `;
    pn.contour_calls.forEach(cc => {
      html += `
        <div style="margin-bottom:0.4rem; background:rgba(168,85,247,0.1); border:1px solid rgba(168,85,247,0.2); padding:0.4rem; border-radius:6px;">
          <div style="font-weight:600; color:var(--text-main);">${escapeHtml(cc.callee_name)} <span style="font-size:0.75rem; color:var(--text-muted);">(fun #${cc.callee_id})</span></div>
          <div style="font-size:0.75rem; color:var(--text-muted); margin:0.2rem 0;">Caller: ES#${cc.from_es} &rarr; Callee: ES#${cc.to_es}</div>
          <div style="display:flex; gap:0.3rem; margin-top:0.3rem;">
            <button class="btn-sm" style="flex:1; font-size:0.75rem; padding:0.2rem;" onclick="jumpToCFG(${cc.callee_id}, ${cc.to_es}, ${pNum})">Inspect Callee CFG &rarr;</button>
            <button class="btn-sm" style="font-size:0.75rem; padding:0.2rem;" onclick="jumpToPassContour(${pNum}, ${cc.to_es})">View in Passes</button>
          </div>
        </div>
      `;
    });
    html += `</div>`;
  }

  inspector.innerHTML = html;
}

function showVarInspector(funId, varId) {
  const f = DATA.phase2_cfg.functions.find(fn => fn.id === funId);
  if (!f) return;
  ensureFunIndexed(f);
  const v = f._varById.get(varId);
  if (!v) return;

  const inspector = document.getElementById('cfg-inspector');
  if (!inspector) return;
  inspector.style.display = 'block';

  const pNum = currentCfgPass === 'final' ? totalPasses : parseInt(currentCfgPass);
  const curType = getTypeForVar(f, v.id, currentCfgPass, currentCfgContour);

  let html = `
    <div style="display:flex; justify-content:space-between; align-items:center;">
      <h4 style="margin:0;">Variable: ${escapeHtml(v.name || 'v'+v.id)}</h4>
      <button class="btn-sm" onclick="document.getElementById('cfg-inspector').style.display='none'" style="padding:0.15rem 0.4rem;">✕</button>
    </div>
    <div class="prop-row" style="margin-top:0.4rem;"><span>Variable ID:</span><span class="prop-val">${v.id}</span></div>
    <div class="prop-row"><span>Scope:</span><span class="prop-val">${v.is_formal ? 'Formal Argument' : (v.is_internal ? 'Internal Temporary' : 'Local Variable')}</span></div>
    <div class="prop-row"><span>Unified Type:</span><span class="prop-val"><code>${escapeHtml(v.type_str)}</code></span></div>
    <div class="prop-row"><span>Current Type (Pass ${pNum}):</span><span class="prop-val"><span class="badge badge-type">${escapeHtml(curType)}</span></span></div>
  `;

  if (v.avars && v.avars.length > 0) {
    html += `
      <div class="inspector-section">
        <div class="inspector-section-title">Contour Mappings (AVars)</div>
        <table class="inspector-table">
          <thead><tr><th>Contour</th><th>AVar ID</th><th>Type</th><th>Actions</th></tr></thead>
          <tbody>
    `;
    v.avars.forEach(av => {
      html += `
        <tr>
          <td>ES#${av.contour_id}</td>
          <td>#${av.av_id}</td>
          <td><code>${escapeHtml(av.type_str)}</code></td>
          <td>
            <button class="btn-sm" style="padding:0.1rem 0.3rem; font-size:0.7rem;" onclick="jumpToDataflow(${av.av_id})">Trace</button>
            <button class="btn-sm" style="padding:0.1rem 0.3rem; font-size:0.7rem;" onclick="jumpToPassContour(${pNum}, ${av.contour_id})">Passes</button>
          </td>
        </tr>
      `;
    });
    html += `</tbody></table></div>`;
  }

  html += `
    <div class="inspector-section">
      <div class="inspector-section-title">Type Evolution Across Passes</div>
      <table class="inspector-table">
        <thead><tr><th>Pass</th><th>Inferred Type</th><th>Action</th></tr></thead>
        <tbody>
  `;
  for (let pi = 1; pi <= totalPasses; pi++) {
    const t = getTypeForVar(f, v.id, pi, 'all');
    html += `
      <tr>
        <td>Pass ${pi}</td>
        <td><code>${escapeHtml(t)}</code></td>
        <td><button class="btn-sm" style="padding:0.1rem 0.3rem; font-size:0.7rem;" onclick="cfgPassSelect.value='${pi}'; cfgPassSelect.dispatchEvent(new Event('change'));">View</button></td>
      </tr>
    `;
  }
  html += `</tbody></table></div>`;

  inspector.innerHTML = html;
}

function jumpToCFG(funId, contourId, passNum) {
  let targetFun = DATA.phase2_cfg.functions.find(fn => fn.id === funId);
  if (!targetFun && contourId !== undefined && contourId !== null && contourId >= 0) {
    targetFun = DATA.phase2_cfg.functions.find(fn => (fn.entry_sets || []).includes(contourId));
  }
  if (!targetFun) {
    targetFun = DATA.phase2_cfg.functions.find(fn => fn.wraps_id === funId);
  }
  const resolvedFunId = targetFun ? targetFun.id : funId;

  switchTab('cfg');

  const fSel = document.getElementById('cfg-fun-select');
  if (fSel) fSel.value = resolvedFunId;
  currentCfgFunId = resolvedFunId;

  if (passNum) {
    const pSel = document.getElementById('cfg-pass-select');
    if (pSel) pSel.value = passNum;
    currentCfgPass = String(passNum);
  }

  const pN = currentCfgPass === 'final' ? totalPasses : parseInt(currentCfgPass);
  loadPassData(pN, () => {
    updateCfgContours();
    if (contourId !== undefined && contourId !== null && contourId >= 0) {
      const cSel = document.getElementById('cfg-contour-select');
      if (cSel && cSel.querySelector(`option[value="${contourId}"]`)) {
        cSel.value = contourId;
        currentCfgContour = String(contourId);
      }
    }
    renderCFGForFunction(resolvedFunId);
  });
}

function jumpToCFGAndNode(funId, pnodeId, contourId, passNum) {
  const cArg = (contourId !== undefined && contourId !== null && contourId >= 0) ? contourId : 'all';
  const pArg = (passNum !== undefined && passNum !== null && passNum > 0) ? passNum : 'final';
  jumpToCFG(funId, cArg, pArg);
  setTimeout(() => {
    showPNodeInspector(currentCfgFunId, pnodeId);
    if (cyCfgInstance) {
      const node = cyCfgInstance.getElementById('pn_' + pnodeId);
      if (node && node.length > 0) {
        cyCfgInstance.animate({
          center: { eles: node },
          zoom: Math.max(cyCfgInstance.zoom(), 1.0),
          duration: 300
        });
      }
    }
  }, 100);
}

function jumpToPassContour(passNum, esId, csId) {
  switchTab('passes');

  const pNum = passNum || totalPasses;
  currentPassIndex = pNum - 1;
  displayPass(pNum);

  if (esId !== undefined && esId !== null && esId >= 0) {
    setTimeout(() => {
      const rows = document.querySelectorAll('#pass-ess-table tbody tr');
      rows.forEach(r => {
        if (r.cells[0] && r.cells[0].textContent.trim() === String(esId)) {
          r.scrollIntoView({ behavior: 'smooth', block: 'center' });
          r.style.outline = '2px solid var(--primary)';
          setTimeout(() => { r.style.outline = 'none'; }, 2200);
        }
      });
    }, 250);
  } else if (csId !== undefined && csId !== null && csId >= 0) {
    setTimeout(() => {
      const rows = document.querySelectorAll('#pass-css-table tbody tr');
      rows.forEach(r => {
        if (r.cells[0] && r.cells[0].textContent.trim() === String(csId)) {
          r.scrollIntoView({ behavior: 'smooth', block: 'center' });
          r.style.outline = '2px solid #c084fc';
          setTimeout(() => { r.style.outline = 'none'; }, 2200);
        }
      });
    }, 250);
  }
}

function jumpToDataflow(avId, passNum) {
  switchTab('dataflow');

  const targetPass = passNum || currentDfPass || totalPasses;
  const pSel = document.getElementById('df-pass-select');
  if (pSel && targetPass !== currentDfPass) {
    pSel.value = targetPass;
    currentDfPass = targetPass;
  }

  const search = document.getElementById('df-search');
  if (search) {
    search.value = String(avId);
  }

  if (!currentDfData || currentDfPass !== targetPass) {
    loadPassData(targetPass, (data) => {
      currentDfData = data;
      renderDataflowTable(String(avId), avId);
    });
  } else {
    renderDataflowTable(String(avId), avId);
  }
}

function renderCFG() {
  if (typeof cytoscape === 'undefined') {
    loadCallGraphScripts(() => {
      renderCFGCurrent();
    });
  } else {
    renderCFGCurrent();
    if (cyCfgInstance) {
      setTimeout(() => {
        cyCfgInstance.resize();
        cyCfgInstance.fit(undefined, 35);
      }, 50);
    }
  }
}

// Tab: IFA Passes & Dynamic Script Loader
const passSlider = document.getElementById('pass-slider');
passSlider.max = Math.max(1, totalPasses);
const pendingPassCallbacks = new Map();

function loadPassData(passNum, callback) {
  const data = getPassData(passNum);
  if (data) {
    callback(data);
    return;
  }
  const passMeta = getPassMeta(passNum);
  if (!passMeta) { callback(null); return; }

  if (!passMeta.external) {
    callback(ensurePassIndexed(passMeta.data || passMeta));
    return;
  }

  if (passMeta.summary_only) {
    const sumObj = passMeta.summary || {};
    callback(ensurePassIndexed({
      summary: sumObj,
      summary_only: true,
      splits: passMeta.splits || sumObj.splits || [],
      entry_sets: [],
      creation_sets: [],
      avars: [],
      violations: []
    }));
    return;
  }

  const fileKey = passMeta.companion_file;
  if (pendingPassCallbacks.has(fileKey)) {
    pendingPassCallbacks.get(fileKey).push(callback);
    return;
  }
  pendingPassCallbacks.set(fileKey, [callback]);

  // Load external companion script once
  const script = document.createElement('script');
  script.src = fileKey;
  script.onload = () => {
    const loaded = getPassData(passNum);
    const cbs = pendingPassCallbacks.get(fileKey) || [];
    pendingPassCallbacks.delete(fileKey);
    cbs.forEach(cb => cb(loaded));
  };
  script.onerror = () => {
    console.error('Failed to load pass script:', fileKey);
    const cbs = pendingPassCallbacks.get(fileKey) || [];
    pendingPassCallbacks.delete(fileKey);
    cbs.forEach(cb => cb(null));
  };
  document.head.appendChild(script);
}

let passDiffMode = true;
let passDiffBaseChoice = 'prev';
let passDiffChangedOnly = false;

let splitsScopeChoice = 'auto'; // 'auto' | 'decided' | 'entering' | 'all'
let splitsKindFilter = 'all';   // 'all' | 'EntrySet' | 'CreationSet' | 'other'
let splitsStageFilter = null;   // null or stage string
let splitsExpandAll = false;
let lastRenderedPassArgs = null;

function getSplitsForPassNum(pNum) {
  if (!pNum || pNum < 1 || pNum > totalPasses) return [];
  const pMeta = getPassMeta(pNum);
  const pData = getPassData(pNum);
  if (pData && Array.isArray(pData.splits)) return pData.splits;
  if (pData && pData.summary && Array.isArray(pData.summary.splits)) return pData.summary.splits;
  if (pMeta && Array.isArray(pMeta.splits)) return pMeta.splits;
  if (pMeta && pMeta.summary && Array.isArray(pMeta.summary.splits)) return pMeta.summary.splits;
  if (pMeta && pMeta.data && Array.isArray(pMeta.data.splits)) return pMeta.data.splits;
  if (pMeta && pMeta.data && pMeta.data.summary && Array.isArray(pMeta.data.summary.splits)) return pMeta.data.summary.splits;
  return [];
}

function getAllPassesSplits() {
  const all = [];
  for (let p = 1; p <= totalPasses; p++) {
    const spArr = getSplitsForPassNum(p);
    spArr.forEach(sp => {
      all.push(Object.assign({}, sp, { pass_num: sp.pass_num || p }));
    });
  }
  return all;
}

function setSplitsScope(scope) {
  splitsScopeChoice = scope;
  if (lastRenderedPassArgs) {
    renderPassSplitsSection.apply(null, lastRenderedPassArgs);
    applyPassFilter();
  }
}

function setSplitsKindFilter(kind) {
  splitsKindFilter = kind;
  ['all', 'es', 'cs', 'other'].forEach(k => {
    const btn = document.getElementById('btn-splits-kind-' + k);
    if (!btn) return;
    const match = (k === 'all' && kind === 'all') ||
                  (k === 'es' && kind === 'EntrySet') ||
                  (k === 'cs' && kind === 'CreationSet') ||
                  (k === 'other' && kind === 'other');
    btn.classList.toggle('active', match);
  });
  if (lastRenderedPassArgs) {
    renderPassSplitsSection.apply(null, lastRenderedPassArgs);
    applyPassFilter();
  }
}

function setSplitsStageFilter(stage) {
  if (splitsStageFilter === stage) {
    splitsStageFilter = null;
  } else {
    splitsStageFilter = stage;
  }
  document.querySelectorAll('#pass-events-badges .stage-event-chip').forEach(ch => {
    ch.classList.toggle('active', splitsStageFilter && ch.dataset.stage === splitsStageFilter);
  });
  if (lastRenderedPassArgs) {
    renderPassSplitsSection.apply(null, lastRenderedPassArgs);
    applyPassFilter();
  }
}

function toggleSplitsExpandAll() {
  splitsExpandAll = !splitsExpandAll;
  const btn = document.getElementById('btn-splits-expand-all');
  if (btn) {
    btn.classList.toggle('active', splitsExpandAll);
    btn.textContent = splitsExpandAll ? '⊟ Collapse All' : '⊞ Expand All';
  }
  document.querySelectorAll('.split-extra-items').forEach(el => {
    el.style.display = splitsExpandAll ? 'flex' : 'none';
  });
  document.querySelectorAll('.split-expand-btn').forEach(b => {
    const cnt = b.dataset.hiddenCount || '';
    b.textContent = splitsExpandAll ? '▴ Show fewer' : `▾ +${cnt} more affected item(s)`;
  });
}

function toggleSplitRowItems(btnId, extraId) {
  const extraEl = document.getElementById(extraId);
  const btnEl = document.getElementById(btnId);
  if (!extraEl || !btnEl) return;
  const isHidden = extraEl.style.display === 'none';
  extraEl.style.display = isHidden ? 'flex' : 'none';
  const cnt = btnEl.dataset.hiddenCount || '';
  btnEl.textContent = isHidden ? '▴ Show fewer' : `▾ +${cnt} more affected item(s)`;
}

const SPLIT_SUBKIND_TOOLTIPS = {
  'TYPE_COMPAT': 'Partition incoming call edges of an EntrySet by argument/return AVar type compatibility',
  'SETTER_COMPAT': 'Partition incoming call edges by setter equivalence class on a confluence AVar',
  'MARK_SETTER_COMPAT': 'Partition incoming call edges by mark-distance setter compatibility',
  'SPLIT_EDGES_DYNAMIC': 'Filter incoming call edges per receiver CreationSet at a dynamic dispatch site',
  'PER_CS_RECEIVER': 'Split a method EntrySet so each receiver CreationSet gets its own method contour',
  'CSM_ELEMENT_CS': 'Split a container method EntrySet when same-class containers carry divergent element types',
  'UNTAGGED_DISPATCH': 'Split calling EntrySet per receiver CreationSet to resolve untagged container dispatch',
  'BOXING_RECEIVER': '2-way caller partition on method EntrySet driven by a BOXING violation on receiver fields/elements',
  'ARG_VIOL_RECEIVER': '2-way caller partition on method EntrySet driven by an ARG_VIOLATIONS demand on receiver',
  'BLOCKING_ES': '2-way caller partition on a shared EntrySet so reachable creation points of a demanded CreationSet can separate',
  'ES_BY_ARG_TYPES': 'Split enclosing EntrySet by caller argument types to duplicate a single-def (defs=1) demanded CreationSet',
  'ES_BY_CALL_SITE': 'Split enclosing EntrySet by returned assign-set reachability for a single-def (defs=1) demanded CreationSet',
  'NUMERIC_SEED_SPLIT': 'Backtrack pure numeric {int, float} confluence to callers with pure numeric seeds',
  'CPA_BY_CONTOUR': 'Separate CreationSet allocation sites by enclosing EntrySet contour and static arity',
  'PEEL_SINGLE_DEF': 'Peel an individual creation point (AVar) off a multi-def CreationSet into a fresh CreationSet',
  'CS_ASSIGN_SET_KEY': 'Route 4 (CS_DEF_PARTITION): partition CreationSet defs by element assign-set type signature',
  'CS_MEMBER_KEY': 'Route 4 (CS_DEF_PARTITION): partition CreationSet defs by instance member/slot type signature',
  'PARENT_FIRST_JOINSPLIT': 'Peel split-child EntrySet creation point from its parent CreationSet on demand',
  'CSELEM_REJOIN': 'Re-merge speculatively split empty-mint CreationSet back into peer CreationSet with identical element type',
  'CSELEM_RESPLIT': 'Re-split previously merged CreationSet whose converged element types diverged',
  'SPLIT_CONSTANTS': 'Mark AVar with split_Constants so distinct constant values split contours on the next pass',
  'DEMOTE_MIXED_ARITY': 'Demote tuple CreationSet with mixed arity across creation points to dynamic length'
};

function renderPassSplitsSection(passNum, pMeta, passData, basePassNum, baseMeta, baseData) {
  lastRenderedPassArgs = [passNum, pMeta, passData, basePassNum, baseMeta, baseData];
  const tbody = document.querySelector('#pass-splits-table tbody');
  const summaryLabel = document.getElementById('pass-splits-summary-label');
  const contextBanner = document.getElementById('pass-splits-context-banner');
  const stageChip = document.getElementById('pass-splits-stage-chip');
  if (!tbody) return;

  const decidedSplits = getSplitsForPassNum(passNum);
  const enteringPassNum = (basePassNum !== null && basePassNum !== undefined) ? basePassNum : (passNum > 1 ? passNum - 1 : null);
  const enteringSplits = enteringPassNum ? getSplitsForPassNum(enteringPassNum) : [];
  const allSplits = getAllPassesSplits();

  // Update scope buttons text & counts
  const btnDecided = document.getElementById('btn-splits-scope-decided');
  const btnEntering = document.getElementById('btn-splits-scope-entering');
  const btnAll = document.getElementById('btn-splits-scope-all');
  if (btnDecided) btnDecided.textContent = `🔨 Decided in Pass ${passNum} (${decidedSplits.length})`;
  if (btnEntering) {
    btnEntering.textContent = enteringPassNum
      ? `📥 Entering from Pass ${enteringPassNum} (${enteringSplits.length})`
      : `📥 Entering Pass (0)`;
  }
  if (btnAll) btnAll.textContent = `📚 All Passes (${allSplits.length})`;

  // Resolve effective scope (smart auto mode)
  let effectiveScope = splitsScopeChoice;
  let autoSwitchedNote = '';
  if (effectiveScope === 'auto') {
    if (decidedSplits.length > 0) {
      effectiveScope = 'decided';
    } else if (enteringPassNum && enteringSplits.length > 0) {
      effectiveScope = 'entering';
      autoSwitchedNote = `Pass ${passNum} reached quiescence with 0 new splits decided at the end of Pass ${passNum}. Showing the <strong>${enteringSplits.length} split(s) decided at the end of Pass ${enteringPassNum}</strong> that created Pass ${passNum}'s newly specialized contours.`;
    } else if (allSplits.length > 0) {
      effectiveScope = 'all';
      autoSwitchedNote = `No splits occurred in Pass ${passNum}${enteringPassNum ? ` or Pass ${enteringPassNum}` : ''}. Showing all <strong>${allSplits.length} split(s)</strong> across the full ${totalPasses}-pass analysis.`;
    } else {
      effectiveScope = 'decided';
    }
  } else if (effectiveScope === 'entering' && enteringPassNum) {
    autoSwitchedNote = `Showing splits decided during <code>extend_analysis()</code> at the end of <strong>Pass ${enteringPassNum}</strong>, which took effect when entering <strong>Pass ${passNum}</strong>.`;
  } else if (effectiveScope === 'decided' && decidedSplits.length > 0 && passNum < totalPasses) {
    autoSwitchedNote = `Showing splits decided during <code>extend_analysis()</code> at the end of <strong>Pass ${passNum}</strong> (after Pass ${passNum}'s types converged), which take effect in <strong>Pass ${passNum + 1}</strong>.`;
  }

  if (btnDecided) btnDecided.classList.toggle('active', effectiveScope === 'decided');
  if (btnEntering) btnEntering.classList.toggle('active', effectiveScope === 'entering');
  if (btnAll) btnAll.classList.toggle('active', effectiveScope === 'all');

  if (contextBanner) {
    if (autoSwitchedNote) {
      contextBanner.innerHTML = autoSwitchedNote;
      contextBanner.style.display = 'block';
    } else {
      contextBanner.style.display = 'none';
    }
  }

  if (stageChip) {
    if (splitsStageFilter) {
      stageChip.style.display = 'inline-block';
      stageChip.innerHTML = `<span class="badge badge-prim" style="cursor:pointer;" onclick="setSplitsStageFilter('${escapeHtml(splitsStageFilter)}')" title="Click to clear stage filter">Stage: ${escapeHtml(splitsStageFilter)} &times;</span>`;
    } else {
      stageChip.style.display = 'none';
    }
  }

  let sourceSplits = [];
  if (effectiveScope === 'decided') {
    sourceSplits = decidedSplits.map(sp => Object.assign({}, sp, { pass_num: sp.pass_num || passNum }));
  } else if (effectiveScope === 'entering') {
    sourceSplits = enteringSplits.map(sp => Object.assign({}, sp, { pass_num: sp.pass_num || enteringPassNum }));
  } else {
    sourceSplits = allSplits;
  }

  // Apply Kind & Stage filters
  const filteredSplits = sourceSplits.filter(sp => {
    if (splitsKindFilter === 'EntrySet' && sp.kind !== 'EntrySet') return false;
    if (splitsKindFilter === 'CreationSet' && sp.kind !== 'CreationSet') return false;
    if (splitsKindFilter === 'other' && (sp.kind === 'EntrySet' || sp.kind === 'CreationSet')) return false;
    if (splitsStageFilter && sp.stage !== splitsStageFilter) return false;
    return true;
  });

  const esSplitCount = filteredSplits.filter(s => s.kind === 'EntrySet').length;
  const csSplitCount = filteredSplits.filter(s => s.kind === 'CreationSet').length;
  const otherSplitCount = filteredSplits.length - esSplitCount - csSplitCount;
  if (summaryLabel) {
    summaryLabel.textContent = `(${filteredSplits.length} split${filteredSplits.length === 1 ? '' : 's'}: ${esSplitCount} EntrySet, ${csSplitCount} CreationSet${otherSplitCount > 0 ? `, ${otherSplitCount} Const/Arity` : ''})`;
  }

  tbody.innerHTML = '';
  if (filteredSplits.length === 0) {
    const emptyMsg = allSplits.length === 0
      ? 'No contour or creation-set splits were needed for this program (converged in initial contours).'
      : `No splits match the current scope/filter (${effectiveScope === 'decided' && enteringSplits.length > 0 ? `<a href="#" onclick="setSplitsScope('entering'); return false;" style="color:var(--primary); text-decoration:underline;">view ${enteringSplits.length} entering split(s) from Pass ${enteringPassNum}</a> or ` : ''}<a href="#" onclick="setSplitsScope('all'); setSplitsKindFilter('all'); if(splitsStageFilter) setSplitsStageFilter(splitsStageFilter); return false;" style="color:var(--primary); text-decoration:underline;">view all ${allSplits.length} split(s) across all passes</a>).`;
    tbody.innerHTML = `<tr><td colspan="5" style="text-align:center; padding:1.35rem; color:var(--text-muted);">${emptyMsg}</td></tr>`;
    return;
  }

  const frag = document.createDocumentFragment();
  filteredSplits.forEach((sp, rowIdx) => {
    const tr = document.createElement('tr');
    tr.dataset.diffStatus = 'new';
    const spPass = sp.pass_num || passNum;
    const effectPass = Math.min(totalPasses, spPass + 1);

    // 1. Pass / #
    const passCellHtml = `
      <div style="display:flex; flex-direction:column; gap:0.2rem;">
        <a href="#" onclick="jumpToPassContour(${spPass}); return false;" class="badge badge-contour" style="text-align:center;" title="Inspect Pass ${spPass}">Pass ${spPass}</a>
        <span style="font-size:0.72rem; color:var(--text-muted); font-family:var(--font-mono);">#${sp.id || (rowIdx + 1)}</span>
      </div>
    `;

    // 2. Stage & Mechanism
    const isViolStage = sp.stage === 'VIOLATION' || !!sp.viol_kind;
    const isCsStage = sp.kind === 'CreationSet' || sp.stage === 'CPA' || sp.stage === 'CS_DEF_PART';
    const stageCls = isViolStage ? 'stage-viol' : (isCsStage ? 'stage-cs' : '');
    let kindBadge = '';
    if (sp.kind === 'EntrySet') {
      kindBadge = `<span class="badge badge-contour" style="cursor:default;">EntrySet</span>`;
    } else if (sp.kind === 'CreationSet') {
      kindBadge = `<span class="badge badge-kind">CreationSet</span>`;
    } else if (sp.kind === 'ConstDemand') {
      kindBadge = `<span class="badge badge-var" style="cursor:default;">ConstDemand</span>`;
    } else {
      kindBadge = `<span class="badge diff-badge-mod">${escapeHtml(sp.kind || 'Split')}</span>`;
    }
    const subTip = SPLIT_SUBKIND_TOOLTIPS[sp.sub_kind] || sp.sub_kind || '';
    const stageCellHtml = `
      <div style="display:flex; flex-direction:column; gap:0.28rem; align-items:flex-start;">
        <span class="split-stage-badge ${stageCls}" style="cursor:pointer;" onclick="setSplitsStageFilter('${escapeHtml(sp.stage)}')" title="Click to filter by stage ${escapeHtml(sp.stage)}">${escapeHtml(sp.stage || 'SPLIT')}</span>
        ${kindBadge}
        <span class="split-subkind-badge" title="${escapeHtml(subTip)}">${escapeHtml(sp.sub_kind || '')}</span>
      </div>
    `;

    // 3. What Was Split (Parent -> Products)
    let targetCellHtml = '';
    const products = sp.products || [];
    if (sp.kind === 'EntrySet') {
      const funLink = sp.target_fun_id >= 0
        ? `<a href="#" onclick="jumpToCFG(${sp.target_fun_id}, ${sp.target_id}, ${spPass}); return false;" style="color:var(--primary); font-weight:700; text-decoration:underline;">${escapeHtml(sp.target_name)}</a>`
        : `<strong>${escapeHtml(sp.target_name)}</strong>`;
      const parentBadge = `<span class="badge badge-contour" onclick="jumpToPassContour(${spPass}, ${sp.target_id})" title="Highlight ES#${sp.target_id} in EntrySets table">ES#${sp.target_id}</span>`;
      const prodsHtml = products.map(pr => {
        const pBadgeCls = pr.is_new ? 'diff-badge-new' : 'badge-contour';
        const pLabel = pr.is_new ? `NEW ES#${pr.id}` : `ES#${pr.id}`;
        const sigHtml = pr.sig ? `<code style="font-size:0.7rem; color:#e2e8f0;">${escapeHtml(pr.sig)}</code>` : '';
        const cfgBtn = sp.target_fun_id >= 0
          ? `<button class="btn-sm" style="padding:0.05rem 0.3rem; font-size:0.66rem;" onclick="jumpToCFG(${sp.target_fun_id}, ${pr.id}, ${effectPass})" title="Inspect ES#${pr.id} in CFG (Pass ${effectPass})">🔀</button>`
          : '';
        return `
          <div class="split-product-pill">
            <span class="badge ${pBadgeCls}" style="cursor:pointer;" onclick="jumpToPassContour(${effectPass}, ${pr.id})" title="Highlight ES#${pr.id} in Pass ${effectPass}">${pLabel}</span>
            <span style="color:var(--text-muted); font-size:0.7rem;">(${pr.moved_count || 0} edge${(pr.moved_count || 0) === 1 ? '' : 's'})</span>
            ${sigHtml}
            ${cfgBtn}
          </div>
        `;
      }).join('');

      targetCellHtml = `
        <div style="margin-bottom:0.25rem;">
          ${parentBadge} ${funLink}
          <span style="font-size:0.72rem; color:var(--text-muted); margin-left:0.25rem;">(${sp.stayed_count || 0} stayed, ${sp.moved_count || 0} moved)</span>
        </div>
        <div style="display:flex; flex-direction:column; gap:0.15rem;">
          ${prodsHtml || '<span style="font-size:0.74rem; color:var(--text-muted);">Re-routed incoming edges</span>'}
        </div>
      `;
    } else if (sp.kind === 'CreationSet') {
      const parentBadge = `<span class="badge badge-kind" style="cursor:pointer;" onclick="jumpToPassContour(${spPass}, null, ${sp.target_id})" title="Highlight CS#${sp.target_id} in CreationSets table">CS#${sp.target_id}</span>`;
      const prodsHtml = products.map(pr => {
        const pBadgeCls = pr.is_new ? 'diff-badge-new' : 'badge-kind';
        const pLabel = pr.is_new ? `NEW CS#${pr.id}` : `CS#${pr.id}`;
        const sigHtml = pr.sig ? `<code style="font-size:0.7rem; color:#e2e8f0;">${escapeHtml(pr.sig)}</code>` : '';
        return `
          <div class="split-product-pill">
            <span class="badge ${pBadgeCls}" style="cursor:pointer;" onclick="jumpToPassContour(${effectPass}, null, ${pr.id})" title="Highlight CS#${pr.id} in Pass ${effectPass}">${pLabel}</span>
            <span style="color:var(--text-muted); font-size:0.7rem;">(${pr.moved_count || 0} def${(pr.moved_count || 0) === 1 ? '' : 's'})</span>
            ${sigHtml}
          </div>
        `;
      }).join('');

      targetCellHtml = `
        <div style="margin-bottom:0.25rem;">
          ${parentBadge} <strong>${escapeHtml(sp.target_name)}</strong>
          <span style="font-size:0.72rem; color:var(--text-muted); margin-left:0.25rem;">(${sp.stayed_count || 0} stayed, ${sp.moved_count || 0} separated)</span>
        </div>
        <div style="display:flex; flex-direction:column; gap:0.15rem;">
          ${prodsHtml || '<span style="font-size:0.74rem; color:var(--text-muted);">Separated creation points</span>'}
        </div>
      `;
    } else if (sp.kind === 'ConstDemand') {
      const esBadge = `<span class="badge badge-contour" onclick="jumpToPassContour(${spPass}, ${sp.target_id})">ES#${sp.target_id}</span>`;
      const funLink = sp.target_fun_id >= 0
        ? `<a href="#" onclick="jumpToCFG(${sp.target_fun_id}, ${sp.target_id}, ${spPass}); return false;" style="color:var(--primary); font-weight:700; text-decoration:underline;">${escapeHtml(sp.target_name)}</a>`
        : `<strong>${escapeHtml(sp.target_name)}</strong>`;
      const favHtml = sp.trigger_av_id >= 0
        ? `<div style="margin-top:0.2rem; font-size:0.74rem;">Formal: <a href="#" onclick="jumpToDataflow(${sp.trigger_av_id}, ${spPass}); return false;" class="badge badge-var">${escapeHtml(sp.trigger_var_name || ('AVar #' + sp.trigger_av_id))}</a> <span style="color:var(--text-muted);">(${sp.moved_count || 0} constant partition(s))</span></div>`
        : '';
      targetCellHtml = `
        <div>
          ${esBadge} ${funLink}
          ${favHtml}
        </div>
      `;
    } else {
      targetCellHtml = `
        <div>
          <span class="badge badge-kind" style="cursor:pointer;" onclick="jumpToPassContour(${spPass}, null, ${sp.target_id})">CS#${sp.target_id}</span>
          <strong>${escapeHtml(sp.target_name)}</strong>
          <div style="margin-top:0.25rem; font-size:0.73rem; color:var(--text-muted);">Marked <code>no_static_arity = 1</code></div>
        </div>
      `;
    }

    // 4. Why It Split (Trigger, Demand & Converged Union)
    let triggerMetaParts = [];
    if (sp.viol_kind) {
      triggerMetaParts.push(`<span class="badge badge-dead" style="text-decoration:none;" title="Type Violation at Quiescence">⚠️ ${escapeHtml(sp.viol_kind)}</span>`);
    }
    if (sp.demand_cs_id !== undefined && sp.demand_cs_id >= 0) {
      triggerMetaParts.push(`<span class="badge badge-kind" style="cursor:pointer;" onclick="jumpToPassContour(${spPass}, null, ${sp.demand_cs_id})" title="Demanded CreationSet driving this split">Demanded: CS#${sp.demand_cs_id} (${escapeHtml(sp.demand_cs_sym || 'CS')})</span>`);
    }
    if (sp.trigger_av_id !== undefined && sp.trigger_av_id >= 0) {
      const vLabel = sp.trigger_var_name ? `${sp.trigger_var_name} (#${sp.trigger_av_id})` : `AVar #${sp.trigger_av_id}`;
      triggerMetaParts.push(`<span>Trigger: <a href="#" onclick="jumpToDataflow(${sp.trigger_av_id}, ${spPass}); return false;" class="badge badge-var" title="Trace AVar #${sp.trigger_av_id} in Bidirectional Dataflow Tracer">${escapeHtml(vLabel)}</a></span>`);
      if (sp.trigger_role) {
        triggerMetaParts.push(`<span style="color:var(--text-dim);">(${escapeHtml(sp.trigger_role)})</span>`);
      }
    }
    if (sp.trigger_fun_id !== undefined && sp.trigger_fun_id >= 0) {
      const cId = (sp.trigger_contour_id !== undefined && sp.trigger_contour_id >= 0) ? sp.trigger_contour_id : 'all';
      const cText = (sp.trigger_contour_id !== undefined && sp.trigger_contour_id >= 0) ? ` [ES#${sp.trigger_contour_id}]` : '';
      triggerMetaParts.push(`<span>in <a href="#" onclick="jumpToCFG(${sp.trigger_fun_id}, '${cId}', ${spPass}); return false;" style="color:var(--primary); text-decoration:underline;">${escapeHtml(sp.trigger_fun_name || ('fun #' + sp.trigger_fun_id))}${escapeHtml(cText)}</a></span>`);
    }
    if (sp.trigger_type_str) {
      const unionParts = sp.trigger_type_str.split(' | ').filter(Boolean);
      const uBadges = unionParts.slice(0, 8).map(t => `<span class="badge badge-type">${escapeHtml(t)}</span>`).join(' ');
      const moreU = unionParts.length > 8 ? ` <span style="font-size:0.7rem; color:var(--text-muted);">+${unionParts.length - 8} more</span>` : '';
      triggerMetaParts.push(`<span>Union: ${uBadges}${moreU}</span>`);
    }

    const whyCellHtml = `
      <div class="split-reason-box">${escapeHtml(sp.reason || '')}</div>
      <div class="split-trigger-meta">${triggerMetaParts.join(' ')}</div>
    `;

    // 5. What Was Affected (Call Edges / Creation Points)
    let impactCellHtml = '';
    const edges = sp.affected_edges || [];
    const defs = sp.affected_defs || [];

    if (edges.length > 0) {
      const renderEdgeItem = (e) => {
        const callerLink = e.from_fun_id >= 0
          ? `<a href="#" onclick="jumpToCFGAndNode(${e.from_fun_id}, ${e.pnode_id}, ${e.from_es}, ${spPass}); return false;" style="color:var(--primary); font-weight:600; text-decoration:underline;" title="Open Caller CFG at PNode #${e.pnode_id}">${escapeHtml(e.from_fun_name)}</a>`
          : escapeHtml(e.from_fun_name || 'caller');
        const fromEsBadge = e.from_es >= 0 ? `<span class="badge badge-contour" onclick="jumpToPassContour(${spPass}, ${e.from_es})">ES#${e.from_es}</span>` : '';
        const lineBtn = e.line > 0
          ? `<a href="#" onclick="jumpToSource(${e.line}); return false;" style="color:var(--text-muted); text-decoration:underline;" title="Jump to Python Source Line ${e.line}">L${e.line}</a>`
          : '';
        const argsCode = e.arg_summary ? `<code style="font-size:0.68rem; color:#cbd5e1;">(${escapeHtml(e.arg_summary)})</code>` : '';
        return `
          <div class="split-impact-item">
            <span style="color:var(--text-dim); font-family:var(--font-mono);">Edge #${e.edge_id}:</span>
            ${callerLink} ${fromEsBadge}
            <span style="color:var(--text-muted);">@ PNode #${e.pnode_id}</span> ${lineBtn}
            <span style="font-family:var(--font-mono);"><span class="diff-old">ES#${e.old_to_es}</span> &rarr; <span class="diff-new">ES#${e.new_to_es}</span></span>
            ${argsCode}
          </div>
        `;
      };
      const visibleEdges = edges.slice(0, 3);
      const hiddenEdges = edges.slice(3);
      const extraId = `split-extra-${spPass}-${rowIdx}`;
      const btnId = `split-btn-${spPass}-${rowIdx}`;
      impactCellHtml = `
        <div style="font-size:0.74rem; font-weight:600; color:#38bdf8; margin-bottom:0.2rem;">
          🔀 ${edges.length} Call Edge${edges.length === 1 ? '' : 's'} Re-routed
        </div>
        <div class="split-impact-list">
          ${visibleEdges.map(renderEdgeItem).join('')}
        </div>
        ${hiddenEdges.length > 0 ? `
          <div id="${extraId}" class="split-impact-list split-extra-items" style="display:${splitsExpandAll ? 'flex' : 'none'}; margin-top:0.3rem;">
            ${hiddenEdges.map(renderEdgeItem).join('')}
          </div>
          <button id="${btnId}" class="btn-sm split-expand-btn" data-hidden-count="${hiddenEdges.length}" style="margin-top:0.3rem; padding:0.12rem 0.45rem; font-size:0.7rem;" onclick="toggleSplitRowItems('${btnId}', '${extraId}')">
            ${splitsExpandAll ? '▴ Show fewer' : `▾ +${hiddenEdges.length} more affected item(s)`}
          </button>
        ` : ''}
      `;
    } else if (defs.length > 0) {
      const renderDefItem = (d) => {
        const avBadge = d.av_id >= 0
          ? `<a href="#" onclick="jumpToDataflow(${d.av_id}, ${spPass}); return false;" class="badge badge-var" title="Inspect Creation Point AVar #${d.av_id} in Dataflow Tracer">AVar #${d.av_id}</a>`
          : '';
        const varCode = d.var_name ? `<code>${escapeHtml(d.var_name)}</code>` : '';
        const funLink = (d.fun_id >= 0 && d.pnode_id >= 0)
          ? `<a href="#" onclick="jumpToCFGAndNode(${d.fun_id}, ${d.pnode_id}, ${d.es_id}, ${spPass}); return false;" style="color:var(--primary); text-decoration:underline;" title="Inspect Allocation Site PNode #${d.pnode_id} in CFG">${escapeHtml(d.fun_name || ('fun #' + d.fun_id))}</a>`
          : (d.fun_id >= 0
              ? `<a href="#" onclick="jumpToCFG(${d.fun_id}, ${d.es_id >= 0 ? d.es_id : "'all'"}, ${spPass}); return false;" style="color:var(--primary); text-decoration:underline;">${escapeHtml(d.fun_name || ('fun #' + d.fun_id))}</a>`
              : escapeHtml(d.fun_name || 'global'));
        const esBadge = d.es_id >= 0 ? `<span class="badge badge-contour" onclick="jumpToPassContour(${spPass}, ${d.es_id})">ES#${d.es_id}</span>` : '';
        const lineBtn = d.line > 0
          ? `<a href="#" onclick="jumpToSource(${d.line}); return false;" style="color:var(--text-muted); text-decoration:underline;" title="Jump to Python Source Line ${d.line}">L${d.line}</a>`
          : '';
        const detHtml = d.detail ? `<code style="font-size:0.68rem; color:#cbd5e1;">[${escapeHtml(d.detail)}]</code>` : '';
        return `
          <div class="split-impact-item">
            ${avBadge} ${varCode}
            <span>in ${funLink}</span> ${esBadge}
            ${d.pnode_id >= 0 ? `<span style="color:var(--text-muted);">@ PNode #${d.pnode_id}</span>` : ''} ${lineBtn}
            <span style="font-family:var(--font-mono);"><span class="diff-old">CS#${d.old_cs}</span> &rarr; <span class="diff-new">CS#${d.new_cs}</span></span>
            ${detHtml}
          </div>
        `;
      };
      const visibleDefs = defs.slice(0, 3);
      const hiddenDefs = defs.slice(3);
      const extraId = `split-extra-${spPass}-${rowIdx}`;
      const btnId = `split-btn-${spPass}-${rowIdx}`;
      impactCellHtml = `
        <div style="font-size:0.74rem; font-weight:600; color:#c084fc; margin-bottom:0.2rem;">
          📦 ${defs.length} Creation Point${defs.length === 1 ? '' : 's'} Separated
        </div>
        <div class="split-impact-list">
          ${visibleDefs.map(renderDefItem).join('')}
        </div>
        ${hiddenDefs.length > 0 ? `
          <div id="${extraId}" class="split-impact-list split-extra-items" style="display:${splitsExpandAll ? 'flex' : 'none'}; margin-top:0.3rem;">
            ${hiddenDefs.map(renderDefItem).join('')}
          </div>
          <button id="${btnId}" class="btn-sm split-expand-btn" data-hidden-count="${hiddenDefs.length}" style="margin-top:0.3rem; padding:0.12rem 0.45rem; font-size:0.7rem;" onclick="toggleSplitRowItems('${btnId}', '${extraId}')">
            ${splitsExpandAll ? '▴ Show fewer' : `▾ +${hiddenDefs.length} more affected item(s)`}
          </button>
        ` : ''}
      `;
    } else if (sp.kind === 'ConstDemand') {
      impactCellHtml = `
        <div class="split-impact-item">
          <span>Enables constant-value contour splitting on</span>
          <a href="#" onclick="jumpToDataflow(${sp.target_id}, ${spPass}); return false;" class="badge badge-var">AVar #${sp.target_id}</a>
          <span>in Pass ${effectPass}</span>
        </div>
      `;
    } else if (sp.kind === 'ArityDemote') {
      impactCellHtml = `
        <div class="split-impact-item">
          <span>Tuple</span>
          <span class="badge badge-kind" onclick="jumpToPassContour(${spPass}, null, ${sp.target_id})">CS#${sp.target_id}</span>
          <span>widened to dynamic length (<code>no_static_arity</code>) for Pass ${effectPass}</span>
        </div>
      `;
    } else {
      impactCellHtml = `<span style="font-size:0.74rem; color:var(--text-muted);">${sp.moved_count || 0} item(s) affected</span>`;
    }

    tr.innerHTML = `
      <td>${passCellHtml}</td>
      <td>${stageCellHtml}</td>
      <td>${targetCellHtml}</td>
      <td>${whyCellHtml}</td>
      <td>${impactCellHtml}</td>
    `;
    frag.appendChild(tr);
  });
  tbody.appendChild(frag);
}

const passDiffBaseSelect = document.getElementById('pass-diff-base-select');
if (passDiffBaseSelect) {
  for (let p = 1; p <= totalPasses; p++) {
    const opt = document.createElement('option');
    opt.value = String(p);
    opt.textContent = `Pass ${p}`;
    passDiffBaseSelect.appendChild(opt);
  }
  passDiffBaseSelect.addEventListener('change', () => {
    passDiffBaseChoice = passDiffBaseSelect.value;
    displayPass(currentPassIndex + 1);
  });
}

const btnPassDiffToggle = document.getElementById('btn-pass-diff-toggle');
if (btnPassDiffToggle) {
  btnPassDiffToggle.addEventListener('click', () => {
    passDiffMode = !passDiffMode;
    btnPassDiffToggle.classList.toggle('active', passDiffMode);
    btnPassDiffToggle.textContent = passDiffMode ? '🔀 Diff Mode: ON' : '🔀 Diff Mode: OFF';
    if (!passDiffMode && passDiffChangedOnly) {
      passDiffChangedOnly = false;
      const coBtn = document.getElementById('btn-pass-changed-only');
      if (coBtn) coBtn.classList.remove('active');
    }
    displayPass(currentPassIndex + 1);
  });
}

const btnPassChangedOnly = document.getElementById('btn-pass-changed-only');
if (btnPassChangedOnly) {
  btnPassChangedOnly.addEventListener('click', () => {
    passDiffChangedOnly = !passDiffChangedOnly;
    btnPassChangedOnly.classList.toggle('active', passDiffChangedOnly);
    if (passDiffChangedOnly && !passDiffMode) {
      passDiffMode = true;
      if (btnPassDiffToggle) {
        btnPassDiffToggle.classList.add('active');
        btnPassDiffToggle.textContent = '🔀 Diff Mode: ON';
      }
      displayPass(currentPassIndex + 1);
    } else {
      applyPassFilter();
    }
  });
}

function getResolvedBasePassNum(passNum) {
  if (!passDiffMode || totalPasses <= 1) return null;
  if (passDiffBaseChoice === 'prev') {
    return passNum > 1 ? passNum - 1 : null;
  }
  const b = parseInt(passDiffBaseChoice, 10);
  if (!isNaN(b) && b >= 1 && b <= totalPasses && b !== passNum) return b;
  return null;
}

function formatDeltaBadge(currVal, baseVal, warnOnIncrease) {
  const c = currVal || 0;
  const b = baseVal || 0;
  const d = c - b;
  if (d === 0) return `<span class="diff-delta zero">±0</span>`;
  if (d > 0) return `<span class="diff-delta ${warnOnIncrease ? 'warn-plus' : 'plus'}">+${d}</span>`;
  return `<span class="diff-delta minus">${d}</span>`;
}

function formatEsArgType(a) {
  return `[${(a.type || []).map(t => escapeHtml(t.sym) + (t.is_const ? `=${escapeHtml(t.val)}` : '')).join('|')}]`;
}

function formatEsRetType(r) {
  return `[${(r.type || []).map(t => escapeHtml(t.sym)).join('|')}]`;
}

function formatCsFieldType(f) {
  return `[${(f.type || []).map(t => escapeHtml(t.sym)).join('|')}]`;
}

function formatAvarTypeSet(av) {
  if (!av || !av.type || av.type.length === 0) return 'void';
  return Array.from(new Set(av.type.map(t => t.sym + (t.is_const ? `=${t.val}` : '')))).join(' | ');
}

function applyPassFilter() {
  const filterInput = document.getElementById('pass-filter-input');
  const q = filterInput ? filterInput.value.trim().toLowerCase() : '';
  const hasActiveDiff = getResolvedBasePassNum(currentPassIndex + 1) !== null;
  document.querySelectorAll('#pass-splits-table tbody tr, #pass-ess-table tbody tr, #pass-css-table tbody tr, #pass-viol-table tbody tr, #pass-resolved-viol-table tbody tr, #pass-avar-diff-table tbody tr').forEach(tr => {
    if (tr.cells.length <= 1) return;
    const matchesText = !q || tr.textContent.toLowerCase().includes(q);
    const matchesChanged = !(passDiffChangedOnly && hasActiveDiff) || (tr.dataset.diffStatus && tr.dataset.diffStatus !== 'unchanged');
    tr.style.display = (matchesText && matchesChanged) ? '' : 'none';
  });
}

const passFilterInput = document.getElementById('pass-filter-input');
if (passFilterInput) {
  passFilterInput.addEventListener('input', applyPassFilter);
}

function displayPass(passNum) {
  document.getElementById('pass-indicator').textContent = `Pass ${passNum} / ${totalPasses}`;
  passSlider.value = passNum;

  const pMeta = getPassMeta(passNum);
  const basePassNum = getResolvedBasePassNum(passNum);

  loadPassData(passNum, (passData) => {
    if (!passData && !pMeta) return;
    if (basePassNum !== null) {
      loadPassData(basePassNum, (baseData) => {
        renderPassWithOptionalDiff(passNum, pMeta, passData, basePassNum, getPassMeta(basePassNum), baseData);
      });
    } else {
      renderPassWithOptionalDiff(passNum, pMeta, passData, null, null, null);
    }
  });
}

function renderPassWithOptionalDiff(passNum, pMeta, passData, basePassNum, baseMeta, baseData) {
  const s = (passData && passData.summary) || (pMeta && pMeta.summary) || passData || {};
  const bs = basePassNum !== null ? ((baseData && baseData.summary) || (baseMeta && baseMeta.summary) || baseData || null) : null;

  const decidedSplits = getSplitsForPassNum(passNum);
  const enteringPassNum = basePassNum !== null ? basePassNum : (passNum > 1 ? passNum - 1 : null);
  const enteringSplits = enteringPassNum ? getSplitsForPassNum(enteringPassNum) : [];

  document.getElementById('pass-card-ess').innerHTML = `${s.ess_count || 0}${bs ? formatDeltaBadge(s.ess_count, bs.ess_count, false) : ''}`;
  document.getElementById('pass-card-css').innerHTML = `${s.css_count || 0}${bs ? formatDeltaBadge(s.css_count, bs.css_count, false) : ''}`;
  const splitsCardEl = document.getElementById('pass-card-splits');
  if (splitsCardEl) {
    const subHint = (decidedSplits.length === 0 && enteringSplits.length > 0)
      ? `<span class="diff-delta plus" title="${enteringSplits.length} split(s) entered from Pass ${enteringPassNum}">${enteringSplits.length} in</span>`
      : '';
    splitsCardEl.innerHTML = `${decidedSplits.length}${subHint}`;
  }
  document.getElementById('pass-card-viol').innerHTML = `${s.violations_count || 0}${bs ? formatDeltaBadge(s.violations_count, bs.violations_count, true) : ''}`;
  document.getElementById('pass-card-dup').innerHTML = `${(s.dup_splits || 0) + (s.cs_dup_splits || 0)}${bs ? formatDeltaBadge((s.dup_splits || 0) + (s.cs_dup_splits || 0), (bs.dup_splits || 0) + (bs.cs_dup_splits || 0), true) : ''}`;

  const summaryOnlyBanner = document.getElementById('pass-summary-only-banner');
  if (summaryOnlyBanner) {
    summaryOnlyBanner.style.display = ((pMeta && pMeta.summary_only) || (passData && passData.summary_only)) ? 'block' : 'none';
  }

  // Splitter Stage Events (supports both Array of stage event objects and key/value map)
  const eventsSec = document.getElementById('pass-events-section');
  const eventsBadges = document.getElementById('pass-events-badges');
  if (eventsSec && eventsBadges) {
    eventsBadges.innerHTML = '';
    const rawEvents = s.events || (passData && passData.events) || [];
    if (Array.isArray(rawEvents)) {
      const activeEvents = rawEvents.filter(ev => ev && ev.splits > 0);
      if (activeEvents.length > 0) {
        eventsSec.style.display = 'block';
        activeEvents.forEach(ev => {
          const chip = document.createElement('div');
          chip.className = 'stage-event-chip' + (splitsStageFilter === ev.stage ? ' active' : '');
          chip.dataset.stage = ev.stage;
          chip.onclick = () => setSplitsStageFilter(ev.stage);
          const deltas = [];
          if (ev.ess_before !== ev.ess_after) deltas.push(`ESS ${ev.ess_before}&rarr;${ev.ess_after}`);
          if (ev.css_before !== ev.css_after) deltas.push(`CSS ${ev.css_before}&rarr;${ev.css_after}`);
          if (ev.viol_before !== undefined && ev.viol_after !== undefined) deltas.push(`Viol ${ev.viol_before}`);
          const deltaStr = deltas.length > 0 ? `<span style="color:var(--text-muted); font-size:0.7rem;">(${deltas.join(', ')})</span>` : '';
          chip.innerHTML = `<strong style="color:#38bdf8; font-family:var(--font-mono);">${escapeHtml(ev.stage)}</strong>: <span>${ev.splits} split${ev.splits === 1 ? '' : 's'}</span> ${deltaStr}`;
          eventsBadges.appendChild(chip);
        });
      } else {
        eventsSec.style.display = 'none';
      }
    } else {
      const activeEvents = Object.entries(rawEvents).filter(([k, v]) => v > 0);
      if (activeEvents.length > 0) {
        eventsSec.style.display = 'block';
        activeEvents.forEach(([k, v]) => {
          const span = document.createElement('span');
          span.className = 'stage-event-chip' + (splitsStageFilter === k ? ' active' : '');
          span.dataset.stage = k;
          span.onclick = () => setSplitsStageFilter(k);
          span.textContent = `${k}: ${v}`;
          eventsBadges.appendChild(span);
        });
      } else {
        eventsSec.style.display = 'none';
      }
    }
  }

  // Render Deep-Dive Splits Section
  renderPassSplitsSection(passNum, pMeta, passData, basePassNum, baseMeta, baseData);

  const hasFullDiff = basePassNum !== null && baseData && !baseData.summary_only && passData && !passData.summary_only;
  const getViolAvId = (vl) => (vl.av_id !== undefined ? vl.av_id : vl.avar_id);
  const getViolTypes = (vl) => Array.isArray(vl.types) ? vl.types : (vl.type_str ? vl.type_str.split(' | ').filter(Boolean) : []);
  const violKey = (vl) => `${vl.kind}:${getViolAvId(vl)}:${vl.var_id}:${vl.contour_id}`;
  const baseViolMap = new Map();
  if (hasFullDiff && baseData.violations) {
    baseData.violations.forEach(vl => baseViolMap.set(violKey(vl), vl));
  }

  // Type Violations Table
  const violSec = document.getElementById('pass-violations-section');
  const violTbody = document.querySelector('#pass-viol-table tbody');
  const currViolKeys = new Set();
  if (violSec && violTbody) {
    violTbody.innerHTML = '';
    const viols = (passData && passData.violations) || [];
    if (viols.length > 0) {
      violSec.style.display = 'block';
      viols.forEach(vl => {
        const k = violKey(vl);
        currViolKeys.add(k);
        const avId = getViolAvId(vl);
        const vTypes = getViolTypes(vl);
        const isNewViol = hasFullDiff && !baseViolMap.has(k);
        const tr = document.createElement('tr');
        tr.dataset.diffStatus = isNewViol ? 'new' : 'changed';
        if (isNewViol) tr.className = 'diff-row-added';
        const statusBadge = hasFullDiff
          ? (isNewViol ? '<span class="badge diff-badge-new" style="margin-right:0.3rem;">NEW</span>' : '<span class="badge diff-badge-pers" style="margin-right:0.3rem;">PERSISTENT</span>')
          : '';
        const funLabel = vl.fun_name ? `${escapeHtml(vl.fun_name)} (#${vl.fun_id})` : (vl.fun_id >= 0 ? `fun #${vl.fun_id}` : 'global');
        const funLink = vl.fun_id >= 0
          ? `<a href="#" onclick="jumpToCFG(${vl.fun_id}, ${vl.contour_id}, ${passNum}); return false;" style="color:var(--primary); text-decoration:underline;">${funLabel}</a>`
          : funLabel;
        const typesBadges = vTypes.map(t => `<span class="badge badge-type">${escapeHtml(t)}</span>`).join(' ');
        tr.innerHTML = `
          <td>${statusBadge}<span class="badge badge-dead" style="text-decoration:none;">${escapeHtml(vl.kind)}</span></td>
          <td><a href="#" onclick="jumpToDataflow(${avId}, ${passNum}); return false;">#${avId}</a></td>
          <td><code>${escapeHtml(vl.var_name || ('v' + vl.var_id))}</code></td>
          <td>${funLink}</td>
          <td>${vl.contour_id >= 0 ? `<a href="#" onclick="jumpToPassContour(${passNum}, ${vl.contour_id}); return false;">ES#${vl.contour_id}</a>` : '-'}</td>
          <td>${typesBadges || '-'}</td>
        `;
        violTbody.appendChild(tr);
      });
    } else {
      violSec.style.display = 'none';
    }
  }

  // Resolved Violations Table (violations in basePassNum that were eliminated in passNum)
  const resViolSec = document.getElementById('pass-resolved-viol-section');
  const resViolTbody = document.querySelector('#pass-resolved-viol-table tbody');
  if (resViolSec && resViolTbody) {
    resViolTbody.innerHTML = '';
    const resolvedViols = hasFullDiff
      ? (baseData.violations || []).filter(vl => !currViolKeys.has(violKey(vl)))
      : [];
    if (resolvedViols.length > 0) {
      resViolSec.style.display = 'block';
      resolvedViols.forEach(vl => {
        const avId = getViolAvId(vl);
        const vTypes = getViolTypes(vl);
        const tr = document.createElement('tr');
        tr.dataset.diffStatus = 'resolved';
        tr.className = 'diff-row-added';
        const funLabel = vl.fun_name ? `${escapeHtml(vl.fun_name)} (#${vl.fun_id})` : (vl.fun_id >= 0 ? `fun #${vl.fun_id}` : 'global');
        const funLink = vl.fun_id >= 0
          ? `<a href="#" onclick="jumpToCFG(${vl.fun_id}, ${vl.contour_id}, ${passNum}); return false;" style="color:var(--primary); text-decoration:underline;">${funLabel}</a>`
          : funLabel;
        const typesBadges = vTypes.map(t => `<span class="badge badge-type">${escapeHtml(t)}</span>`).join(' ');
        tr.innerHTML = `
          <td><span class="badge diff-badge-resolved" style="margin-right:0.3rem;">RESOLVED</span><span class="badge badge-kind">${escapeHtml(vl.kind)}</span></td>
          <td><a href="#" onclick="jumpToDataflow(${avId}, ${passNum}); return false;">#${avId}</a></td>
          <td><code>${escapeHtml(vl.var_name || ('v' + vl.var_id))}</code></td>
          <td>${funLink}</td>
          <td>${vl.contour_id >= 0 ? `<a href="#" onclick="jumpToPassContour(${passNum}, ${vl.contour_id}); return false;">ES#${vl.contour_id}</a>` : '-'}</td>
          <td>${typesBadges || '-'}</td>
        `;
        resViolTbody.appendChild(tr);
      });
    } else {
      resViolSec.style.display = 'none';
    }
  }

  // EntrySets table with Pass-to-Pass Diff
  const essTbody = document.querySelector('#pass-ess-table tbody');
  const essDiffSummary = document.getElementById('pass-ess-diff-summary');
  essTbody.innerHTML = '';
  const baseEsMap = new Map();
  if (hasFullDiff && baseData.entry_sets) {
    baseData.entry_sets.forEach(es => baseEsMap.set(es.id, es));
  }
  const currEsMap = new Map();
  (passData && passData.entry_sets || []).forEach(es => currEsMap.set(es.id, es));

  let newEsCount = 0, modEsCount = 0;
  (passData && passData.entry_sets || []).forEach(es => {
    const tr = document.createElement('tr');
    let diffStatus = 'unchanged';
    let diffBadgeHtml = '';
    let argsHtml = '';
    let retsHtml = '';

    if (hasFullDiff) {
      const baseEs = baseEsMap.get(es.id);
      if (!baseEs) {
        diffStatus = 'new';
        newEsCount++;
        tr.className = 'diff-row-added';
        diffBadgeHtml = `<span class="badge diff-badge-new" style="margin-right:0.35rem;">NEW SPLIT</span>`;
        const parentEs = baseEsMap.get(es.split_origin) || currEsMap.get(es.split_origin);
        const parentArgMap = new Map((parentEs && parentEs.args || []).map(a => [a.var_name, formatEsArgType(a)]));
        argsHtml = (es.args || []).map(a => {
          const curT = formatEsArgType(a);
          const parT = parentArgMap.get(a.var_name);
          if (parT && parT !== curT) {
            return `${escapeHtml(a.var_name)}: <span class="diff-old">${parT}</span> &rarr; <span class="diff-new">${curT}</span>`;
          }
          return `${escapeHtml(a.var_name)}: ${curT}`;
        }).join(', ');
        retsHtml = (es.rets || []).map(r => formatEsRetType(r)).join(', ');
      } else {
        const baseArgMap = new Map((baseEs.args || []).map(a => [a.var_name, formatEsArgType(a)]));
        let anyChanged = (baseEs.can_raise !== es.can_raise);
        argsHtml = (es.args || []).map(a => {
          const curT = formatEsArgType(a);
          const oldT = baseArgMap.get(a.var_name);
          if (oldT !== undefined && oldT !== curT) {
            anyChanged = true;
            return `${escapeHtml(a.var_name)}: <span class="diff-old">${oldT}</span> &rarr; <span class="diff-new">${curT}</span>`;
          }
          return `${escapeHtml(a.var_name)}: ${curT}`;
        }).join(', ');

        const baseRets = (baseEs.rets || []).map(r => formatEsRetType(r));
        retsHtml = (es.rets || []).map((r, idx) => {
          const curR = formatEsRetType(r);
          const oldR = baseRets[idx];
          if (oldR !== undefined && oldR !== curR) {
            anyChanged = true;
            return `<span class="diff-old">${oldR}</span> &rarr; <span class="diff-new">${curR}</span>`;
          }
          return curR;
        }).join(', ');

        if (anyChanged) {
          diffStatus = 'changed';
          modEsCount++;
          tr.className = 'diff-row-changed';
          diffBadgeHtml = `<span class="badge diff-badge-mod" style="margin-right:0.35rem;">MODIFIED</span>`;
        }
      }
    } else {
      argsHtml = (es.args || []).map(a => `${escapeHtml(a.var_name)}: ${formatEsArgType(a)}`).join(', ');
      retsHtml = (es.rets || []).map(r => formatEsRetType(r)).join(', ');
    }

    tr.dataset.diffStatus = diffStatus;
    tr.innerHTML = `
      <td>${es.id}</td>
      <td><a href="#" onclick="jumpToCFG(${es.fun_id}, ${es.id}, ${passNum}); return false;" style="color:var(--primary); font-weight:600; text-decoration:underline;">${escapeHtml(es.fun_name)}</a></td>
      <td>${argsHtml || '-'}</td>
      <td>${retsHtml || '-'}</td>
      <td>${es.can_raise ? '<span class="badge badge-dead">RAISES</span>' : '<span class="badge badge-live">SAFE</span>'}</td>
      <td>${diffBadgeHtml}${es.split_origin >= 0 ? `Split from <a href="#" onclick="jumpToPassContour(${passNum}, ${es.split_origin}); return false;" style="color:var(--primary); text-decoration:underline;">ES#${es.split_origin}</a>` : 'Original'} <button class="btn-sm" style="padding:0.1rem 0.35rem; font-size:0.7rem; margin-left:0.25rem;" onclick="jumpToCFG(${es.fun_id}, ${es.id}, ${passNum})">🔀 CFG</button></td>
    `;
    essTbody.appendChild(tr);
  });
  if (essDiffSummary) {
    essDiffSummary.textContent = hasFullDiff
      ? `(vs Pass ${basePassNum}: +${newEsCount} new splits, ${modEsCount} modified)`
      : '';
  }

  // CreationSets table with Pass-to-Pass Diff
  const cssTbody = document.querySelector('#pass-css-table tbody');
  const cssDiffSummary = document.getElementById('pass-css-diff-summary');
  cssTbody.innerHTML = '';
  const baseCsMap = new Map();
  if (hasFullDiff && baseData.creation_sets) {
    baseData.creation_sets.forEach(cs => baseCsMap.set(cs.id, cs));
  }
  const currCsMap = new Map();
  (passData && passData.creation_sets || []).forEach(cs => currCsMap.set(cs.id, cs));

  let newCsCount = 0, modCsCount = 0;
  (passData && passData.creation_sets || []).forEach(cs => {
    const tr = document.createElement('tr');
    let diffStatus = 'unchanged';
    let diffBadgeHtml = '';
    let fldsHtml = '';
    const arityStr = cs.static_arity >= 0 ? String(cs.static_arity) : (cs.no_static_arity ? 'Dynamic' : '-');
    let arityHtml = arityStr;

    if (hasFullDiff) {
      const baseCs = baseCsMap.get(cs.id);
      if (!baseCs) {
        diffStatus = 'new';
        newCsCount++;
        tr.className = 'diff-row-added';
        diffBadgeHtml = `<span class="badge diff-badge-new" style="margin-right:0.35rem;">NEW SPLIT</span>`;
        const parentCs = baseCsMap.get(cs.split_origin) || currCsMap.get(cs.split_origin);
        const parentFldMap = new Map((parentCs && parentCs.fields || []).map(f => [f.name || ('f' + f.index), formatCsFieldType(f)]));
        fldsHtml = (cs.fields || []).map(f => {
          const fname = f.name || ('f' + f.index);
          const curF = formatCsFieldType(f);
          const parF = parentFldMap.get(fname);
          if (parF && parF !== curF) {
            return `${escapeHtml(fname)}: <span class="diff-old">${parF}</span> &rarr; <span class="diff-new">${curF}</span>`;
          }
          return `${escapeHtml(fname)}: ${curF}`;
        }).join(', ');
      } else {
        const baseArityStr = baseCs.static_arity >= 0 ? String(baseCs.static_arity) : (baseCs.no_static_arity ? 'Dynamic' : '-');
        let anyChanged = false;
        if (baseArityStr !== arityStr) {
          anyChanged = true;
          arityHtml = `<span class="diff-old">${baseArityStr}</span> &rarr; <span class="diff-new">${arityStr}</span>`;
        }
        const baseFldMap = new Map((baseCs.fields || []).map(f => [f.name || ('f' + f.index), formatCsFieldType(f)]));
        fldsHtml = (cs.fields || []).map(f => {
          const fname = f.name || ('f' + f.index);
          const curF = formatCsFieldType(f);
          const oldF = baseFldMap.get(fname);
          if (oldF !== undefined && oldF !== curF) {
            anyChanged = true;
            return `${escapeHtml(fname)}: <span class="diff-old">${oldF}</span> &rarr; <span class="diff-new">${curF}</span>`;
          } else if (oldF === undefined && (baseCs.fields || []).length > 0) {
            anyChanged = true;
            return `${escapeHtml(fname)}: <span class="diff-new">${curF} (new)</span>`;
          }
          return `${escapeHtml(fname)}: ${curF}`;
        }).join(', ');

        if (anyChanged) {
          diffStatus = 'changed';
          modCsCount++;
          tr.className = 'diff-row-changed';
          diffBadgeHtml = `<span class="badge diff-badge-mod" style="margin-right:0.35rem;">MODIFIED</span>`;
        }
      }
    } else {
      fldsHtml = (cs.fields || []).map(f => `${escapeHtml(f.name || 'f'+f.index)}: ${formatCsFieldType(f)}`).join(', ');
    }

    tr.dataset.diffStatus = diffStatus;
    tr.innerHTML = `
      <td>${cs.id}</td>
      <td><strong>${escapeHtml(cs.sym)}</strong></td>
      <td>${arityHtml}</td>
      <td>${fldsHtml || '-'}</td>
      <td>${diffBadgeHtml}${cs.split_origin >= 0 ? `Split from <a href="#" onclick="jumpToPassContour(${passNum}, null, ${cs.split_origin}); return false;" style="color:#c084fc; text-decoration:underline;">CS#${cs.split_origin}</a>` : 'Original'}</td>
    `;
    cssTbody.appendChild(tr);
  });
  if (cssDiffSummary) {
    cssDiffSummary.textContent = hasFullDiff
      ? `(vs Pass ${basePassNum}: +${newCsCount} new splits, ${modCsCount} modified)`
      : '';
  }

  // AVars with Converged Type Changes in this Pass
  const avarDiffSec = document.getElementById('pass-avar-diff-section');
  const avarDiffTbody = document.querySelector('#pass-avar-diff-table tbody');
  const avarDiffSummary = document.getElementById('pass-avar-diff-summary');
  if (avarDiffSec && avarDiffTbody) {
    avarDiffTbody.innerHTML = '';
    if (hasFullDiff && passData.avars && baseData._avarById) {
      const funMap = new Map((DATA.phase2_cfg.functions || []).map(f => [f.id, f.name]));
      const changedAvars = [];
      passData.avars.forEach(av => {
        const newT = formatAvarTypeSet(av);
        const baseAv = baseData._avarById.get(av.id);
        if (!baseAv) {
          if (newT !== 'void') {
            changedAvars.push({ av, kind: 'NEW CONTOUR AVAR', badgeCls: 'diff-badge-new', oldT: null, newT, priority: 2 });
          }
        } else {
          const oldT = formatAvarTypeSet(baseAv);
          if (oldT !== newT) {
            const oldCount = (baseAv.type || []).length;
            const newCount = (av.type || []).length;
            let kind = 'TYPE CHANGED';
            let badgeCls = 'diff-badge-mod';
            if (oldCount > newCount && newCount > 0) {
              kind = 'NARROWED';
              badgeCls = 'diff-badge-resolved';
            } else if (newCount > oldCount) {
              kind = 'WIDENED';
              badgeCls = 'diff-badge-mod';
            }
            changedAvars.push({ av, kind, badgeCls, oldT, newT, priority: 1 });
          }
        }
      });

      changedAvars.sort((a, b) => a.priority - b.priority || a.av.id - b.av.id);
      if (changedAvars.length > 0) {
        avarDiffSec.style.display = 'block';
        const shown = changedAvars.slice(0, 300);
        if (avarDiffSummary) {
          avarDiffSummary.textContent = changedAvars.length > 300
            ? `(vs Pass ${basePassNum}: showing 300 of ${changedAvars.length} changed AVars)`
            : `(vs Pass ${basePassNum}: ${changedAvars.length} changed AVars)`;
        }
        const frag = document.createDocumentFragment();
        shown.forEach(item => {
          const av = item.av;
          const tr = document.createElement('tr');
          tr.dataset.diffStatus = 'changed';
          tr.className = item.priority === 1 ? 'diff-row-changed' : 'diff-row-added';
          const funName = av.fun_name || funMap.get(av.fun_id) || (av.fun_id >= 0 ? `fun #${av.fun_id}` : 'global');
          const transitionHtml = item.oldT !== null
            ? `<span class="diff-old">${escapeHtml(item.oldT)}</span> &rarr; <span class="diff-new">${escapeHtml(item.newT)}</span>`
            : `<span class="diff-new">${escapeHtml(item.newT)}</span>`;
          tr.innerHTML = `
            <td><a href="#" onclick="jumpToDataflow(${av.id}, ${passNum}); return false;"><strong>#${av.id}</strong></a></td>
            <td><code>${escapeHtml(av.var_name || ('v' + av.var_id))}</code></td>
            <td>${av.fun_id >= 0 ? `<a href="#" onclick="jumpToCFG(${av.fun_id}, ${av.contour_id}, ${passNum}); return false;" style="color:var(--primary); text-decoration:underline;">${escapeHtml(funName)}</a>` : escapeHtml(funName)}</td>
            <td>${av.contour_id >= 0 ? `<a href="#" onclick="jumpToPassContour(${passNum}, ${av.contour_id}); return false;">ES#${av.contour_id}</a>` : '-'}</td>
            <td><span class="badge ${item.badgeCls}">${item.kind}</span></td>
            <td><code>${transitionHtml}</code></td>
          `;
          frag.appendChild(tr);
        });
        avarDiffTbody.appendChild(frag);
      } else {
        avarDiffSec.style.display = 'none';
      }
    } else {
      avarDiffSec.style.display = 'none';
    }
  }

  applyPassFilter();
}

document.getElementById('btn-prev-pass').addEventListener('click', () => {
  if (currentPassIndex > 0) {
    currentPassIndex--;
    displayPass(currentPassIndex + 1);
  }
});

document.getElementById('btn-next-pass').addEventListener('click', () => {
  if (currentPassIndex < totalPasses - 1) {
    currentPassIndex++;
    displayPass(currentPassIndex + 1);
  }
});

passSlider.addEventListener('input', () => {
  currentPassIndex = parseInt(passSlider.value) - 1;
  displayPass(currentPassIndex + 1);
});

if (totalPasses > 0) {
  currentPassIndex = totalPasses - 1;
  displayPass(totalPasses);
}

// Tab: Dataflow Tracer
let currentDfPass = totalPasses;
let currentDfData = null;

function renderDataflowTable(filterText, highlightAvId) {
  const tbody = document.querySelector('#df-table tbody');
  const countEl = document.getElementById('df-count');
  if (!tbody) return;

  if (!currentDfData || !currentDfData.avars) {
    loadPassData(currentDfPass, (passData) => {
      if (passData && passData.avars) {
        currentDfData = passData;
        renderDataflowTable(filterText, highlightAvId);
      }
    });
    tbody.innerHTML = '<tr><td colspan="7" style="text-align:center; padding:1.5rem; color:var(--text-muted);">Loading AVar data...</td></tr>';
    if (countEl) countEl.textContent = '';
    return;
  }

  const allAvars = currentDfData.avars;
  const searchInput = document.getElementById('df-search');
  const qRaw = filterText !== undefined ? filterText : (searchInput ? searchInput.value : '');
  const q = qRaw.trim();
  const funMap = new Map((DATA.phase2_cfg.functions || []).map(f => [f.id, f.name]));

  let avarsToShow = [];
  if (!q) {
    avarsToShow = allAvars.slice(0, 500);
    if (countEl) {
      countEl.textContent = allAvars.length > 500
        ? `Showing 500 of ${allAvars.length} AVars (type in search box to search all variables)`
        : `Showing all ${allAvars.length} AVars`;
    }
  } else {
    const qLower = q.toLowerCase();
    const cleanQ = q.startsWith('#') ? q.slice(1) : q;
    const qNum = parseInt(cleanQ, 10);
    const isNumeric = !isNaN(qNum) && String(qNum) === cleanQ;

    const scored = [];
    for (let i = 0; i < allAvars.length; i++) {
      const av = allAvars[i];
      let score = 0;
      const vName = (av.var_name || '').toLowerCase();
      const fName = (av.fun_name || funMap.get(av.fun_id) || '').toLowerCase();
      const typeStr = (av.type || []).map(t => t.sym + (t.is_const ? `(${t.val})` : '')).join(' | ').toLowerCase();
      const varIdStr = 'v' + av.var_id;

      if (isNumeric && av.id === qNum) {
        score = 1000;
      } else if (vName === qLower) {
        score = 500;
      } else if (varIdStr === qLower) {
        score = 450;
      } else if (vName.startsWith(qLower)) {
        score = 300;
      } else if (vName.includes(qLower)) {
        score = 200;
      } else if (String(av.id).includes(cleanQ)) {
        score = 150;
      } else if (String(av.var_id).includes(cleanQ)) {
        score = 120;
      } else if (fName === qLower) {
        score = 110;
      } else if (fName.includes(qLower)) {
        score = 90;
      } else if (typeStr.includes(qLower)) {
        score = 70;
      }

      if (score > 0) {
        scored.push({ av, score });
      }
    }

    scored.sort((a, b) => b.score - a.score || a.av.id - b.av.id);
    const totalMatches = scored.length;
    avarsToShow = scored.slice(0, 500).map(s => s.av);
    if (countEl) {
      countEl.textContent = `Found ${totalMatches} matching AVar(s) for "${q}" (showing ${avarsToShow.length})`;
    }
  }

  tbody.innerHTML = '';
  if (avarsToShow.length === 0) {
    tbody.innerHTML = `<tr><td colspan="7" style="text-align:center; padding:1.5rem; color:var(--text-muted);">No AVars found matching "${escapeHtml(q)}"</td></tr>`;
    return;
  }

  const fragment = document.createDocumentFragment();
  avarsToShow.forEach(av => {
    const tr = document.createElement('tr');
    if (highlightAvId && av.id === highlightAvId) {
      tr.style.background = 'rgba(16, 185, 129, 0.15)';
      tr.style.outline = '2px solid var(--accent-green)';
    }
    const typeStr = (av.type || []).map(t => t.sym + (t.is_const ? `(${t.val})` : '')).join(' | ');
    const funName = av.fun_name || funMap.get(av.fun_id) || ('fun #' + av.fun_id);
    tr.innerHTML = `
      <td><strong>#${av.id}</strong></td>
      <td><a href="#" onclick="jumpToCFG(${av.fun_id}, ${av.contour_id}, ${currentDfPass}); return false;" style="color:var(--accent-green); font-weight:600; text-decoration:underline;" title="View in CFG">${escapeHtml(av.var_name || 'v'+av.var_id)}</a></td>
      <td><a href="#" onclick="jumpToCFG(${av.fun_id}, ${av.contour_id}, ${currentDfPass}); return false;" style="color:var(--text-main); text-decoration:underline;">${escapeHtml(funName)}</a></td>
      <td><a href="#" onclick="jumpToPassContour(${currentDfPass}, ${av.contour_id}); return false;" style="color:var(--primary); text-decoration:underline;">contour #${av.contour_id}</a></td>
      <td><code>${escapeHtml(typeStr || 'void')}</code></td>
      <td>${(av.backward || []).map(id => `<a href="#" onclick="jumpToDataflow(${id}, ${currentDfPass}); return false;">#${id}</a>`).join(', ') || '-'}</td>
      <td>${(av.forward || []).map(id => `<a href="#" onclick="jumpToDataflow(${id}, ${currentDfPass}); return false;">#${id}</a>`).join(', ') || '-'}</td>
    `;
    fragment.appendChild(tr);
  });
  tbody.appendChild(fragment);
}

function initDataflow() {
  const pSel = document.getElementById('df-pass-select');
  if (pSel) {
    pSel.innerHTML = '';
    for (let p = 1; p <= totalPasses; p++) {
      const opt = document.createElement('option');
      opt.value = p;
      opt.textContent = p === totalPasses ? `Pass ${p} (Converged)` : `Pass ${p}`;
      if (p === totalPasses) opt.selected = true;
      pSel.appendChild(opt);
    }
    pSel.addEventListener('change', () => {
      currentDfPass = parseInt(pSel.value);
      loadPassData(currentDfPass, (passData) => {
        currentDfData = passData;
        renderDataflowTable();
      });
    });
  }

  loadPassData(totalPasses, (lastPassData) => {
    currentDfData = lastPassData;
    renderDataflowTable();
  });
}
initDataflow();

const dfSearchInput = document.getElementById('df-search');
if (dfSearchInput) {
  dfSearchInput.addEventListener('input', () => {
    renderDataflowTable();
  });
}

// Tab: Transfer Functions
let allTransferRows = [];

function buildTransferFunctionCatalog() {
  allTransferRows = [];
  (DATA.phase2_cfg.functions || []).forEach(f => {
    (f.pnodes || []).forEach(pn => {
      if (pn.prim) {
        const searchStr = `${pn.id} ${f.name} ${pn.prim} ${(pn.rvals||[]).map(v => (v.name||'') + ' ' + (v.type||'')).join(' ')} ${(pn.lvals||[]).map(v => (v.name||'') + ' ' + (v.type||'')).join(' ')}`.toLowerCase();
        allTransferRows.push({ f, pn, searchStr });
      }
    });
  });
}

function renderTransferFunctions(filterQuery) {
  const tbody = document.querySelector('#tf-table tbody');
  const countEl = document.getElementById('tf-count');
  if (!tbody) return;
  const q = (filterQuery || '').trim().toLowerCase();
  const matched = q ? allTransferRows.filter(item => item.searchStr.includes(q)) : allTransferRows;
  const shown = matched.slice(0, 500);
  if (countEl) {
    countEl.textContent = matched.length > 500
      ? `Showing 500 of ${matched.length} primitive operations`
      : `Showing ${matched.length} primitive operations`;
  }
  tbody.innerHTML = '';
  const frag = document.createDocumentFragment();
  shown.forEach(({ f, pn }) => {
    const tr = document.createElement('tr');
    let statusBadge = '<span class="badge badge-live">ACTIVE</span>';
    if (!pn.live && !pn.fa_live) statusBadge = '<span class="badge" style="background:#475569; color:#cbd5e1; font-size:0.65rem;">DEAD</span>';
    else if (!pn.live && pn.fa_live) statusBadge = '<span class="badge" style="background:#334155; color:#94a3b8; font-size:0.65rem;">FOLDED</span>';
    const rvalsHtml = (pn.rvals || []).map(v =>
      `<span class="badge badge-var" onclick="jumpToCFG(${f.id}); showVarInspector(${f.id}, ${v.id});">${escapeHtml(v.name || 'v'+v.id)}</span> <span class="badge badge-type">${escapeHtml(v.type || 'void')}</span>`
    ).join(', ') || '-';
    const lvalsHtml = (pn.lvals || []).map(v =>
      `<span class="badge badge-var" onclick="jumpToCFG(${f.id}); showVarInspector(${f.id}, ${v.id});">${escapeHtml(v.name || 'v'+v.id)}</span> <span class="badge badge-type">${escapeHtml(v.type || 'void')}</span>`
    ).join(', ') || '-';
    tr.innerHTML = `
      <td><a href="#" onclick="jumpToCFGAndNode(${f.id}, ${pn.id}); return false;" style="color:var(--primary); font-weight:600; text-decoration:underline;">#${pn.id}</a></td>
      <td><a href="#" onclick="jumpToCFG(${f.id}); return false;" style="color:var(--text-main); text-decoration:underline;">${escapeHtml(f.name)}</a></td>
      <td><span class="badge badge-prim">${escapeHtml(pn.prim)}</span></td>
      <td>${rvalsHtml}</td>
      <td>${lvalsHtml}</td>
      <td>${statusBadge}</td>
    `;
    frag.appendChild(tr);
  });
  tbody.appendChild(frag);
}

buildTransferFunctionCatalog();
renderTransferFunctions('');

const tfSearchInput = document.getElementById('tf-search');
if (tfSearchInput) {
  tfSearchInput.addEventListener('input', (e) => {
    renderTransferFunctions(e.target.value);
  });
}

// Tab: Clones & Memory Layouts
function initMemoryLayouts() {
  // 1. Function Clones & Specializations Table
  const clonesTbody = document.querySelector('#clones-table tbody');
  if (clonesTbody) {
    clonesTbody.innerHTML = '';
    const fclones = DATA.phase4_cloning.function_clones || [];
    if (fclones.length === 0) {
      clonesTbody.innerHTML = '<tr><td colspan="8" style="text-align:center; color:var(--text-muted); padding:1rem;">No function clones recorded.</td></tr>';
    } else {
      fclones.forEach(fc => {
        const tr = document.createElement('tr');
        const formalsStr = (fc.formals || []).map(f => `${escapeHtml(f.name)}: <code>${escapeHtml(f.type)}</code>`).join(', ') || '()';
        const retsStr = (fc.returns || []).map(r => `<code>${escapeHtml(r.type)}</code>`).join(', ') || 'void';
        const loc = fc.line > 0
          ? `<a href="#" onclick="jumpToSource(${fc.line}); return false;">${escapeHtml(fc.filename ? `${fc.filename}:${fc.line}` : `line ${fc.line}`)}</a>`
          : (fc.filename ? escapeHtml(fc.filename) : '-');
        const kindBadge = fc.is_clone 
          ? `<span class="badge badge-kind">CLONE of #${fc.wraps_id}</span>` 
          : `<span class="badge badge-live">ORIGINAL</span>`;
        const essBadges = (fc.entry_sets || []).map(cid => `<span class="badge badge-contour" onclick="jumpToPassContour(null, ${cid})">ES#${cid}</span>`).join(' ') || '-';
        const cgBtn = (fc.cg_start_line && fc.cg_start_line > 0)
          ? ` <button class="btn-sm" style="padding:0.15rem 0.4rem; font-size:0.75rem;" onclick="jumpToCodegen(${fc.cg_start_line}, ${fc.cg_end_line}, ${fc.id})" title="${escapeHtml(fc.cg_name)} (L${fc.cg_start_line}–${fc.cg_end_line})">⚙️ ${escapeHtml((cgData && cgData.language) || 'Code')}</button>`
          : '';

        tr.innerHTML = `
          <td>${fc.id}</td>
          <td><a href="#" onclick="jumpToCFG(${fc.id}); return false;" style="color:var(--primary); font-weight:600; text-decoration:underline;">${escapeHtml(fc.name)}</a></td>
          <td>${kindBadge}</td>
          <td style="color:var(--text-muted); font-size:0.8rem;">${loc}</td>
          <td>${essBadges}</td>
          <td>${formalsStr}</td>
          <td>${retsStr}</td>
          <td style="white-space:nowrap;"><button class="btn-sm" style="padding:0.15rem 0.4rem; font-size:0.75rem;" onclick="jumpToCFG(${fc.id})">🔀 CFG</button>${cgBtn}</td>
        `;
        clonesTbody.appendChild(tr);
      });
    }
  }

  // 2. Concrete Types Memory Layouts
  const container = document.getElementById('memory-layouts-container');
  if (container) {
    container.innerHTML = '';
    const types = DATA.phase4_cloning.concrete_types || [];
    if (types.length === 0) {
      container.innerHTML = '<p style="color:var(--text-muted); padding:1rem;">No composite concrete records allocated.</p>';
    } else {
      types.forEach(t => {
        const card = document.createElement('div');
        card.className = 'card memory-type-card';
        card.style.marginBottom = '1.25rem';
        let memBar = '<div class="memory-bar">';
        let curOff = 0;
        (t.fields || []).forEach(f => {
          if (f.offset > curOff) {
            const padBytes = f.offset - curOff;
            memBar += `<div class="memory-chunk padding" style="flex:${Math.max(1, padBytes)};" title="Alignment Padding (${padBytes}B @ offset ${curOff})">pad<br><span style="font-size:0.65rem;">${padBytes}B</span></div>`;
          }
          memBar += `<div class="memory-chunk field" style="flex:${Math.max(1, f.size)};" title="${escapeHtml(f.name)}: ${escapeHtml(f.type)} (${f.size}B @ offset ${f.offset})">${escapeHtml(f.name)}<br><span style="font-size:0.65rem;">${f.size}B</span></div>`;
          curOff = f.offset + f.size;
        });
        if (t.size > curOff && (t.fields || []).length > 0) {
          const tailPad = t.size - curOff;
          memBar += `<div class="memory-chunk padding" style="flex:${Math.max(1, tailPad)};" title="Tail Alignment Padding (${tailPad}B @ offset ${curOff})">pad<br><span style="font-size:0.65rem;">${tailPad}B</span></div>`;
        } else if ((t.fields || []).length === 0) {
          memBar += `<div class="memory-chunk padding" style="flex:1;">${t.element_type ? `Container Element: ${escapeHtml(t.element_type)}` : 'No live instance fields'}</div>`;
        }
        memBar += '</div>';

        const elemBadge = t.element_type ? ` <span class="badge badge-type">elem: ${escapeHtml(t.element_type)}</span>` : '';
        const elidedBadge = t.elided_slots > 0 ? ` <span class="badge badge-live">${t.elided_slots} elided slot(s)</span>` : '';

        card.innerHTML = `
          <div style="display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap; gap:0.5rem;">
            <h4>${escapeHtml(t.name)} (id: ${t.id})${elemBadge}${elidedBadge}</h4>
            <span class="badge badge-kind">${escapeHtml(t.type_kind)} | Total: ${t.size} bytes | Align: ${t.alignment}</span>
          </div>
          ${memBar}
          <div style="margin-top:0.75rem; font-size:0.8rem; color:var(--text-muted);">
            Fields: ${(t.fields || []).map(f => `<strong>${escapeHtml(f.name)}</strong> (<code>${escapeHtml(f.type)}</code>, ${f.size}B @ offset ${f.offset})`).join(', ') || 'none'}
          </div>
        `;
        container.appendChild(card);
      });
    }
  }

  // 3. Search / Filter Input
  const searchInput = document.getElementById('memory-search');
  if (searchInput) {
    searchInput.addEventListener('input', () => {
      const q = searchInput.value.trim().toLowerCase();
      document.querySelectorAll('#clones-table tbody tr').forEach(tr => {
        tr.style.display = (!q || tr.textContent.toLowerCase().includes(q)) ? '' : 'none';
      });
      document.querySelectorAll('.memory-type-card').forEach(card => {
        card.style.display = (!q || card.textContent.toLowerCase().includes(q)) ? '' : 'none';
      });
    });
  }
}
initMemoryLayouts();

)JS", fp);
  fprintf(fp, "</script>\n</body>\n</html>\n");

  fclose(fp);
    fprintf(stderr, "wrote %s\n", html_fn.c_str());
    if (ifa_open_html) {
      open_html_in_browser(html_fn.c_str());
    }
  }
  // Both sidecars are per compilation: `ifa a.v b.v` must not carry a's
  // passes or inline events into b's explorer.
  g_recorded_passes.clear();
  inline_events_reset();
}
