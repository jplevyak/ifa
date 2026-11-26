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

// The explorer's page assets live in common/explorer/ as ordinary CSS,
// HTML and JS files, so they can be edited, linted and diffed as what they
// are. The Makefile wraps each one in a raw string literal (<file>.inc).
static const char kExplorerCss[] =
#include "explorer/explorer.css.inc"
    ;
static const char kExplorerHtml[] =
#include "explorer/explorer.html.inc"
    ;
static const char kExplorerJs[] =
#include "explorer/explorer.js.inc"
    ;

int ifa_dump_html = 0;
int ifa_open_html = 0;
int ifa_html_max_size_mb = 100;
int ifa_dump_json = 0;

// FA's own stage names (analysis/fa.cc, declared in fa_internal.h), so the
// two cannot drift.
extern const char *kStageName[FA::kNumFAPassStages];
#define fa_stage_names kStageName

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

// AVars dominate the recording (on chull ~17k per pass, ~87% of the pass
// data when every pass stored them whole). What an AVar IS -- its var,
// contour and function -- does not change from pass to pass, so it is kept
// once (AVarInfo); what the analysis derives -- its type and its flow
// edges -- is kept as ids (AVarState), and a pass stores only the AVars
// whose state changed since the previous pass (RecordedPass::av_changed),
// plus the ids that disappeared (av_gone). The current state of every AVar
// is g_av_state, so the final pass can always be written whole.
struct AVarInfo {
  AVar *av;
  Var *var;
  Fun *fun;
  EntrySet *es;
  int var_id;
  int fun_id;
  int contour_id;
};

struct AVarState {
  std::vector<int> cs;   // the type, as CreationSet ids (see g_cs_seen)
  std::vector<int> fwd;  // forward flow edges, as AVar ids
  std::vector<int> bwd;  // backward flow edges, as AVar ids
  bool operator==(const AVarState &o) const { return cs == o.cs && fwd == o.fwd && bwd == o.bwd; }
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
  // Not reported by FA: built here from the splits of a stage that has none
  // of FA's own events (a splitter that records no FAPassEvent).
  bool synthesized = false;
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
  // viol_kind was not handed over by the splitter but guessed: the first
  // violation on the trigger AVar or in the split contour. Shown as such.
  bool viol_inferred = false;
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
  // Only the EntrySets / CreationSets whose record changed this pass, and
  // the ids that disappeared; g_es_cur / g_cs_cur hold the current state.
  std::vector<RecordedES> entry_sets;
  std::vector<RecordedCS> creation_sets;
  std::vector<int> es_gone;
  std::vector<int> cs_gone;
  int ess_count = 0;
  int css_count = 0;
  std::vector<RecordedViol> violations;
  std::vector<std::pair<int, AVarState>> av_changed;
  std::vector<int> av_gone;
  // Over the --html-max-size budget: only the summary (counts, stage
  // events, splits) was kept. Detail resumes with a keyframe at the end.
  bool summary_only = false;
};

static std::vector<RecordedPass> g_recorded_passes;
static std::unordered_map<int, AVarInfo> g_av_info;
static std::unordered_map<int, AVarState> g_av_state;
static std::unordered_map<int, CreationSet *> g_cs_seen;  // every CS an AVar type named
static std::map<int, RecordedES> g_es_cur;
static std::map<int, RecordedCS> g_cs_cur;
static std::unordered_map<int, std::string> g_es_fp, g_cs_fp;  // fingerprints of g_*_cur
static size_t g_recorded_bytes = 0;                        // estimate, for the budget

static size_t html_budget_bytes() { return (size_t)std::max(1, ifa_html_max_size_mb) * 1024ULL * 1024ULL; }

static void reset_recording() {
  g_recorded_passes.clear();
  g_av_info.clear();
  g_av_state.clear();
  g_cs_seen.clear();
  g_es_cur.clear();
  g_cs_cur.clear();
  g_es_fp.clear();
  g_cs_fp.clear();
  g_recorded_bytes = 0;
}

// What a record says about the analysis, for "did it change since the last
// pass". Pointers and names are fixed per id, so ids and types suffice.
static std::string es_fingerprint(const RecordedES &r) {
  std::ostringstream o;
  o << r.can_raise << r.returns << r.raises << '|' << r.split_origin << '|' << r.fun_id;
  for (const auto &a : r.args) o << "|a" << a.pos << ':' << a.av_id << ':' << a.type_str;
  for (const auto &x : r.rets) o << "|r" << x.av_id << ':' << x.type_str;
  for (const auto &e : r.out_edges) o << "|e" << e.id << ':' << e.to_es << ':' << e.pnode_id;
  return o.str();
}

static std::string cs_fingerprint(const RecordedCS &r) {
  std::ostringstream o;
  o << r.static_arity << r.no_static_arity << '|' << r.split_origin;
  for (const auto &f : r.fields) o << "|f" << f.index << ':' << f.av_id << ':' << f.type_str;
  return o.str();
}

// Keep `rec` in the pass only if it differs from the current record of its
// id; update the current state either way.
template <class R>
static void record_if_changed(R &&rec, std::string fp, std::map<int, std::remove_reference_t<R>> &cur,
                              std::unordered_map<int, std::string> &fps, std::vector<std::remove_reference_t<R>> &out,
                              bool keep) {
  int id = rec.id;
  auto it = fps.find(id);
  if (it != fps.end() && it->second == fp) return;
  if (keep) {
    g_recorded_bytes += 64 + fp.size();
    out.push_back(rec);
  }
  fps[id] = std::move(fp);
  cur[id] = std::move(rec);
}

template <class R>
static void drop_gone(const std::unordered_set<int> &present, std::map<int, R> &cur,
                      std::unordered_map<int, std::string> &fps, std::vector<int> &gone, bool keep) {
  for (auto it = cur.begin(); it != cur.end();) {
    if (present.count(it->first)) { ++it; continue; }
    if (keep) gone.push_back(it->first);
    fps.erase(it->first);
    it = cur.erase(it);
  }
}

static void check_env_config() {
  static bool env_checked = false;
  if (!env_checked) {
    env_checked = true;
    if (cchar *v = getenv("PYC_HTML_MAX_SIZE")) ifa_html_max_size_mb = atoi(v);
    if (cchar *v = getenv("IFA_HTML_MAX_SIZE")) ifa_html_max_size_mb = atoi(v);
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
  // The budget is spent while recording, not just when writing: past it a
  // pass keeps only its summary (counts, stage events, splits). The current
  // state is still tracked, so the final pass is written whole.
  if (g_recorded_bytes > html_budget_bytes()) rp.summary_only = true;
  std::unordered_set<int> es_present, cs_present;
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
    es_present.insert(res.id);
    std::string fp = es_fingerprint(res);
    record_if_changed(std::move(res), std::move(fp), g_es_cur, g_es_fp, rp.entry_sets, !rp.summary_only);
  }
  drop_gone(es_present, g_es_cur, g_es_fp, rp.es_gone, !rp.summary_only);
  rp.ess_count = (int)g_es_cur.size();

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
    cs_present.insert(rcs.id);
    std::string fp = cs_fingerprint(rcs);
    record_if_changed(std::move(rcs), std::move(fp), g_cs_cur, g_cs_fp, rp.creation_sets, !rp.summary_only);
  }
  drop_gone(cs_present, g_cs_cur, g_cs_fp, rp.cs_gone, !rp.summary_only);
  rp.css_count = (int)g_cs_cur.size();

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

  // AVars: update the current state, and keep only what changed.
  std::unordered_set<int> present;
  for (Fun *f : fa->funs) {
    if (!f) continue;
    for (Var *v : f->fa_all_Vars) {
      if (!v) continue;
      for (int vi = 0; vi < v->avars.n; vi++) {
        if (!v->avars[vi].key || !v->avars[vi].value) continue;
        AVar *av = v->avars[vi].value;
        if (!av || !(av->out || av->in || av->forward.n || av->backward.n)) continue;
        // The same AVar is reachable from several Var slots (on chull ~7
        // visits per AVar); the old recorder wrote one entry per visit.
        if (!present.insert(av->id).second) continue;
        EntrySet *es_contour = av->contour_is_entry_set ? (EntrySet *)av->contour : nullptr;
        g_av_info[av->id] = AVarInfo{av, v, f, es_contour, v->id, f->id, es_contour ? es_contour->id : -1};
        AVarState st;
        if (av->out) {
          for (CreationSet *cs : *av->out) {
            if (!cs || !cs->sym) continue;
            st.cs.push_back(cs->id);
            g_cs_seen.emplace(cs->id, cs);
          }
        }
        for (AVar *x : av->forward) if (x) st.fwd.push_back(x->id);
        for (AVar *x : av->backward) if (x) st.bwd.push_back(x->id);
        auto it = g_av_state.find(av->id);
        if (it != g_av_state.end() && it->second == st) continue;
        if (!rp.summary_only) {
          g_recorded_bytes += 24 + 6 * (st.cs.size() + st.fwd.size() + st.bwd.size());
          rp.av_changed.emplace_back(av->id, st);
        }
        g_av_state[av->id] = std::move(st);
      }
    }
  }
  for (auto it = g_av_state.begin(); it != g_av_state.end();) {
    if (present.count(it->first)) { ++it; continue; }
    if (!rp.summary_only) rp.av_gone.push_back(it->first);
    it = g_av_state.erase(it);
  }
  std::sort(rp.av_changed.begin(), rp.av_changed.end(),
            [](const std::pair<int, AVarState> &a, const std::pair<int, AVarState> &b) { return a.first < b.first; });
  std::sort(rp.av_gone.begin(), rp.av_gone.end());

  g_recorded_passes.push_back(std::move(rp));
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
      ev.ess_before = rp.ess_count;
      ev.ess_after = rp.ess_count;
      ev.css_before = rp.css_count;
      ev.css_after = rp.css_count;
      ev.viol_before = (int)rp.violations.size();
      ev.viol_after = (int)rp.violations.size();
      ev.synthesized = true;
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
  rs.viol_inferred = matched_viol && !viol;
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
      rs.reason = "Type violation" + (rs.viol_kind.empty() ? "" : (" (" + rs.viol_kind + (rs.viol_inferred ? ", inferred" : "") + ")")) + " traced to " +
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
  } else if (rs.sub_kind == "BLOCKING_ES") {
    rs.reason = "Shared contour ES#" + std::to_string(es->id) + " (" + rs.target_name +
                ") blocked creation-point separation of demanded CreationSet CS#" + std::to_string(rs.demand_cs_id) +
                " (" + rs.demand_cs_sym + "); splitting in-edges into " +
                std::to_string(rs.products.size() + (rs.stayed_count ? 1 : 0)) + " groups by reachable creation points";
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

static bool is_avar_info_live(const AVarInfo &ai) {
  Fun *cur_fun = (ai.es && ai.es->fun) ? ai.es->fun : ai.fun;
  bool fun_live = (cur_fun && cur_fun->live) || (ai.fun && ai.fun->live);
  if (!fun_live) return false;
  Var *cur_var = (ai.av && ai.av->var) ? ai.av->var : ai.var;
  return (cur_var && cur_var->live) || (ai.var && ai.var->live);
}

// Liveness is a property of the program after optimization, so it is
// decided once, at write time, for every id the recording ever saw.
struct PassOutCtx {
  std::unordered_set<int> live_av;  // AVar ids
  std::unordered_set<int> live_es;  // EntrySet ids
  std::unordered_set<int> live_cs;  // CreationSet ids
};

// One pass, in the explorer's delta encoding (see dump_html's "encoding"):
// `entry_sets` / `creation_sets` / `avars` hold what changed since the
// previous pass, `*_gone` the ids that went away. A keyframe pass is
// written from the current state instead, and replaces everything.
static void format_recorded_pass(const RecordedPass &rp, const PassOutCtx &ctx, bool keyframe, int ess_live,
                                 int css_live, std::string &summary_json, std::string &full_json) {
  std::vector<const RecordedES *> es_list;
  std::vector<const RecordedCS *> cs_list;
  std::vector<std::pair<int, const AVarState *>> av_list;
  if (keyframe) {
    for (const auto &kv : g_es_cur) if (ctx.live_es.count(kv.first)) es_list.push_back(&kv.second);
    for (const auto &kv : g_cs_cur) if (ctx.live_cs.count(kv.first)) cs_list.push_back(&kv.second);
    for (const auto &kv : g_av_state) if (ctx.live_av.count(kv.first)) av_list.emplace_back(kv.first, &kv.second);
    std::sort(av_list.begin(), av_list.end(),
              [](const std::pair<int, const AVarState *> &a, const std::pair<int, const AVarState *> &b) {
                return a.first < b.first;
              });
  } else {
    for (const RecordedES &r : rp.entry_sets) if (ctx.live_es.count(r.id)) es_list.push_back(&r);
    for (const RecordedCS &r : rp.creation_sets) if (ctx.live_cs.count(r.id)) cs_list.push_back(&r);
    for (const auto &kv : rp.av_changed) if (ctx.live_av.count(kv.first)) av_list.emplace_back(kv.first, &kv.second);
  }
  auto write_gone = [](std::ostringstream &os, const std::vector<int> &gone, const std::unordered_set<int> &live) {
    os << "[";
    bool first = true;
    for (int id : gone) {
      if (!live.count(id)) continue;
      if (!first) os << ",";
      first = false;
      os << id;
    }
    os << "]";
  };

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
              << ",\"viol_inferred\":" << (rs.viol_inferred ? "true" : "false")
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
         << ",\"ess_count\":" << ess_live
         << ",\"css_count\":" << css_live
         << ",\"violations_count\":" << rp.violations.size()
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
           << (ev.synthesized ? ",\"synthesized\":true" : "")
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
  for (size_t li = 0; li < es_list.size(); li++) {
    if (li > 0) full_os << ",";
    const RecordedES &res = *es_list[li];
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
  for (size_t li = 0; li < cs_list.size(); li++) {
    if (li > 0) full_os << ",";
    const RecordedCS &rcs = *cs_list[li];
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
  for (size_t li = 0; li < rp.violations.size(); li++) {
    if (li > 0) full_os << ",";
    const RecordedViol &rviol = rp.violations[li];
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

  // AVars as [id, [type CS ids], [forward ids], [backward ids]]; the
  // var/contour/function of each id is in phase3_ifa.avar_info, the CS ids
  // in phase3_ifa.cs_info.
  auto write_ids = [&](const std::vector<int> &ids, bool live_only) {
    full_os << "[";
    bool first = true;
    for (int id : ids) {
      if (live_only && !ctx.live_av.count(id)) continue;
      if (!first) full_os << ",";
      first = false;
      full_os << id;
    }
    full_os << "]";
  };
  full_os << ",\"avars\":[";
  for (size_t ai = 0; ai < av_list.size(); ai++) {
    if (ai > 0) full_os << ",";
    const AVarState &st = *av_list[ai].second;
    full_os << "[" << av_list[ai].first << ",";
    write_ids(st.cs, false);
    full_os << ",";
    write_ids(st.fwd, true);
    full_os << ",";
    write_ids(st.bwd, true);
    full_os << "]";
  }
  full_os << "]";
  full_os << ",\"keyframe\":" << (keyframe ? "true" : "false");
  if (!keyframe) {
    full_os << ",\"es_gone\":";
    write_gone(full_os, rp.es_gone, ctx.live_es);
    full_os << ",\"cs_gone\":";
    write_gone(full_os, rp.cs_gone, ctx.live_cs);
    full_os << ",\"av_gone\":";
    write_gone(full_os, rp.av_gone, ctx.live_av);
  }

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

void dump_html(FA *fa, cchar *fn, cchar *cg_base) {
  // `fn` is the program as named on the command line, and the explorer's
  // output goes beside it; `cg_base` is where codegen wrote <cg_base>.c /
  // .ll, which under pyc -x is a cache entry the user never sees (and which
  // is renamed into place after this runs).
  check_env_config();

  // If no passes were recorded, record one now
  if (g_recorded_passes.empty() && fa) {
    html_record_fa_pass(fa, 0);
  }

  std::string base_fn(cg_base ? cg_base : fn);
  std::string html_fn = std::string(fn) + ".html";
  std::string json_fn = std::string(fn) + ".json";

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
    for (const auto &kv : g_es_cur) {
      const RecordedES &res = kv.second;
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

  // Phase 3: Flow Analysis, in the delta encoding ("encoding": "delta-v1"):
  //   cs_info   [[cs id, sym, is_const, const value, static arity], ...]
  //   avar_info [[av id, var id, orig var id, contour id, fun id,
  //               orig fun id, var name, fun name], ...]
  //   passes    [{pass, summary_only, summary} or {pass, summary_only, data}]
  // where a pass's `data` lists the EntrySets, CreationSets and AVars that
  // CHANGED since the previous pass (`*_gone`: ids that went away), and a
  // pass with "keyframe": true replaces the state outright. A reader
  // rebuilds pass N by applying passes 1..N in order (explorer.js
  // materializePass). Passes past the --html-max-size budget keep only
  // their summary; the last pass is then a keyframe.
  PassOutCtx ctx;
  for (const auto &kv : g_av_info)
    if (is_avar_info_live(kv.second)) ctx.live_av.insert(kv.first);
  std::unordered_set<int> live_ref_cs_ids;
  auto note_es = [&](const RecordedES &r) {
    if (!is_recorded_es_live(r)) return;
    ctx.live_es.insert(r.id);
    for (const auto &a : r.args) for (int cid : a.cs_ids) live_ref_cs_ids.insert(cid);
    for (const auto &x : r.rets) for (int cid : x.cs_ids) live_ref_cs_ids.insert(cid);
  };
  auto note_av = [&](int id, const AVarState &st) {
    if (ctx.live_av.count(id)) for (int cid : st.cs) live_ref_cs_ids.insert(cid);
  };
  for (const RecordedPass &rp : g_recorded_passes) {
    for (const RecordedES &r : rp.entry_sets) note_es(r);
    for (const auto &kv : rp.av_changed) note_av(kv.first, kv.second);
  }
  for (const auto &kv : g_es_cur) note_es(kv.second);
  for (const auto &kv : g_av_state) note_av(kv.first, kv.second);
  auto note_cs = [&](const RecordedCS &r) {
    if (is_creation_set_live(r.cs, live_ref_cs_ids)) ctx.live_cs.insert(r.id);
  };
  for (const RecordedPass &rp : g_recorded_passes)
    for (const RecordedCS &r : rp.creation_sets) note_cs(r);
  for (const auto &kv : g_cs_cur) note_cs(kv.second);

  root << "  \"phase3_ifa\": {\n";
  root << "    \"encoding\": \"delta-v1\",\n";
  {
    std::vector<int> ids;
    for (const auto &kv : g_cs_seen) ids.push_back(kv.first);
    std::sort(ids.begin(), ids.end());
    root << "    \"cs_info\": [";
    for (size_t k = 0; k < ids.size(); k++) {
      CreationSet *cs = g_cs_seen[ids[k]];
      if (k) root << ",";
      root << "[" << cs->id << ",\"" << json_escape(cs->sym->name ? cs->sym->name : "*anon*") << "\","
           << (cs->sym->is_constant ? 1 : 0) << ",\""
           << json_escape(cs->sym->is_constant && cs->sym->constant ? cs->sym->constant : "") << "\","
           << cs->static_arity << "]";
    }
    root << "],\n";
  }
  {
    std::vector<int> ids(ctx.live_av.begin(), ctx.live_av.end());
    std::sort(ids.begin(), ids.end());
    root << "    \"avar_info\": [";
    for (size_t k = 0; k < ids.size(); k++) {
      const AVarInfo &ai = g_av_info[ids[k]];
      Fun *cur_fun = (ai.es && ai.es->fun && ai.es->fun->live) ? ai.es->fun : ai.fun;
      Var *cur_var = (ai.av && ai.av->var && ai.av->var->live) ? ai.av->var : ai.var;
      if (k) root << ",";
      root << "[" << ids[k] << "," << (cur_var ? cur_var->id : ai.var_id) << "," << ai.var_id << "," << ai.contour_id
           << "," << (cur_fun ? cur_fun->id : ai.fun_id) << "," << ai.fun_id << ",\""
           << json_escape(ai.var && ai.var->sym && ai.var->sym->name ? ai.var->sym->name : "") << "\",\""
           << json_escape(ai.fun && ai.fun->sym && ai.fun->sym->name ? ai.fun->sym->name : "") << "\"]";
    }
    root << "],\n";
  }
  root << "    \"passes\": [\n";
  for (size_t pi = 0; pi < g_recorded_passes.size(); pi++) {
    const RecordedPass &rp = g_recorded_passes[pi];
    bool last = pi + 1 == g_recorded_passes.size();
    bool keyframe = last && rp.summary_only;
    std::string summary_json, full_json;
    format_recorded_pass(rp, ctx, keyframe, rp.ess_count, rp.css_count, summary_json, full_json);
    if (pi > 0) root << ",\n";
    if (rp.summary_only && !keyframe) {
      root << "      {\"pass\": " << rp.pass_num << ", \"summary_only\": true, \"summary\": " << summary_json << "}";
    } else {
      root << "      {\"pass\": " << rp.pass_num << ", \"summary_only\": false, \"data\": " << full_json << "}";
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
      reset_recording();
      inline_events_reset();
      return;
    }

  fprintf(fp, "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n");
  fprintf(fp, "<meta charset=\"UTF-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n");
  fprintf(fp, "<meta name=\"color-scheme\" content=\"dark\">\n");
  fprintf(fp, "<title>pyc Compiler Explorer - %s</title>\n", html_escape_str(fn).c_str());

  // Modern Embedded CSS
  fprintf(fp, "<style>\n");
  fputs(kExplorerCss, fp);
  fprintf(fp, "</style>\n</head>\n<body>\n");

  // Application Layout HTML
  fputs(kExplorerHtml, fp);

  // Embed Compilation JSON Dataset
  fprintf(fp, "<script id=\"pyc-compiler-data\" type=\"application/json\">\n");
  fprintf(fp, "%s\n", full_program_json.c_str());
  fprintf(fp, "</script>\n");

  // Embedded Client-side Explorer UI Logic (Vanilla ES6)
  fprintf(fp, "<script>\n");
  fputs(kExplorerJs, fp);
  fprintf(fp, "</script>\n</body>\n</html>\n");

  fclose(fp);
    fprintf(stderr, "wrote %s\n", html_fn.c_str());
    if (ifa_open_html) {
      open_html_in_browser(html_fn.c_str());
    }
  }
  // Both sidecars are per compilation: `ifa a.v b.v` must not carry a's
  // passes or inline events into b's explorer.
  reset_recording();
  inline_events_reset();
}
