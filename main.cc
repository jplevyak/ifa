#define EXTERN
#include "ifa_version.h"
#include "ifadefs.h"
#include "fa.h"
#include "html.h"
#include "inline.h"
#include "log.h"
#include "parse.h"
#include "unit.h"

static const char *ifa_config_filenames = "~/.ifalib,ifa.init";
static int fhtml = 0;
static int fgraph = 0;
static int fgraph_vcg = 0;
static int fcode = 0;
static char log_flags[512] = "";
static int do_unit_tests = 0;

extern int d_debug_level;
extern int d_verbose_level;

static void help(ArgumentState *arg_state, char *arg_unused);
static void copyright(ArgumentState *arg_state, char *arg_unused);

static ArgumentDescription arg_desc[] = {
    {"system-directory", 'D', "System Directory", "S511", system_dir, "IFA_SYSTEM_DIRECTORY", NULL},
    {"debug", 'd', "Debug", "+", &ifa_debug, "IFA_DEBUG", NULL},
    {"verbose", 'v', "Verbose", "+", &ifa_verbose, "IFA_VERBOSE", NULL},
    {"copyright", ' ', "Show Copyright", NULL, NULL, NULL, copyright},
    {"help", 'h', "Help", NULL, NULL, NULL, help},

    {"", ' ', "-- Internal / development options (not a stable CLI contract) --", NULL, NULL, NULL, NULL},
    {"html", 't', "Write Program in HTML", "T", &fhtml, "IFA_HTML", NULL},
    {"browse", 'B', "Generate HTML and open in local browser (implies --html)", "T", &ifa_open_html, "IFA_BROWSE", NULL},
    {"html-open", ' ', "Generate HTML and open in local browser (implies --html)", "T", &ifa_open_html, "IFA_HTML_OPEN",
     NULL},
    {"html-max-size", ' ', "Max HTML telemetry size in MB (default 100)", "I", &ifa_html_max_size_mb,
     "IFA_HTML_MAX_SIZE", NULL},
    {"html-pass-limit", ' ', "Pass companion file size threshold in MB (default 2)", "I", &ifa_html_pass_limit_mb,
     "IFA_HTML_PASS_LIMIT", NULL},
    {"dump-json", ' ', "Output compilation database as JSON", "T", &ifa_dump_json, "IFA_DUMP_JSON", NULL},
    {"code", 'c', "Write Program in Core Code", "T", &fcode, "IFA_CODE", NULL},
    {"graph", 'G', "Write Program Graphs", "T", &fgraph, "IFA_GRAPH", NULL},
    {"graph-format", ' ', "GraphViz = 0, VCG = 1", "I", &fgraph_vcg, "IFA_GRAPH_FORMAT", NULL},
    {"test", ' ', "Unit Test", "F", &do_unit_tests, "IFA_TEST", NULL},
    {"log-dir", ' ', "Log Directory", "S512", log_dir, "IFA_LOG_DIR", NULL},
    {"log", 'l', "Debug Logging Flags", "S512", log_flags, "IFA_LOG_FLAGS", log_flags_arg},
    {"dparser-debug", ' ', "DParser Debug Level", "+", &d_debug_level, "IFA_D_DEBUG_LEVEL", NULL},
    {"dparser-verbose-prelude", ' ', "DParser Verbose Level (prelude)", "+", &d_verbose_level,
     "IFA_PARSER_VERBOSE_PRELUDE", NULL},
    {"dparser-verbose", ' ', "DParser Verbose Level (except prelude)", "+", &parser_verbose_non_prelude,
     "IFA_PARSER_VERBOSE_NON_PRELUDE", NULL},
    {0}};

static ArgumentState arg_state("ifa", arg_desc);

static void copyright(ArgumentState *arg_state, char *arg_unused) {
  fprintf(stderr,
          "\n"
#include "LICENSE.i"
          "\n");
  exit(0);
}

static void short_copyright() {
  fprintf(stderr,
#include "COPYRIGHT.i"
  );
}

static void help(ArgumentState *arg_state, char *arg_unused) {
  char ver[100];
  ifa_version(ver, sizeof(ver));
  fprintf(stderr, "IFA Version %s ", ver);
  short_copyright();
  usage(arg_state, arg_unused);
}

int main(int argc, char *argv[]) {
  MEM_INIT();
  process_args(&arg_state, argc, argv);
  if (arg_state.nfile_arguments < 1 && !do_unit_tests) help(&arg_state, NULL);
  graph_type = !fgraph_vcg ? GraphViz : VCG;
  strcpy(config_filenames, ifa_config_filenames);
  init_config();
  if (ifa_open_html) {
    fhtml = 1;
  }
  if (fhtml || log_flags[0]) init_logs();
  if (fhtml) {
    ifa_dump_html = 1;
    fa_events_enable();
    inline_events_enable();
  }
  if (ifa_dump_json) {
    fa_events_enable();
    inline_events_enable();
  }
  if (do_unit_tests) _exit(UnitTest::run_all());
  for (int i = 0; i < arg_state.nfile_arguments; i++) {
    compile_one_file(arg_state.file_argument[i]);
    if (fhtml || ifa_dump_json) ifa_html(arg_state.file_argument[i], ".");
    if (fcode) ifa_code(arg_state.file_argument[i]);
  }
  return 0;
}
