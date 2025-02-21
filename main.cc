/* -*-Mode: c++;-*-
   Copyright (c) 2003-2008 John Plevyak, All Rights Reserved
*/
#define EXTERN
#include "ifa_version.h"
#include "ifadefs.h"
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
    {"system_directory", 'D', "System Directory", "S511", system_dir, "IFA_SYSTEM_DIRECTORY", NULL},
    {"html", 't', "Write Program in HTML", "T", &fhtml, "IFA_HTML", NULL},
    {"code", 'c', "Write Program in Core Code", "T", &fcode, "IFA_CODE", NULL},
    {"graph", 'G', "Write Program Graphs", "T", &fgraph, "IFA_GRAPH", NULL},
    {"graph-format", ' ', "GraphViz = 0, VCG = 1", "I", &fgraph_vcg, "IFA_GRAPH_FORMAT", NULL},
    {"test", ' ', "Unit Test", "F", &do_unit_tests, "IFA_TEST", NULL},
    {"log-dir", ' ', "Log Directory", "S512", log_dir, "CHPL_LOG_DIR", NULL},
    {"log", 'l', "Debug Logging Flags", "S512", log_flags, "CHPL_LOG_FLAGS", log_flags_arg},
    {"debug", 'd', "Debug", "+", &ifa_debug, "IFA_DEBUG", NULL},
    {"verbose", 'v', "Verbose", "+", &ifa_verbose, "IFA_VERBOSE", NULL},
    {"ddebug", ' ', "DParser Debug Level", "+", &d_debug_level, "IFA_D_DEBUG_LEVEL", NULL},
    {"ddverbose", ' ', "DParser Verbose Level (prelude)", "+", &d_verbose_level, "IFA_PARSER_VERBOSE_PRELUDE", NULL},
    {"dverbose", ' ', "DParser Verbose Level (except prelude)", "+", &parser_verbose_non_prelude,
     "IFA_PARSER_VERBOSE_NON_PRELUDE", NULL},
    {"copyright", ' ', "Show Copyright", NULL, NULL, NULL, copyright},
    {"help", 'h', "Help", NULL, NULL, NULL, help},
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
  ifa_version(ver);
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
  if (fhtml || log_flags[0]) init_logs();
  if (do_unit_tests) _exit(UnitTest::run_all());
  for (int i = 0; i < arg_state.nfile_arguments; i++) {
    compile_one_file(arg_state.file_argument[i]);
    if (fhtml) ifa_html(arg_state.file_argument[i], ".");
    if (fcode) ifa_code(arg_state.file_argument[i]);
  }
  return 0;
}
