// SPDX-License-Identifier: BSD-3-Clause
// ifa-test — IF1-level test harness.
//
// Walks fixtures under ifa/tests/ir/<phase>/<n>_<name>.ir, runs the
// IFA pipeline up to the requested phase, captures the per-phase
// normalized output, and diffs against <fixture>.<phase>.expected.
//
// See ifa/testing/TEST_RUNNER.md for the design and CLI surface.

// Provide storage for the EXTERN-declared globals (system_dir, log_dir,
// config_filenames, num_kind_string, int_type_precision, etc.). This is
// the standard libifa trick: the binary linking the archive defines
// EXTERN to empty so all extern declarations turn into definitions.
// Without this, main.o (in libifa_gc.a) would be pulled to satisfy
// those globals, which would conflict with our own main().
#define EXTERN

#include "ifadefs.h"

#include "ast.h"
#include "if1.h"
#include "ifa.h"
#include "pdb.h"
#include "sym.h"
#include "testing/parse_ir.h"
#include "ast.h"
#include "testing/fa_setup.h"
#include "testing/print_argpos.h"
#include "testing/print_cfg.h"
#include "testing/print_dispatch.h"
#include "testing/print_dom.h"
#include "testing/print_fa.h"
#include "testing/print_finalize.h"
#include "testing/print_loops.h"
#include "testing/print_patterns.h"
#include "testing/print_ssu.h"
#include "testing/test_callbacks.h"
#include "testing/write_ir.h"

#include <dirent.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

// ---------------------------------------------------------------------------
// Phase registry
// ---------------------------------------------------------------------------

struct Phase {
  cchar *name;
  // pre_parse (optional): called after ifa_init() but BEFORE parsing
  // the fixture. Use this to register builtin Syms the .ir text needs
  // to be able to reference (e.g., @int32) — init_default_builtin_types.
  void (*pre_parse)(IF1 *p);
  // run: pipeline steps assuming the IF1 is parsed and earlier phases
  // have run.
  void (*run)(IF1 *p);
  // print: emit the normalized output for golden compare.
  void (*print)(FILE *fp, IF1 *p);
};

static void phase_finalize_run(IF1 *p) { if1_finalize(p); }

// `cfg`, `ssu`, and `dom` all run finalize first; their printers build
// the per-closure Funs themselves with the appropriate FUN_BUILD_*
// flags (and, for `dom`, call build_cfg_dominators).
static void phase_cfg_run(IF1 *p) { if1_finalize(p); }
static void phase_ssu_run(IF1 *p) { if1_finalize(p); }
static void phase_dom_run(IF1 *p) { if1_finalize(p); }
static void phase_loops_run(IF1 *p) { if1_finalize(p); }
static void phase_argpos_run(IF1 *p) { if1_finalize(p); }
// pre_parse helper used by patterns and fa-init: registers the
// default builtin type Syms (sym_int32 / sym_float64 / etc.) so the
// .ir fixture can reference them via @int32-style refs.
static void pre_parse_builtin_types(IF1 *) { init_default_builtin_types(); }

static void phase_patterns_run(IF1 *p) { if1_finalize(p); }
// fa_setup_environment synthesizes sym___main__ as a stub closure,
// splices the .ir's `(entry %x)` body into it, then runs
// finalize_types + build_type_hierarchy + if1_finalize sub-phases.
// init_default_builtin_types was already run via pre_parse so .ir
// `@int32` refs resolve.
static void phase_fa_run(IF1 *p) { fa_setup_environment(p); }
// `dispatch` shares the full fa_setup chain — its printer enumerates
// AEdges produced by FA::analyze to report per-call-site dispatch.
static void phase_dispatch_run(IF1 *p) { fa_setup_environment(p); }

static Phase phases[] = {
    {"finalize", 0,                       phase_finalize_run, print_finalize_normalized},
    {"cfg",      0,                       phase_cfg_run,      print_cfg_normalized},
    {"ssu",      0,                       phase_ssu_run,      print_ssu_normalized},
    {"dom",      0,                       phase_dom_run,      print_dom_normalized},
    {"loops",    0,                       phase_loops_run,    print_loops_normalized},
    {"argpos",   0,                       phase_argpos_run,   print_argpos_normalized},
    {"patterns", pre_parse_builtin_types, phase_patterns_run, print_patterns_normalized},
    {"fa-init",  pre_parse_builtin_types, phase_fa_run,       print_fa_normalized},
    {"dispatch", pre_parse_builtin_types, phase_dispatch_run, print_dispatch_normalized},
    // Future phases: fa-converge, clone, dce, freq, inline,
    // codegen-c, codegen-llvm. See ifa/testing/phases/00_INDEX.md.
    {0, 0, 0, 0},
};

static Phase *find_phase(cchar *name) {
  for (Phase *p = phases; p->name; p++) if (!strcmp(p->name, name)) return p;
  return 0;
}

// ---------------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------------

static cchar *opt_phase = "finalize";
static cchar *opt_fixtures_root = "tests/ir";
static cchar *opt_pattern = "";
static int opt_verbose = 0;
static int opt_keep = 0;
static int opt_bail = 0;
static int opt_rebless = 0;

static void usage(FILE *fp) {
  fputs(
      "ifa-test [options] [pattern]\n"
      "\n"
      "  --phase NAME          run pipeline through this phase (default: finalize)\n"
      "  --list-phases         print phase names and exit\n"
      "  --rebless             update .expected files instead of diffing\n"
      "  --keep                keep tests/ir/build/ outputs after run\n"
      "  --bail                stop at first failure\n"
      "  --fixtures-root DIR   fixture root (default: tests/ir)\n"
      "  -v / --verbose        show every pass result, not just failures\n"
      "  -h / --help           this help\n"
      "\n"
      "  pattern               substring match on test name (default: all)\n",
      fp);
}

static int parse_args(int argc, char **argv) {
  for (int i = 1; i < argc; i++) {
    cchar *a = argv[i];
    auto need_val = [&]() -> cchar * {
      if (i + 1 >= argc) {
        fprintf(stderr, "ifa-test: %s requires a value\n", a);
        return 0;
      }
      return argv[++i];
    };
    if (!strcmp(a, "--phase")) {
      cchar *v = need_val(); if (!v) return -1; opt_phase = v;
    } else if (!strcmp(a, "--fixtures-root")) {
      cchar *v = need_val(); if (!v) return -1; opt_fixtures_root = v;
    } else if (!strcmp(a, "--list-phases")) {
      for (Phase *p = phases; p->name; p++) printf("%s\n", p->name);
      exit(0);
    } else if (!strcmp(a, "--rebless")) {
      opt_rebless = 1;
    } else if (!strcmp(a, "--keep")) {
      opt_keep = 1;
    } else if (!strcmp(a, "--bail")) {
      opt_bail = 1;
    } else if (!strcmp(a, "-v") || !strcmp(a, "--verbose")) {
      opt_verbose = 1;
    } else if (!strcmp(a, "-h") || !strcmp(a, "--help")) {
      usage(stdout); exit(0);
    } else if (a[0] == '-') {
      fprintf(stderr, "ifa-test: unknown option '%s'\n", a);
      usage(stderr); return -1;
    } else {
      opt_pattern = a;
    }
  }
  if (!find_phase(opt_phase)) {
    fprintf(stderr, "ifa-test: unknown phase '%s' (use --list-phases)\n", opt_phase);
    return -1;
  }
  return 0;
}

// ---------------------------------------------------------------------------
// Fixture discovery
// ---------------------------------------------------------------------------

struct Fixture {
  cchar *path;     // tests/ir/<phase>/<file>.ir
  cchar *expected; // tests/ir/<phase>/<file>.<phase>.expected
};

static int compar_fixture(const void *a, const void *b) {
  return strcmp(((const Fixture *)a)->path, ((const Fixture *)b)->path);
}

static void scan_fixtures(cchar *root, cchar *phase, Vec<Fixture> &out) {
  char dir[1024];
  snprintf(dir, sizeof(dir), "%s/%s", root, phase);
  DIR *d = opendir(dir);
  if (!d) return;
  struct dirent *de;
  while ((de = readdir(d))) {
    cchar *n = de->d_name;
    int nl = strlen(n);
    if (nl < 4 || strcmp(n + nl - 3, ".ir") != 0) continue;
    char path[1024];
    snprintf(path, sizeof(path), "%s/%s", dir, n);
    char exp[1024];
    snprintf(exp, sizeof(exp), "%s.%s.expected", path, phase);
    Fixture f;
    f.path = dupstr(path);
    f.expected = dupstr(exp);
    out.add(f);
  }
  closedir(d);
  if (out.n > 1) qsort(out.v, out.n, sizeof(Fixture), compar_fixture);
}

// ---------------------------------------------------------------------------
// Single-fixture driver
// ---------------------------------------------------------------------------

// Color helpers (TTY-aware).
static cchar *col_R = "", *col_G = "", *col_Y = "", *col_N = "";

static char *slurp(cchar *path, int *plen = 0) {
  FILE *fp = fopen(path, "rb");
  if (!fp) return 0;
  fseek(fp, 0, SEEK_END);
  int n = (int)ftell(fp);
  fseek(fp, 0, SEEK_SET);
  char *buf = (char *)MALLOC(n + 1);
  fread(buf, 1, n, fp);
  buf[n] = 0;
  fclose(fp);
  if (plen) *plen = n;
  return buf;
}

static int write_file(cchar *path, cchar *content, int len) {
  FILE *fp = fopen(path, "wb");
  if (!fp) return -1;
  fwrite(content, 1, len, fp);
  fclose(fp);
  return 0;
}

static cchar *short_name(cchar *path, cchar *root) {
  int rl = strlen(root);
  if (!strncmp(path, root, rl) && path[rl] == '/') return path + rl + 1;
  return path;
}

static int run_one(Fixture &f, Phase *phase, int &out_failed) {
  cchar *name = short_name(f.path, opt_fixtures_root);

  ifa_reset();
  ifa_init(new IRCallbacks);
  parse_ir_reset();

  // Match the production frontend default. With Partial_OK, every
  // SEND constructed by the .ir parser is treated as a partial
  // application by FA — `covers_formals` sets m->is_partial=1 even
  // when the call is full, which routes the dispatch into
  // make_closure() and trips its `contour_is_entry_set` assertion
  // on the SEND's result. Partial_NEVER says "this SEND is a full
  // call; don't form a closure," which is what test fixtures want.
  if1->partial_default = Partial_NEVER;

  if (phase->pre_parse) phase->pre_parse(if1);

  if (parse_ir_file(f.path) != 0) {
    fprintf(stderr, "%s%sFAIL%s  %s  (parse)\n", col_R, "", col_N, name);
    out_failed++;
    return -1;
  }

  // Run the requested phase.
  phase->run(if1);

  // Capture output to a buffer.
  char *got_buf = NULL;
  size_t got_size = 0;
  FILE *memfp = open_memstream(&got_buf, &got_size);
  if (!memfp) { fprintf(stderr, "ifa-test: open_memstream failed\n"); return -1; }
  phase->print(memfp, if1);
  fclose(memfp);

  if (opt_rebless) {
    if (write_file(f.expected, got_buf, (int)got_size) != 0) {
      fprintf(stderr, "ifa-test: cannot write %s\n", f.expected);
      free(got_buf);
      return -1;
    }
    printf("  %sBLESSED%s  %s  → %s (%zu bytes)\n",
           col_Y, col_N, name, f.expected, got_size);
    free(got_buf);
    return 0;
  }

  int exp_len = 0;
  char *exp_buf = slurp(f.expected, &exp_len);
  if (!exp_buf) {
    fprintf(stderr, "%sMISSING%s  %s  (no %s — use --rebless to create)\n",
            col_Y, col_N, name, f.expected);
    out_failed++;
    free(got_buf);
    return -1;
  }

  int ok = ((int)got_size == exp_len) && (memcmp(got_buf, exp_buf, exp_len) == 0);
  if (!ok) {
    fprintf(stderr, "%sFAIL%s    %s  (output differs from %s)\n",
            col_R, col_N, name, f.expected);
    // Brief diff: show first 40 lines of each.
    fputs("  --- expected ---\n", stderr);
    int lines = 0; cchar *p = exp_buf; while (*p && lines < 40) {
      fputc(' ', stderr); fputc(' ', stderr);
      while (*p && *p != '\n') { fputc(*p, stderr); p++; }
      fputc('\n', stderr); if (*p) p++; lines++;
    }
    fputs("  --- actual ---\n", stderr);
    lines = 0; p = got_buf; while (*p && lines < 40) {
      fputc(' ', stderr); fputc(' ', stderr);
      while (*p && *p != '\n') { fputc(*p, stderr); p++; }
      fputc('\n', stderr); if (*p) p++; lines++;
    }
    out_failed++;
    free(got_buf);
    return -1;
  }

  if (opt_verbose) {
    printf("  %sPASS%s    %s\n", col_G, col_N, name);
  }
  free(got_buf);
  return 0;
}

// ---------------------------------------------------------------------------
// main
// ---------------------------------------------------------------------------

int main(int argc, char **argv) {
  MEM_INIT();
  if (parse_args(argc, argv) != 0) return 2;

  if (isatty(fileno(stdout))) {
    col_R = "\033[31m"; col_G = "\033[32m"; col_Y = "\033[33m"; col_N = "\033[0m";
  }

  Phase *phase = find_phase(opt_phase);  // already validated

  Vec<Fixture> fixtures;
  scan_fixtures(opt_fixtures_root, phase->name, fixtures);
  if (fixtures.n == 0) {
    fprintf(stderr, "ifa-test: no fixtures found under %s/%s/\n", opt_fixtures_root, phase->name);
    return 1;
  }

  int total = 0, failed = 0;
  for (Fixture &f : fixtures) {
    if (opt_pattern[0] && !strstr(f.path, opt_pattern)) continue;
    total++;
    int before = failed;
    run_one(f, phase, failed);
    if (opt_bail && failed > before) break;
  }

  printf("\n%s---- summary ----%s\n", "\033[1m", col_N);
  printf("  phase:   %s\n", phase->name);
  printf("  passed:  %d\n", total - failed);
  printf("  failed:  %d\n", failed);
  printf("  total:   %d\n", total);

  return failed ? 1 : 0;
}
