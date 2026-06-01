// SPDX-License-Identifier: BSD-3-Clause
// Round-trip test: parse a .ir string, write it back, parse the result,
// confirm the two in-memory IF1 states are byte-equivalent under
// `write_ir`. Registered with the UnitTest framework so `ifa --test`
// exercises it.

#include "ifadefs.h"

#include "ast.h"
#include "code.h"
#include "ifa.h"
#include "if1.h"
#include "pdb.h"
#include "sym.h"
#include "unit.h"
#include "testing/parse_ir.h"
#include "testing/write_ir.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

// Minimal callbacks suitable for testing — no frontend semantics.
class IRCallbacks : public IFACallbacks {
 public:
  Sym *new_Sym(cchar *name) override {
    Sym *s = new Sym();
    if1_register_sym(if1, s, name);
    return s;
  }
};

// Make a small fixture .ir source. Mirrors the worked example in
// IF1_TEXT_FORMAT.md §5.
static cchar *sample_ir() {
  return
    ";; sample: a tiny add function\n"
    "(sym %a :is-local)\n"
    "(sym %b :is-local)\n"
    "(sym %r :is-local)\n"
    "(sym %cont)\n"
    "(sym %ret)\n"
    "(fun %add\n"
    "  :args (%a %b)\n"
    "  :rets (%r)\n"
    "  :cont %cont\n"
    "  :body\n"
    "    (send %a %b => %r)\n"
    "    (label %L_end))\n"
    "(entry %add)\n";
}

static int slurp_to_string(FILE *fp, char *buf, int cap) {
  fseek(fp, 0, SEEK_SET);
  int n = fread(buf, 1, cap - 1, fp);
  buf[n] = 0;
  return n;
}

static int run_roundtrip() {
  // Fresh state per test.
  ifa_reset();
  ifa_init(new IRCallbacks);
  parse_ir_reset();

  cchar *src = sample_ir();
  int rc = parse_ir_string(src, "sample.ir");
  if (rc != 0) {
    printf("  parse #1 failed\n");
    return 1;
  }

  // Capture sanity: the closure was registered and the entry was set.
  if (!if1->top) {
    printf("  no entry set after first parse\n");
    return 1;
  }
  Sym *add = parse_ir_lookup("add");
  if (!add || !add->is_fun) {
    printf("  %%add missing or not marked is_fun\n");
    return 1;
  }
  if (add->has.n != 2) {
    printf("  %%add expected 2 args, got %d\n", add->has.n);
    return 1;
  }
  if (!add->code) {
    printf("  %%add has no code\n");
    return 1;
  }

  // Write to a memory buffer (via tmpfile).
  char first_out[8192];
  FILE *fp1 = tmpfile();
  if (!fp1) { printf("  tmpfile() failed\n"); return 1; }
  write_ir(fp1, if1);
  int n1 = slurp_to_string(fp1, first_out, sizeof(first_out));
  fclose(fp1);
  if (n1 == 0) {
    printf("  write #1 produced empty output\n");
    return 1;
  }

  // Re-parse into a fresh IF1.
  ifa_reset();
  ifa_init(new IRCallbacks);
  parse_ir_reset();

  rc = parse_ir_string(first_out, "roundtrip.ir");
  if (rc != 0) {
    printf("  parse #2 failed; first writer output was:\n%s\n", first_out);
    return 1;
  }

  // Write again.
  char second_out[8192];
  FILE *fp2 = tmpfile();
  if (!fp2) { printf("  tmpfile() failed\n"); return 1; }
  write_ir(fp2, if1);
  int n2 = slurp_to_string(fp2, second_out, sizeof(second_out));
  fclose(fp2);

  if (n1 != n2 || memcmp(first_out, second_out, n1) != 0) {
    printf("  round-trip mismatch.\n--- first ---\n%s\n--- second ---\n%s\n",
           first_out, second_out);
    return 1;
  }

  return 0;
}

// Minimal smoke test: bare-bones parse with no IF1 features touched.
static int run_smoke() {
  ifa_reset();
  ifa_init(new IRCallbacks);
  parse_ir_reset();
  int rc = parse_ir_string("(sym %x)\n(entry %x)\n", "smoke.ir");
  if (rc != 0) return 1;
  if (parse_ir_lookup("x") == NULL) return 1;
  if (if1->top != parse_ir_lookup("x")) return 1;
  return 0;
}

// Rich fixture: types with :has, immediates of several kinds, control
// flow (move/send/if/goto/label), :is-constant + :immediate combo.
static cchar *rich_ir() {
  return
    "(type %Point :kind RECORD)\n"
    "(sym %x :in %Point)\n"
    "(sym %y :in %Point)\n"
    "(type %PointFull :kind RECORD :has (%x %y))\n"
    "(sym %ten :is-constant :immediate (int32 10))\n"
    "(sym %pi :is-constant :immediate (float64 3.14159))\n"
    "(sym %hi :is-constant :immediate (string \"hello\"))\n"
    "(sym %t :is-constant :immediate (bool true))\n"
    "(sym %v1 :is-local)\n"
    "(sym %v2 :is-local)\n"
    "(sym %v3 :is-local)\n"
    "(sym %cond :is-local)\n"
    "(sym %cont)\n"
    "(sym %ret)\n"
    "(fun %f\n"
    "  :args (%v1 %v2)\n"
    "  :rets (%v3)\n"
    "  :cont %cont\n"
    "  :body\n"
    "    (move %v1 %v3)\n"
    "    (send %v1 %v2 => %v3)\n"
    "    (if %cond %L1 %L2)\n"
    "    (label %L1)\n"
    "    (move %ten %v3)\n"
    "    (goto %L_done)\n"
    "    (label %L2)\n"
    "    (move %pi %v3)\n"
    "    (label %L_done))\n"
    "(entry %f)\n";
}

static int run_rich_roundtrip() {
  ifa_reset();
  ifa_init(new IRCallbacks);
  parse_ir_reset();

  if (parse_ir_string(rich_ir(), "rich.ir") != 0) {
    printf("  rich parse #1 failed\n");
    return 1;
  }

  char first_out[16384];
  FILE *fp1 = tmpfile();
  write_ir(fp1, if1);
  int n1 = slurp_to_string(fp1, first_out, sizeof(first_out));
  fclose(fp1);

  ifa_reset();
  ifa_init(new IRCallbacks);
  parse_ir_reset();

  if (parse_ir_string(first_out, "rich-rt.ir") != 0) {
    printf("  rich parse #2 failed.\nfirst output was:\n%s\n", first_out);
    return 1;
  }

  char second_out[16384];
  FILE *fp2 = tmpfile();
  write_ir(fp2, if1);
  int n2 = slurp_to_string(fp2, second_out, sizeof(second_out));
  fclose(fp2);

  if (n1 != n2 || memcmp(first_out, second_out, n1) != 0) {
    printf("  rich round-trip mismatch.\n--- first ---\n%s\n--- second ---\n%s\n",
           first_out, second_out);
    return 1;
  }
  return 0;
}

UNIT_TEST_FUN(run_smoke);
UNIT_TEST_FUN(run_roundtrip);
UNIT_TEST_FUN(run_rich_roundtrip);
