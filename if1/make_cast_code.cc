#include <stdio.h>

/*
To be used to build the body of the function:

void coerce_immediate(from, to);

Also generates check_cast.cc which is the body of:

bool check_cast(Immediate *from, Immediate *to);

Compile with:

c++ -std=c++23 make_cast_code.cc -o make_cast_code && ./make_cast_code
*/

// IF1_NUM_KIND_UINT=1, IF1_NUM_KIND_INT=2, IF1_NUM_KIND_FLOAT=3
// IF1_INT_TYPE_1=0 (bool)
static const char *num_kind_string[4][8] = {
    {0, 0, 0, 0, 0, 0, 0, 0},
    {"bool", "uint8", "uint16", "uint32", "uint64", 0, 0, 0},
    {"bool", "int8", "int16", "int32", "int64", 0, 0, 0},
    {0, "float32", 0, "float64", 0, 0, 0, "float128"}};

static const int IF1_NUM_KIND_UINT = 1;
static const int IF1_INT_TYPE_1 = 0;

static void make_cast_code() {
  FILE *fp = fopen("cast_code.cc", "w");
  fprintf(fp, "switch (to->const_kind) {\n");
  fprintf(fp, "default: assert(!\"case\"); break;\n");
  for (int tt = 1; tt < 4; tt++) {
    fprintf(fp, "case %d: \n", tt);
    fprintf(fp, "switch (to->num_index) {\n");
    for (int tn = 0; tn < 8; tn++) {
      fprintf(fp, "case %d:\n", tn);
      if (!num_kind_string[tt][tn]) {
        fprintf(fp, "assert(!\"case\"); break;\n");
      } else {
        fprintf(fp, "switch (from->const_kind) {\n");
        fprintf(fp, "default: assert(!\"case\"); break;\n");
        for (int st = 1; st < 4; st++) {
          fprintf(fp, "case %d:\n", st);
          fprintf(fp, "switch (from->num_index) {\n");
          for (int sn = 0; sn < 8; sn++) {
            fprintf(fp, "case %d:\n", sn);
            if (!num_kind_string[st][sn]) {
              fprintf(fp, "assert(!\"case\"); break;\n");
            } else {
              if (tt == IF1_NUM_KIND_UINT && tn == IF1_INT_TYPE_1)
                fprintf(fp, "to->v_%s = (%s)!!from->v_%s; break;\n", num_kind_string[tt][tn], num_kind_string[tt][tn],
                        num_kind_string[st][sn]);
              else
                fprintf(fp, "to->v_%s = (%s)from->v_%s; break;\n", num_kind_string[tt][tn], num_kind_string[tt][tn],
                        num_kind_string[st][sn]);
            }
          }
          fprintf(fp, "} break;\n");
        }
        fprintf(fp, "} break;\n");
      }
    }
    fprintf(fp, "} break;\n");
  }
  fprintf(fp, "}\n");
  fclose(fp);
}

static void make_check_cast() {
  FILE *fp = fopen("check_cast.cc", "w");
  fprintf(fp, "switch (to->const_kind) {\n");
  fprintf(fp, "default: return false;\n");
  for (int tt = 1; tt < 4; tt++) {
    fprintf(fp, "case %d: \n", tt);
    fprintf(fp, "switch (to->num_index) {\n");
    for (int tn = 0; tn < 8; tn++) {
      fprintf(fp, "case %d:\n", tn);
      if (!num_kind_string[tt][tn]) {
        fprintf(fp, "return false;\n");
      } else {
        fprintf(fp, "switch (from->const_kind) {\n");
        fprintf(fp, "default: return false;\n");
        for (int st = 1; st < 4; st++) {
          fprintf(fp, "case %d:\n", st);
          fprintf(fp, "switch (from->num_index) {\n");
          for (int sn = 0; sn < 8; sn++) {
            fprintf(fp, "case %d:\n", sn);
            if (!num_kind_string[st][sn]) {
              fprintf(fp, "return false;\n");
            } else {
              fprintf(fp, "return true;\n");
            }
          }
          fprintf(fp, "} break;\n");
        }
        fprintf(fp, "} break;\n");
      }
    }
    fprintf(fp, "} break;\n");
  }
  fprintf(fp, "}\n");
  fprintf(fp, "return false;\n");
  fclose(fp);
}

int main() {
  make_cast_code();
  make_check_cast();
}
