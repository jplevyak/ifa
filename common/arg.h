#ifndef _arg_H
#define _arg_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef __alpha
#define atoll atol
#endif

#define DBL_ARG_UNSET (DBL_MAX)
#define ARG_OPT_STOP_ON_FILE_ARGUMENT 0x1
#define ARG_OPT_NO_DEFAULT_USAGE_HEADER 0x2

/* Argument Handling
 */
struct ArgumentState;

typedef void ArgumentFunction(struct ArgumentState *arg_state, char *arg);

typedef struct {
  cchar *name;
  char key;
  cchar *description;
  cchar *type;
  const void *location;
  cchar *env;
  ArgumentFunction *pfn;
} ArgumentDescription;

typedef struct ArgumentState {
  cchar **file_argument;
  int nfile_arguments;
  cchar *program_name;
  unsigned int options;
  ArgumentDescription *desc;
#if defined __cplusplus
  ArgumentState(cchar *name, ArgumentDescription *adesc, int aoptions = 0)
      : file_argument(0), nfile_arguments(0), program_name(name), options(aoptions), desc(adesc) {}
#endif
} ArgumentState;

void usage(ArgumentState *arg_state, char *exit_if_null);
int process_args(ArgumentState *arg_state, int argc, char **argv);  // ret = # done
void free_args(ArgumentState *arg_state);

#endif
