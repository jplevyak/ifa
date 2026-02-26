#include "plib.h"

static cchar *SPACES = "                                                                               ";
static cchar *arg_types_keys = (char *)"ISDfF+TLvVC ";
static cchar *arg_types_desc[] = {"int     ", "string  ", "double  ", "set off ", "set on  ", "add one ",
                                  "toggle  ", "int64   ", "intvec  ", "dblvec  ", "config  ", "        "};

static void bad_flag(char *flag) {
  fprintf(stderr, "Unrecognized flag: '%s' (use '-h' for help)\n", flag);
  exit(1);
}

static void missing_arg(char *flag) {
  fprintf(stderr, "Missing argument for flag: '%s' (use '-h' for help)\n", flag);
  exit(1);
}

static void copyivec(const void *l, char *arg, int n) {
  int *v = (int *)l;
  for (int i = 0; i < n && arg; i++) {
    v[i] = atoi(arg);
    if ((arg = strchr(arg, ','))) arg++;
  }
}

static void copydvec(const void *l, char *arg, int n) {
  double *v = (double *)l;
  for (int i = 0; i < n && arg; i++) {
    v[i] = atof(arg);
    if ((arg = strchr(arg, ','))) arg++;
  }
}

static int process_arg(ArgumentState *arg_state, int i, char ***argv, char *flag) {
  int done = 0;
  char *arg = NULL;
  ArgumentDescription *desc = arg_state->desc;
  if (desc[i].type && desc[i].location) {
    char type = desc[i].type[0];
    switch (type) {
      case 'F':
      case 'f':
        *(bool *)desc[i].location = type == 'F' ? true : false;
        break;
      case 'T':
        *(int *)desc[i].location = !*(int *)desc[i].location;
        break;
      case '+':
        (*(int *)desc[i].location)++;
        break;
      default: {
        if (*++(**argv))
          arg = **argv;
        else {
          arg = *++(*argv);
          done++;
        }
        if (!arg) missing_arg(flag);
        switch (type) {
          case 'I':
            *(int *)desc[i].location = atoi(arg);
            break;
          case 'D':
            *(double *)desc[i].location = atof(arg);
            break;
          case 'L':
            *(int64 *)desc[i].location = atoll(arg);
            break;
          case 'S':
            strncpy((char *)desc[i].location, arg, atoi(desc[i].type + 1));
            break;
          case 'v':
            copyivec(desc[i].location, arg, atoi(desc[i].type + 1));
            break;
          case 'V':
            copydvec(desc[i].location, arg, atoi(desc[i].type + 1));
            break;
          case 'C':
            string_config(SET_CONFIG, 0, arg, (char *)desc[i].location);
            break;
          default:
            fprintf(stderr, "%s:bad argument description\n", arg_state->program_name);
            exit(1);
            break;
        }
        **argv += strlen(**argv) - 1;
        break;
      }
    }
  }
  if (desc[i].pfn) desc[i].pfn(arg_state, arg);
  return done;
}

int process_args(ArgumentState *arg_state, int argc, char **argv) {
  int i, len, done = 0;
  char *end = 0;
  ArgumentDescription *desc = arg_state->desc;
  /* Grab Environment Variables */
  for (i = 0;; i++) {
    if (!desc[i].name) break;
    if (desc[i].env) {
      char type = desc[i].type[0];
      char *env = getenv(desc[i].env);
      if (!env || !desc[i].location) continue;
      switch (type) {
        case '+':
          (*(int *)desc[i].location)++;
          break;
        case 'f':
        case 'F':
          *(bool *)desc[i].location = type == 'F' ? 1 : 0;
          break;
        case 'T':
          *(int *)desc[i].location = !*(int *)desc[i].location;
          break;
        case 'I':
          *(int *)desc[i].location = strtol(env, NULL, 0);
          break;
        case 'D':
          *(double *)desc[i].location = strtod(env, NULL);
          break;
        case 'L':
          *(int64 *)desc[i].location = strtoll(env, NULL, 0);
          break;
        case 'S':
          strncpy((char *)desc[i].location, env, strtol(desc[i].type + 1, NULL, 0));
          break;
        case 'v':
          copyivec(desc[i].location, env, atoi(desc[i].type + 1));
          break;
        case 'V':
          copydvec(desc[i].location, env, atoi(desc[i].type + 1));
          break;
      }
      if (desc[i].pfn) desc[i].pfn(arg_state, env);
    }
  }
  /* Grab Command Line Arguments */
  while (*++argv) {
    if (**argv == '-') {
      done++;
      if ((*argv)[1] == '-') {
        for (i = 0;; i++) {
          if (!desc[i].name) bad_flag(*argv);
          if ((end = strchr((*argv) + 2, '=')))
            len = end - ((*argv) + 2);
          else
            len = strlen((*argv) + 2);
          if (len == (int)strlen(desc[i].name) && !strncmp(desc[i].name, (*argv) + 2, len)) {
            char *flag = dupstr(*argv);
            if (!end)
              *argv += strlen(*argv) - 1;
            else
              *argv = end;
            done += process_arg(arg_state, i, &argv, flag);
            break;
          }
        }
      } else {
        while (*++(*argv))
          for (i = 0;; i++) {
            if (!desc[i].name) bad_flag((*argv) - 1);
            if (desc[i].key == **argv) {
              done += process_arg(arg_state, i, &argv, (*argv) - 1);
              break;
            }
          }
      }
    } else {
      arg_state->file_argument =
          (cchar **)REALLOC(arg_state->file_argument, sizeof(cchar **) * (arg_state->nfile_arguments + 2));
      arg_state->file_argument[arg_state->nfile_arguments++] = dupstr(*argv);
      arg_state->file_argument[arg_state->nfile_arguments] = NULL;
      if (arg_state->options & ARG_OPT_STOP_ON_FILE_ARGUMENT) break;
    }
  }
  return done;
}

static void print_string(cchar *s) {
  if (s && *s) {
    if (strlen(s) < 10)
      fprintf(stderr, " %-9s", s);
    else {
      char l[8];
      strncpy(l, s, 7);
      l[7] = 0;
      fprintf(stderr, " %-7s..", l);
    }
  } else
    fprintf(stderr, " (null)   ");
}

void usage(ArgumentState *arg_state, char *arg) {
  ArgumentDescription *desc = arg_state->desc;
  int i;

  if (!(arg_state->options & ARG_OPT_NO_DEFAULT_USAGE_HEADER))
    fprintf(stderr, "Usage: %s [flags|args]\n", arg_state->program_name);
  for (i = 0;; i++) {
    if (!desc[i].name) break;
    if (!desc[i].description) continue;
    fprintf(stderr, "  %c%c%c %s%s%s%s", desc[i].key != ' ' ? '-' : ' ', desc[i].key,
            (desc[i].key != ' ' && desc[i].name && desc[i].name[0]) ? ',' : ' ',
            (desc[i].name && desc[i].name[0] != '\0') ? "--" : "  ", desc[i].name,
            (strlen(desc[i].name) + 61 < 79) ? &SPACES[strlen(desc[i].name) + 61] : " ",
            arg_types_desc[desc[i].type ? strchr(arg_types_keys, desc[i].type[0]) - arg_types_keys
                                        : strlen(arg_types_keys) - 1]);
    switch (desc[i].type ? desc[i].type[0] : 0) {
      case 0:
        fprintf(stderr, "          ");
        break;
      case 'L':
        fprintf(stderr, " %-9" PRId64, *(int64 *)desc[i].location);
        break;
      case 'S':
      case 'v':
      case 'V':
        print_string((char *)desc[i].location);
        break;
      case 'D':
        if (*(double *)desc[i].location == DBL_ARG_UNSET)
          fprintf(stderr, " %-9s", "<unset>");
        else
          fprintf(stderr, " %-9.3e", *(double *)desc[i].location);
        break;
      case '+':
      case 'I':
        fprintf(stderr, " %-9d", *(int *)desc[i].location);
        break;
      case 'T':
      case 'f':
      case 'F':
        fprintf(stderr, " %-9s", *(bool *)desc[i].location ? "true " : "false");
        break;
      case 'C': {
        cchar *c = 0;
        string_config(GET_CONFIG, &c, "", (char *)desc[i].location);
        print_string(c);
        break;
      }
    }
    fprintf(stderr, " %s\n", desc[i].description);
  }
  if (!arg) exit(1);
}

void free_args(ArgumentState *arg_state) {
  if (arg_state->file_argument) FREE(arg_state->file_argument);
}
