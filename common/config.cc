
#include "common.h"
#ifndef __CYGWIN__
#include <wordexp.h>
#else
#include <glob.h>
#endif

struct ConfigCallback {
  config_callback_pfn pfn;
  void *data;
  struct ConfigCallback *next;
};

typedef int (*dynamic_config_fn_t)(char *, char *);

struct DynamicFn : public gc {
  dynamic_config_fn_t fn;
  char *data;
  DynamicFn(dynamic_config_fn_t afn, char *adata) : fn(afn), data(adata) {}
};

typedef MapElem<char *, char *> MECharChar;
typedef MapElem<char *, Vec<DynamicFn *> *> MECharVecDyn;

static ConfigCallback *callbacks = 0;
static HashMap<char *, StringHashFns, char *> config;
static HashMap<char *, StringHashFns, Vec<DynamicFn *> *> dynamic_config;
static pthread_mutex_t config_lock;

static void build_name(char *name, cchar *n1, cchar *n2, cchar *n3) {
  char *n = scpy(name, n1);
  if (n2) {
    *n++ = '.';
    n = scpy(n, n2);
    if (n3) {
      *n++ = '.';
      n = scpy(n, n3);
    }
  }
  *n++ = 0;
}

int int64_dynfn(char *data, char *value) {
  *((int64 *)data) = (int64)strtoll(value, 0, 0);
  return 0;
}

int int_dynfn(char *data, char *value) {
  *((int *)data) = (int)strtol(value, 0, 0);
  return 0;
}

int string_dynfn(char *data, char *value) {
  *((char **)data) = value;
  return 0;
}

static void dynamic(char *name, char *data, dynamic_config_fn_t fn) {
  MECharVecDyn *e = dynamic_config.get_internal(name);
  if (e) {
    e->value->add(new DynamicFn(fn, data));
    return;
  }
  Vec<DynamicFn *> *x = new Vec<DynamicFn *>;
  x->add(new DynamicFn(int_dynfn, data));
  dynamic_config.put(dupstr(name), x);
  return;
}

static void callback_dynamic(char *name) {
  Vec<DynamicFn *> *d = dynamic_config.get(name);
  if (d) {
    char *v = config.get(name);
    if (v) {
      for (DynamicFn *dd : *d) dd->fn(dd->data, v);
    }
  }
}

int int64_config(int dyn, int64 *pint, int64 def, cchar *n1, cchar *n2, cchar *n3) {
  char name[1024];
  int result = 0;
  build_name(name, n1, n2, n3);
  pthread_mutex_lock(&config_lock);
  switch (dyn) {
    case DYNAMIC_CONFIG:
      dynamic(name, (char *)pint, int64_dynfn);
    // fall through
    case GET_CONFIG: {
      char *v = config.get(name);
      if (!v) {
        *pint = def;
        RETURN(-1);
      }
      *pint = strtoll(v, 0, 0);
      RETURN(0);
    }
    case SET_CONFIG: {
      char value[1024];
      value[1023] = 0;
      char *v = xlltoa(def, value + 1022);
      MECharChar *e = config.get_internal(name);
      if (e) {
        if (pint) *pint = (int64)strtoll(e->value, 0, 0);
        FREE(e->value);
        e->value = dupstr(v);
        RETURN(0);
      }
      config.put(dupstr(name), dupstr(v));
      RETURN(-1);
    }
  }
Lreturn:
  if (dyn == SET_CONFIG) callback_dynamic(name);
  pthread_mutex_unlock(&config_lock);
  return result;
}

int int_config(int dyn, int *pint, int def, cchar *n1, cchar *n2, cchar *n3) {
  char name[1024];
  int result = 0;
  build_name(name, n1, n2, n3);
  pthread_mutex_lock(&config_lock);
  switch (dyn) {
    case DYNAMIC_CONFIG:
      dynamic(name, (char *)pint, int_dynfn);
    // fall through
    case GET_CONFIG: {
      char *v = config.get(name);
      if (!v) {
        *pint = def;
        RETURN(-1);
      }
      *pint = strtol(v, 0, 0);
      RETURN(0);
    }
    case SET_CONFIG: {
      char value[1024];
      value[1023] = 0;
      char *v = xlltoa(def, value + 1022);
      MECharChar *e = config.get_internal(name);
      if (e) {
        if (pint) *pint = (int)strtol(e->value, 0, 0);
        FREE(e->value);
        e->value = dupstr(v);
        RETURN(0);
      }
      config.put(dupstr(name), dupstr(v));
      RETURN(-1);
    }
  }
Lreturn:
  if (dyn == SET_CONFIG) callback_dynamic(name);
  pthread_mutex_unlock(&config_lock);
  return result;
}

int string_config(int dyn, cchar **pstring, cchar *def, cchar *n1, cchar *n2, cchar *n3) {
  char name[1024];
  int result = 0;
  build_name(name, n1, n2, n3);
  pthread_mutex_lock(&config_lock);
  switch (dyn) {
    case DYNAMIC_CONFIG:
      dynamic(name, (char *)pstring, string_dynfn);
    // fall through
    case GET_CONFIG: {
      char *v = config.get(name);
      if (!v) {
        *pstring = def;
        RETURN(-1);
      }
      *pstring = v;
      RETURN(0);
    }
    case SET_CONFIG: {
      MECharChar *e = config.get_internal(name);
      if (e) {
        if (pstring)
          *pstring = e->value;
        else
          FREE(e->value);
        e->value = dupstr(def);
        RETURN(0);
      }
      config.put(dupstr(name), dupstr(def));
      RETURN(-1);
    }
  }
Lreturn:
  if (dyn == SET_CONFIG) callback_dynamic(name);
  pthread_mutex_unlock(&config_lock);
  return result;
}

static void call_callbacks() {
  pthread_mutex_lock(&config_lock);
  ConfigCallback *cc = callbacks;
  while (cc) {
    cc->pfn(cc->data);
    cc = cc->next;
  }
  pthread_mutex_unlock(&config_lock);
}

void init_config() {
  init_recursive_mutex(&config_lock);
  reinit_config();
}

static void read_config(FILE *fp) {
  char ss[256], *s;
  pthread_mutex_lock(&config_lock);
  while (fgets(ss, 255, fp)) {
    s = trim(ss);
    if (*s == '#' || *s == '/' || *s == 0 || *s == ';') continue;
    char *eq = strchr(s, '=');
    if (!eq) continue;
    *eq++ = 0;
    trim(s);
    eq = trim(eq);
    if (*eq == 0) continue;
    string_config(SET_CONFIG, 0, eq, s);
  }
  pthread_mutex_unlock(&config_lock);
  fclose(fp);
}

static void read_config() {
  char *s = (char *)alloca(strlen(config_filenames) + 1);
  strcpy(s, config_filenames);
  while (s) {
    char *e = strchr(s, ',');
    if (!e) {
      e = s + strlen(s);
    } else {
      *e = 0;
      e++;
    }
#ifndef __CYGWIN__
    wordexp_t p;
    if (wordexp(s, &p, WRDE_NOCMD | WRDE_UNDEF) < 0) continue;
    char **w = p.we_wordv;
    for (int i = 0; i < (int)p.we_wordc; i++) {
      FILE *fp = fopen(w[i], "r");
      if (!fp) continue;
      read_config(fp);
    }
    wordfree(&p);
#else
    glob_t g;
    if (glob(s, GLOB_TILDE | GLOB_BRACE | GLOB_NOMAGIC, NULL, &g) < 0) continue;
    char **w = g.gl_pathv;
    for (int i = 0; i < (int)g.gl_pathc; i++) {
      FILE *fp = fopen(w[i], "r");
      if (!fp) continue;
      read_config(fp);
    }
    globfree(&g);
#endif
    if (*e == 0) break;
    s = e;
  }
}

void write_config(FILE *fp) {
  pthread_mutex_unlock(&config_lock);
  form_Map(MECharChar, m, config) fprintf(fp, "%s = %s\n", m->key, m->value);
  pthread_mutex_lock(&config_lock);
}

int replace_config(cchar *fn) {
  char f[1024];
  strcpy(f, fn);
  char *d = strrchr(f, '/');
  if (!d) return -1;
  strcpy(d + 1, "conftmpXXXXXX");
  int fd = mkstemp(f);
  if (fd < 0) {
    fprintf(stderr, "mkstmp failed %d: %s\n", errno, strerror(errno));
    return fd;
  }
  FILE *tmpf = fdopen(fd, "w");
  write_config(tmpf);
  fclose(tmpf);
  int r = rename(f, fn);
  if (r < 0) {
    fprintf(stderr, "rename failed %d: %s, %s %s\n", errno, strerror(errno), f, fn);
    unlink(f);
  }
  return r;
}


void reinit_config() {
  read_config();
  call_callbacks();
}

void config_callback(config_callback_pfn pfn, void *data) {
  ConfigCallback *cc = new ConfigCallback;
  cc->pfn = pfn;
  cc->data = data;
  cc->next = callbacks;
  callbacks = cc;
}
