#include <glob.h>
#include <net/if.h>
#include <signal.h>
#include <sys/ioctl.h>
#include "common.h"

int buf_read(int fd, char **buf, int *len) {
  struct stat sb;
  memset(&sb, 0, sizeof(sb));
  fstat(fd, &sb);
  *len = sb.st_size;
  *buf = (char *)MALLOC(*len + 2);
  (*buf)[*len] = 0;     /* terminator */
  (*buf)[*len + 1] = 0; /* sentinal */
  assert(read(fd, *buf, *len) == *len);
  return *len;
}

int buf_read(cchar *pathname, char **buf, int *len) {
  int fd;

  *buf = 0;
  *len = 0;
  fd = open(pathname, O_RDONLY);
  if (fd <= 0) return -1;
  int ret = buf_read(fd, buf, len);
  close(fd);
  return ret;
}

void fail(cchar *str, ...) {
  char nstr[256];
  va_list ap;

  fflush(stdout);
  fflush(stderr);

  va_start(ap, str);
  snprintf(nstr, 255, "fail: %s\n", str);
  vfprintf(stderr, nstr, ap);
  va_end(ap);
  exit(1);
}

void error(cchar *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  fflush(stderr);
  return;
}

char *trim(char *s, char *e) {
  if (!e) e = s + strlen(s) - 1;
  while (s < e && isspace(*s)) s++;
  for (; e > s; e--) {
    if (isspace(*e))
      *e = 0;
    else
      break;
  }
  return s;
}

bool str2bool(char *s) {
  if (s && tolower(*s) == 't') return true;
  return false;
}

int str2strVec(char *s, Vec<char *> &v, const char *delimitor) {
  v.clear();
  char *x = s;
  int l = strlen(delimitor);
  while (*s) {
    if (memchr(delimitor, *s, l)) {
      *s = 0;
      v.add(dupstr(x));
      s++;
      x = s;
    } else
      s++;
  }
  return 0;
}

int appendfile2fp(FILE *fp, char *filename) {
  char *b;
  int l;
  if (buf_read(filename, &b, &l) < 0) return -1;
  assert(fwrite(b, l, 1, fp) == (size_t)l);
  FREE(b);
  return 0;
}

int expand_filename(char *p, int len) {
  (void)len;
  if (p[0] == '~') {
    char *home = getenv("HOME");
    if (home) {
      if (strlen(home) + strlen(p) + 1 > (uint)len) return -1;
      memmove(p + strlen(home) - 1, p, (int)strlen(p) + 1);
      memcpy(p, home, strlen(home));
      p[strlen(home)] = '/';
    }
  }
  return 0;
}

int xmkdir(cchar *p, mode_t mode) {
  char pp[512];
  strcpy(pp, p);
  if (pp[strlen(pp) - 1] == '/') pp[strlen(pp) - 1] = 0;
  if (pp[0] == '~') {
    char *home = getenv("HOME");
    if (home) {
      memmove(pp + strlen(home) - 1, pp, strlen(pp) + 1);
      memcpy(pp, home, strlen(home));
      pp[strlen(home)] = '/';
    }
  }
  return mkdir(pp, mode);
}

#define ESC(_c) \
  *ss++ = '\\'; \
  *ss++ = _c;   \
  break;
char *escape_string(cchar *s) {
  char *ss = (char *)MALLOC((strlen(s) + 3) * 4), *sss = ss;
  *ss++ = '\"';
  for (; *s; s++) {
    switch (*s) {
      case '\b':
        ESC('b');
      case '\f':
        ESC('f');
      case '\n':
        ESC('n');
      case '\r':
        ESC('r');
      case '\t':
        ESC('t');
      case '\v':
        ESC('v');
      case '\a':
        ESC('a');
      case '\\':
      case '\"':
        *ss++ = '\\';
        *ss++ = *s;
        break;

      default:
        if (isprint(*s)) {
          *ss++ = *s;
        } else {
          *ss++ = '\\';
          *ss++ = 'x';
          *ss++ = tohex2(*s);
          *ss++ = tohex1(*s);
        }
        break;
    }
  }
  *ss++ = '\"';
  *ss = 0;
  return sss;
}

char *quote_string(cchar *s) {
  int l = strlen(s);
  char *ss = (char *)MALLOC(l + 3), *sss = ss;
  *ss++ = '"';
  strcpy(ss, s);
  ss += l;
  *ss++ = '"';
  *ss++ = 0;
  return sss;
}

// could be faster
char *dupstrs(cchar *p1, cchar *p2, cchar *p3, cchar *p4) {
  int l = 1 + strlen(p1) + (p2 ? strlen(p2) : 0) + (p3 ? strlen(p3) : 0) + (p4 ? strlen(p4) : 0);
  char *s = (char *)MALLOC(l);
  strcpy(s, p1);
  if (p2) strcat(s, p2);
  if (p3) strcat(s, p3);
  if (p4) strcat(s, p4);
  return s;
}

bool is_regular_file(cchar *p1, cchar *p2, cchar *p3, cchar *p4) {
  char f[PATH_MAX];
  cchar *filename = p1;
  if (p2) {
    strcpy(f, p1);
    strcat(f, p2);
    if (p3) strcat(f, p3);
    if (p4) strcat(f, p4);
    filename = f;
  }
  struct stat sb;
  if (stat(filename, &sb) < 0) return false;
  return ((sb.st_mode & S_IFMT) == S_IFREG);
}

bool is_directory(cchar *p1, cchar *p2, cchar *p3, cchar *p4) {
  char f[PATH_MAX];
  cchar *filename = p1;
  if (p2) {
    strcpy(f, p1);
    strcat(f, p2);
    if (p3) strcat(f, p3);
    if (p4) strcat(f, p4);
    filename = f;
  }
  struct stat sb;
  if (stat(filename, &sb) < 0) return false;
  return ((sb.st_mode & S_IFMT) == S_IFDIR);
}

bool file_exists(cchar *p1, cchar *p2, cchar *p3, cchar *p4) {
  char f[PATH_MAX];
  cchar *filename = p1;
  if (p2) {
    strcpy(f, p1);
    strcat(f, p2);
    if (p3) strcat(f, p3);
    if (p4) strcat(f, p4);
    filename = f;
  }
  struct stat sb;
  if (stat(filename, &sb) < 0) return false;
  return 1;
}

int getifaddrname(struct sockaddr_in *addr, int *pmtu, char *ifname, int ifname_len) {
  int fd;
  struct ifconf ifc;
  struct ifreq ibuf[16], *ifr, *ifrend;

  if ((fd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) return -1;

  memset(ibuf, 0, sizeof(ibuf));
  ifc.ifc_len = sizeof(ibuf);
  ifc.ifc_buf = (caddr_t)ibuf;
  if (ioctl(fd, SIOCGIFCONF, (char *)&ifc) < 0 || ifc.ifc_len < (int)sizeof(struct ifreq)) return -1;
  ifr = ibuf;
  ifrend = (struct ifreq *)((char *)ibuf + ifc.ifc_len);

  for (; ifr < ifrend; ifr++) {
    if (ioctl(fd, SIOCGIFFLAGS, (char *)ifr) < 0 || !(ifr->ifr_flags & IFF_UP) || ifr->ifr_flags & IFF_LOOPBACK)
      continue;
    // if I have already seen this interface, skip it
    if (ifname && ifname_len && ifname[0] && !strstr(ifr->ifr_name, ifname)) continue;
    if (ifname && ifname_len) {
      strncpy(ifname, ifr->ifr_name, ifname_len - 1);
      ifname[ifname_len - 1] = 0;
    }
    if (ioctl(fd, SIOCGIFADDR, (char *)ifr) < 0) return -1;
    *addr = *(struct sockaddr_in *)&ifr->ifr_addr;
    if (pmtu) {
      if (ioctl(fd, SIOCGIFMTU, (char *)ifr) < 0)
        *pmtu = -1;
      else {
#ifdef __sun__
        *pmtu = ifr->ifr_metric;
#else
        *pmtu = ifr->ifr_mtu;
#endif
      }
    }
    close(fd);
    return 0;
  }
  close(fd);
  return -1;
}
