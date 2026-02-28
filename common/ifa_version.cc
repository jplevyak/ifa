#include "ifadefs.h"

void ifa_version(char *v, int size) {
  int n = snprintf(v, size, "%d.%d", MAJOR_VERSION, MINOR_VERSION);
  if (strcmp("", BUILD_VERSION)) snprintf(v + n, size - n, ".%s", BUILD_VERSION);
}
