/* -*-Mode: c++;-*-
   Copyright (c) 2002-2008 John Plevyak, All Rights Reserved
*/
#include "ifadefs.h"

void ifa_version(char *v) {
  v += sprintf(v, "%d.%d", MAJOR_VERSION, MINOR_VERSION);
  if (strcmp("", BUILD_VERSION)) v += sprintf(v, ".%s", BUILD_VERSION);
}
