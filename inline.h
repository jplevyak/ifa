/* -*-Mode: c++;-*-
   Copyright (c) 2003-2008 John Plevyak, All Rights Reserved
*/
#ifndef _inline_H_
#define _inline_H_

class FA;

int frequency_estimation(FA *fa);  // static estimates in place of profiling
int simple_inlining(FA *fa);

#endif
