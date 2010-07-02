/* -*-Mode: c++;-*-
   Copyright (c) 2004-2008 John Plevyak, All Rights Reserved
*/
#ifndef _dead_h_
#define _dead_h_

int mark_live_code(FA *fa);
void mark_live_types(FA *fa);
void mark_live_funs(FA *fa);

#endif
