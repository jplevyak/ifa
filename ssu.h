/* -*-Mode: c++;-*-
   Copyright (c) 2003-2008 John Plevyak, All Rights Reserved
*/
#ifndef _ssu_h_
#define _ssu_h_

class SSUVar : public gc {
 public:
  Vec<PNode *> defs, uses;
  Vec<PNode *> phis, phys;
};

#endif
