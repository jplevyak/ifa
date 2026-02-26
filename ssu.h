#ifndef _ssu_h_
#define _ssu_h_

class SSUVar : public gc {
 public:
  Vec<PNode *> defs, uses;
  Vec<PNode *> phis, phys;
};

#endif
