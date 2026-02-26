#ifndef _pdb_H_
#define _pdb_H_

class FA;
class Fun;
class IF1;
struct LoopGraph;

// Program Database, stores the set of all functions

class PDB : public gc {
 public:
  IF1 *if1;
  FA *fa;
  Vec<Fun *> funs;
  LoopGraph *loops;

  Sym *find_global(char *);
  void add(Fun *f);
  int clone(FA *fa, Fun *top);

  PDB(IF1 *aif1);
};

extern PDB *pdb;

#endif
