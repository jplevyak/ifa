/* -*-Mode: c++;-*-
   Copyright (c) 2004-2008 John Plevyak, All Rights Reserved
*/
#ifndef _graph_H_
#define _graph_H_

#include "ifa.h"

class FA;

void graph(FA *fa, cchar *fn);
void graph_contours(FA *fa, cchar *fn);
void graph_node(FILE *fp, void *id, cchar *label, int options = 0);
void graph_edge(FILE *fp, void *a, void *b, int options = 0);

extern char graph_fun[80];
extern char graph_var[80];
extern int graph_type;
extern int fgraph_frequencies;
extern int fgraph_constants;

#endif
