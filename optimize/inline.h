#ifndef _inline_H_
#define _inline_H_

#include "ifadefs.h"

class FA;
class Fun;
class PNode;

int frequency_estimation(FA *fa);  // static estimates in place of profiling
int simple_inlining(FA *fa);

// Sidecar record of what simple_inlining did, for testing.
// Events are collected only if `inline_events_enable()` has been
// called; production builds skip the bookkeeping entirely.
enum InlineEventKind {
  INLINE_SINGLE_SEND,   // call site replaced with the callee's single body SEND
  INLINE_IDENTITY,      // identity-fun call converted to MOVE
  INLINE_CLOSURE,       // closure-create + call collapsed to direct call
};

struct InlineEvent {
  InlineEventKind kind;
  Fun *caller;     // the Fun whose code was rewritten
  PNode *pnode;    // the call-site PNode (post-rewrite)
  Fun *callee;     // the inlined fun, where meaningful (null for INLINE_CLOSURE)
};

void inline_events_enable();
void inline_events_disable();
void inline_events_reset();
const Vec<InlineEvent *> &inline_events_get();

#endif
