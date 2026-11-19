#!/usr/bin/env python3
# 208-attr.py FILE.folded [top] [under=FUNC]: time by nearest non-utility frame ("owner"), collector kept apart
import os, re, sys, collections
S = os.path.dirname(os.path.abspath(__file__))
name = sys.argv[1]; top = int(sys.argv[2]) if len(sys.argv) > 2 else 30
under = sys.argv[3] if len(sys.argv) > 3 else None
rows = [l.rstrip("\n").split("\t") for l in open(name)]
rows = [(float(t), [re.sub(r"<.*>", "<>", f) for f in s.split(";")]) for t, s in rows if s]
n = len(rows); per = rows[-1][0] / n
UTIL = re.compile(r"^(Vec<>|Map<>|HashMap<>|ChainHash<>|Accum<>|PointerHash<>|BlockHash<>|GC_|__mem|__GI_|std::|operator new|DefaultAlloc|\?\?|canonical_in_set|qsort|_int_|malloc|calloc|free|cfree|Intervals::|StringChainHash|__str|__libc|unlink_chunk|get_AEdges_slot)")
own = collections.Counter(); gco = collections.Counter(); pair = collections.Counter()
for t, st in rows:
    if under and under not in st: continue
    isgc = any(f in ("GC_try_to_collect_inner", "GC_collect_a_little_inner") for f in st)
    o = next((f for f in reversed(st) if not UTIL.match(f)), "?")
    (gco if isgc else own)[o] += 1
    if not isgc:
        leaf = st[-1] if UTIL.match(st[-1]) else "(self)"
        pair[(o, leaf)] += 1
tot = sum(own.values()) + sum(gco.values())
print(f"{name}{' under '+under if under else ''}: {tot*per:.1f} s; collector {sum(gco.values())*per:.1f} s")
for f, c in own.most_common(top):
    leaves = ", ".join(f"{l} {k*per:.1f}" for (o, l), k in sorted(pair.items(), key=lambda x: -x[1]) if o == f and k * per >= 0.02 * tot * per)[:110]
    print(f"  {c*per:7.1f} s {100*c/n:5.1f}%  {f[:44]:44s} {leaves}")
print("  collections triggered from:", ", ".join(f"{f} {c*per:.1f}" for f, c in gco.most_common(6)))
