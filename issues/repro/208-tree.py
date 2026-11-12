#!/usr/bin/env python3
# 208-tree.py FILE.folded [min%] [maxdepth] [root]: inclusive call tree from NAME.folded
import os, re, sys
S = os.path.dirname(os.path.abspath(__file__))
name = sys.argv[1]; minp = float(sys.argv[2]) if len(sys.argv) > 2 else 2.0
maxd = int(sys.argv[3]) if len(sys.argv) > 3 else 9
root = sys.argv[4] if len(sys.argv) > 4 else None
rows = [l.rstrip("\n").split("\t") for l in open(name)]
rows = [(float(t), [re.sub(r"<.*>", "<>", f) for f in s.split(";")]) for t, s in rows if s]
n = len(rows); per = rows[-1][0] / n
SKIP = {"_start", "__libc_start_main_impl", "__libc_start_call_main", "??"}
tree = {}
for t, st in rows:
    st = [f for f in st if f not in SKIP]
    if root:
        if root not in st: continue
        st = st[st.index(root):]
    # collapse direct recursion
    c = []
    for f in st:
        if not c or c[-1] != f: c.append(f)
    node = tree
    for f in c:
        node = node.setdefault(f, [0, {}])
        node[0] += 1
        node = node[1]
def show(node, d):
    for f, (c, ch) in sorted(node.items(), key=lambda x: -x[1][0]):
        if 100 * c / n < minp or d > maxd: continue
        print(f"{c*per:7.1f} s {100*c/n:5.1f}% {'  '*d}{f[:70]}")
        show(ch, d + 1)
show(tree, 0)
