#!/usr/bin/env python3
"""ifa/issues/105: reject candidates that reference undefined names.

CPython resolves names at RUNTIME, so a reduced program can exit 0 while
still containing calls to names whose definitions the reducer deleted --
they simply sit in code paths that never execute. pyc is a whole-program
static analyser: it analyses those paths anyway, and it accepts undefined
names silently, so it infers over garbage there. That is invisible to any
runtime-based oracle and can fabricate exactly the degenerate types under
investigation.

Exit 0 iff every Name load resolves to a builtin, import, assignment,
parameter, def/class, comprehension target, or except/with binding.
"""
import ast, builtins, sys

src = open(sys.argv[1]).read()
tree = ast.parse(src)

defined = set(dir(builtins)) | {"__name__", "__file__", "__doc__"}
for n in ast.walk(tree):
    if isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
        defined.add(n.name)
    elif isinstance(n, ast.Name) and isinstance(n.ctx, (ast.Store, ast.Del)):
        defined.add(n.id)
    elif isinstance(n, ast.arg):
        defined.add(n.arg)
    elif isinstance(n, (ast.Import, ast.ImportFrom)):
        for a in n.names:
            defined.add((a.asname or a.name).split(".")[0])
    elif isinstance(n, ast.ExceptHandler) and n.name:
        defined.add(n.name)
    elif isinstance(n, ast.Global) or isinstance(n, ast.Nonlocal):
        defined.update(n.names)

used = {n.id for n in ast.walk(tree)
        if isinstance(n, ast.Name) and isinstance(n.ctx, ast.Load)}

undef = sorted(used - defined)
if undef:
    print(" ".join(undef), file=sys.stderr)
    sys.exit(1)
sys.exit(0)
