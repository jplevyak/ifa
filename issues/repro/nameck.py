#!/usr/bin/env python3
"""ifa/issues/105: reject candidates that read names bound nowhere in scope.

SCOPE-AWARE. The first version collected every Store name in the file
into one flat set, so a name assigned in *another function* counted as
defined -- and a 212-line reduction passed it while `parse()` read `A`,
`tolabel` and `unary`, whose local assignments the reducer had deleted.
CPython would raise NameError on those paths; they simply never execute.

For each function: a Load must resolve to a local binding, an enclosing
function's binding, a module-level binding, or a builtin.

Exit 0 iff every function is clean.
"""
import ast, builtins, sys


def _walk_scope(node):
    """Yield nodes in `node` WITHOUT descending into nested function or
    class bodies -- those are separate scopes. Walking into them is the
    bug the flat version had: module bindings picked up every local
    assignment in the file, so a name assigned only inside some other
    function counted as module-level."""
    stack = list(ast.iter_child_nodes(node))
    while stack:
        x = stack.pop()
        yield x
        if isinstance(x, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            continue  # separate scope; its NAME is bound here, its body is not
        stack.extend(ast.iter_child_nodes(x))


def bindings(node, include_nested_defs=True):
    b = set()
    for x in _walk_scope(node):
        if isinstance(x, ast.Name) and isinstance(x.ctx, (ast.Store, ast.Del)):
            b.add(x.id)
        elif isinstance(x, ast.arg):
            b.add(x.arg)
        elif isinstance(x, ast.ExceptHandler) and x.name:
            b.add(x.name)
        elif isinstance(x, (ast.Global, ast.Nonlocal)):
            b.update(x.names)
        elif isinstance(x, (ast.Import, ast.ImportFrom)):
            for a in x.names:
                b.add((a.asname or a.name).split(".")[0])
        elif isinstance(x, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            b.add(x.name)
    # a function's own parameters live on the node, not among its children
    if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
        for a in ast.walk(node.args):
            if isinstance(a, ast.arg):
                b.add(a.arg)
    return b


def main(path):
    tree = ast.parse(open(path).read())
    module = set(dir(builtins)) | {"__name__", "__file__", "__doc__"}
    module |= bindings(tree)

    bad = {}
    funcs = [x for x in ast.walk(tree)
             if isinstance(x, (ast.FunctionDef, ast.AsyncFunctionDef))]
    for fn in funcs:
        # enclosing scopes: any function that lexically contains this one
        enclosing = set()
        for other in funcs:
            if other is not fn and any(n is fn for n in ast.walk(other)):
                enclosing |= bindings(other)
        local = bindings(fn)
        used = {x.id for x in _walk_scope(fn)
                if isinstance(x, ast.Name) and isinstance(x.ctx, ast.Load)}
        u = sorted(used - local - enclosing - module)
        if u:
            bad[fn.name] = u

    if bad:
        for k, v in bad.items():
            print(f"{k}: {' '.join(v)}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1]))
