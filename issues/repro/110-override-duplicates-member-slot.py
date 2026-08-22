# ifa/issues/110 -- a method override gets a SECOND member slot.
#
# S overrides step() and inherits more(). The override adds a slot
# rather than replacing the inherited one, so `i` sits one field later
# on S than on B -- while more(), shared between both receivers, blind-
# casts to B's layout and reads S's second `step` pointer as `i`.
#
#   CPython: 3 / 4        pyc: no output, hangs forever
#
# Either loop ALONE is correct; it takes both, so that more() is shared
# rather than monomorphized per receiver.

class B:
    def __init__(self, n):
        self.i = 0
        self.n = n
    def more(self):
        return self.i < self.n
    def step(self):
        self.i = self.i + 1

class S(B):
    def step(self):
        self.i = self.i + 2

a = B(3)
while a.more():
    a.step()
print(a.i)

b = S(3)
while b.more():
    b.step()
print(b.i)
