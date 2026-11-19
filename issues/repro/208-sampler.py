# Stack sampler for a machine where perf is blocked and gdb cannot attach.
#   SAMPLE_OUT=x.folded SAMPLE_INTERVAL=0.1 gdb -batch -nx -x 208-sampler.py --args ./pyc -D . prog.py
# Spawns the inferior, interrupts it every SAMPLE_INTERVAL seconds and writes
# "seconds<TAB>root;...;leaf" per sample to SAMPLE_OUT. Read it with
# 208-attr.py (time by owning function) and 208-tree.py (inclusive tree).
import gdb, os, signal, threading, time

interval = float(os.environ.get("SAMPLE_INTERVAL", "0.25"))
out = open(os.environ.get("SAMPLE_OUT", "samples.folded"), "w")
depth = int(os.environ.get("SAMPLE_DEPTH", "64"))

gdb.execute("set pagination off")
gdb.execute("set confirm off")
gdb.execute("set print thread-events off")
gdb.execute("handle SIGINT stop print nopass")
gdb.execute("handle SIGPWR nostop noprint pass")
gdb.execute("handle SIGXCPU nostop noprint pass")
gdb.execute("handle SIGCHLD nostop noprint pass")

state = {"pid": 0, "run": True}


def ticker():
    while state["run"]:
        time.sleep(interval)
        pid = state["pid"]
        if pid and state["run"]:
            try:
                os.kill(pid, signal.SIGINT)
            except OSError:
                pass


def stack():
    names = []
    f = gdb.newest_frame()
    n = 0
    while f is not None and n < depth:
        try:
            nm = f.name()
        except gdb.error:
            nm = None
        names.append(nm or "??")
        try:
            f = f.older()
        except gdb.error:
            break
        n += 1
    return ";".join(reversed(names))


gdb.execute("starti")
state["pid"] = gdb.selected_inferior().pid
t = threading.Thread(target=ticker, daemon=True)
t.start()
t0 = time.time()
n = 0
while True:
    try:
        gdb.execute("continue", to_string=True)
    except gdb.error:
        break
    inf = gdb.selected_inferior()
    if not inf.is_valid() or inf.pid == 0:
        break
    try:
        # sample the main thread: the compiler is single-threaded
        gdb.execute("thread 1", to_string=True)
        out.write("%.2f\t%s\n" % (time.time() - t0, stack()))
        n += 1
    except gdb.error:
        pass
state["run"] = False
out.close()
gdb.write("sampler: %d samples in %.1f s\n" % (n, time.time() - t0))
