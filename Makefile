# Makefile for IFA

all: defaulttarget

USE_PLIB=1
MODULE=ifa
#DEBUG=1
OPTIMIZE=1
#PROFILE=1
USE_GC=1
#LEAK_DETECT=1
#VALGRIND=1
TEST_EXEC=ifa_tests

include ../plib/Makefile

MAJOR=0
MINOR=5

CFLAGS += -I../plib
ifdef USE_GC
LIBS += -L../plib -lplib_gc -ldparse_gc -lgc
else
LIBS += -L../plib -lplib -ldparse
endif
ifneq ($(OS_TYPE),CYGWIN)
ifneq ($(OS_TYPE),Darwin)
  LIBS += -lrt
endif
endif

AUX_FILES = $(MODULE)/index.html $(MODULE)/manual.html $(MODULE)/faq.html $(MODULE)/ifa.1 $(MODULE)/ifa.cat
TAR_FILES = $(AUX_FILES) $(TEST_FILES) 

LIB_SRCS = ast.cc builtin.cc cdb.cc cfg.cc cg.cc clone.cc dead.cc dom.cc fa.cc \
	fail.cc fun.cc graph.cc html.cc if1.cc ifa.cc inline.cc \
	ifalog.cc loop.cc num.cc pattern.cc pdb.cc pnode.cc prim.cc prim_data.cc \
	main.cc ssu.cc sym.cc var.cc ifa_version.cc
LIB_OBJS = $(LIB_SRCS:%.cc=%.o)

IFA_DEPEND_SRCS = main.cc parse.cc scope.cc make_ast.cc ast_to_if1.cc
IFA_SRCS = $(IFA_DEPEND_SRCS) v.g.d_parser.cc python.g.d_parser.cc 
IFA_OBJS = $(IFA_SRCS:%.cc=%.o)

EXECUTABLE_FILES = ifa
ifdef USE_GC
LIBRARY = libifa_gc.a
else
LIBRARY = libifa.a
endif
INSTALL_LIBRARIES = $(LIBRARY)
#INCLUDES =
MANPAGES = ifa.1

CLEAN_FILES += *.cat tests/*.out tests/*.c

ifeq ($(OS_TYPE),CYGWIN)
EXECUTABLES = $(EXECUTABLE_FILES:%=%.exe)
IFA = ifa.exe
MAKE_PRIMS = make_prims.exe
else
EXECUTABLES = $(EXECUTABLE_FILES)
IFA = ifa
MAKE_PRIMS = make_prims
endif

ALL_SRCS = $(IFA_SRCS) $(LIB_SRCS)

defaulttarget: $(EXECUTABLES) $(LIBRARY) ifa.cat

install:
	cp $(EXECUTABLES) $(PREFIX)/bin
	cp $(MANPAGES) $(PREFIX)/man/man1
	cp $(INCLUDES) $(PREFIX)/include
	cp $(INSTALL_LIBRARIES) $(PREFIX)/lib

deinstall:
	rm $(EXECUTABLES:%=$(PREFIX)/bin/%)
	rm $(MANPAGES:%=$(PREFIX)/man/man1/%)
	rm $(INCLUDES:%=$(PREFIX)/include/%)
	rm $(INSTALL_LIBRARIES:%=$(PREFIX)/lib/%)

$(IFA): $(IFA_OBJS) $(LIB_OBJS) $(LIBRARIES)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^ $(LIBS) 

$(LIBRARY): $(LIB_OBJS)
	ar $(AR_FLAGS) $@ $^

$(MAKE_PRIMS): make_prims.cc
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^ $(LIBS) 

ifa.cat: ifa.1
	rm -f ifa.cat
	nroff -man ifa.1 | sed -e 's/.//g' > ifa.cat

%.g.d_parser.cc: %.g
	make_dparser -v -Xcc -I $<

main.o: LICENSE.i COPYRIGHT.i

ifa_version.o: Makefile ifa_version.cc
	$(CC) $(CFLAGS) $(VERSIONCFLAGS) -c ifa_version.cc

-include .depend
# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

main.o: main.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h ifa_version.h ../plib/log.h parse.h ../plib/unit.h LICENSE.i \
  COPYRIGHT.i
parse.o: parse.cc ifadefs.h ../plib/plib.h ../plib/arg.h \
  ../plib/barrier.h ../plib/config.h ../plib/dlmalloc.h \
  ../plib/freelist.h ../plib/defalloc.h ../plib/list.h ../plib/log.h \
  ../plib/vec.h ../plib/map.h ../plib/threadpool.h ../plib/misc.h \
  ../plib/util.h ../plib/conn.h ../plib/md5.h ../plib/mt64.h \
  ../plib/persist.h ../plib/prime.h ../plib/service.h ../plib/timer.h \
  ../plib/unit.h ast.h ifa.h ifalog.h if1.h sym.h num.h prim_data.h \
  code.h builtin.h builtin_symbols.h fail.h fa.h prim.h var.h pnode.h \
  fun.h pdb.h clone.h cg.h parse_structs.h parse.h make_ast.h \
  ast_to_if1.h ../plib/map.h scope.h ast_kinds.h ../plib/log.h
scope.o: scope.cc ifadefs.h ../plib/plib.h ../plib/arg.h \
  ../plib/barrier.h ../plib/config.h ../plib/dlmalloc.h \
  ../plib/freelist.h ../plib/defalloc.h ../plib/list.h ../plib/log.h \
  ../plib/vec.h ../plib/map.h ../plib/threadpool.h ../plib/misc.h \
  ../plib/util.h ../plib/conn.h ../plib/md5.h ../plib/mt64.h \
  ../plib/persist.h ../plib/prime.h ../plib/service.h ../plib/timer.h \
  ../plib/unit.h ast.h ifa.h ifalog.h if1.h sym.h num.h prim_data.h \
  code.h builtin.h builtin_symbols.h fail.h fa.h prim.h var.h pnode.h \
  fun.h pdb.h clone.h cg.h scope.h
make_ast.o: make_ast.cc ifadefs.h ../plib/plib.h ../plib/arg.h \
  ../plib/barrier.h ../plib/config.h ../plib/dlmalloc.h \
  ../plib/freelist.h ../plib/defalloc.h ../plib/list.h ../plib/log.h \
  ../plib/vec.h ../plib/map.h ../plib/threadpool.h ../plib/misc.h \
  ../plib/util.h ../plib/conn.h ../plib/md5.h ../plib/mt64.h \
  ../plib/persist.h ../plib/prime.h ../plib/service.h ../plib/timer.h \
  ../plib/unit.h ast.h ifa.h ifalog.h if1.h sym.h num.h prim_data.h \
  code.h builtin.h builtin_symbols.h fail.h fa.h prim.h var.h pnode.h \
  fun.h pdb.h clone.h cg.h ast_to_if1.h ../plib/map.h scope.h ast_kinds.h \
  parse.h parse_structs.h make_ast.h
ast_to_if1.o: ast_to_if1.cc ifadefs.h ../plib/plib.h ../plib/arg.h \
  ../plib/barrier.h ../plib/config.h ../plib/dlmalloc.h \
  ../plib/freelist.h ../plib/defalloc.h ../plib/list.h ../plib/log.h \
  ../plib/vec.h ../plib/map.h ../plib/threadpool.h ../plib/misc.h \
  ../plib/util.h ../plib/conn.h ../plib/md5.h ../plib/mt64.h \
  ../plib/persist.h ../plib/prime.h ../plib/service.h ../plib/timer.h \
  ../plib/unit.h ast.h ifa.h ifalog.h if1.h sym.h num.h prim_data.h \
  code.h builtin.h builtin_symbols.h fail.h fa.h prim.h var.h pnode.h \
  fun.h pdb.h clone.h cg.h parse_structs.h ast_to_if1.h ../plib/map.h \
  scope.h ast_kinds.h html.h ../plib/vec.h graph.h
v.g.d_parser.o: v.g.d_parser.cc ifadefs.h ../plib/plib.h ../plib/arg.h \
  ../plib/barrier.h ../plib/config.h ../plib/dlmalloc.h \
  ../plib/freelist.h ../plib/defalloc.h ../plib/list.h ../plib/log.h \
  ../plib/vec.h ../plib/map.h ../plib/threadpool.h ../plib/misc.h \
  ../plib/util.h ../plib/conn.h ../plib/md5.h ../plib/mt64.h \
  ../plib/persist.h ../plib/prime.h ../plib/service.h ../plib/timer.h \
  ../plib/unit.h ast.h ifa.h ifalog.h if1.h sym.h num.h prim_data.h \
  code.h builtin.h builtin_symbols.h fail.h fa.h prim.h var.h pnode.h \
  fun.h pdb.h clone.h cg.h parse_structs.h ast_to_if1.h ../plib/map.h \
  scope.h ast_kinds.h make_ast.h
python.g.d_parser.o: python.g.d_parser.cc ifadefs.h ../plib/plib.h \
  ../plib/arg.h ../plib/barrier.h ../plib/config.h ../plib/dlmalloc.h \
  ../plib/freelist.h ../plib/defalloc.h ../plib/list.h ../plib/log.h \
  ../plib/vec.h ../plib/map.h ../plib/threadpool.h ../plib/misc.h \
  ../plib/util.h ../plib/conn.h ../plib/md5.h ../plib/mt64.h \
  ../plib/persist.h ../plib/prime.h ../plib/service.h ../plib/timer.h \
  ../plib/unit.h ast.h ifa.h ifalog.h if1.h sym.h num.h prim_data.h \
  code.h builtin.h builtin_symbols.h fail.h fa.h prim.h var.h pnode.h \
  fun.h pdb.h clone.h cg.h parse_structs.h ast_to_if1.h ../plib/map.h \
  scope.h ast_kinds.h make_ast.h python.g.d_parser.h
ast.o: ast.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h pattern.h
builtin.o: builtin.cc builtin.h builtin_symbols.h
cdb.o: cdb.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h cdb.h
cfg.o: cfg.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h
cg.o: cg.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h pattern.h
clone.o: clone.cc ifadefs.h ../plib/plib.h ../plib/arg.h \
  ../plib/barrier.h ../plib/config.h ../plib/dlmalloc.h \
  ../plib/freelist.h ../plib/defalloc.h ../plib/list.h ../plib/log.h \
  ../plib/vec.h ../plib/map.h ../plib/threadpool.h ../plib/misc.h \
  ../plib/util.h ../plib/conn.h ../plib/md5.h ../plib/mt64.h \
  ../plib/persist.h ../plib/prime.h ../plib/service.h ../plib/timer.h \
  ../plib/unit.h ast.h ifa.h ifalog.h if1.h sym.h num.h prim_data.h \
  code.h builtin.h builtin_symbols.h fail.h fa.h prim.h var.h pnode.h \
  fun.h pdb.h clone.h cg.h pattern.h ../plib/log.h
dead.o: dead.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h pattern.h dom.h
dom.o: dom.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h dom.h
fa.o: fa.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h pattern.h graph.h ../plib/log.h ../plib/timer.h
fail.o: fail.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h
fun.o: fun.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h pattern.h
graph.o: graph.cc ifadefs.h ../plib/plib.h ../plib/arg.h \
  ../plib/barrier.h ../plib/config.h ../plib/dlmalloc.h \
  ../plib/freelist.h ../plib/defalloc.h ../plib/list.h ../plib/log.h \
  ../plib/vec.h ../plib/map.h ../plib/threadpool.h ../plib/misc.h \
  ../plib/util.h ../plib/conn.h ../plib/md5.h ../plib/mt64.h \
  ../plib/persist.h ../plib/prime.h ../plib/service.h ../plib/timer.h \
  ../plib/unit.h ast.h ifa.h ifalog.h if1.h sym.h num.h prim_data.h \
  code.h builtin.h builtin_symbols.h fail.h fa.h prim.h var.h pnode.h \
  fun.h pdb.h clone.h cg.h dom.h loop.h graph.h pattern.h ../plib/log.h
html.o: html.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h pattern.h ../plib/log.h
if1.o: if1.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h ../plib/log.h
ifa.o: ifa.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h dead.h dom.h inline.h html.h ../plib/vec.h graph.h ../plib/log.h
inline.o: inline.cc ifadefs.h ../plib/plib.h ../plib/arg.h \
  ../plib/barrier.h ../plib/config.h ../plib/dlmalloc.h \
  ../plib/freelist.h ../plib/defalloc.h ../plib/list.h ../plib/log.h \
  ../plib/vec.h ../plib/map.h ../plib/threadpool.h ../plib/misc.h \
  ../plib/util.h ../plib/conn.h ../plib/md5.h ../plib/mt64.h \
  ../plib/persist.h ../plib/prime.h ../plib/service.h ../plib/timer.h \
  ../plib/unit.h ast.h ifa.h ifalog.h if1.h sym.h num.h prim_data.h \
  code.h builtin.h builtin_symbols.h fail.h fa.h prim.h var.h pnode.h \
  fun.h pdb.h clone.h cg.h inline.h loop.h
ifalog.o: ifalog.cc ifadefs.h ../plib/plib.h ../plib/arg.h \
  ../plib/barrier.h ../plib/config.h ../plib/dlmalloc.h \
  ../plib/freelist.h ../plib/defalloc.h ../plib/list.h ../plib/log.h \
  ../plib/vec.h ../plib/map.h ../plib/threadpool.h ../plib/misc.h \
  ../plib/util.h ../plib/conn.h ../plib/md5.h ../plib/mt64.h \
  ../plib/persist.h ../plib/prime.h ../plib/service.h ../plib/timer.h \
  ../plib/unit.h ast.h ifa.h ifalog.h if1.h sym.h num.h prim_data.h \
  code.h builtin.h builtin_symbols.h fail.h fa.h prim.h var.h pnode.h \
  fun.h pdb.h clone.h cg.h ../plib/log.h
loop.o: loop.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h loop.h dom.h
num.o: num.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h cast_code.cc
pattern.o: pattern.cc ifadefs.h ../plib/plib.h ../plib/arg.h \
  ../plib/barrier.h ../plib/config.h ../plib/dlmalloc.h \
  ../plib/freelist.h ../plib/defalloc.h ../plib/list.h ../plib/log.h \
  ../plib/vec.h ../plib/map.h ../plib/threadpool.h ../plib/misc.h \
  ../plib/util.h ../plib/conn.h ../plib/md5.h ../plib/mt64.h \
  ../plib/persist.h ../plib/prime.h ../plib/service.h ../plib/timer.h \
  ../plib/unit.h ast.h ifa.h ifalog.h if1.h sym.h num.h prim_data.h \
  code.h builtin.h builtin_symbols.h fail.h fa.h prim.h var.h pnode.h \
  fun.h pdb.h clone.h cg.h pattern.h ../plib/log.h
pdb.o: pdb.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h
pnode.o: pnode.cc ifadefs.h ../plib/plib.h ../plib/arg.h \
  ../plib/barrier.h ../plib/config.h ../plib/dlmalloc.h \
  ../plib/freelist.h ../plib/defalloc.h ../plib/list.h ../plib/log.h \
  ../plib/vec.h ../plib/map.h ../plib/threadpool.h ../plib/misc.h \
  ../plib/util.h ../plib/conn.h ../plib/md5.h ../plib/mt64.h \
  ../plib/persist.h ../plib/prime.h ../plib/service.h ../plib/timer.h \
  ../plib/unit.h ast.h ifa.h ifalog.h if1.h sym.h num.h prim_data.h \
  code.h builtin.h builtin_symbols.h fail.h fa.h prim.h var.h pnode.h \
  fun.h pdb.h clone.h cg.h
prim.o: prim.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h
prim_data.o: prim_data.cc prim_data_incs.h ifadefs.h ../plib/plib.h \
  ../plib/arg.h ../plib/barrier.h ../plib/config.h ../plib/dlmalloc.h \
  ../plib/freelist.h ../plib/defalloc.h ../plib/list.h ../plib/log.h \
  ../plib/vec.h ../plib/map.h ../plib/threadpool.h ../plib/misc.h \
  ../plib/util.h ../plib/conn.h ../plib/md5.h ../plib/mt64.h \
  ../plib/persist.h ../plib/prime.h ../plib/service.h ../plib/timer.h \
  ../plib/unit.h ast.h ifa.h ifalog.h if1.h sym.h num.h prim_data.h \
  code.h builtin.h builtin_symbols.h fail.h fa.h prim.h var.h pnode.h \
  fun.h pdb.h clone.h cg.h
main.o: main.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h ifa_version.h ../plib/log.h parse.h ../plib/unit.h LICENSE.i \
  COPYRIGHT.i
ssu.o: ssu.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h ssu.h dom.h
sym.o: sym.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h
var.o: var.cc ifadefs.h ../plib/plib.h ../plib/arg.h ../plib/barrier.h \
  ../plib/config.h ../plib/dlmalloc.h ../plib/freelist.h \
  ../plib/defalloc.h ../plib/list.h ../plib/log.h ../plib/vec.h \
  ../plib/map.h ../plib/threadpool.h ../plib/misc.h ../plib/util.h \
  ../plib/conn.h ../plib/md5.h ../plib/mt64.h ../plib/persist.h \
  ../plib/prime.h ../plib/service.h ../plib/timer.h ../plib/unit.h ast.h \
  ifa.h ifalog.h if1.h sym.h num.h prim_data.h code.h builtin.h \
  builtin_symbols.h fail.h fa.h prim.h var.h pnode.h fun.h pdb.h clone.h \
  cg.h
ifa_version.o: ifa_version.cc ifadefs.h ../plib/plib.h ../plib/arg.h \
  ../plib/barrier.h ../plib/config.h ../plib/dlmalloc.h \
  ../plib/freelist.h ../plib/defalloc.h ../plib/list.h ../plib/log.h \
  ../plib/vec.h ../plib/map.h ../plib/threadpool.h ../plib/misc.h \
  ../plib/util.h ../plib/conn.h ../plib/md5.h ../plib/mt64.h \
  ../plib/persist.h ../plib/prime.h ../plib/service.h ../plib/timer.h \
  ../plib/unit.h ast.h ifa.h ifalog.h if1.h sym.h num.h prim_data.h \
  code.h builtin.h builtin_symbols.h fail.h fa.h prim.h var.h pnode.h \
  fun.h pdb.h clone.h cg.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
