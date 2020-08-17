# Makefile for IFA

all: defaulttarget

USE_PLIB=1
MODULE=ifa
DEBUG=1
#OPTIMIZE=1
#PROFILE=1
USE_GC=1
#LEAK_DETECT=1
#VALGRIND=1
TEST_EXEC=ifa_tests

include ../plib/Makefile

MAJOR=0
MINOR=5

CFLAGS += -I../plib
# CFLAGS += -flto=thin
LDFLAGS += -L/usr/local/lib -fuse-ld=lld
ifdef USE_GC
LIBS += -L../plib -lplib_gc -ldparse_gc -lgc -pthread
else
LIBS += -L../plib -lplib -ldparse
endif
ifneq ($(OS_TYPE),CYGWIN)
ifneq ($(OS_TYPE),Darwin)
  LIBS += -lrt
endif
endif

CXX ?= clang++
AR = llvm-ar

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

DEPEND_SRCS = $(IFA_DEPEND_SRCS) $(LIB_SRCS)

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
	$(CXX) $(CFLAGS) $(LDFLAGS) -o $@ $^ $(LIBS)

$(LIBRARY): $(LIB_OBJS)
	$(AR) $(AR_FLAGS) $@ $^

$(MAKE_PRIMS): make_prims.cc
	$(CXX) $(CFLAGS) $(LDFLAGS) -o $@ $^ $(LIBS)

ifa.cat: ifa.1
	rm -f ifa.cat
	nroff -man ifa.1 | sed -e 's/.//g' > ifa.cat

%.g.d_parser.cc: %.g
	make_dparser -v -Xcc -I $<

main.o: LICENSE.i COPYRIGHT.i

ifa_version.o: Makefile ifa_version.cc
	$(CXX) $(CFLAGS) $(VERSIONCFLAGS) -c ifa_version.cc

-include .depend
