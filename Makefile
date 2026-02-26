# Makefile for IFA

all: defaulttarget

MODULE=ifa
DEBUG=1
#OPTIMIZE=1
#PROFILE=1
USE_GC=1
#LEAK_DETECT=1
#VALGRIND=1
TEST_EXEC=ifa_tests

CXX ?= clang++
AR ?= llvm-ar
PREFIX ?= /usr/local

OS_TYPE = $(shell uname -s | \
  awk '{ split($$1,a,"_"); printf("%s", a[1]);  }')
OS_VERSION = $(shell uname -r | \
  awk '{ split($$1,a,"."); sub("V","",a[1]); \
  printf("%d%d%d",a[1],a[2],a[3]); }')
ARCH = $(shell uname -m)
ifeq ($(ARCH),i386)
  ARCH = x86
endif
ifeq ($(ARCH),i486)
  ARCH = x86
endif
ifeq ($(ARCH),i586)
  ARCH = x86
endif
ifeq ($(ARCH),i686)
  ARCH = x86
endif

ifeq ($(OS_TYPE),Darwin)
  AR_FLAGS = crvs
else
  AR_FLAGS = crv
endif

MAJOR=0
MINOR=5

BUILD_VERSION = $(shell git show-ref 2> /dev/null | head -1 | cut -d ' ' -f 1)
ifeq ($(BUILD_VERSION),)
  BUILD_VERSION = $(shell cat BUILD_VERSION 2>/dev/null)
endif
VERSIONCFLAGS += -DMAJOR_VERSION=$(MAJOR) -DMINOR_VERSION=$(MINOR) -DBUILD_VERSION=\"$(BUILD_VERSION)\"

CFLAGS += -Wall
ifdef DEBUG
CFLAGS += -g -DDEBUG=1
endif
ifdef OPTIMIZE
CFLAGS += -O3 -march=native
endif
ifdef PROFILE
CFLAGS += -pg
endif
ifdef VALGRIND
CFLAGS += -DVALGRIND_TEST
endif

CFLAGS += -Iplib -I/opt/homebrew/include
LDFLAGS += -L/usr/local/lib -L/opt/homebrew/lib

# GC configuration
ifeq ($(OS_TYPE),Darwin)
GC_CFLAGS += -I/usr/local/include
ifneq ($(wildcard /opt/homebrew/include),)
  GC_CFLAGS += -I/opt/homebrew/include
endif
ifneq ($(wildcard /opt/homebrew/lib),)
  LDFLAGS += -L/opt/homebrew/lib
endif
else
GC_CFLAGS += -I/usr/local/include
LIBS += -lrt -lpthread
endif

ifdef USE_GC
CFLAGS += -DUSE_GC $(GC_CFLAGS)
LIBS += -lgc -lgccpp
endif
ifdef LEAK_DETECT
CFLAGS += -DLEAK_DETECT $(GC_CFLAGS)
LIBS += -lleak
endif

# LLVM Configuration
LLVM_CXXFLAGS = $(shell llvm-config --cxxflags)
LLVM_LDFLAGS = $(shell llvm-config --ldflags --libs core irreader executionengine mcjit native target CodeGen AsmPrinter AsmParser | sed 's/-NDEBUG //')
# Removed -NDEBUG as DEBUG=1 is often set. LLVM_CXXFLAGS usually includes appropriate -DNDEBUG or not.

CFLAGS += $(LLVM_CXXFLAGS)
LDFLAGS_EXEC = $(LDFLAGS) $(LLVM_LDFLAGS) # LDFLAGS for executables needing LLVM libs

CFLAGS += -std=c++23
CPPFLAGS += $(CFLAGS)

LIBS += -ldparse_gc -lm
ifneq ($(OS_TYPE),CYGWIN)
ifneq ($(OS_TYPE),Darwin)
  LIBS += -lrt
endif
endif

PLIB_SRCS = plib/arg.cc plib/config.cc plib/misc.cc plib/service.cc \
            plib/vec.cc plib/vec_test.cc plib/unit.cc plib/log.cc
PLIB_OBJS = $(PLIB_SRCS:%.cc=%.o)

LIB_SRCS = ast.cc builtin.cc cdb.cc cfg.cc cg.cc llvm.cc llvm_codegen.cc llvm_primitives.cc clone.cc dead.cc dom.cc fa.cc \
	fail.cc fun.cc graph.cc html.cc if1.cc ifa.cc inline.cc \
	ifalog.cc loop.cc num.cc pattern.cc pdb.cc pnode.cc prim.cc prim_data.cc \
	main.cc ssu.cc sym.cc var.cc ifa_version.cc
LIB_OBJS = $(LIB_SRCS:%.cc=%.o)

IFA_DEPEND_SRCS = main.cc parse.cc scope.cc make_ast.cc ast_to_if1.cc cg.cc llvm.cc llvm_codegen.cc llvm_primitives.cc
IFA_SRCS = $(IFA_DEPEND_SRCS) v.g.d_parser.cc python.g.d_parser.cc
IFA_OBJS = $(IFA_SRCS:%.cc=%.o)

EXECUTABLE_FILES = ifa
ifdef USE_GC
LIBRARY = libifa_gc.a
else
LIBRARY = libifa.a
endif
INSTALL_LIBRARIES = $(LIBRARY)
MANPAGES = ifa.1

AUX_FILES = $(MODULE)/index.html $(MODULE)/manual.html $(MODULE)/faq.html $(MODULE)/ifa.1 $(MODULE)/ifa.cat
TAR_FILES = $(AUX_FILES)

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
	cp $(INSTALL_LIBRARIES) $(PREFIX)/lib

deinstall:
	rm $(EXECUTABLES:%=$(PREFIX)/bin/%)
	rm $(MANPAGES:%=$(PREFIX)/man/man1/%)
	rm $(INSTALL_LIBRARIES:%=$(PREFIX)/lib/%)

$(IFA): $(IFA_OBJS) $(LIB_OBJS) $(PLIB_OBJS)
	$(CXX) $(CFLAGS) $(LDFLAGS_EXEC) -o $@ $^ $(LIBS)

$(LIBRARY): $(LIB_OBJS) $(PLIB_OBJS)
	$(AR) $(AR_FLAGS) $@ $^

$(MAKE_PRIMS): make_prims.cc
	$(CXX) $(CFLAGS) $(LDFLAGS) -o $@ $^ $(LIBS)

ifa.cat: ifa.1
	rm -f ifa.cat
	nroff -man ifa.1 | sed -e 's/.//g' > ifa.cat

%.g.d_parser.cc: %.g
	make_dparser -v -Xcc -I $<

LICENSE.i: LICENSE
	rm -f LICENSE.i
	cat $< | sed s/\"/\\\\\"/g | sed s/\^/\"/g | sed s/$$/\\\\n\"/g | sed 's/%/%%/g' > $@

COPYRIGHT.i: LICENSE
	rm -f COPYRIGHT.i
	head -1 LICENSE | sed s/\"/\\\\\"/g | sed s/\^/\"/g | sed s/$$/\\\\n\"/g > $@

main.o: LICENSE.i COPYRIGHT.i

ifa_version.o: Makefile ifa_version.cc
	$(CXX) $(CFLAGS) $(VERSIONCFLAGS) -c ifa_version.cc

clean:
	\rm -f *.o plib/*.o core *.core *.gmon $(EXECUTABLES) $(CLEAN_FILES) LICENSE.i COPYRIGHT.i

realclean: clean
	\rm -f *.a *.orig *.rej

depend:
	./mkdep $(CFLAGS) $(DEPEND_SRCS)

-include .depend

# Test target for LLVM backend
test_llvm: ifa
	@echo "Testing LLVM backend..."
	IFA_LLVM=1 ./ifa test_llvm.v
	clang test_llvm.o -o test_llvm
	@echo "Running test_llvm..."
	./test_llvm
	@echo "Test passed!"

test: ifa
	./ifa --test
	./ifa_tests

.PHONY: test test_llvm clean realclean depend install deinstall
