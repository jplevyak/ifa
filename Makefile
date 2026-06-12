# Makefile for IFA

all: defaulttarget

MODULE=ifa
DEBUG=1
#OPTIMIZE=1
#PROFILE=1
# Boehm GC is a hard dependency — no non-GC build path.
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

CFLAGS += -MMD -MP -Wall
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

CFLAGS += -I. -Icommon -Iif1 -Ifrontend -Ioptimize -Icodegen -Ianalysis -I/opt/homebrew/include
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

CFLAGS += -DUSE_GC $(GC_CFLAGS)
LIBS += -lgc -lgccpp
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

PLIB_SRCS = common/arg.cc common/config.cc common/misc.cc common/service.cc \
            common/vec.cc common/vec_test.cc common/unit.cc common/log.cc \
            common/fail.cc common/html.cc common/ifa_version.cc
PLIB_OBJS = $(PLIB_SRCS:%.cc=%.o)

LIB_SRCS = ifa.cc main.cc \
	if1/ast.cc if1/builtin.cc if1/fun.cc if1/if1.cc if1/num.cc if1/pattern.cc \
	if1/pnode.cc if1/prim.cc if1/prim_data.cc if1/sym.cc if1/var.cc \
	analysis/fa.cc analysis/pdb.cc analysis/graph.cc analysis/clone.cc analysis/ifalog.cc \
	codegen/codegen_common.cc codegen/cg.cc codegen/llvm.cc codegen/llvm_codegen.cc codegen/llvm_primitives.cc \
	codegen/cg_normalize.cc \
	optimize/cfg.cc optimize/dead.cc optimize/dom.cc optimize/inline.cc optimize/loop.cc optimize/ssu.cc \
	testing/parse_ir.cc testing/write_ir.cc testing/test_callbacks.cc \
	testing/printer_util.cc testing/ir_builder.cc testing/ir_builder_test.cc \
	testing/ir_shapes.cc \
	testing/print_finalize.cc testing/print_cfg.cc testing/print_ssu.cc \
	testing/print_dom.cc testing/print_loops.cc testing/print_argpos.cc \
	testing/print_patterns.cc testing/print_fa.cc testing/print_fa_converge.cc \
	testing/print_dispatch.cc testing/print_clone.cc testing/print_dce.cc \
	testing/print_codegen.cc testing/print_inline.cc \
	testing/fa_setup.cc testing/roundtrip_test.cc testing/lattice_test.cc \
	testing/cg_normalize_test.cc testing/cg_to_llvm_type_test.cc \
	testing/create_llvm_function_from_cgfun_test.cc \
	testing/emit_cg_test.cc
LIB_OBJS = $(LIB_SRCS:%.cc=%.o)

IFA_DEPEND_SRCS = main.cc frontend/parse.cc frontend/scope.cc frontend/make_ast.cc frontend/ast_to_if1.cc \
	codegen/codegen_common.cc codegen/cg.cc codegen/llvm.cc codegen/llvm_codegen.cc codegen/llvm_primitives.cc \
	codegen/cg_normalize.cc codegen/emit_cg.cc
IFA_SRCS = $(IFA_DEPEND_SRCS) frontend/v.g.d_parser.cc frontend/python.g.d_parser.cc
IFA_OBJS = $(IFA_SRCS:%.cc=%.o)

EXECUTABLE_FILES = ifa ifa-test
LIBRARY = libifa_gc.a
INSTALL_LIBRARIES = $(LIBRARY)
MANPAGES = ifa.1

AUX_FILES = $(MODULE)/index.html $(MODULE)/manual.html $(MODULE)/faq.html $(MODULE)/ifa.1 $(MODULE)/ifa.cat
TAR_FILES = $(AUX_FILES)

ifeq ($(OS_TYPE),CYGWIN)
EXECUTABLES = $(EXECUTABLE_FILES:%=%.exe)
IFA = ifa.exe
MAKE_PRIMS = make_prims.exe
MAKE_CAST_CODE = make_cast_code.exe
else
EXECUTABLES = $(EXECUTABLE_FILES)
IFA = ifa
MAKE_PRIMS = make_prims
MAKE_CAST_CODE = make_cast_code
endif

CLEAN_FILES += *.cat tests/*.out tests/*.c frontend/*.d_parser.cc frontend/*.d_parser.h \
	$(MAKE_CAST_CODE) \
	$(PLIB_OBJS:.o=.d) $(LIB_OBJS:.o=.d) $(IFA_OBJS:.o=.d)

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

# IF1-level test harness. Links against the static library so the
# archive can skip main.o (ifa_test_main.o provides its own main) and
# also skip frontend objects pulled in by ifa.cc that we don't need.
IFA_TEST_OBJS = testing/ifa_test_main.o
ifa-test: $(IFA_TEST_OBJS) $(LIBRARY)
	$(CXX) $(CFLAGS) $(LDFLAGS_EXEC) -o $@ $(IFA_TEST_OBJS) $(LIBRARY) $(LIBS)

$(LIBRARY): $(LIB_OBJS) $(PLIB_OBJS)
	$(AR) $(AR_FLAGS) $@ $^

$(MAKE_PRIMS): tools/make_prims.cc
	$(CXX) $(CFLAGS) $(LDFLAGS) -o $@ $^ $(LIBS)

$(MAKE_CAST_CODE): if1/make_cast_code.cc
	$(CXX) -std=c++23 -o $@ $^

if1/cast_code.cc if1/check_cast.cc: $(MAKE_CAST_CODE)
	(cd if1 && ../$(MAKE_CAST_CODE))

ifa.cat: ifa.1
	rm -f ifa.cat
	nroff -man ifa.1 | sed -e 's/.//g' > ifa.cat

frontend/v.g.d_parser.cc: frontend/v.g frontend/c.g
	(cd frontend && make_dparser -v -Xcc -I v.g)

frontend/python.g.d_parser.cc: frontend/python.g
	(cd frontend && make_dparser -v -Xcc -I python.g)

%.g.d_parser.cc: %.g
	make_dparser -v -Xcc -I $<

LICENSE.i: LICENSE
	rm -f LICENSE.i
	cat $< | sed s/\"/\\\\\"/g | sed s/\^/\"/g | sed s/$$/\\\\n\"/g | sed 's/%/%%/g' > $@

COPYRIGHT.i: LICENSE
	rm -f COPYRIGHT.i
	head -1 LICENSE | sed s/\"/\\\\\"/g | sed s/\^/\"/g | sed s/$$/\\\\n\"/g > $@

main.o: LICENSE.i COPYRIGHT.i

common/ifa_version.o: Makefile common/ifa_version.cc
	$(CXX) $(CFLAGS) $(VERSIONCFLAGS) -c common/ifa_version.cc -o common/ifa_version.o

clean:
	\rm -f *.o common/*.o if1/*.o frontend/*.o optimize/*.o codegen/*.o analysis/*.o core *.core *.gmon $(EXECUTABLES) $(CLEAN_FILES) LICENSE.i COPYRIGHT.i

realclean: clean
	\rm -f *.a *.orig *.rej

-include $(PLIB_OBJS:.o=.d) $(LIB_OBJS:.o=.d) $(IFA_OBJS:.o=.d)

# Test target for LLVM backend.
#
# `IFA_LLVM=1 ./ifa test_llvm.v` runs the LLVM codegen and then
# `llvm_codegen_compile` (codegen/llvm.cc) spawns clang itself to
# produce the final `test_llvm` binary — including the
# -lm -lgc -lgccpp link flags needed for GC_malloc references in
# the emitted IR. No separate clang invocation needed here. See
# issue 012 for the history.
test_llvm: ifa
	@echo "Testing LLVM backend..."
	IFA_LLVM=1 ./ifa test_llvm.v
	@echo "Running test_llvm..."
	./test_llvm
	@echo "Test passed!"

# ifa-test runs one --phase at a time. The driver loop here iterates
# every registered phase so a single `make test-ir` covers them all.
test: ifa ifa-test
	./ifa --test
	$(MAKE) test-ir

test-ir: ifa-test
	@set -e; for phase in `./ifa-test --list-phases`; do \
	  echo "=== ifa-test --phase $$phase ==="; \
	  ./ifa-test --phase $$phase; \
	done

test-ir-rebless: ifa-test
	@set -e; for phase in `./ifa-test --list-phases`; do \
	  echo "=== ifa-test --phase $$phase --rebless ==="; \
	  ./ifa-test --phase $$phase --rebless; \
	done

.PHONY: test test-ir test-ir-rebless test_llvm clean realclean install deinstall
