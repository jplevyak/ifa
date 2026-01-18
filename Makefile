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
#LDFLAGS += -L/usr/local/lib -fuse-ld=lld
LDFLAGS += -L/usr/local/lib

# LLVM Configuration
LLVM_CXXFLAGS = $(shell llvm-config --cxxflags)
LLVM_LDFLAGS = $(shell llvm-config --ldflags --libs core irreader executionengine mcjit native target CodeGen AsmPrinter AsmParser | sed 's/-NDEBUG //')
# Removed -NDEBUG as DEBUG=1 is often set. LLVM_CXXFLAGS usually includes appropriate -DNDEBUG or not.

CFLAGS += $(LLVM_CXXFLAGS)
LDFLAGS_EXEC = $(LDFLAGS) $(LLVM_LDFLAGS) # LDFLAGS for executables needing LLVM libs

# Objects for ifa-llvm (No GC)
NOGC_DIR = nogc
$(shell mkdir -p $(NOGC_DIR))

LLVM_BACKEND_SRCS = sym.cc var.cc fun.cc if1.cc prim.cc prim_data.cc pnode.cc cfg.cc dom.cc \
                    dead.cc clone.cc llvm.cpp ir_deserialize.cc llvm_main.cpp fa.cc pdb.cc \
                    graph.cc builtin.cc num.cc ssu.cc \
                    ast.cc html.cc ifalog.cc ifa.cc inline.cc loop.cc pattern.cc

LLVM_BACKEND_OBJS = $(addprefix $(NOGC_DIR)/, $(LLVM_BACKEND_SRCS:.cc=.o))
LLVM_BACKEND_OBJS := $(LLVM_BACKEND_OBJS:.cpp=.o)

# Override flags: remove -DUSE_GC, add -I., -I../plib, -I../dparser
NOGC_CXXFLAGS = $(filter-out -DUSE_GC, $(CFLAGS)) -I. -I../plib -I../dparser

# Compile rules
$(NOGC_DIR)/%.o: %.cc
	$(CXX) $(NOGC_CXXFLAGS) -c -o $@ $<

$(NOGC_DIR)/%.o: %.cpp
	$(CXX) $(NOGC_CXXFLAGS) -c -o $@ $<

# Link with nogc libs
LIBS_NOGC = -L../plib -lplib_nogc -L. -ldparse_nogc -lrt -lpthread -lm

ifa-llvm: $(LLVM_BACKEND_OBJS)
	$(CXX) $(NOGC_CXXFLAGS) -o $@ $(filter-out nogc/fail.o, $(LLVM_BACKEND_OBJS)) $(LLVM_LDFLAGS) $(LIBS_NOGC)

clean_llvm:
	rm -rf $(NOGC_DIR) ifa-llvm

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

OBJS = main.o parse.o scope.o make_ast.o ast_to_if1.o cg.o llvm.o ir_serialize.o $(PARSER_OBJS) \
       ast.o builtin.o cdb.o cfg.o clone.o dead.o dom.o fa.o fail.o fun.o \
       graph.o html.o if1.o ifa.o inline.o ifalog.o loop.o num.o pattern.o \
       pdb.o pnode.o prim.o prim_data.o ssu.o sym.o var.o ifa_version.o

OBJS_LIB = ast.o builtin.o cdb.o cfg.o cg.o llvm.o ir_serialize.o clone.o dead.o dom.o fa.o \
           fail.o fun.o graph.o html.o if1.o ifa.o inline.o ifalog.o loop.o \
           num.o pattern.o pdb.o pnode.o prim.o prim_data.o main.o ssu.o \
           sym.o var.o ifa_version.o

CXX ?= clang++
AR = llvm-ar

AUX_FILES = $(MODULE)/index.html $(MODULE)/manual.html $(MODULE)/faq.html $(MODULE)/ifa.1 $(MODULE)/ifa.cat
TAR_FILES = $(AUX_FILES) $(TEST_FILES)

LIB_SRCS = ast.cc builtin.cc cdb.cc cfg.cc cg.cc llvm.cc clone.cc dead.cc dom.cc fa.cc \
	fail.cc fun.cc graph.cc html.cc if1.cc ifa.cc inline.cc \
	ifalog.cc loop.cc num.cc pattern.cc pdb.cc pnode.cc prim.cc prim_data.cc \
	ir_serialize.cc \
	main.cc ssu.cc sym.cc var.cc ifa_version.cc
LIB_OBJS = $(LIB_SRCS:%.cc=%.o)

IFA_DEPEND_SRCS = main.cc parse.cc scope.cc make_ast.cc ast_to_if1.cc cg.cc llvm.cc ir_serialize.cc
IFA_SRCS = $(IFA_DEPEND_SRCS) v.g.d_parser.cc python.g.d_parser.cc
IFA_OBJS = $(IFA_SRCS:%.cc=%.o)

EXECUTABLE_FILES = ifa ifa-llvm
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
	$(CXX) $(CFLAGS) $(LDFLAGS_EXEC) -o $@ $^ $(LIBS)

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

# Test target for LLVM backend
test_llvm: ifa ifa-llvm
	@echo "Testing LLVM backend..."
	IFA_LLVM=1 ./ifa test_llvm.v
	clang test_llvm.o -o test_llvm
	@echo "Running test_llvm..."
	./test_llvm
	@echo "Test passed!"

.PHONY: test_llvm
