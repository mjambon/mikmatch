PCRE_DIR = $(shell ocamlfind query pcre)
# PCRE_DIR = something else

ifndef BINDIR
  BINDIR = $(shell dirname `which ocaml`)
endif

SOURCES = \
  mm_util.ml \
  global_def.mli global_def.ml \
  match.ml \
  pcre_lib.ml \
  syntax_common.ml \
  syntax_pcre.ml

RESULT = mikmatch_pcre

OCAMLFLAGS = -dtypes

OCAMLLDFLAGS = \
  messages.cmo charset.cmo \
  constants.cmo \
  regexp_ast.cmo \
  select_lib.cmo

INCDIRS = ../common

USE_CAMLP4 = yes

COMMON_LIBINSTALL_FILES = \
  pa_mikmatch_pcre.cma pa_mikmatch_pcre.cmo pa_mikmatch_pcre.cmi \
  run_mikmatch_pcre.cmi mikmatch.cmi mikmatch.mli
BC_LIBINSTALL_FILES = \
  run_mikmatch_pcre.cma run_mikmatch_pcre.cmo 
NC_LIBINSTALL_FILES = \
  run_mikmatch_pcre.cmxa run_mikmatch_pcre.cmx \
  run_mikmatch_pcre.a run_mikmatch_pcre.o mikmatch.cmx
ALL_LIBINSTALL_FILES = \
  $(COMMON_LIBINSTALL_FILES) $(BC_LIBINSTALL_FILES) $(NC_LIBINSTALL_FILES)

.PHONY: default all-bc all-nc all force links pa_lib install uninstall \
        version topinstall misc-bc misc-nc

default: version links pa_lib misc-bc misc-nc
	touch bytecode
	touch nativecode
all-bc: links pa_lib misc-bc
	touch bytecode
all-nc: links pa_lib misc-nc
	touch nativecode

all: # needed by libinstall

force:
	touch $(SOURCES)
	$(MAKE)

links: mikmatch.mli mikmatch.ml match.ml syntax_common.ml mm_util.ml \
	global_def.mli global_def.ml

mikmatch.mli: ../common/mikmatch.mli
	ln -s $< $@
mikmatch.ml: ../common/mikmatch.ml
	ln -s $< $@
syntax_common.ml: ../common/syntax_common.ml
	ln -s $< $@
match.ml: ../common/match.ml
	ln -s $< $@
mm_util.ml: ../common/mm_util.ml
	ln -s $< $@
global_def.mli: ../common/global_def.mli
	ln -s $< $@
global_def.ml: ../common/global_def.ml
	ln -s $< $@

pa_lib:
	$(MAKE) RESULT=pa_mikmatch_pcre pabc bcl

install: topinstall
	if test -f nativecode; \
	then \
		$(MAKE) "LIBINSTALL_FILES=$(ALL_LIBINSTALL_FILES)" libinstall;\
	else \
		$(MAKE) "LIBINSTALL_FILES=$(COMMON_LIBINSTALL_FILES) $(BC_LIBINSTALL_FILES)" libinstall; \
	fi

uninstall: 
	$(MAKE) libuninstall

version:
	sed -e "s/VERSION/$(VERSION)/" < META.template > META

misc-bc:
	ocamlc -c mikmatch.mli
	ocamlc -a -o run_mikmatch_pcre.cma -I $(PCRE_DIR) \
		mikmatch.ml run_mikmatch_pcre.ml
	ocamlmktop -o mikmatch_pcre.top \
		-I +camlp4 -I $(PCRE_DIR) \
		dynlink.cma \
		camlp4o.cma \
		pa_mikmatch_pcre.cma pcre.cma unix.cma run_mikmatch_pcre.cma

misc-nc:
	ocamlc -c mikmatch.mli
	ocamlopt -a -o run_mikmatch_pcre.cmxa -I $(PCRE_DIR) \
		mikmatch.ml run_mikmatch_pcre.ml


.PHONY: test simple-test more-tests test-install
test: simple-test more-tests

# Toplevel (preinstall test)
simple-test:
	camlp4o ./pa_mikmatch_pcre.cma -printer o test.ml > test.ppo
	ocamlmktop -o mikmatch_pcre.test -I +camlp4 -I .  -I $(PCRE_DIR) \
		dynlink.cma \
		camlp4o.cma \
		pa_mikmatch_pcre.cma pcre.cma unix.cma run_mikmatch_pcre.cma

# Preinstall test
more-tests:
	camlp4o ./pa_mikmatch_pcre.cma -printer o -direct test.ml > test.ppo
	ocamlopt \
		-pp 'camlp4o ./pa_mikmatch_pcre.cma' \
		-I $(PCRE_DIR) pcre.cmxa run_mikmatch_pcre.cmxa \
		test.ml -o test
	./test

# Postinstall test
test-install:
	ocamlfind ocamlopt \
		-syntax camlp4o \
		-package pcre,mikmatch_pcre\
		-linkpkg \
		test.ml -o test
	./test

# Debugging
.PHONY: test2
test2:
	camlp4o ./pa_mikmatch_pcre.cma -printer o test2.ml > test2.ppo

TRASH = \
 *~ *.ppo *.cm[ioxa] *.cmxa *.o *.a *.top \
 *.test test test.more mikmatch bytecode nativecode mikmatch_pcre.ml


OCAMLMAKEFILE = ../OCamlMakefile
include $(OCAMLMAKEFILE)
