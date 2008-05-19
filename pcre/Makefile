ifndef BINDIR
  BINDIR = $(shell dirname `which ocaml`)
endif

SOURCES = \
  mm_util.ml \
  match.ml \
  pcre_lib.ml \
  syntax_common.ml \
  syntax_pcre.ml

RESULT = micmatch_pcre

OCAMLFLAGS = -dtypes

OCAMLLDFLAGS = \
  declare_once.cmo \
  pa_use.cmo \
  messages.cmo charset.cmo \
  constants.cmo \
  regexp_ast.cmo \
  select_lib.cmo

INCDIRS = ../common

USE_CAMLP4 = yes

COMMON_LIBINSTALL_FILES = \
  pa_micmatch_pcre.cma pa_micmatch_pcre.cmo pa_micmatch_pcre.cmi \
  run_micmatch_pcre.cmi micmatch.cmi micmatch.mli
BC_LIBINSTALL_FILES = \
  run_micmatch_pcre.cma run_micmatch_pcre.cmo 
NC_LIBINSTALL_FILES = \
  run_micmatch_pcre.cmxa run_micmatch_pcre.cmx \
  run_micmatch_pcre.a run_micmatch_pcre.o micmatch.cmx
ALL_LIBINSTALL_FILES = \
  $(COMMON_LIBINSTALL_FILES) $(BC_LIBINSTALL_FILES) $(NC_LIBINSTALL_FILES)

.PHONY: default all-bc all-nc all force links pa_lib install uninstall \
        version topinstall misc-bc misc-nc

default: links pa_lib misc-bc misc-nc
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

links: micmatch.mli micmatch.ml match.ml syntax_common.ml mm_util.ml

micmatch.mli: ../common/micmatch.mli
	ln -s $< $@
micmatch.ml: ../common/micmatch.ml
	ln -s $< $@
syntax_common.ml: ../common/syntax_common.ml
	ln -s $< $@
match.ml: ../common/match.ml
	ln -s $< $@
mm_util.ml: ../common/mm_util.ml
	ln -s $< $@

pa_lib:
	$(MAKE) RESULT=pa_micmatch_pcre pabc bcl

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
	sed -e "s/VERSION/$VERSION" < META.template > META

misc-bc:
	ocamlc -c micmatch.mli
	ocamlc -a -o run_micmatch_pcre.cma -I $(PCRE_DIR) \
		micmatch.ml run_micmatch_pcre.ml
	ocamlmktop -o micmatch_pcre.top \
		-I +camlp4 -I $(PCRE_DIR) \
		camlp4o.cma \
		pa_micmatch_pcre.cma pcre.cma unix.cma run_micmatch_pcre.cma

misc-nc:
	ocamlc -c micmatch.mli
	ocamlopt -a -o run_micmatch_pcre.cmxa -I $(PCRE_DIR) \
		micmatch.ml run_micmatch_pcre.ml


.PHONY: test more-tests test-install
test: test more-tests

# Toplevel (preinstall test)
test:
	camlp4o ./pa_micmatch_pcre.cma -printer o test.ml > test.ppo
	ocamlmktop -o micmatch_pcre.test -I +camlp4 -I .  -I $(PCRE_DIR) \
		camlp4o.cma \
		pa_micmatch_pcre.cma pcre.cma unix.cma run_micmatch_pcre.cma

# Preinstall test
more-tests:
	camlp4o ./pa_micmatch_pcre.cma -printer o -direct test.ml > test.ppo
	ocamlfind ocamlopt -syntax camlp4o \
		-package pcre \
		-ppopt ./pa_micmatch_pcre.cma -ppopt -direct \
		run_micmatch_pcre.cmxa \
		test.ml -o test
	./test

# Postinstall test
test-install:
	ocamlfind ocamlopt \
		-syntax camlp4o \
		-package pcre,micmatch_pcre\
		-linkpkg \
		test.ml -o test
	./test

TRASH = \
 *~ *.ppo *.cm[ioxa] *.cmxa *.o *.a *.top \
 *.test test test.more micmatch bytecode nativecode micmatch_pcre.ml


OCAMLMAKEFILE = ../OCamlMakefile
include $(OCAMLMAKEFILE)
