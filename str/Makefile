ifndef BINDIR
  BINDIR = $(shell dirname `which ocaml`)
endif

SOURCES = \
  mm_util.ml \
  global_def.mli global_def.ml \
  match.ml \
  str_lib.ml \
  syntax_common.ml \
  syntax_str.ml

RESULT = micmatch_str

OCAMLFLAGS = -dtypes

OCAMLLDFLAGS = \
  messages.cmo charset.cmo \
  constants.cmo \
  regexp_ast.cmo \
  select_lib.cmo

INCDIRS = ../common

USE_CAMLP4 = yes

LIBINSTALL_FILES := \
  pa_micmatch_str.cma pa_micmatch_str.cmo pa_micmatch_str.cmi \
  run_micmatch_str.cma run_micmatch_str.cmo run_micmatch_str.cmi \
  micmatch.cmi micmatch.cmo micmatch.cmx micmatch.mli \
  run_micmatch_str.cmxa run_micmatch_str.cmx \
  run_micmatch_str.a run_micmatch_str.o

.PHONY: default force all links 

default: links pa_lib misc
force:
	touch $(SOURCES)
	$(MAKE)

all:

links: micmatch.mli micmatch.ml match.ml syntax_common.ml mm_util.ml \
	global_def.mli global_def.ml

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
global_def.mli: ../common/global_def.mli
	ln -s $< $@
global_def.ml: ../common/global_def.ml
	ln -s $< $@

.PHONY: pa_lib install uninstall version topinstall misc

pa_lib:
	$(MAKE) RESULT=pa_micmatch_str pabc bcl

install: libinstall topinstall
uninstall: libuninstall
	rm -f $(BINDIR)/micmatch_str.top $(BINDIR)/micmatch_str

version:
	sed -e "s/VERSION/$VERSION" < META.template > META

topinstall:
	install -m 0755 micmatch_str.top micmatch_str $(BINDIR)

misc:
	ocamlc -c micmatch.mli
	ocamlc -a -o run_micmatch_str.cma \
		micmatch.ml run_micmatch_str.ml
	ocamlopt -a -o run_micmatch_str.cmxa \
		micmatch.ml run_micmatch_str.ml


.PHONY: test1 test-install
test: test1

# (preinstall test)
test1:
	camlp4o ./pa_micmatch_str.cma -printer o test1.ml > test1.ppo
	ocamlopt -pp 'camlp4o ./pa_micmatch_str.cma' \
		str.cmxa unix.cmxa \
		run_micmatch_str.cmxa \
		test1.ml -o test1
	./test1
#	ocamlmktop -o micmatch_str.test -I +camlp4 -I . camlp4o.cma \
#		pa_micmatch_str.cma str.cma unix.cma run_micmatch_str.cma
#	./micmatch_str.test test1.ml


# Compilation with ocamlfind (postinstall test)
test-install:
	ocamlfind ocamlopt \
		-syntax camlp4o \
		-package micmatch_str\
		-linkpkg \
		test1.ml -o test1_inst
	./test1_inst


TRASH = \
 *~ *.ppo *.cm[ioxa] *.cmxa *.o *.a *.top \
 *.test test1 test1.more test1_mt micmatch_str micmatch_str.ml


OCAMLMAKEFILE = ../OCamlMakefile
include $(OCAMLMAKEFILE)
