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

RESULT = mikmatch_str

OCAMLFLAGS = -dtypes

OCAMLLDFLAGS = \
  messages.cmo charset.cmo \
  constants.cmo \
  regexp_ast.cmo \
  select_lib.cmo

INCDIRS = ../common

USE_CAMLP4 = yes

LIBINSTALL_FILES := \
  pa_mikmatch_str.cma pa_mikmatch_str.cmo pa_mikmatch_str.cmi \
  run_mikmatch_str.cma run_mikmatch_str.cmo run_mikmatch_str.cmi \
  mikmatch.cmi mikmatch.cmo mikmatch.cmx mikmatch.mli \
  run_mikmatch_str.cmxa run_mikmatch_str.cmx \
  run_mikmatch_str.a run_mikmatch_str.o

.PHONY: default force all links 

default: links pa_lib misc
force:
	touch $(SOURCES)
	$(MAKE)

all:

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

.PHONY: pa_lib install uninstall topinstall misc

pa_lib:
	$(MAKE) RESULT=pa_mikmatch_str pabc bcl

#install: libinstall topinstall
install: libinstall
uninstall: libuninstall
	rm -f $(BINDIR)/mikmatch_str.top $(BINDIR)/mikmatch_str

topinstall:
	install -m 0755 mikmatch_str.top mikmatch_str $(BINDIR)

misc:
	ocamlc -c mikmatch.mli
	ocamlc -a -o run_mikmatch_str.cma \
		mikmatch.ml run_mikmatch_str.ml
	ocamlopt -a -o run_mikmatch_str.cmxa \
		mikmatch.ml run_mikmatch_str.ml


.PHONY: test1 test-install
test: test1

# (preinstall test)
test1:
	camlp4o ./pa_mikmatch_str.cma -printer o test1.ml > test1.ppo
	ocamlopt -pp 'camlp4o ./pa_mikmatch_str.cma' \
		str.cmxa unix.cmxa \
		run_mikmatch_str.cmxa \
		test1.ml -o test1
	./test1
#	ocamlmktop -o mikmatch_str.test -I +camlp4 -I . camlp4o.cma \
#		pa_mikmatch_str.cma str.cma unix.cma run_mikmatch_str.cma
#	./mikmatch_str.test test1.ml


# Compilation with ocamlfind (postinstall test)
test-install:
	ocamlfind ocamlopt \
		-syntax camlp4o \
		-package mikmatch_str\
		-linkpkg \
		test1.ml -o test1_inst
	./test1_inst


TRASH = \
 *~ *.ppo *.cm[ioxa] *.cmxa *.o *.a *.top \
 *.test test1 test1.more mikmatch_str mikmatch_str.ml


OCAMLMAKEFILE = ../OCamlMakefile
include $(OCAMLMAKEFILE)
