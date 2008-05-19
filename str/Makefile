BINDIR = $(shell dirname `which ocaml`)

SOURCES = \
  mm_util.ml \
  match.ml \
  str_lib.ml \
  syntax_common.ml \
  syntax_str.ml

RESULT = micmatch_str

OCAMLLDFLAGS = \
  messages.cmo charset.cmo \
  constants.cmo \
  regexp_ast.cmo \
  select_lib.cmo

INCDIRS = ../common

USE_CAMLP4 = yes
OCAMLFLAGS = -dtypes

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
	ocamlmktop -o micmatch_str.top \
		-I +camlp4 \
		camlp4o.cma \
		pa_micmatch_str.cma str.cma unix.cma run_micmatch_str.cma
	./make_micmatch_str


.PHONY: test1 more-tests test-install
test: test1 more-tests

# Toplevel (preinstall test)
test1:
	camlp4o ./pa_micmatch_str.cma -printer o test1.ml > test1.ppo
	ocamlmktop -o micmatch_str.test -I +camlp4 -I . camlp4o.cma \
		pa_micmatch_str.cma str.cma unix.cma run_micmatch_str.cma
	./micmatch_str.test test1.ml

# Compilation without ocamlfind (preinstall test)
more-tests:
	camlp4o ./pa_micmatch_str.cma -printer o -thread -direct test1.ml \
		> test1_mt.ppo
	ocamlopt -pp 'camlp4o ./pa_micmatch_str.cma -thread -direct' \
		 -thread threads.cmxa str.cmxa unix.cmxa \
		run_micmatch_str_mt.cmxa \
		test1.ml -o test1_mt
	./test1_mt

# Compilation with ocamlfind (postinstall test)
test-install:
	ocamlfind ocamlopt \
		-syntax camlp4o \
		-package micmatch_str\
		-linkpkg \
		test1.ml -o test1
	ocamlfind ocamlopt \
		-thread \
		-syntax camlp4o \
		-package micmatch_str \
		-ppopt -thread \
		-ppopt -direct \
		-linkpkg \
		test1.ml -o test1_mt
	./test1
	./test1_mt

TRASH = \
 *~ *.ppo *.cm[ioxa] *.cmxa *.o *.a *.top \
 *.test test1 test1.more test1_mt micmatch_str micmatch_str.ml


OCAMLMAKEFILE = ../OCamlMakefile
include $(OCAMLMAKEFILE)
