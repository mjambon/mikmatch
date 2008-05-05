SOURCES = \
  declare_once.mli declare_once.ml \
  pa_use.ml \
  messages.ml charset.ml \
  constants.ml \
  regexp_ast.ml \
  select_lib.ml

USE_CAMLP4 = yes

DECLDIR = ../external-libs/regexp-pp/common

default: external bcnl
external:
#	cp -f $(DECLDIR)/declare_once.mli $(DECLDIR)/declare_once.ml .
	test -f pa_use.ml || ln -s ../public-domain/pa_use.ml .

TRASH = \
 *~ version_filter declare_once.mli declare_once.ml \
 pa_use.ml

OCAMLMAKEFILE = ../OCamlMakefile
include $(OCAMLMAKEFILE)
