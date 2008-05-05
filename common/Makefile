SOURCES = \
  global_def.ml \
  messages.ml charset.ml \
  constants.ml \
  regexp_ast.ml \
  select_lib.ml

OCAMLFLAGS = -dtypes
USE_CAMLP4 = yes

.PHONY: default
default: bcnl

TRASH = *~ 

OCAMLMAKEFILE = ../OCamlMakefile
include $(OCAMLMAKEFILE)
