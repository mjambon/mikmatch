all: mmodoc/index.html
	camlmix -o micmatch-manual.tex -clean micmatch-manual.tex.mlx
	pdflatex -interaction=nonstopmode micmatch-manual
	pdflatex -interaction=nonstopmode micmatch-manual
	hevea -fix macros.hva micmatch-manual

mmodoc/index.html: ../micmatch_common/micmatch.mli
	ocamldoc -d mmodoc -html $<
	ocamldoc $< -o mmodoc.tex -latex -noheader -notoc -notrailer

clean:
	rm -f *~ *.haux *.log *.aux *.toc *.pdf *.html *.out *.htoc mmodoc/*
