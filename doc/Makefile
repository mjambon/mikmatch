all: mmodoc/index.html
	camlmix -o mikmatch-manual.tex -clean mikmatch-manual.tex.mlx
	pdflatex -interaction=nonstopmode mikmatch-manual
	pdflatex -interaction=nonstopmode mikmatch-manual
	hevea -fix macros.hva mikmatch-manual

mmodoc/index.html: ../common/mikmatch.mli
	ocamldoc -d mmodoc -html $<
	ocamldoc $< -o mmodoc.tex -latex -noheader -notoc -notrailer

clean:
	rm -f *~ *.haux *.log *.aux *.toc *.pdf *.html *.out *.htoc mmodoc/*
