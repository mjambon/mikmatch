all: mikmatch-ocamldoc/index.html
	camlmix -o mikmatch-manual.tex -clean mikmatch-manual.tex.mlx
	pdflatex -interaction=nonstopmode mikmatch-manual
	pdflatex -interaction=nonstopmode mikmatch-manual
	hevea -fix macros.hva mikmatch-manual

mikmatch-ocamldoc/index.html: ../common/mikmatch.mli
	ocamldoc -d mikmatch-ocamldoc -html $<
	ocamldoc $< -o mikmatch-ocamldoc.tex -latex -noheader -notoc -notrailer

clean:
	rm -f *~ *.haux *.log *.aux *.toc *.pdf *.html *.out *.htoc \
		mikmatch-ocamldoc/*
