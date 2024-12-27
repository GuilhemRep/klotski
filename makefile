all:
	ocamlc -c board.ml
	ocamlc -c klotski.ml
	ocamlc -o klotski board.cmo klotski.cmo

clean:
	rm -rf klotski *.cmi *.cmo *~ sol.tex sol.aux sol.log sol.pdf

run:
	./klotski
	pdflatex sol.tex
	rm -rf sol.tex sol.aux sol.log
