all:
	ocamlc -c Board.ml
	ocamlc -c Klotski.ml
	ocamlc -o klotski Board.cmo Klotski.cmo

clean:
	rm -rf klotski *.cmi *.cmx *.o *.cmo *~ sol.tex sol.aux sol.log sol.pdf

run:
	./klotski start.txt end.txt 0
	pdflatex sol.tex
	rm -rf *.aux *.log

test:
	./klotski start0.txt end0.txt 1
	pdflatex sol.tex
	rm -rf *.aux *.log

static:
	ocamlopt -c Board.ml
	ocamlopt -c Klotski.ml
	ocamlopt -o klotski Board.ml Klotski.ml

lab:
	./klotski start.txt end.txt 1
	pdflatex sol.tex
	rm -rf *.aux *.log
