all: Board.cmo
	ocamlc -c Klotski.ml
	ocamlc -o klotski Board.cmo Klotski.cmo

Board.cmo:
	ocamlc -c Board.ml

clean:
	rm -rf klotski *.cmi *.cmx *.o *.cmo *~ sol.tex sol.aux sol.log sol.pdf

run:
	./klotski start.txt end.txt 0
	pdflatex sol.tex
	rm -rf *.aux *.log

lab:
	./klotski start.txt end.txt 1
	pdflatex sol.tex
	rm -rf *.aux *.log

shape:
	./klotski start.txt end.txt 2
	pdflatex sol.tex
	rm -rf *.aux *.log
