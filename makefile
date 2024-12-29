all:
	ocamlc -c board.ml
	ocamlc -c Klotski.ml
	ocamlc -o Klotski board.cmo Klotski.cmo

clean:
	rm -rf Klotski *.cmi *.cmo *~ sol.tex sol.aux sol.log sol.pdf

run:
	./Klotski start.txt end.txt 0
	pdflatex sol.tex
	rm -rf *.aux *.log

lab:
	./Klotski start.txt end.txt 1
	pdflatex sol.tex
	rm -rf *.aux *.log
