all:
	ocamlc -c Board.ml
	ocamlc -c Klotski.ml
	ocamlc -o klotski Board.cmo Klotski.cmo

clean:
	rm -rf klotski *.cmi *.cmo *~ sol.tex sol.aux sol.log sol.pdf

run:
	./Klotski start.txt end.txt 0
	pdflatex sol.tex
	rm -rf *.aux *.log

lab:
	./Klotski start.txt end.txt 1
	pdflatex sol.tex
	rm -rf *.aux *.log
