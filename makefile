all:
	ocamlc -c board.ml
	ocamlc -c klotski.ml
	ocamlc -o klotski board.cmo klotski.cmo

clean:
	rm -rf klotski *.cmi *.cmo *~ sol.tex sol.aux sol.log sol.pdf

run:
	./klotski start.txt end.txt 0
	pdflatex sol.tex
	rm -rf *.aux *.log

lab:
	./klotski start.txt end.txt 1
	pdflatex sol.tex
	rm -rf *.aux *.log
