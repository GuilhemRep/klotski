all:
	ocamlc -c board.ml
	ocamlc -c klotski.ml
	ocamlc -o klotski board.cmo klotski.cmo

clean:
	rm -rf klotski *.cmi *.cmo *~ sol.tex sol.aux sol.log sol.pdf

run:
	./klotski start.txt end.txt
	pdflatex sol.tex
	rm -rf *.tex *.aux *.log
