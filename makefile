all:
	dune build


# all:
# 	ocamlopt -c game.ml
# 	ocamlopt -c klotski.ml
# 	ocamlopt -o main.native game.cmx klotski.ml
# 	ocamlbuild main.native

# -g for debug, then OCAMLRUNPARAM=b ./klotski start.txt end.txt 0


clean:
	dune clean
	rm -rf *.cmi *.cmx *.o *.cmo *~ *.tex *.aux *.log *.pdf

run:
	./_build/default/klotski.exe start.txt end.txt 0 solution.tex
	pdflatex solution.tex
	rm -rf *.aux *.log

lab:
	./_build/default/klotski.exe start.txt end.txt 1 solution.tex
	pdflatex solution.tex
	rm -rf *.aux *.log

shape:
	./_build/default/klotski.exe start.txt end.txt 2 solution.tex
	pdflatex solution.tex
	rm -rf *.aux *.log

