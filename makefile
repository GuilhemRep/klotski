all: game.cmo klotski.cmo
	ocamlc -o klotski game.cmo klotski.cmo -g

game.cmo: game.ml 
	ocamlc -c game.ml -g
	
klotski.cmo: klotski.ml
	ocamlc -c klotski.ml -g


# all:
# 	ocamlopt -c game.ml
# 	ocamlopt -c klotski.ml
# 	ocamlopt -o main.native game.cmx klotski.ml
# 	ocamlbuild main.native

# -g for debug, then OCAMLRUNPARAM=b ./klotski start.txt end.txt 0


clean:
	rm -rf klotski *.cmi *.cmx *.o *.cmo *~ *.tex *.aux *.log *.pdf

classical:
	./klotski start.txt end.txt 0
	pdflatex solution.tex
	rm -rf *.aux *.log

lab:
	./klotski start.txt end.txt 1
	pdflatex solution.tex
	rm -rf *.aux *.log

shape:
	./klotski start.txt end.txt 2
	pdflatex solution.tex
	rm -rf *.aux *.log

