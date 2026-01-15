all: game.cmo
	ocamlc -c klotski.ml -g
	ocamlc -o klotski game.cmo klotski.cmo -g

# all:
# 	ocamlopt -c game.ml
# 	ocamlopt -c klotski.ml
# 	ocamlopt -o main.native game.cmx klotski.ml
# 	ocamlbuild main.native

# -g for debug, then OCAMLRUNPARAM=b ./klotski start.txt end.txt 0

game.cmo:
	ocamlc -c game.ml -g

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

