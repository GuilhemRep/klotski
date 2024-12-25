all:
	ocamlc -c klotski.ml
	ocamlc -o klotski klotski.cmo

clean:
	rm -rf klotski *.cmi *.cmo *~
