automatique_native:
	ocamlbuild -o main.native

automatique_bytecode:
	ocamlbuild -o main.byte

a_la_main:
	# Faut faire -c sur tous les fichiers .ml
	# On récupère tous les .cmx
	# (les .cmi sont générés mais peuvent venir des .mli)
	ocamlopt -c mod.ml
	
	# A partir du .ml finel et de tous les .cmx (générés à l'étape d'avant)
	# Obligatoire que ça soit dans cet ordre là
	ocamlopt -o main.native mod.cmx main.ml

clean:
	rm *.cmx *.cmi *.o *.native *.byte
