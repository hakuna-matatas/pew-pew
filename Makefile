router:
	ocamlbuild -use-ocamlfind router.byte && ./router.byte

gui:
	ocamlbuild -use-ocamlfind gui.byte && ./gui.byte

clean:
	ocamlbuild -clean
