router:
	ocamlbuild -use-ocamlfind router.byte && ./router.byte

clean:
	ocamlbuild -clean
