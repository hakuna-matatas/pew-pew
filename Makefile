check:
	ocamlbuild -use-ocamlfind state.byte && rm state.byte

router:
	ocamlbuild -use-ocamlfind router.byte && ./router.byte

clean:
	ocamlbuild -clean
