check:
	ocamlbuild -use-ocamlfind state.byte && rm state.byte

test:
	ocamlbuild -use-ocamlfind collision_test.byte && ./collision_test.byte && rm collision_test.byte

router:
	ocamlbuild -use-ocamlfind router.byte && ./router.byte

gui:
	ocamlbuild -use-ocamlfind gui.byte && ./gui.byte

clean:
	ocamlbuild -clean
