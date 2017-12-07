check:
	ocamlbuild -use-ocamlfind state.byte && rm state.byte

router:
	ocamlbuild -use-ocamlfind router.byte && ./router.byte

server:
	ocamlbuild -use-ocamlfind server.byte && ./server.byte

main:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

clean:
	ocamlbuild -clean
