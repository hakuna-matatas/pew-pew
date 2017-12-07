check:
	ocamlbuild -use-ocamlfind state.byte && rm state.byte

test:
	ocamlbuild -use-ocamlfind collision_test.byte && ./collision_test.byte && rm collision_test.byte

gui:
	ocamlbuild -use-ocamlfind client.byte && ./client.byte && rm client.byte

router:
	ocamlbuild -use-ocamlfind router.byte && ./router.byte

server:
	ocamlbuild -use-ocamlfind server.byte && ./server.byte

main:
	ocamlbuild -tag thread -use-ocamlfind main.byte && ./main.byte

lobby:
	ocamlbuild -tag thread -use-ocamlfind lobby.byte && ./lobby.byte

clean:
	ocamlbuild -clean
