all: build

NAME=netkat_decide
J=4

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure $(ASYNC) $(TESTS)

build: setup.data setup.ml
	ocaml setup.ml -build -j $(J)

install: setup.data setup.ml
	ocaml setup.ml -install

test: setup.ml build
	_build/test/Test.byte inline-test-runner netkat

reinstall: setup.ml
	ocamlfind remove $(NAME) || true
	ocaml setup.ml -reinstall

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log

distclean:
	ocaml setup.ml -distclean
	rm -f setup.data setup.log

uninstall: setup.ml
	ocamlfind remove $(NAME)
	ocaml setup.ml -uninstall
