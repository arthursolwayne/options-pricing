.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

interface:
	OCAMLRUNPARAM=b dune exec bin/interface.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f newestrepo.zip
	zip -r newestrepo.zip . -x@exclude.lst

clean:
	dune clean
	rm -f enigma.zip

docs:
	dune build @doc