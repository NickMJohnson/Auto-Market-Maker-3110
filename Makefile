.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/test.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f Final_project.zip
	zip -r Final_project . -x@exclude.lst

clean:
	dune clean
	rm -f Final_project

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh	
	@bash opendoc.sh	
