
TEMPFILE := $(shell mktemp)
pwd = $(shell pwd)
buildDate = $(shell date)

oce = opam config exec --

buildCmd = $(oce) ocamlbuild -use-ocamlfind -tag thread -menhir "menhir --table --strict --explain --dump"

locker = docker run --rm -it -v $(pwd):$(pwd) -w $(pwd) ocaml-core

elc: src/* docker-build-date src/error_messages.ml
	$(locker) $(buildCmd) ./src/main.native
	mv main.native elc

verify: elc
	rm -f bisect*.out
	$(locker) ./test/verify-examples.sh
	$(locker) $(oce) bisect-ppx-report -I _build/ -html coverage/ -text coverage.txt bisect*.out
	rm -f bisect*.out


interactive: docker-build-date
	$(locker) bash

docker-build-date: Dockerfile
	docker build -t ocaml-core $(pwd)
	echo $(buildDate) > docker-build-date

src/handmade.messages: src/elang_parser.mly
	menhir --update-errors src/handmade.messages src/elang_parser.mly > $(TEMPFILE)
	mv $(TEMPFILE) src/handmade.messages
	rm -f $(TEMPFILE)

src/error_messages.ml: src/handmade.messages
	menhir --compile-errors src/handmade.messages src/elang_parser.mly > src/error_messages.ml

clean:
	rm -rf _build coverage* elc


nodocker: src/*
	$(buildCmd) ./src/main.native
