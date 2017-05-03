
TEMPFILE := $(shell mktemp)
pwd = $(shell pwd)
buildDate = $(shell date)

oce = opam config exec --

buildCmd = $(oce) ocamlbuild -use-ocamlfind -tag thread -menhir "menhir --table"

locker = docker run --rm -it -v $(pwd):$(pwd) -w $(pwd) ocaml-core

src/main.native: src/* docker-build-date src/error_messages.ml
	$(locker) $(buildCmd) ./src/main.native

test: test.native
	$(locker) ./test.native

test.native: src/* test/* docker-build-date
	$(locker) $(buildCmd) -I src ./test/test.native

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
	rm -rf _build


nodocker: src/*
	$(buildCmd) ./src/main.native
