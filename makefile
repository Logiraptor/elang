

pwd = $(shell pwd)
buildDate = $(shell date)

oce = opam config exec --

buildCmd = $(oce) ocamlbuild -use-ocamlfind -tag thread

locker = docker run --rm -it -v $(pwd):$(pwd) -w $(pwd) ocaml-core

src/main.native: src/* docker-build-date
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

clean:
	rm -rf _build


nodocker: *.ml parser.mly lexer.mll
	ocamlbuild -use-ocamlfind -use-menhir -tag thread -pkg core main.native