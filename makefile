
TEMPFILE := $(shell mktemp)
pwd = $(shell pwd)
buildDate = $(shell date)

oce = opam config exec --

buildCmd = $(oce) ocamlbuild -use-ocamlfind -tag thread -menhir "menhir --table --strict --explain --dump"

locker = docker run --rm -it -v $(pwd):$(pwd) -w $(pwd) -e TRAVIS=$(TRAVIS) -e TRAVIS_JOB_ID=$(TRAVIS_JOB_ID) ocaml-core

elc: docker-build-date _tags src/* # src/error_messages.ml
	$(locker) $(buildCmd) ./src/main.native
	mv main.native elc

verify: elc
	$(locker) rm -f bisect*.out
	$(locker) $(buildCmd) ./test/verify-examples.native
	$(locker) ./verify-examples.native

coveralls: docker-build-date
	$(locker) $(oce) ocveralls --prefix _build --send bisect000*.out

coverage: docker-build-date bisect*.out
	$(locker) $(oce) bisect-ppx-report -I _build/ -html coverage/ bisect*.out

interactive: docker-build-date
	$(locker) bash

docker-build-date: Dockerfile
	docker build -t ocaml-core $(pwd)
	echo $(buildDate) > docker-build-date

# src/handmade.messages: docker-build-date src/elang_parser.mly
# 	$(locker) $(oce) menhir --update-errors src/handmade.messages src/elang_parser.mly > $(TEMPFILE)
# 	mv $(TEMPFILE) src/handmade.messages
# 	rm -f $(TEMPFILE)

# src/error_messages.ml: docker-build-date src/handmade.messages
# 	$(locker) $(oce) menhir --compile-errors src/handmade.messages src/elang_parser.mly > src/error_messages.ml

clean:
	$(locker) rm -rf _build coverage* elc

