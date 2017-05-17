FROM ubuntu:trusty

# add add-apt-repository
RUN apt-get update && \
    apt-get install -y software-properties-common

# taken from https://ocaml.org/docs/install.html#Ubuntu
RUN add-apt-repository --yes ppa:avsm/ppa
RUN apt-get update -qq && apt-get install -y opam

RUN opam init

RUN opam update

RUN apt-get install -y pkg-config
RUN apt-get install -y m4
RUN apt-get install -y make

RUN opam install -y core

RUN opam install -y menhir

RUN opam install -y OUnit

RUN apt-get install -y llvm-3.8-dev libllvm-3.8-ocaml-dev llvm-3.8

RUN apt-get install -y python g++

RUN opam install -y ctypes-foreign

RUN opam install -y llvm.3.8

RUN opam pin add llvm 3.8

RUN opam install -y qcheck

RUN opam install -y sexplib

RUN opam install -y ppx_sexp_conv

RUN opam install -y ppx_driver

RUN opam install -y merlin

RUN opam install -y ocp-indent

RUN opam install -y ANSITerminal

RUN opam install -y bisect_ppx

RUN opam install -y ocveralls

RUN apt-get install -y curl