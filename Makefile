.PHONE: lib bench tests

all: deps tests lib

deps:
	opam install qcheck core core_bench

lib:
	dune build -p hexstring

bench:
	dune build lib/bench 
	./_build/default/lib/bench/bench.exe

tests:
	dune runtest
	dune exec lib/tests/tests.exe
