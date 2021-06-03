.PHONE: lib bench tests

all: deps tests lib

deps:
	opam install dune qcheck core core_bench opam-dune-lint ocamlformat

lib:
	dune build -p hexstring

bench:
	dune build lib/bench 
	./_build/default/lib/bench/bench.exe

coverage:
	find . -name '*.coverage' | xargs rm -f
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report summary
	bisect-ppx-report html

tests:
	dune runtest
