examples: examples.ml parser
	ocamlfind ocamlc -package core -thread -linkpkg build/parser.cmo -I build examples.ml -annot -g -o build/examples

parser: parser.ml init
	ocamlfind ocamlc -package core -thread parser.ml -linkpkg -annot -g -c -o build/parser

init:
	mkdir -p build

clean:
	rm build/*
