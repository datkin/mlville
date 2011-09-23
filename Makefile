tests: test/test_parser.ml parser
	mkdir -p build/tests
	ocamlfind ocamlc -package core,ounit -thread -linkpkg -I build parser.cmo test/test_parser.ml -c -o build/tests/parser-tests
	ocamlfind ocamlc -package core,ounit -thread -linkpkg -I build -I build/tests parser.cmo parser-tests.cmo -o build/tests/parser-tests
	./build/tests/parser-tests

parser: src/parser.mli src/parser.ml init
	ocamlfind ocamlc -package core -thread src/parser.mli -I build -linkpkg -annot -g -c -o build/parser
	ocamlfind ocamlc -package core -thread src/parser.ml -I build -linkpkg -annot -g -c -o build/parser

init:
	mkdir -p build

clean:
	rm build/*
