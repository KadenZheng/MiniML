all: miniml expr evaluation tests
	ocamlbuild -use-ocamlfind expr.byte
	ocamlbuild -use-ocamlfind miniml.byte
	ocamlbuild -use-ocamlfind evaluation.byte
	ocamlbuild -use-ocamlfind tests.byte

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

tests: tests.ml
	ocamlbuild -use-ocamlfind tests.byte