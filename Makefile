
all: main.ml tools.ml syntax.ml lexer.mll parser.mly
	ocamlopt -c tools.ml syntax.ml
	ocamllex lexer.mll
	menhir --infer --explain parser.mly
	ocamlopt -c parser.mli
	ocamlopt -c lexer.ml
	ocamlopt -c parser.ml
	ocamlopt -o spyc tools.cmx syntax.cmx parser.cmx lexer.cmx main.ml

clean:
	rm -f *.cmi *.cmx *.cmo *.o *~
