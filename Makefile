CMO=lexer.cmo parser.cmo interp.cmo main.cmo mips.cmo typing.cmo
GENERATED = lexer.ml parser.ml parser.mli 
BIN=main.native
FLAGS=-annot

all: $(BIN)
	

$(BIN):
	ocamlbuild -use-menhir $(BIN)
	cp _build/$(BIN) minic++

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

clean:
	rm -rf main.native _build minic++


