BIN=main.native

all: 
	ocamlbuild -use-menhir $(BIN)
	cp _build/$(BIN) minic++



clean:
	rm -rf main.native _build minic++


