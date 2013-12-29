BIN=main.native

all: $(BIN) *.ml *.mll *.mly 



	
$(BIN):
	ocamlbuild -use-menhir $(BIN)
	cp _build/$(BIN) minic++
	rm main.native


.SUFFIXES: .mli .ml .mll .mly

clean:
	rm -rf main.native _build minic++


