BIN=main.native

all: $(BIN)
	
$(BIN):
	ocamlbuild -use-menhir $(BIN)
	cp _build/$(BIN) minic++
	rm main.native

clean:
	rm -rf main.native _build minic++


