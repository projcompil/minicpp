# Pour éviter de recompiler tout à chaque fois
ocamlbuild -use-menhir main.native
cp _build/main.native minic++
