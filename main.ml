
(* Fichier principal du compilateur mini-c++ *)

open Format
open Lexing
open Typing
(* Option de compilation, pour s'arrêter à l'issue du parser *)
let parse_only = ref false
let type_only = ref false

(* Noms des fichiers source et cible *)
let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s 

(* Les options du compilateur que l'on affiche en tapant arithc --help *)
let options = 
  ["--parse-only", Arg.Set parse_only, 
   "  Pour ne faire uniquement que la phase d'analyse syntaxique" ; 
   "--type-only", Arg.Set type_only ,
   "  Pour ne faire que les phases d'analyse syntaxique et de typage"]

let usage = "usage: minic++ [option] file.cpp"

(* localise une erreur en indiquant la ligne et la colonne *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let () = 
  (* Parsing de la ligne de commande *)
  Arg.parse options (set_file ifile) usage;

  (* On vérifie que le nom du fichier source a bien été indiqué *)
  if !ifile="" then begin eprintf "Aucun fichier à compiler\n@?"; exit 1 end; 

  (* Ce fichier doit avoir l'extension .cpp *)
  if not (Filename.check_suffix !ifile ".cpp") then begin
    eprintf "Le fichier d'entrée doit avoir l'extension .cpp\n@?";
    Arg.usage options usage;
    exit 1
  end;

  (* Ouverture du fichier source en lecture *)
  let f = open_in !ifile in
    
  (* Création d'un tampon d'analyse lexicale *)
  let buf = Lexing.from_channel f in
  
  try
    (* Parsing: la fonction  Parser.prog transforme le tampon lexical en un 
       arbre de syntaxe abstraite si aucune erreur (lexicale ou syntaxique) 
       n'est détectée.
       La fonction Lexer.token est utilisée par Parser.prog pour obtenir 
       le prochain token. *)
    let p = Parser.fichier Lexer.token buf in
       	(* On s'arrête ici si on ne veut faire que le parsing *)
   	if !parse_only then begin
		close_in f ;
		exit 0;
	end
   	else begin
		let tarbre = Typing.typfichier p in
		if tarbre.tbincludeios then print_string "Alors\n"
		else print_string "Bon\n" ;
   			if !type_only then begin
				print_string "Typage correct\n";
				close_in f ;
				exit 0;
			end
			else begin
				print_string "OK.\n";
				close_in f ;
				exit 0;
			end
	end
       	(*Interp.prog p *)
  with
    | Lexer.Lexing_error c -> 
	(* Erreur lexicale. On récupère sa position absolue et 
	   on la convertit en numéro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse lexicale: %s@." c;
	exit 1
    | Parser.Error -> 
	(* Erreur syntaxique. On récupère sa position absolue et on la 
	   convertit en numéro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse syntaxique@.";
	exit 1
    | Typing.Error (loc, s) -> 
	(* Erreur syntaxique. On récupère sa position absolue et on la 
	   convertit en numéro de ligne *)
	localisation (fst loc) ;
	localisation (snd loc) ;
	print_string s ;
	eprintf "Erreur dans le typage@.";
	exit 1
    | Failure s ->
	localisation (Lexing.lexeme_start_p buf);
        eprintf "Erreur du compilateur.";
	print_string s ;
	exit 2;
    | _ -> 
        localisation (Lexing.lexeme_start_p buf);
        eprintf "Erreur du compilateur.";
        exit 2
    (*| Interp.Error s-> 
	(* Erreur pendant l'interprétation *)
	eprintf "Erreur : %s@." s;
	exit 1*)
	




