
(* Analyseur lexical pour mini-C++ *)

{
  open Lexing
  open Parser
  (*open Lexerhack*)
   
  exception Lexing_error of string

  (* tables des mots-clés *)
  let kwd_tbl = 
    ["class", CLASS; "else", ELSE; "false", FALSE;
     "if", IF; "for", FOR; "int", INT; "new", NEW;
     "NULL", NULL; "public", PUBLIC;
     "return", RETURN; "this", THIS; "true", TRUE;  "virtual", VIRTUAL; "void", VOID; "while", WHILE
    ]

  let table = Hashtbl.create 17 ;; (* la table du lexer hack *)

  Hashtbl.add table "void" () ;;

  Hashtbl.add table "int" () ;;

  let id_or_kwd = 
    let h = Hashtbl.create 17 in
    List.iter (fun (s,t) -> Hashtbl.add h s t) kwd_tbl;
    fun s -> 
      (*let s = String.lowercase s in la casse n'est pas significative *)
      try List.assoc s kwd_tbl with _ -> if Hashtbl.mem table s then TIDENT s else IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}


let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let oct_digit = ['0'-'7']
let hex_digit =  ['0'-'9' 'a'-'f' 'A'-'F']
let ident = ( letter | '_' ) ( letter | digit | '_' )*
let integer = '0' | ['1'-'9'] digit* | '0' oct_digit+ | "0x" hex_digit+
let space = [' ' '\t']
let char = [' '-'!' '#'-'[' ']'-'\127'] | "\\" | "\"" | '\n' | '\t' | "\\x" hex_digit hex_digit
let string = '"' char* '"'

rule token = parse
  | '\n'    { newline lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | ident as id { id_or_kwd id }
  | "#include<iostream>" { INCLUDEIOS }
  | "#include <iostream>" { INCLUDEIOS }
  | "std::cout" { STDCOUT }
  | '"'     { CHAINE (String.concat "" (chaine lexbuf) )}
  | '{'     { LACC }
  | '}'     { RACC }
  | "<<"    { CHEVRON }
  | "->"    { SDEREF }
  | '.'     { DOT }
  | '!'     { NOT }
  | "++"    { INCR }
  | "--"    { DECR }
  | '&'     { LAND }
  | '*'     { TIMES }
  | '/'     { DIV }
  | '%'     { MODULO}
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '<'     { LT }
  | "<="     { LE }
  | '>'     { GT }
  | ">="     { GE }
  | "=="     { EQ }
  | "!="     { NEQ }
  | "&&"     {AND}
  | "||"    {OR}
  | '='     {ASSIGN}
  | '('     { LPAR }
  | ')'     { RPAR }
  | ','     { COMMA }
  | ':'     { COLON }
  | ";"     { SEMICOLON }
  | "/*"    { comment lexbuf }
  | "//"    { commentendl lexbuf}
  | integer as s { INTEGER (int_of_string s) }
  | eof     { EOF }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

(* note : les commentaires ne sont pas imbriqués en C++ *)
and comment = parse
  | "*/"    { token lexbuf }
  | _       { comment lexbuf }
  | eof     { raise (Lexing_error ("unterminated comment")) }
 
and commentendl = parse
  | '\n' { newline lexbuf ; token lexbuf }
  | _  {commentendl lexbuf}
  | eof {EOF}

and chaine = parse
  | "\\n"   { "\n"::(chaine lexbuf) }
  | "\\t"   { "\t"::(chaine lexbuf) }
  | "\\\""  { "\""::(chaine lexbuf) }
  | "\\\\"  { "\\"::(chaine lexbuf) }
  | "\\x" (hex_digit hex_digit as s)  { (String.make 1 (char_of_int (int_of_string ("0x" ^ s) ) ))::(chaine lexbuf) }
  | '\\'    { chaine lexbuf }
  | '"'     { [] }
  | [' '-'\127'] as c { (String.make 1 c)::(chaine lexbuf) }
  | eof     { raise (Lexing_error "End of file before a string is finished.") }
  | _       { raise (Lexing_error "Invalid char inside a string.") }

