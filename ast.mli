
(* Syntaxe abstraite pour mini-C++ *)

(* ********************************************** *)


type loc = Lexing.position * Lexing.position

type 'a pos = { v: 'a ; loc:loc }

(*type sup = dsup pos*)

type dsup = string list

type typedef =
  | Void
  | Int
  | Tid of string


type var = dvar pos 
and dvar =
  | Ident of string
  | Po of var
  | Ad of var

type arg = darg pos

and darg = Arg of typedef * var

type qident = dqident pos 

and dqident =
  | Qident of string
  | Double of string * string

type qvar = dqvar pos

and dqvar =
  | Qvar of qident
  | Qpo of qvar
  | Qad of qvar

type proto = dproto pos

and dproto =
  | Plong of typedef * qvar * (arg list)
  | Pshort of string * (arg list)
  | Pdouble of string * string * (arg list)

type decl_v = ddecl_v pos

and ddecl_v = Declv of typedef * (var list)


type membre = dmembre pos

and dmembre =
  | Mvar of decl_v
  | Mvir of bool * proto

type decl_c = ddecl_c pos

and ddecl_c =
  | Class of string * (*sup anciennement *) dsup * (membre list)
  


type operateur = Eq | Neq | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | Mod | And | Or

type expr = { dexpr:dexpr ; loc:loc }

and dexpr  =
  | Eint of int
  | Ethis
  | Ebool of bool
  | Enull
  | Eqident of qident
  | Epointeur of expr
  | Eattr of expr * string
  | Esderef of expr * string
  | Eassign of expr * expr
  | Efcall of expr * (expr list)
  | Enew of string * (expr list)
  | Elincr of expr
  | Eldecr of expr
  | Erincr of expr
  | Erdecr of expr
  | Eland of expr
  | Enot of expr
  | Euminus of expr
  | Euplus of expr
  | Eop of operateur * expr * expr
  | Epar of expr


type expr_str = dexpr_str pos 
and dexpr_str =
  | Esexpr of expr 
  | Estring of string


type inst = dinst pos 

and dinst =
  | Nothing
  | Iexpr of expr
  | Idecls of typedef * var
  | Idecl of typedef * var * expr
  | Aidecl of typedef * var * string * (expr list)
  | If of expr * inst
  | Ifelse of expr * inst * inst
  | While of expr * inst
  | For of (expr list) * expr * (expr list) * inst
  | Afor of (expr list) * (expr list) * inst
  | Ibloc of bloc
  | Cout of expr_str list
  | Return of expr
  | Areturn

and bloc = dbloc pos

and dbloc = Bloc of inst list

and decl = ddecl pos

and ddecl =
  | Dv of decl_v
  | Dc of decl_c
  | Db of proto * bloc

type fichier = dfichier pos

and dfichier =
  { bincludeios : bool;
    decls: decl list;
  }
