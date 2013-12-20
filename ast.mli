
(* Syntaxe abstraite pour mini-C++ *)

(* ********************************************** *)


type loc = Lexing.position * Lexing.position


type sup = Super of string list

type typedef =
  | Void
  | Int
  | Tid of string


type var =
  | Ident of string
  | Po of var
  | Ad of var






type arg = Arg of typedef * var

type qident =
  | Qident of string
  | Double of string * string

type qvar =
  | Qvar of qident
  | Qpo of qvar
  | Qad of qvar

type prototype =
  | Plong of typedef * qvar * (arg list)
  | Pshort of string * (arg list)
  | Pdouble of string * string * (arg list)

type decl_v = Declv of typedef * (var list)

type membre =
  | Mvar of decl_v
  | Mvir of bool * prototype

type decl_c =
  | Class of bool*string * (membre list)
  


type operateur = Eq | Neq | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | Mod | And | Or

type expr = { dexpr:dexpr ; loc:loc; }

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

type expr_str = 
  | Esexpr of expr 
  | Estring of string

type inst =
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

and bloc = Bloc of inst list

and decl =
  | Dv of decl_v
  | Dc of decl_c
  | Db of prototype * bloc

type fichier =
  { bincludeios : bool;
    decls: decl list;
  }
