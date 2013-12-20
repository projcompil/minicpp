
(* Syntaxe abstraite pour mini-C++ *)

(* ********************************************** *)


type loc = Lexing.position * Lexing.position


type sup = Super of string list

type typedef =
  | Void
  | Int
  | Tid of string


type variable =
  | Ident of string
  | Po of variable
  | Ad of variable






type arg = Arg of typedef * variable

type qidentifiant =
  | Qident of string
  | Double of string * string

type qvariable =
  | Qvar of qidentifiant
  | Qpo of qvariable
  | Qad of qvariable

type prototype =
  | Plong of typedef * qvariable * (arg list)
  | Pshort of string * (arg list)
  | Pdouble of string * string * (arg list)

type declaration_v = Declv of typedef * (variable list)

type membre =
  | Mvar of declaration_v
  | Mvir of bool * prototype

type declaration_c =
  | Class of bool*string * (membre list)
  


type operateur = Eq | Neq | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | Mod | And | Or

type expr = { dexpr:dexpr ; loc:loc; }

and dexpr  =
  | Eint of int
  | Ethis
  | Ebool of bool
  | Enull
  | Eqident of qidentifiant
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
  | Idecls of typedef * variable
  | Idecl of typedef * variable * expr
  | Aidecl of typedef * variable * string * (expr list)
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

and declaration =
  | Dv of declaration_v
  | Dc of declaration_c
  | Db of prototype * bloc

type fichier =
  { bincludeios : bool;
    declarations: declaration list;
  }
