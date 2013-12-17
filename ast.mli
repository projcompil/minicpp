
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
  


type operator = Eq | Neq | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | Mod | And | Or

type expression = { desc:desc ; loc:loc; }

and desc =
  | Eint of int
  | Ethis
  | Ebool of bool
  | Enull
  | Eqident of qidentifiant
  | Epointeur of expression
  | Eattr of expression * string
  | Esderef of expression * string
  | Eassign of expression * expression
  | Efcall of expression * (expression list)
  | Enew of string * (expression list)
  | Elincr of expression
  | Eldecr of expression
  | Erincr of expression
  | Erdecr of expression
  | Eland of expression
  | Enot of expression
  | Euminus of expression
  | Euplus of expression
  | Eop of operator * expression * expression
  | Epar of expression

type expression_str = 
  | Esexpr of expression 
  | Estring of string

type inst =
  | Nothing
  | Iexpr of expression
  | Idecls of typedef * variable
  | Idecl of typedef * variable * expression
  | Aidecl of typedef * variable * string * (expression list)
  | If of expression * inst
  | Ifelse of expression * inst * inst
  | While of expression * inst
  | For of (expression list) * expression * (expression list) * inst
  | Afor of (expression list) * (expression list) * inst
  | Ibloc of bloc
  | Cout of expression_str list
  | Return of expression
  | Areturn

and bloc = Bloc of inst list

and declaration =
  | Dv of declaration_v
  | Dc of declaration_c
  | Db of prototype * bloc
type file =
  { bincludeios : bool;
    declarations: declaration list;
  }
