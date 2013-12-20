(* Typage pour Mini-C++ *)


open Ast

exception Error of loc * string

type typ =
    | Tnull
    | Tint
    | Tvoid
    | Tpointeur of typ
    | Tclass of string


(*type targ = TArg of typedef * var

type tqident =
  | TQident of string
  | TDouble of string * string

type tqvar =
  | TQvar of tqident
  | TQpo of tqvar
  | TQad of tqvar
*)
type tproto =
  | TProtovide
(*  | TPlong of typedef * tqvar * (arg list)
  | TPshort of string * (arg list)
  | TPdouble of string * string * (arg list)
i*)
(*
type decl_v = Declv of typedef * (var list)
i*)
type texpr = { tdexpr:tdexpr ; typ:typ; }

and tdexpr  =
  | TEint of int
  | TEthis
  | TEbool of bool
  | TEnull
  | TEqident of qident
  | TEpointeur of expr
  | TEattr of expr * string
  | TEsderef of expr * string
  | TEassign of expr * expr
  | TEfcall of expr * (expr list)
  | TEnew of string * (expr list)
  | TElincr of expr
  | TEldecr of expr
  | TErincr of expr
  | TErdecr of expr
  | TEland of expr
  | TEnot of expr
  | TEuminus of expr
  | TEuplus of expr
  | TEop of operateur * expr * expr
  | TEpar of expr


type texpr_str = { tdexpr_str:tdexpr_str ; typ:typ; }
and tdexpr_str =
  | TEsexpr of texpr
  | TEstring of string


type tinst = { tdinst:tdinst ; typ:typ; }

and tdinst =
  | TNothing
  | TIexpr of expr
(*  | TIdecls of typedef * var
  | TIdecl of typedef * var * expr
  | TAidecl of typedef * var * string * (expr list)
  | TIf of expr * tinst
  | TIfelse of expr * tinst * tinst
  | TWhile of expr * tinst
  | TFor of (expr list) * expr * (expr list) * tinst
  | TAfor of (expr list) * (expr list) * tinst
*)  | TIbloc of bloc
  | TCout of expr_str list
  | TReturn of expr
  | TAreturn

and tbloc = TBloc of tinst list

and tdecl =
(*  | TDv of tdecl_v
  | TDc of tdecl_c*)
  | TDb of tproto * tbloc

type tfichier =
  { tbincludeios : bool;
    tdecls: tdecl list;
  }






(* '*************************)
module Smap = Map.Make(String)

type env = typ Smap.t

let table_f = Hashtbl.create 17 ;; (* on enregistre les fonctions en clé et les listes des arguments possibles pour prendre en compte la surcharge *)

Hashtbl.add table_f "" [""] ;;

let table_c = Hashtbl.create 17 ;; (* on enregistre ici les classes en clé, leurs super classes en champ, toujours avec le chamo "" pour pouvoir enregistrer les classes sans super classes *)

Hashtbl.add table_c "" "";;


(* '*************************)




let typdinst p env= match p with
	| Nothing -> TNothing
	| Iexpr e -> failwith "non implementé"
	| Idecls (tdef, v)-> failwith "non implémenté"
	| Idecl (tdef, v, e) -> failwith "non implémenté"
	| Aidecl (tdef, v, s, l)-> failwith "non implémenté"
 	| If _-> failwith "non implémenté"
	| Ifelse _-> failwith "non implémenté"
	| While _ -> failwith "non implémenté"
	| For _ -> failwith "non implémenté"
	| Afor _ -> failwith "non implémenté"
	| Ibloc _ -> failwith "non implémenté"
 	| Cout _ -> failwith "non implémenté"
	| Return _ -> failwith "non implémenté"
	| Areturn -> TAreturn


let typbloc bl env = match bl with
	| _ -> failwith "non implémenté"
(*
	| Bloc of [] -> (*(TBloc []) , env*) failwith "non implémenté"
	| Bloc of (i::l) -> failwith "non implémenté"
*)

(*
let typdecl p env = match p with
 	| Db (pr,bl) -> let (r, envir) = typbloc bl env in
				(TDb (TProtovide, r)), envir
	| _ -> failwith "non implémenté"
				
*)
let typfichier p = 
	failwith "non implémenté"
(*
    let rec auxf l envi = match l with
	| [] -> [] 
	| x::l -> let (r, envir) = typdecl x envi in
			r::(auxf l envir)
    in  { tbincludeios = p.bincludeios ;
	tdecls= (auxf (p.decls) Smap.empty) }


*)



