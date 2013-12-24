(* Typage pour Mini-C++ *)


open Ast

exception Error of loc * string

type typ =
    | Tnull
    | Tint
    | Tvoid
    | Tpointeur of typ
    | Tclass of string


type 'a atype = { v:'a ; typ:typ }



type tsupers =  TSuper of  string list


type tvar = tdvar atype
and tdvar =
  | Ident of string
  | Po of tvar
  | Ad of tvar


type targ = TArg of typ * tvar


type tqident =
  | TQident of string
  | TDouble of string * string


type tqvar =
  | TQvar of tqident
  | TQpo of tqvar
  | TQad of tqvar


type tproto =
  | TPlong of typ * tqvar * (targ list)
  | TPshort of string * (targ list)
  | TPdouble of string * string * (targ list)


type tdecl_v = TDeclv of typ * (tvar list)

type tmembre =
  | TMvar of tdecl_v
  | TMvir of bool * tproto


type tdecl_c =
  | TClass of string *  tsupers * (tmembre list)
  


type texpr = tdexpr atype

and tdexpr  =
  | TEint of int
  | TEthis
  | TEbool of bool
  | TEnull
  | TEqident of tqident
  | TEpointeur of texpr
  | TEattr of texpr * string
  | TEsderef of texpr * string
  | TEassign of texpr * texpr
  | TEfcall of texpr * (texpr list)
  | TEnew of string * (texpr list)
  | TElincr of texpr
  | TEldecr of texpr
  | TErincr of texpr
  | TErdecr of texpr
  | TEland of texpr
  | TEnot of texpr
  | TEuminus of texpr
  | TEuplus of texpr
  | TEop of operateur * texpr * texpr
  | TEpar of texpr


type texpr_str = tdexpr_str atype 
and tdexpr_str =
  | TEsexpr of texpr 
  | TEstring of string



type tinst =
  | TNothing
  | TIexpr of texpr
  | TIdecls of typ * tvar
  | TIdecl of typ * tvar * texpr
  | TAidecl of typ * tvar * string * (texpr list)
  | TIf of texpr * tinst
  | TIfelse of texpr * tinst * tinst
  | TWhile of texpr * tinst
  | TFor of (texpr list) * texpr * (texpr list) * tinst
  | TAfor of (texpr list) * (texpr list) * tinst
  | TIbloc of tbloc
  | TCout of texpr_str list
  | TReturn of texpr
  | TAreturn


and tbloc = TBloc of tinst list


and tdecl =
  | TDv of tdecl_v
  | TDc of tdecl_c
  | TDb of tproto * tbloc


type tfichier =
  { tbincludeios : bool;
    tdecls: tdecl list;
  }

(* ********************************** *)

(* '*************************)
module Smap = Map.Make(String)

type env = typ Smap.t

let table_f = Hashtbl.create 17 ;; (* on enregistre les fonctions en clé et les listes des arguments possibles pour prendre en compte la surcharge *)

Hashtbl.add table_f "" ([]:(targ list)) ;;

let table_c = (Hashtbl.create 17) ;; (* on enregistre ici les classes en clé, leurs super classes en champ, toujours avec le chamo "" pour pouvoir enregistrer les classes sans super classes *)

Hashtbl.add table_c "" "";;


(* '*************************)



(* renvoie true si c est une super-classe d'un des éléments de l, ou d'une super-classe de l, false sinon*)
let rec remonte c l = match l with
	| [] -> false
	| ""::ll -> remonte c l
	| cl::l -> (c = cl) || (remonte c (Hashtbl.find_all table_c cl)) || (remonte c l)

let rec is_sub_class c1 c2 =
	(c1 = c2) || (
		try
			(remonte c2 (Hashtbl.find_all table_c c1))
		with _ -> failwith ("La classe " ^ c1 ^ " n'est pas définie."))


let rec is_sub_type t1 t2 = match (t1, t2) with
	| Tint, Tint -> true
	| Tnull, Tpointeur(_) -> true
	| (Tpointeur a), (Tpointeur b) -> is_sub_type a b
	| (Tclass a), (Tclass b) -> is_sub_class a b
	| _ -> false


let is_num t = match t with
	| Tnull | Tint | Tpointeur _ -> true
	| _ -> false


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



