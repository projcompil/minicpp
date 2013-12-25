(* Typage pour Mini-C++ *)


open Ast

exception Error of loc * string

type typ =
    | Tnull
    | Tint
    | Tvoid
    | Tpointeur of typ
    | Tclass of string


type 'a atype = { c:'a ; typ:typ }


type ident = { rep:string; typ:typ ; lvl:int } 

type tsupers =  TSuper of  typ list


type tvar = tdvar atype
and tdvar =
  | TIdent of ident 
  | TPo of tvar
  | TAd of tvar


type targ = TArg of typ * tvar



type tqident =
  | TQident of ident
  | TStatic of string * ident 


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
  | TMmeth of bool * tproto


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
  | TEattr of texpr * ident
  | TEsderef of texpr * ident 
  | TEassign of texpr * texpr
  | TEfcall of texpr * (texpr list)
  | TEnew of string * (texpr list)
  | TElincr of texpr
  | TEldecr of texpr
  | TErincr of texpr
  | TErdecr of texpr
  | TEaddr of texpr
  | TEnot of texpr
  | TEuminus of texpr
  | TEuplus of texpr
  | TEop of operateur * texpr * texpr
  | TEpar of texpr


type texpr_str =
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

type environnement = typ Smap.t

let table_f = Hashtbl.create 17 ;; (* on enregistre les fonctions en clé et les listes des arguments possibles et de valeurs de retour possibles  pour prendre en compte la surcharge *)

Hashtbl.add table_f "" ((Tnull,[]):(typ * (targ list))) ;;

let table_c = (Hashtbl.create 17) ;; (* on enregistre ici les classes en clé, leurs super classes en champ, toujours avec le chamo "" pour pouvoir enregistrer les classes sans super classes *)

Hashtbl.add table_c "" "";;


(* '*************************)



(* renvoie true si c est une super-classe d'un des éléments de l, ou d'une super-classe de l, false sinon*)
let rec remonte c l = match l with
	| [] -> false
	| ""::l -> remonte c l
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

let is_type s = (Hashtbl.mem Lexerhack.table s)

let rec is_bf t = match t with
	| Tint -> true
	| Tpointeur t -> is_bf t
	| Tclass s -> Hashtbl.mem table_c s
	| _ -> false


let rec is_left_value e (env:environnement) = match e.v with
	| Eqident ex -> begin match ex.v with
				| Qident s -> Smap.mem s env
				| Static _ -> false	
			end  (* c'est faux prendre en compte en outre les références qui sont des valeurs gauches *) 
	| Epointeur _ | Esderef _ | Eattr _ -> true
	| Epar ex -> is_left_value ex env
	| _ -> false  (* y en a-t-ul d'autres ? *)

let not_left loc =
	raise (Error (loc, "L'expression n'est pas une valeur gauche.\n"))

(* ******************************************************************************* *)



let typsupers sup =
	let rec aux l = match l with
		| [] -> []
		| s::l -> if Hashtbl.mem table_c s then
				(Tclass s)::(aux l)
			  else raise (Error (sup.loc, "Le nom " ^ s ^ " n'est pas le nom d'une classe déjà définie plus haut\n"))
	in match sup.v with Super l -> TSuper (aux l)



let rec typvar v env = match v.v with
	| Ident s -> begin try
			let t = Smap.find s env in
			{ c = (TIdent { rep = s; typ = t ; lvl = 0 }) ; typ = t } (* a priori non satisfaisant pour lvl : remplacer ident par string ? *)
		     with Not_found -> raise (Error(v.loc, "L'identifiant " ^ s ^ " n'est pas le nom d'une variable déclarée plus tôt.\n"))
		     end
	| Po va -> let tva = typvar va env in
			{ c =  (TPo tva) ; typ = (Tpointeur (tva.typ)) }
	| Ad va -> let tva = typvar va env in
			{ c = (TAd tva) ; typ = tva.typ }

(*
type arg = darg pos

and darg = Arg of typedef * var

type qident = dqident pos 

and dqident =
  | Qident of string
  | Static of string * string

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
  | Mmeth of bool * proto

type decl_c = ddecl_c pos

and ddecl_c =
  | Class of string *  supers * (membre list)
  
*)

let rec typexpr expr env = match expr.v with
  | Eint i -> { c = TEint i ; typ = Tint }
  | Ethis -> failwith "Expression non encore implémentée.\n"
  | Ebool b -> { c = TEint (if b then 1 else 0) ; typ = Tint }
  | Enull-> { c = TEnull ; typ = Tnull }
  | Eqident q -> failwith "Expression non encore implémentée.\n" (* à faire !!!! *)
  | Epointeur e -> if is_left_value e env then
			let te = typexpr e env in begin match te.typ with 
				| Tpointeur t -> {c = TEpointeur te ; typ = t }
				| _ -> raise (Error (expr.loc, "Déférencement d'une expression qui n'est pas un pointeur.\n"))
			end
		   else not_left expr.loc 
  | Eattr (e,s)-> failwith "Expression non encore implémentée.\n"
  | Esderef (e,s) ->failwith "Expression non encore implémentée.\n"
  | Eassign (e,f)-> if (is_left_value e env) then
			let te = typexpr e env and tf = typexpr f env in
				if is_sub_type tf.typ te.typ then
					if is_num te.typ then
						{ c = TEassign ( te, tf) ; typ = te.typ }
					else
						raise (Error (expr.loc, "Le type de la première expression dans l'assignation n'est pas un type numérique.\n"))
				else raise (Error (expr.loc, "Le type de la deuxième expression dans l'assignation n'est pas un sous-type du type de la première.\n"))
		    else raise (Error (expr.loc, "L'expression n'est pas une valeur gauche.\n"))
  | Efcall (e, l)->failwith "Expression non encore implémentée.\n"
  | Enew (nc, l) ->failwith "Expression non encore implémentée.\n"
  | Elincr e-> if is_left_value e env then
                        let te = typexpr e env in begin match te.typ with 
                                | Tint-> {c = TElincr te ; typ = Tint }
                                | _ -> raise (Error (expr.loc, "Incrémentation à gauche d'une expression non entière.\n"))
                        end
	       else not_left expr.loc
  | Eldecr e -> if is_left_value e env then
                        let te = typexpr e env in begin match te.typ with
                                | Tint-> {c = TEldecr te ; typ = Tint }
                                | _ -> raise (Error (expr.loc, "Décrémentation à gauche d'une expression non entière.\n"))
                        end
                else not_left expr.loc 
  | Erincr e -> if is_left_value e env then
                        let te = typexpr e env in begin match te.typ with
                                | Tint-> {c = TErincr te ; typ = Tint }
                                | _ -> raise (Error (expr.loc, "Incrémentation à droite d'une expression non entière.\n"))
                        end
                else not_left expr.loc
  | Erdecr e -> if is_left_value e env then
                        let te = typexpr e env in begin match te.typ with
                                | Tint-> {c = TErdecr te ; typ = Tint }
                                | _ -> raise (Error (expr.loc, "Décrémentation à droite d'une expression non entière.\n"))
                        end
                else not_left expr.loc
  | Eaddr e -> if is_left_value e env then
			let te = typexpr e env in
				{ c = TEaddr te ; typ = te.typ }
	       else not_left expr.loc
  | Enot e -> let te = typexpr e env in begin match te.typ with
		| Tint -> { c = TEnot te ; typ = Tint }
		| _ -> raise (Error (expr.loc, "Négation d'une valeur non entière.\n"))
		end
  | Euminus e -> let te = typexpr e env in begin match te.typ with
                | Tint -> { c = TEuminus te ; typ = Tint }
                | _ -> raise (Error (expr.loc, "Signe (moins) d'une valeur non entière.\n"))
                end
  | Euplus e-> let te = typexpr e env in begin match te.typ with
                | Tint -> { c = TEuplus te ; typ = Tint }
                | _ -> raise (Error (expr.loc, "Signe (plus) d'une valeur non entière.\n"))
                end
  | Eop (op, e, f)-> begin match op with
			| Eq | Neq -> let te = typexpr e env in
					if is_num te.typ then
						let tf = typexpr f env in
							if te.typ = tf.typ then
								{ c = TEop(op, te, tf) ; typ = Tint }
							else raise (Error(expr.loc, "Les deux expressions n'ont pas le même type, il est impossible de les comparer via == ou !=.\n"))
					else raise (Error(expr.loc, "Le type de l'expression à gauche de l'opérateur de test d'" ^ (if op = Eq then "" else "in") ^ "égalité n'est pas numérique."))
			
			| _ -> let te = typexpr e env in
					if te.typ = Tint then
						let tf = typexpr f env in
							if tf.typ = Tint then
								{ c = TEop(op, te, tf) ; typ = Tint }
							else raise (Error (expr.loc, "Opération sur une valeur non entière. L'expression de droite n'est pas entière\n"))
					else raise (Error (expr.loc, "Opération sur une valeur non entière. L'expression de gauche n'est pas entière\n"))
		     end
  | Epar e -> let te = typexpr e env in
		{ c = TEpar te ; typ = te.typ }

let rec typdinst i env = match i with
	| Nothing -> TNothing, env
	| Iexpr e -> TIexpr (typexpr e env), env 
	| Idecls (tdef, v)-> failwith "non implémenté"
	| Idecl (tdef, v, e) -> failwith "non implémenté"
	| Aidecl (tdef, v, s, l)-> failwith "non implémenté"
 	| If (e, ins)-> let te = typexpr e env in
				if te.typ = Tint then
					let (tins,envir) = typinst ins env in
						(TIf (te, tins)), env
				else raise (Error (e.loc, "L'expression à l'intérieur du if n'est pas entière.\n"))
	| Ifelse (e, ins1, ins2) -> let te = typexpr e env in
                               				if te.typ = Tint then
                                        			let (tins1,envir1) = typinst ins1 env and (tins2, envir2) = typinst ins2 env in
                                                		(TIfelse (te, tins1, tins2)), env
                                			else raise (Error (e.loc, "L'expression à l'intérieur du if n'est pas entière.\n")) 
	| While (e, ins) -> let te = typexpr e env in
                                if te.typ = Tint then
                                        let (tins,envir) = typinst ins env in
                                                (TWhile (te, tins)), env
                                else raise (Error (e.loc, "L'expression à l'intérieur du while n'est pas entière.\n"))
	| For (l1, e, l2, ins) -> let tl1 = List.map (fun x -> typexpr x env) l1 in
					let te = typexpr e env in
					if te.typ = Tint then
						let tl2 = List.map (fun x -> typexpr x env) l2 in
							let tins, envir = typinst ins env in
								(TFor (tl1, te, tl2, tins)), env
					else raise (Error (e.loc, "L'expression de contrôle à l'intérieur du for n'est pas entière.\n"))
	| Afor (l1, l2, ins) -> let tl1 = List.map (fun x -> typexpr x env) l1 in
                    			let tl2 = List.map (fun x -> typexpr x env) l2 in
                        			let tins, envir = typinst ins env in
                                			(TFor (tl1, {c = TEint 1; typ = Tint } , tl2, tins)), env 
	| Ibloc b -> (TIbloc (typbloc b env)), env
 	| Cout le -> let rec auxcout l = match l with
			| [] -> []
			| x::l -> (match x.v with
					| Esexpr e -> let te = typexpr e env in
							if te.typ = Tint then
								TEsexpr te
							else raise (Error (x.loc, "Cout d'une expression qui n'est ni entière, ni une chaîne.\n"))
					| Estring s -> TEstring s)::(auxcout l)
		     in (TCout (auxcout le)), env
	| Return e -> (TReturn (typexpr e env)), env 
	| Areturn -> TAreturn, env

and typinst i env = (typdinst (i.v) env)

and typdbloc bl env = match bl with
	| Bloc [] -> (TBloc [])
	| Bloc (i::l) -> let (ti, envir) = typinst i env in
				let (TBloc tl) = typdbloc (Bloc l) envir in
					TBloc (ti::tl)
and typbloc bl env = (typdbloc (bl.v) env)


(*
and decl = ddecl pos

and ddecl =
  | Dv of decl_v
  | Dc of decl_c
  | Db of proto * bloc

*)
(*
let typdecl p env = match p with
 	| Db (pr,bl) -> let (r, envir) = typbloc bl env in
				(TDb (TProtovide, r)), envir
	| _ -> failwith "non implémenté"
				
*)
let typfichier f = 
	failwith "non implémenté"
(*
    let rec auxf l envi = match l with
	| [] -> [] 
	| x::l -> let (r, envir) = typdecl x envi in
			r::(auxf l envir)
    in  { tbincludeios = p.bincludeios ;
	tdecls= (auxf (p.decls) Smap.empty) }


*)



