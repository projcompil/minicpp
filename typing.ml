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
  | TQident of  string * typ (* retour éventuel à string * typ *)
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

let biostream = ref false
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
			end  
	| Epointeur _ | Esderef _ | Eattr _ -> true
	| Epar ex -> is_left_value ex env
	| _ -> false  (* y en a-t-ul d'autres ? *)

let not_left loc =
	raise (Error (loc, "L'expression n'est pas une valeur gauche.\n"))

(* ******************************************************************************* *)


let typtypedef t = match t with
	| Void -> Tvoid
	| Int -> Tint
	| Tid s -> Tclass s


let typsupers sup =
	let rec aux l = match l with
		| [] -> []
		| s::l -> if Hashtbl.mem table_c s then
				(Tclass s)::(aux l)
			  else raise (Error (sup.loc, "Le nom " ^ s ^ " n'est pas le nom d'une classe déjà définie plus haut\n"))
	in match sup.v with Super l -> TSuper (aux l)



let rec typvar v niveau env = match v.v with
	| Ident s -> begin try
			let t = Smap.find s env in
			{ c = (TIdent { rep = s; typ = t ; lvl = niveau }) ; typ = t } (* a priori non satisfaisant pour lvl : remplacer ident par string ? *)
		     with Not_found -> raise (Error(v.loc, "L'identifiant " ^ s ^ " n'est pas le nom d'une variable déclarée plus tôt.\n"))
		     end
	| Po va -> let tva = typvar va niveau env in
			{ c =  (TPo tva) ; typ = (Tpointeur (tva.typ)) }
	| Ad va -> let tva = typvar va niveau env in
			{ c = (TAd tva) ; typ = tva.typ }


let typarg a env = match a.v with
	| Arg(t, v) ->  let tt = typtypedef t in
				if is_bf tt then
					TArg( (typtypedef t), (typvar v 1 env))
				else raise (Error (a.loc, "Le type de l'argument n'est pas bien formé.\n"))



(* Faux dans certains cas : valeur retour fonction pour qvar --> rajouter argument si type qvar ou non *)
let typqident q env = match q.v with
  | Qident s -> begin try
			let tq = Smap.find s env in
				TQident (s, tq)
		      with Not_found -> raise (Error (q.loc, "L'identifiant " ^ s ^ " : not in scope."))
		end
  | Static (st, s) -> failwith "Non implémenté\n"

module Sset = Set.Make(String)

let find_duplicate liste =
	let rec auxd l ens  = match l with
		| [] -> (false, None)
		| x::l -> if Sset.mem x ens then
				(true, (Some x))
			else auxd l (Sset.add x ens)
	in auxd liste (Sset.empty)


(* ******************************* Non implémenté ************************* *)


let rec typqvar v env = match v.v with
	| Qvar q -> failwith "Non implémenté\n"
	| Qpo qv -> failwith "Non implémenté\n" 
	| Qad qv -> failwith "Non implémenté\n"


(* vérifier les doublons *)

let typproto p env = match p.v with
	| Plong (t, qv, l) -> failwith "Non implémenté\n"
	| Pshort (s, l) -> failwith "Non implémenté\n"
	| Pdouble (s, s2, l) -> failwith "Non implémenté\n"

(* Retourner l'environnement, vérifier les doublons *)
let typdecl_v dv env = match dv.v with
	| Declv(t, l) -> failwith "Non implémenté\n"



let typmembre m env = match m.v with
	| Mvar dv -> failwith "Non implémenté\n"
	| Mmeth (b, p) -> failwith "Non implémenté\n" 


let typdecl_c dc env = match dc.v with
  | Class (s, sup, l) -> failwith "Non implémenté\n" 
  

(* ***********************Fin Non implémenté ************************* *)

let rec typexpr expr env = match expr.v with
  | Eint i -> { c = TEint i ; typ = Tint }
  | Ethis -> (*failwith "Expression non encore implémentée.\n"*)
		begin try 
			let t = Smap.find "this" env in 
  				begin match t with 
  					| Tpointeur (Tclass s) -> { c = TEthis ; typ = t }
  					| _ -> raise (Error (expr.loc, "this est un pointeur vers un objet\n"))
				end
  			with Not_found -> raise (Error (expr.loc, "Utilisation de this en dehors d'une classe.\n")) end	
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

let rec typinst i env = match i.v with
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

 	| Cout le -> if (not !biostream) then raise (Error (i.loc, "Appel de std::cout, mais le fichier n'inclut pas la bibliothèque iostream.\n"))
		     else
			let rec auxcout l = match l with
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

(* and typinst i env = (typdinst (i.v) env)*)

and typdbloc bl env = match bl with
	| Bloc [] -> (TBloc [])
	| Bloc (i::l) -> let (ti, envir) = typinst i env in
				let (TBloc tl) = typdbloc (Bloc l) envir in
					TBloc (ti::tl)
and typbloc bl env = (typdbloc (bl.v) env)



let typdecl d env = match d.v with
	| Dv dv -> let (tdv, envir) = typdecl_v dv env in ( TDv tdv), envir
        | Dc dc -> failwith "non implémenté\n"
        | Db (p, bl) -> let (tp, envir) = typproto p env in
				let tbl = typbloc bl envir in
					(TDb (tp, tbl)), env 

(*
 	| Db (pr,bl) -> let (r, envir) = typbloc bl env in
				(TDb (TProtovide, r)), envir
	|
*)				

let rec auxtfichier l env = match l with
	| [] -> [] 
	| x::l -> let (r, envir) = typdecl x env in
			r::(auxtfichier l envir)
let typfichier f = 
		biostream := (f.v).bincludeios ;
		let tf = { tbincludeios = (f.v).bincludeios ;
                                        tdecls= (auxtfichier ((f.v).decls) Smap.empty) } in
			match (Hashtbl.find_all table_f "main") with
				| [] -> raise (Error (f.loc, "Il n'y a pas de fonction main déclarée dans le fichier.\n"))
				| [ (Tint, []) ]-> tf
				| [ (_, []) ]-> raise (Error (f.loc, "L'unique fonction main du fichier n'est pas de type int.\n"))
				| [ (Tint, _) ] -> raise (Error (f.loc, "L'unique fonction main du fichier possède des arguments dans son prototype, ce qui n'est pas autorisé.\n"))

				| _ -> raise (Error (f.loc, "Il y a plusieurs fonctions main déclarées dans le fichier.\n"))


