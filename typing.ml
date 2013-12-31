(* Typage pour Mini-C++ *)


open Ast

exception Error of loc * string

type typ =
    | Tnull
    | Tint
    | Tvoid
    | Tpointeur of typ
    | Tclass of string
    | Fonc


type 'a atype = { c:'a ; typ:typ }


type ident = { rep:string; mutable (* oui c'est moche *) typ:typ ; lvl:int ; offset : int ;  byref:bool} 

type tsupers =  TSuper of  typ list


type tvar = tdvar atype
and tdvar =
  | TIdent of ident 
  | TPo of tvar
  | TAd of tvar


type targ = TArg of typ * tvar



type tqident =
  | TQident of ident (* retour éventuel à ident * typ /// string * typ *)
  | TQmeth of string * ident 


type tqvar =
  | TQvar of tqident
  | TQpo of tqvar
  | TQad of tqvar


type tproto =
  | TProto of typ * tqvar * (targ list)
  | TPcons of string * (targ list)
  | TPconshc of string * string * (targ list)


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
  | TIdecl of typ * tvar
  | TIdeclinit of typ * tvar * texpr
  | TIdeclobj of typ * tvar * string * (texpr list)
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

type environnement = (*typ*) ident Smap.t

let table_f = Hashtbl.create 17 ;; (* on enregistre les fonctions en clé et les listes des arguments possibles et de valeurs de retour possibles  pour prendre en compte la surcharge *)

Hashtbl.add table_f "" ((Tnull,[]):(typ * (targ list))) ;;

let table_c = (Hashtbl.create 17) ;; (* on enregistre ici les classes en clé, leurs super classes en champ, toujours avec le chamo "" pour pouvoir enregistrer les classes sans super classes *)

Hashtbl.add table_c "" "";;


let table_c_meth = (Hashtbl.create 17) ;;

let table_c_member = (Hashtbl.create 17) ;;

let junk1 = Hashtbl.create 17 ;; (* Pour les besoins de l'initialisation des types. *)
let junk2 = Hashtbl.create 17 ;;

Hashtbl.add junk1 "" ((Tnull,[]):(typ * (targ list))) ;;

Hashtbl.add junk2 "" { rep = "" ; typ = Tvoid ; lvl = 0 ; offset = 0 ; byref = false };;

Hashtbl.add table_c_meth "" junk1 ;;

Hashtbl.add table_c_member "" junk2 ;;



let biostream = ref false

let chtypereturn = "@typereturn"

(* '*************************)


let erreur loc s =
	raise (Error(loc, s))

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
	| Tint, Tint | Tnull, Tpointeur(_) -> true
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
				| Qmeth _ -> false	
			end  
	| Epointeur _ | Esderef _ | Eattr _ -> true
	| Epar ex -> is_left_value ex env
	| _ -> false  (* y en a-t-ul d'autres ? *)

let not_left loc =
(*	erreurloc, "L'expression n'est pas une valeur gauche.\n"))*)
	erreur loc "L'expression n'est pas une valeur gauche.\n"


let rec size_type t = match t with
	| Tvoid -> 0
	| Tint -> 4
	| Tnull -> 4 (* ou 0 ?*)
	| Tpointeur _ -> 4
	| Tclass s -> failwith "Taille de type classe non implémentée.\n" (* aller chercher taille dans une table *) 
	| Fonc -> 0

let rec extract_var v = match v.v with
	| Ident s -> s
	| Po va | Ad va-> extract_var va

let rec extract_qvar qv = match qv.v with
	| Qvar q -> q
	| Qpo qva | Qad qva -> extract_qvar qva

let rec extract_tvar tv = match tv.c with
	| TIdent i -> i
	| TPo tva | TAd tva-> extract_tvar tva

let rec extract_tqvar tqv = match tqv with
	| TQvar q -> q
	| TQpo tqva | TQad tqva -> extract_tqvar tqva

let add_meth c m l =
	Hashtbl.add (Hashtbl.find table_c_meth c) m l

let find_meth c m =
	Hashtbl.find (Hashtbl.find table_c_meth c) m

let add_member c m i =
	Hashtbl.add (Hashtbl.find table_c_member c) m i

let find_meth c m =
        Hashtbl.find (Hashtbl.find table_c_member c) m

let rec tvar_by_ref tva = match tva.c with
	| TIdent id -> id.byref
	| TPo tva | TAd tva -> tvar_by_ref tva

let tqvar_by_ref tqva = match tqva.c with
	| TQad _ -> true
	| _ -> false

let type_of_tqident tq = match tq with
	| TQident i -> i.typ
	| _ -> Fonc


let rec signaturef l = match l with
	| [] -> []
	| (TArg(tt, v))::l -> tt::(signaturef l)

let rec egal_sign l1 l2 =
	(signaturef l1) = (signaturef l2) (* on peut faire plus efficace en parcourant les deux listes en même temps *)

let rec f_is_in_list l lsf = match lsf with
	| [] -> false
	| (tt, lg)::lsf -> (egal_sign l lg) || (f_is_in_list l lsf)

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
			  else erreur sup.loc ("Le nom " ^ s ^ " n'est pas le nom d'une classe déjà définie plus haut\n")
	in match sup.v with Super l -> TSuper (aux l)


(* prend le type attendu t et essaie de l'ajouter à l'environnement *)
let rec typvar v env lvl t = 
	let rec auxvar v b t = match v.v with
		| Ident s -> begin try
			let tid = Smap.find s env in
				if tid.lvl = lvl then
					erreur v.loc ("Impossible de redéfinir " ^ s ^ ", car cet identifiant est déjà défini à ce niveau.")
				else { c = (TIdent { rep = s; typ = t ; lvl = lvl; offset = 0 (* le changer *); byref = b  }) ; typ = t } 
		     with Not_found -> {c =  (TIdent { rep = s; typ = t ; lvl = lvl; offset = 0 (* le changer *); byref = b }) ; typ = t } (*erreur v.loc ("L'identifiant " ^ s ^ " n'est pas le nom d'une variable déclarée plus tôt.\n")*)
		     end
		| Po { v = Ad va ; loc = loc } ->  erreur v.loc "Impossible de de prendre un type de pointeur vers une référence.\n"
		| Po va -> let tva = auxvar va b t in
				{ c =  (TPo tva) ; typ = (Tpointeur (tva.typ)) }
		| Ad { v = Ad va ; loc = loc } -> erreur v.loc "Impossible de de prendre une référence de référence.\n"
		| Ad va -> let tva = auxvar va true t in
				{ c = (TAd tva) ; typ = tva.typ }

	in let tv = auxvar v false t in
		let idtv = extract_tvar tv in
			idtv.typ <- tv.typ; (* hack assez moche, mais cela fera l'affaire pour l'instant *)
			let envir = Smap.add (idtv.rep) idtv env in
				(tv, envir)

(** sûrement un problème sur cette fonction, on ajoute un ident de type t, mais comment savoir de quelle type il sera ? *)


let typarg a env = match a.v with
	| Arg(t, v) ->  let tt = typtypedef t in
				if is_bf tt then
				let (tv, envir) = typvar v env 1 tt in
					if (is_num tv.typ) || (tvar_by_ref tv) then
						(TArg( (typtypedef t), tv)), envir
					else erreur a.loc "Les paramètres d'une fonctions doivent être numériques, ou passées par référence.\n"

				else erreur a.loc "Le type de l'argument n'est pas bien formé.\n"



(* Faux dans certains cas : valeur retour fonction pour qvar --> rajouter argument si type qvar ou non *)
let typqident q env lvl bdecl = match q.v with
  | Qident s -> begin try
			let tid = Smap.find s env in
				if not bdecl then
					if tid.lvl <= lvl then
						TQident tid
					else erreur q.loc  ("L'identifiant " ^ s ^ " : not in scope.")
				else
					if tid.lvl <> lvl then
						TQident tid
					else erreur q.loc ("Impossible de redéfinir l'identifiant " ^ s ^"qui a déjà été défini au même niveau.\n")
		      with Not_found -> if bdecl || (Hashtbl.mem table_f s) then
						TQident { rep = s ; typ = Fonc ;  lvl = lvl ; offset = 0 ; byref = false }
				else erreur q.loc ("L'identifiant " ^ s ^ " : not in scope.")
		end
  | Qmeth (st, s) -> failwith "Non implémenté\n"

module Sset = Set.Make(String)

let find_duplicate liste =
	let rec auxd l ens  = match l with
		| [] -> (false, None)
		| x::l -> if Sset.mem x ens then
				(true, (Some x))
			else auxd l (Sset.add x ens)
	in auxd liste (Sset.empty)


(* ******************************* Non implémenté ************************* *)


let rec typqvar v env lvl bdecl = match v.v with
	| Qvar q -> TQvar (typqident q env lvl bdecl)
	| Qpo { v = Qad qv ; loc = loc } -> erreur v.loc "Impossible de déclarer un pointeur vers une référence.\n"
	| Qpo qv -> TQpo (typqvar qv env lvl bdecl)
	| Qad { v = Qad qv ; loc = loc } -> erreur v.loc "Impossible d'utiliser une référence de référence.\n"
	| Qad qv -> TQad (typqvar qv env lvl bdecl)


(* vérifier les doublons *)


(* ajoute une liste de variables issues d'arguments à l'environnement *)
let rec add_args l (*lvl*) env = match l with
	| [] -> [], env
	| a::l ->  let (ta, envir) = typarg a env in
			let (tl, renv) = (add_args l envir) in
				(ta::tl), renv

let typproto p env = match p.v with
	| Proto (t, qv, l) -> let tt = typtypedef t in
				let tqv = typqvar qv env 0 true in
				   let (tl, envir) = add_args l env in
				   let tqid = extract_tqvar tqv in begin
				   match tqid with
					| TQident id -> if (f_is_in_list tl (Hashtbl.find_all table_f (id.rep))) then
	erreur p.loc "Une fonction de même signature a déjà été déclarée.\n"
							else begin
								Hashtbl.add table_f id.rep (tt, tl) ;
								(* à complétér*)
								let renv = Smap.add (id.rep) { rep = id.rep ; typ = Fonc ; lvl = 0 ; offset = 0 ; byref = false } envir in
				let brenv = (Smap.add "@typereturn" {rep = "@typereturn" ; typ = tt ; lvl = 1 ; offset = 0 ; byref = false (* à changer *)} renv) in
								(TProto(tt, tqv, tl)), brenv, renv
							end


					| TQmeth(s,id) -> failwith "Non implémenté\n"
				end
	| Pcons (s, l) -> failwith "Non implémenté\n"
	| Pconshc (s, s2, l) -> failwith "Non implémenté\n"

(* Retourner l'environnement, vérifier les doublons *)
let rec auxdecl_v l env lvl t = match l with
	| [] -> [], env
	| v::l -> let (tv, envir) = typvar v env lvl t in
                        let (tl, renv) = (auxdecl_v l envir lvl t) in
                                (tv::tl), renv

let rec typdecl_v dv env lvl = match dv.v with
	| Declv(t, l) -> let tt = typtypedef t in
			   if is_bf tt then
			   	let (tl, envir) = (auxdecl_v l env lvl tt) in
					(TDeclv(tt, tl)), envir
			
			   else erreur dv.loc "Le type de cette déclaration n'est pas bien formé.\n"
 (*failwith "Non implémenté\n" (* let tt = typtypedef t in


				let rec auxdv l env = match l with
					| [] ->
				auxdv l env*)
			*)	 



let typmembre m env = match m.v with
	| Mvar dv -> failwith "Non implémenté\n"
	| Mmeth (b, p) -> failwith "Non implémenté\n" 


let typdecl_c dc env lvl = match dc.v with
  | Class (s, sup, l) -> failwith "Non implémenté\n" 
  

(* ***********************Fin Non implémenté ************************* *)

let rec typexpr expr env lvl = match expr.v with
  | Eint i -> { c = TEint i ; typ = Tint }
  | Ethis -> (*failwith "Expression non encore implémentée.\n"*)
		begin try 
			let tid = Smap.find "this" env in 
  				begin match tid.typ with 
  					| Tpointeur (Tclass s) -> { c = TEthis ; typ = tid.typ }
  					| _ -> erreur expr.loc "this est un pointeur vers un objet\n"
				end
  			with Not_found -> erreur expr.loc "Utilisation de this en dehors d'une classe.\n" end
  | Ebool b -> { c = TEint (if b then 1 else 0) ; typ = Tint }
  | Enull-> { c = TEnull ; typ = Tnull }
  | Eqident q -> let tq = typqident q env lvl false in { c = TEqident tq ; typ = (type_of_tqident tq)}
  | Epointeur e -> if is_left_value e env then
			let te = typexpr e env lvl in begin match te.typ with 
				| Tpointeur t -> {c = TEpointeur te ; typ = t }
				| _ -> erreur expr.loc "Déférencement d'une expression qui n'est pas un pointeur.\n"
			end
		   else not_left expr.loc 
  | Eattr (e,s)-> failwith "Expression non encore implémentée.\n"
  | Esderef (e,s) -> failwith "Expression non encore implémentée.\n"(* let te = typexpr e env lvl in
			match te.typ with
				| Tpointeur (TClass s) -> {} *)
  | Eassign (e,f)-> if (is_left_value e env) then
			let te = typexpr e env lvl and tf = typexpr f env lvl in
				if is_sub_type tf.typ te.typ then
					if is_num te.typ then
						{ c = TEassign ( te, tf) ; typ = te.typ }
					else
						erreur expr.loc "Le type de la première expression dans l'assignation n'est pas un type numérique.\n"
				else erreur expr.loc "Le type de la deuxième expression dans l'assignation n'est pas un sous-type du type de la première.\n"
		    else erreur expr.loc "L'expression n'est pas une valeur gauche.\n"
  | Efcall (e, l)-> let te = typexpr e env lvl in begin
			let tl = List.map (fun x -> typexpr x env lvl) l in
			match te.c with
				| TEqident (TQident i) -> failwith "non implemented"
				| TEqident (TQmeth(_)) -> failwith "Non implémenté.\n" 
				| _ -> erreur e.loc "Cette expression n'est pas une fonction et donc ne peut être appliquée.\n"
		end

  | Enew (nc, l) ->failwith "Expression non encore implémentée.\n"
  | Elincr e-> if is_left_value e env then
                        let te = typexpr e env lvl in begin match te.typ with 
                                | Tint-> {c = TElincr te ; typ = Tint }
                                | _ -> erreur expr.loc "Incrémentation à gauche d'une expression non entière.\n"
                        end
	       else not_left expr.loc
  | Eldecr e -> if is_left_value e env then
                        let te = typexpr e env lvl in begin match te.typ with
                                | Tint-> {c = TEldecr te ; typ = Tint }
                                | _ -> erreur expr.loc "Décrémentation à gauche d'une expression non entière.\n"
                        end
                else not_left expr.loc 
  | Erincr e -> if is_left_value e env then
                        let te = typexpr e env lvl in begin match te.typ with
                                | Tint-> {c = TErincr te ; typ = Tint }
                                | _ -> erreur expr.loc "Incrémentation à droite d'une expression non entière.\n"
                        end
                else not_left expr.loc
  | Erdecr e -> if is_left_value e env then
                        let te = typexpr e env lvl in begin match te.typ with
                                | Tint-> {c = TErdecr te ; typ = Tint }
                                | _ -> erreur expr.loc "Décrémentation à droite d'une expression non entière.\n"
                        end
                else not_left expr.loc
  | Eaddr e -> if is_left_value e env then
			let te = typexpr e env lvl in
				{ c = TEaddr te ; typ = te.typ }
	       else not_left expr.loc
  | Enot e -> let te = typexpr e env lvl in begin match te.typ with
		| Tint -> { c = TEnot te ; typ = Tint }
		| _ -> erreur expr.loc "Négation d'une valeur non entière.\n"
		end
  | Euminus e -> let te = typexpr e env lvl in begin match te.typ with
                | Tint -> { c = TEuminus te ; typ = Tint }
                | _ -> erreur expr.loc "Signe (moins) d'une valeur non entière.\n"
                end
  | Euplus e-> let te = typexpr e env lvl in begin match te.typ with
                | Tint -> { c = TEuplus te ; typ = Tint }
                | _ -> erreur expr.loc "Signe (plus) d'une valeur non entière.\n"
                end
  | Eop (op, e, f)-> begin match op with
			| Eq | Neq -> let te = typexpr e env lvl in
					if is_num te.typ then
						let tf = typexpr f env lvl in
							if te.typ = tf.typ then
								{ c = TEop(op, te, tf) ; typ = Tint }
							else erreur expr.loc "Les deux expressions n'ont pas le même type, il est impossible de les comparer via == ou !=.\n"
					else erreur expr.loc ("Le type de l'expression à gauche de l'opérateur de test d'" ^ (if op = Eq then "" else "in") ^ "égalité n'est pas numérique.")
			
			| _ -> let te = typexpr e env lvl in
					if te.typ = Tint then
						let tf = typexpr f env lvl in
							if tf.typ = Tint then
								{ c = TEop(op, te, tf) ; typ = Tint }
							else erreur expr.loc "Opération sur une valeur non entière. L'expression de droite n'est pas entière\n"
					else erreur expr.loc "Opération sur une valeur non entière. L'expression de gauche n'est pas entière\n"
		     end
  | Epar e -> let te = typexpr e env lvl in
		{ c = TEpar te ; typ = te.typ }




(** instruction **)


let rec typinst i env lvl = match i.v with
	| Nothing -> TNothing, env
	| Iexpr e -> TIexpr (typexpr e env lvl), env 
	| Idecl (tdef, v)-> let tt = typtypedef tdef in
				if is_bf tt then
			    	let tv, envir = (typvar v env lvl tt) in
					(TIdecl(tt, tv)), envir
				else erreur i.loc "Déclaration d'une variable de type non bien formé.œ\n"
	| Ideclinit (tdef, v, e) -> (* vérifier si v est une référence, que e est une valeur gauche *) 				let tt = typtypedef tdef in
					if is_bf tt then
                            	    		let tv, envir = (typvar v env lvl tt) in
						let te = typexpr e env lvl in
							if is_sub_type te.typ tt then
								if not(tvar_by_ref tv) then
									(TIdeclinit(tt, tv,te)), envir
								else
									if is_left_value e env then
										(TIdeclinit(tt, tv,te)), envir
									else erreur e.loc "Cette expression n'est pas une valeur gauche, mais est pourtant assignée à une référence.\n"
							else erreur e.loc "Cette expression n'est pas d'un type qui est sous-type du type de déclaration de la variable.\n"
					else erreur i.loc "Déclaration d'une variable de type non bien formé.\n"
	| Ideclobj (tdef, v, s, l)-> failwith "non implémenté"
 	| If (e, ins)-> let te = typexpr e env lvl in
				if te.typ = Tint then
					let (tins,envir) = typinst ins env lvl in
						(TIf (te, tins)), env
				else erreur e.loc "L'expression à l'intérieur du if n'est pas entière.\n"
	| Ifelse (e, ins1, ins2) -> let te = typexpr e env lvl in
                               				if te.typ = Tint then
                                        			let (tins1,envir1) = typinst ins1 env lvl and (tins2, envir2) = typinst ins2 env lvl in
                                                		(TIfelse (te, tins1, tins2)), env
                                			else erreur e.loc "L'expression à l'intérieur du if n'est pas entière.\n"
	| While (e, ins) -> let te = typexpr e env lvl in
                                if te.typ = Tint then
                                        let (tins,envir) = typinst ins env lvl in
                                                (TWhile (te, tins)), env
                                else erreur e.loc "L'expression à l'intérieur du while n'est pas entière.\n"
	| For (l1, e, l2, ins) -> let tl1 = List.map (fun x -> typexpr x env lvl) l1 in
					let te = typexpr e env lvl in
					if te.typ = Tint then
						let tl2 = List.map (fun x -> typexpr x env lvl) l2 in
							let tins, envir = typinst ins env lvl in
								(TFor (tl1, te, tl2, tins)), env
					else erreur e.loc "L'expression de contrôle à l'intérieur du for n'est pas entière.\n"
	| Afor (l1, l2, ins) -> let tl1 = List.map (fun x -> typexpr x env lvl) l1 in
                    			let tl2 = List.map (fun x -> typexpr x env lvl) l2 in
                        			let tins, envir = typinst ins env lvl in
                                			(TFor (tl1, {c = TEint 1; typ = Tint } , tl2, tins)), env 
	| Ibloc b -> (TIbloc (typbloc b env (lvl + 1))), env

 	| Cout le -> if (not !biostream) then erreur i.loc "Appel de std::cout, mais le fichier n'inclut pas la bibliothèque iostream.\n"
		     else
			let rec auxcout l = match l with
			| [] -> []
			| x::l -> (match x.v with
					| Esexpr e -> let te = typexpr e env lvl in
							if te.typ = Tint then
								TEsexpr te
							else erreur x.loc "Cout d'une expression qui n'est ni entière, ni une chaîne.\n"
					| Estring s -> TEstring s)::(auxcout l)
		     in (TCout (auxcout le)), env

	| Return e -> begin try let tr = Smap.find chtypereturn env in
			let te = (typexpr e env lvl) in 
				if is_sub_type te.typ (tr.typ) then (TReturn te), env 
				else erreur e.loc "Le type de l'expression retournée ne correspond pas à un sous-type de retour du prototype de la fonction.\n"
			with Not_found -> erreur i.loc ("Return en dehors d'une fonction ?!! La fonction n'a pas ajouté " ^ chtypereturn ^" au contexte.\n")
		       end
	| Areturn -> begin try let tr = Smap.find chtypereturn env in 
				if tr.typ = Tvoid then TAreturn, env
                                else erreur i.loc "Le type de l'expression retournée ne correspond pas au type de retour du prototype de la fonction.\n"
			with Not_found -> erreur i.loc ("Return en dehors d'une fonction ?!! La fonction n'a pas ajouté " ^ chtypereturn ^" au contexte.\n")
                     end

(* and typinst i env = (typdinst (i.v) env)*)


(* ajouter la prise en charge des niveaux d'imbrication *)
and typdbloc bl env lvl = match bl with
	| Bloc [] -> (TBloc [])
	| Bloc (i::l) -> let (ti, envir) = typinst i env lvl in
				let (TBloc tl) = typdbloc (Bloc l) envir lvl in
					TBloc (ti::tl)
and typbloc bl env lvl = (typdbloc (bl.v) env lvl)



let typdecl d env = match d.v with
	| Dv dv -> let (tdv, envir) = typdecl_v dv env 0 in ( TDv tdv), envir
        | Dc dc -> let (tdc, envir) = typdecl_c dc env 0 in (TDc tdc), envir
        | Db (p, bl) -> let (tp, envir, env_hb) = typproto p env in
				let tbl = typbloc bl envir 1 in
					(TDb (tp, tbl)), env_hb 

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
				| [] -> erreur f.loc "Il n'y a pas de fonction main déclarée dans le fichier.\n"
				| [ (Tint, []) ]-> tf
				| [ (_, []) ]-> erreur f.loc "L'unique fonction main du fichier n'est pas de type int.\n"
				| [ (Tint, _) ] -> erreur f.loc "L'unique fonction main du fichier possède des arguments dans son prototype, ce qui n'est pas autorisé en Mini-C++.\n"

				| _ -> erreur f.loc "Il y a plusieurs fonctions main déclarées au sein du fichier.\n"


