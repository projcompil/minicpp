(* Typage pour Mini-C++ *)


open Ast

exception Error of loc * string

exception Member_dejavu of string

exception Not_implementedt of string

exception Class_not_found of string
let ratet s =
	raise (Not_implementedt("Typage de cette caractéristique non implémenté : " ^ s ^ "\n"))

type typ =
    | Tnull
    | Tint
    | Tvoid
    | Tpointeur of typ
    | Tclass of string
    | Fonc
    | Tclassdecl

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

type tqvar = tdqvar atype
and tdqvar =
  | TQvar of tqident
  | TQpo of tqvar
  | TQad of tqvar


type tproto =
  | TProto of typ * tqvar * (targ list) * int
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
  (*| TEbool of bool*)
  | TEnull
  | TEqident of tqident
  | TEpointeur of texpr
  | TEattr of texpr * ident
  (*| TEsderef of texpr * ident *)
  | TEassign of texpr * texpr
  | TEfcall of texpr * (texpr list) * int * bool (* numéro fonction appelée et indique si son retour est une référence*)
  | TEmcall of texpr * (texpr list) * string (* nom de la classe de la méthode appelée *) * int * bool
  | TEnew of string * (texpr list) * int (* numéro du constructeur appelé *)
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
  | TIdeclobj of typ * tvar * string * (texpr list) * int (* numéro du constructeur appelé *)
  (*| TIf of texpr * tinst*)
  | TIfelse of texpr * tinst * tinst
  | TWhile of texpr * tinst
  | TFor of (texpr list) * texpr * (texpr list) * tinst
  (*| TAfor of (texpr list) * (texpr list) * tinst*)
  | TIbloc of tbloc
  | TCout of texpr_str list
  | TReturn of texpr
  | TAreturn


and tbloc = TBloc of (tinst list) * int


and tdecl =
  | TDv of tdecl_v
  | TDc of tdecl_c
  | TDb of tproto * tbloc * int


type tfichier =
  { tbincludeios : bool;
    tdecls: tdecl list;
  }

(* ********************************** *)
module Smap = Map.Make(String)

type environnement = ident Smap.t

let ( table_f : (string, (int * bool * typ * (targ list))) Hashtbl.t) = Hashtbl.create 17 ;; (* on enregistre les fonctions en clé et les listes des arguments possibles et de valeurs de retour possibles  pour prendre en compte la surcharge *)

Hashtbl.add table_f "" ((0,false,Tnull,[]):(int * bool * typ * (targ list))) ;;

type hashstring = (string, string) Hashtbl.t
let (table_c : hashstring) = (Hashtbl.create 17) ;; (* on enregistre ici les classes en clé, leurs super classes en champ, toujours avec le chamo "" pour pouvoir enregistrer les classes sans super classes *)

Hashtbl.add table_c "" "";;


type tdemeths =( string,  (int * (bool*bool) * typ * (targ list))) Hashtbl.t
let ( table_c_meth : (string, tdemeths) Hashtbl.t ) = (Hashtbl.create 17) ;;

type tdemems = (string, ident) Hashtbl.t
let (table_c_member : (string, tdemems) Hashtbl.t ) = (Hashtbl.create 17) ;;

let ( table_c_size : (string, int) Hashtbl.t )= (Hashtbl.create 17) 


let junk1 = Hashtbl.create 17 ;; (* Pour les besoins de l'initialisation des types. *)
let junk2 = Hashtbl.create 17 ;;

Hashtbl.add junk1 "" ((0, ((* retourne une référence *) false, (*virtual*) false), Tnull, []):(int * (bool*bool) * typ * (targ list))) ;;

Hashtbl.add junk2 "" { rep = "" ; typ = Tvoid ; lvl = 0 ; offset = 0 ; byref = false };;

Hashtbl.add table_c_meth "" junk1 ;;

Hashtbl.add table_c_member "" junk2 ;;

Hashtbl.add table_c_size "" 0 ;;

let ( table_c_env : (string, environnement) Hashtbl.t ) = Hashtbl.create 17;;

Hashtbl.add table_c_env "" Smap.empty;;

let biostream = ref false

let chtypereturn = "@typereturn"

let chcons = "@constructor"
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
		with _ -> 
			raise (Class_not_found("La classe " ^ c1 ^ " n'est pas définie.")))


let rec is_sub_type t1 t2 = match (t1, t2) with
	| Tint, Tint | Tnull, Tpointeur(_) -> true
	| (Tpointeur (Tclass a)), (Tpointeur (Tclass b)) -> is_sub_class a b
	| (Tclass a), (Tclass b) -> is_sub_class a b
    | a, b when a = b -> true
	| _ -> false

let is_sub_targ (TArg(t1, v1)) (TArg(t2, v2)) =
	is_sub_type t1 t2

let is_num t = match t with
	| Tnull | Tint | Tpointeur _ -> true
	| _ -> false

let is_type s = (Hashtbl.mem Lexerhack.table s)

let rec is_bf t = match t with
	| Tint -> true
	| Tpointeur t -> is_bf t
	| Tclass s -> Hashtbl.mem table_c s
	| _ -> false

let is_pointeur = function
	| Tpointeur _ | Tnull -> true
	| _ -> false

let classe_de = function
	| Tclass s -> s
    | Tpointeur(Tclass s) -> s
	| _ -> failwith "Erreur : l'expression n'est pas un objet ou un pointeur vers un objet.\n"

let is_classe = function
	| Tclass s-> true
    | _ -> false

(**********)
let union_env env1 env2 = (* fusion des deux environnements avec priorité pour les éléments du premier*)
    let f k a b = match (a,b) with
        | None, None -> None
        | Some(x), _ -> Some(x)
        | None, Some(y) -> Some(y)
    in Smap.merge f env1 env2

let add_func tab f t b l =
	let lf = Hashtbl.find_all tab f in
    let j = (match lf with
        	                | [] -> 0
        	                | (i, _, _, _)::l -> (i+1))
    in
        Hashtbl.add tab f (j, b, t, l);
        j    

let add_f f t b l =
	add_func table_f f t b l

let find_all_meth_sr c m =
	Hashtbl.find_all (Hashtbl.find table_c_meth c) m

let find_all_member_sr c m =
        Hashtbl.find_all (Hashtbl.find table_c_member c) m

let rec nettoie l = match l with
    | [] -> []
	| ""::l -> nettoie l
	(*| x::l when not (Hashtbl.mem table_c_meth x) -> l*)
	| y::l -> y::(nettoie l)

let remontegen c m f =
	let rec auxremonte l = 
		let ln = nettoie l in
		(List.flatten (List.map (fun x -> f x m) ln)) @ (List.flatten (List.map (fun x -> auxremonte (Hashtbl.find_all table_c x) ) ln))
	in auxremonte [c]

let remontemeth c m = remontegen c m find_all_meth_sr
let remontemember c m = remontegen c m find_all_member_sr

let find_all_meth c m =
	remontemeth c m

let add_meth c m t b (* attention !! couple de booléens !!! *) l =
	add_func (Hashtbl.find table_c_meth c) m t b l

let remonteisgen c m f =
	let rec auxremonte l = 
		let ln = nettoie l in
		(List.exists (fun x -> f x m) ln) || (List.exists (fun x -> auxremonte (Hashtbl.find_all table_c x) ) ln)
	in auxremonte [c]

let is_meth_sr c m =
	Hashtbl.mem ((Hashtbl.find table_c_meth c)) m 

let is_meth c m =
    remonteisgen c m is_meth_sr

let find_meth c m =
	Hashtbl.find (Hashtbl.find table_c_meth c) m



let is_member_sr c m =
	Hashtbl.mem ((Hashtbl.find table_c_member c)) m 

let is_member c m =
    remonteisgen c m is_member_sr
	(*Hashtbl.mem ((Hashtbl.find table_c_member c)) m *)

let add_member c m i =
	let tc = (Hashtbl.find table_c_member c) in
		if Hashtbl.mem tc m then
			raise (Member_dejavu m)
		else Hashtbl.add tc m i


let find_all_member c m =
    remontemember c m    
(*    Hashtbl.find_all (Hashtbl.find table_c_member c) m*)

let find_member c m =
        Hashtbl.find (Hashtbl.find table_c_member c) m

let remonteenvgen c f =
	let rec auxremonteenv l = begin match l with
        | [] -> Smap.empty
        | l -> 
		let ln = nettoie l in
            let conc = fun y x -> union_env y (f x) in
            let concl = List.fold_left (conc) Smap.empty in
		        union_env (concl ln) (List.fold_left union_env Smap.empty (List.map (fun x -> auxremonteenv (Hashtbl.find_all table_c x)) ln))

    end
    in auxremonteenv [c]

let get_envc_sr c =
    try
	    Hashtbl.find table_c_env c 
    with Not_found -> raise (Class_not_found c)

let get_envc c =
    remonteenvgen c get_envc_sr
    

let ajoute_env c env =
	let bindings = Smap.bindings (Hashtbl.find table_c_env c) in
		List.fold_left (fun envi (k, v) -> Smap.add k v envi) env bindings

(*******)


let rec is_left_value e (env:environnement) = match e.v with
	| Eqident ex -> begin match ex.v with
				| Qident s -> begin try 
						let id = Smap.find s env in
							(id.typ <> Fonc) || id.byref
					      with Not_found -> false
					      end
				| Qmeth _ -> false	
			end  
	| Epointeur _ | Esderef _ | Eattr _ -> true
	| Epar ex -> is_left_value ex env
	| _ -> false  


let rec is_left_tvalue te env = match te.c with
    | TEqident ex -> begin match ex with
                                | TQident id -> (id.typ <> Fonc) || (id.byref)
                                | TQmeth _ -> false
                        end
    | TEpointeur _ | TEthis | TEnew _ -> true
	| TEattr ({ c = te ; typ = (Tclass nc) }, s) (*| TEsderef ({ c = te ; typ = (Tpointeur (Tclass nc)) }, s) *) -> is_member nc s.rep
	| TEfcall(_,_,_, b) -> b
	| TEmcall(_,_,_,_,b) -> b
    | TElincr _ | TEldecr _ -> true
    | TEpar ex -> is_left_tvalue ex env
    | _ -> false  (* y en a-t-il d'autres ? *)

let not_left loc =
	erreur loc "L'expression n'est pas une valeur gauche.\n"


let rec size_type t = match t with
	| Tint -> 4
	| Tnull -> 4 (* ou 0 ?*)
	| Tpointeur _ -> 4
	| Tclass s -> begin try Hashtbl.find table_c_size s
                        with Not_found -> raise (Class_not_found "")
                  end
	| Tvoid | Fonc | Tclassdecl -> 0

let rec extract_var v = match v.v with
	| Ident s -> s
	| Po va | Ad va-> extract_var va

let rec extract_qvar qv = match qv.v with
	| Qvar q -> q
	| Qpo qva | Qad qva -> extract_qvar qva

let rec extract_tvar tv = match tv.c with
	| TIdent i -> i
	| TPo tva | TAd tva-> extract_tvar tva


let size_tvar tv = size_type((extract_tvar tv).typ)

let rec extract_tqvar tqv = match tqv.c with
	| TQvar q -> q
	| TQpo tqva | TQad tqva -> extract_tqvar tqva


let rec name_tqvar tqv =
    match (extract_tqvar tqv) with
                    | TQident tid -> tid.rep
                    | _ -> ratet "Nom de TQmeth non implémenté.\n"

let rec tvar_by_ref tva = match tva.c with
	| TIdent id -> id.byref
	| TPo tva | TAd tva -> tvar_by_ref tva

let tqvar_by_ref (tqva :tqvar) = match tqva.c with
	| TQad _ -> true
	| TQvar _ -> false
    | TQpo _ -> false


let is_qualified_qvar qv =
	match (extract_qvar qv).v with
		| Qident _ -> false
		| Qmeth _ -> true

let type_of_tqident tq = match tq with
	| TQident i -> i.typ
	| _ -> Fonc


let rec signaturef l = match l with
	| [] -> []
	| (TArg(tt, v))::l -> (v.typ)::(signaturef l) (* ou remplacer par t:: *)

let rec egal_sign l1 l2 =
	(signaturef l1) = (signaturef l2) (* on peut faire plus efficace en parcourant les deux listes en même temps *)

let rec f_is_in_list l lsf = match lsf with
	| [] -> false
	| (i,b, tt, lg)::lsf -> (egal_sign l lg) || (f_is_in_list l lsf)


let rec fit_types l la = match (l, la) with
	| [], [] -> true
	| [], _ -> false
	| _, [] -> false
	| (x::l), (TArg(t,v)::la) -> (is_sub_type x (v.typ) (* tt *)) && (fit_types l la)

(* prendre le minimum dans l'ensemble *)

let rec include_sign f g = match (f,g) with
	| [], [] -> true
	| [], _ | _, [] -> false
	| tax::f, tay::g -> (is_sub_targ tax tay) && (include_sign f g)

let min_sign f g =
	if include_sign f g then f else g

let scan_lf l lf =
	let rec auxscan lf min min_res = match lf with
		| [] -> min_res
		| (ni, b, tt,la)::lf -> if fit_types l la then
                                    begin match min with
                                        | None -> auxscan lf (Some la) (Some(la, ni, tt, b))
                                        | Some(lmin) -> if include_sign la lmin then 
							                                auxscan lf (Some(la)) (Some(la, ni, tt, b))
						                                else auxscan lf min min_res

                                    end
			    		        else auxscan lf min min_res
	in auxscan lf None None


module Sset = Set.Make(String)

let find_duplicate liste =
	let rec auxd l ens  = match l with
		| [] -> None
		| x::l -> if Sset.mem x ens then
				(Some x)
			else auxd l (Sset.add x ens)
	in auxd liste (Sset.empty)

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
	in match sup.v with Super l -> 
		begin match find_duplicate l with
			| None -> TSuper (aux l)
			| Some(x) -> erreur sup.loc ("La liste des superclasses contient des doublons, dont celui-ci : " ^ x ^ ".\n")
		end

(* prend le type attendu t et essaie de l'ajouter à l'environnement *)
let rec typvar v env lvl t off = 
	let rec auxvar v b t = match v.v with
		| Ident s -> begin try
			let tid = Smap.find s env in
				if tid.lvl = lvl && (tid.typ <> Fonc || t <> Fonc) then
					erreur v.loc ("Impossible de redéfinir " ^ s ^ ", car cet identifiant est déjà défini à ce niveau.")
				else { c = (TIdent { rep = s; typ = t ; lvl = lvl; offset = off (* le changer *); byref = b  }) ; typ = t } 
		     with Not_found -> {c =  (TIdent { rep = s; typ = t ; lvl = lvl; offset = off (* le changer *); byref = b }) ; typ = t } 
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


let typarg a env = match a.v with
	| Arg(t, v) ->  let tt = typtypedef t in
				if is_bf tt then
				let (tv, envir) = typvar v env 1 tt 0  in
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
					else erreur q.loc  ("L'identifiant " ^ s ^ "  n'est pas à portée. (bdecl = true)\n")
				else
					if tid.lvl <> lvl || tid.typ = Fonc then
						TQident tid
					else erreur q.loc ("Impossible de redéfinir l'identifiant " ^ s ^" qui a déjà été défini au même niveau.\n")
		      with Not_found -> if bdecl || (Hashtbl.mem table_f s) then
						TQident { rep = s ; typ = Fonc ;  lvl = lvl ; offset = 0 ; byref = false }
				else erreur q.loc ("L'identifiant " ^ s ^ " n'est pas à portée.\n")
		end
  | Qmeth (st, s) -> begin
			if bdecl then
				 TQmeth(st, { rep = s ; typ = Fonc ;  lvl = lvl ; offset = 0 ; byref = false })
			else try
				let tid = Smap.find "this" env in
					if is_sub_type tid.typ (Tpointeur (Tclass st)) then 
                        if is_meth st s then
                            TQmeth (st, { rep = s ; typ = Fonc ;  lvl = lvl ; offset = 0 ; byref = false })
                        else erreur q.loc (s ^ " n'est pas une méthode de la classe " ^ st )
					else erreur q.loc ("this n'est pas d'un type sous-type de " ^ st ^"\n")
				with Not_found -> erreur q.loc ("this n'est pas ajouté à l'environnement (bug !!).\n")

		     end 





let rec typqvar v env lvl bdecl tt = match v.v with
    | Qvar q -> { c = TQvar (typqident q env lvl bdecl) ; typ = tt }
	| Qpo { v = Qad qv ; loc = loc } -> erreur v.loc "Impossible de déclarer un pointeur vers une référence.\n"
    | Qpo qv -> let tqv = (typqvar qv env lvl bdecl tt) in { c = (TQpo tqv) ; typ = Tpointeur (tqv.typ) }
	| Qad { v = Qad qv ; loc = loc } -> erreur v.loc "Impossible d'utiliser une référence de référence.\n"
	| Qad qv -> let tqv = (typqvar qv env lvl bdecl tt) in  { c = (TQad tqv) ; typ = (tqv.typ) }



(* ajoute une liste de variables issues d'arguments à l'environnement *)
let rec add_args l (*lvl*) env = match l with
	| [] -> [], env
	| a::l ->  let (ta, envir) = typarg a env in
			let (tl, renv) = (add_args l envir) in
				(ta::tl), renv

let typproto p env in_class = match p.v with
	| Proto (t, qv, l) -> let tt = typtypedef t in
				let tqv = typqvar qv env 0 true tt in
				   let (tl, envir) = add_args l env in 
(* ce serait mieux de l'avoir après mais sans duplication de code *)				   let tqid = extract_tqvar tqv in begin
				   match (tqid, in_class) with
					| (TQident id), None -> if (f_is_in_list tl (Hashtbl.find_all table_f (id.rep))) then
	erreur p.loc "Une fonction de même signature a déjà été déclarée.\n"
							else begin
                                let i = add_f id.rep tqv.typ (tqvar_by_ref tqv) tl in
								let prov = { rep = id.rep ; typ = Fonc ; lvl = 0 ; offset = 0 ; byref = (tqvar_by_ref tqv)}
	in let renv = Smap.add (id.rep) prov env in
				let brenv = Smap.add (id.rep) prov (Smap.add chtypereturn {rep = chtypereturn ; typ = tqv.typ ; lvl = 1 ; offset = 0 ; byref = (tqvar_by_ref tqv) } envir) in
								(TProto(tt, tqv, tl, i)), brenv, renv
							end


					| (TQmeth(s,id)), None ->  if (f_is_in_list tl (find_all_meth_sr  s (id.rep))) then 
                        let brenv = union_env (Smap.add "this" { rep = "this" ; typ = (Tpointeur(Tclass s)) ; lvl = 0 ; offset = 0 ; byref = false } (Smap.add chtypereturn {rep = chtypereturn ; typ = tqv.typ ; lvl = 1 ; offset = 0 ; byref = (tqvar_by_ref tqv) } envir)) (get_envc s) in
                                                    (TProto(tt, tqv, tl, (*prvisoire : i *) 0)), brenv, env
                                               else erreur p.loc ("Cette méthode n'a pas été déclarée dans la classe " ^ s^ ".\n")
					| (TQident id), (Some (nc, bvir)) -> if f_is_in_list tl (find_all_meth_sr nc id.rep) then erreur p.loc "Une méthode de même signature a déjà été déclarée.\n"
	else begin
        let i = add_meth nc id.rep tqv.typ ((tqvar_by_ref tqv), bvir) tl in
        let prov = { rep = id.rep ; typ = Fonc ; lvl = 0 ; offset = 0 ; byref = (tqvar_by_ref tqv)}
	        in let renv = Smap.add (id.rep) prov env in

        (TProto(tt, tqv, tl, i)), env, renv 
	end
					| (TQmeth(s,id)), (Some (nc, bvir)) -> erreur p.loc "Extra-qualification.\n" 
				end
	| Pcons (s, l) -> let tl, renv = add_args l env in
				if f_is_in_list tl (find_all_meth s (chcons ^ s)) then
					erreur p.loc "Un constructeur de même signature a déjà été déclarée.\n"				       
				else begin let i = add_meth s (chcons ^ s) (Tclass s) (false, (match in_class with | None -> failwith "Heisenbug 3.\n"
								     | Some (nc, bvir) -> bvir) ) tl in
					(TPcons(s, tl)), env, env
				end
	| Pconshc (s, s2, l) -> assert (in_class = None); 
				if s <> s2 then
					erreur p.loc "Ce prototype n'est pas le prototype d'un constructeur.\n"
				else let (tl, envir) = add_args l env in
					if not (f_is_in_list tl (find_all_meth s (chcons ^ s))) then
						erreur p.loc "Aucun constructeur avec cette signature n'a été déclaré au sein de la classe.\n"
					else 
                        let brenv = union_env (Smap.add "this" { rep = "this" ; typ = (Tpointeur(Tclass s)) ; lvl = 0 ; offset = 0 ; byref = false } (Smap.add chtypereturn {rep = chtypereturn ; typ = (Tclass s) ; lvl = 1 ; offset = 0 ; byref = false (* ou true ? *) } envir)) (get_envc s) in
                                                    (TPconshc(s, s2, tl)), brenv, env

	
(* Retourner l'environnement, vérifier les doublons *)
let rec auxdecl_v l env lvl t = match l with
	| [] -> [], env, 0
	| v::l -> let (tv, envir) = typvar v env lvl t 0 (* bug le changer *) in
                        let (tl, renv, offs) = (auxdecl_v l envir lvl t) in
                                (tv::tl), renv, (offs + (size_tvar tv))

let rec typdecl_v env lvl dv = match dv.v with
	| Declv(t, l) -> let tt = typtypedef t in
			   if is_bf tt then
			   	let (tl, envir, off) = (auxdecl_v l env lvl tt) in
					(TDeclv(tt, tl)), envir, off
			
			   else erreur dv.loc "Le type de cette déclaration n'est pas bien formé.\n"



let typmembre s (* classe du membre *) env lvl m  = match m.v with
	| Mvar dv -> begin try let ((TDeclv(tt, tl)), envir, off) = typdecl_v env lvl dv in
			let tlid = List.map (fun x -> let tid = (extract_tvar x) in if tid.typ = (Tclass s) then erreur m.loc ("Ce champ : " ^ tid.rep  ^ "a un type incomplet (utiliser un pointeur pour déclarer des champs du même type que la classe dans cette-même classe).\n") else tid) tl in
		     	 begin try
				List.iter (fun x -> (add_member s x.rep x)) tlid;
				(TMvar (TDeclv(tt, tl))), envir, off
			with (Member_dejavu nm) -> erreur m.loc ("Le membre " ^ nm ^ " est déjà déclarée dans cette classe (ou est un doublon dans cette déclaratioon.\n")
			     end
            with Class_not_found s -> erreur m.loc "Cette déclaration comprend une variable de type une classe non définie, ou encore non totalement définie (utiliser des pointeurs pour obtenir des structures récursives).\n"
         end

	| Mmeth(_, { v = Proto(_, qv, _) ; loc = _ }) when (is_qualified_qvar qv) -> erreur m.loc "Extra-qualification du prototype de la méthode au sein de la déclaration d'une classe.\n"
	| Mmeth (b, p) -> let (tp, benvir ,envir) = typproto p env (Some (s, b)) in
				TMmeth(b, tp), envir, 0  


let typdecl_c dc env lvl = match dc.v with
  | Class (s, sup, l) -> if Hashtbl.mem table_c s then
				erreur dc.loc ("La classe " ^ s ^" a déjà été déclarée.\n")
			 else 
				let (TSuper tl) = typsupers sup in
					List.iter (Hashtbl.add table_c s) (""::(List.map (function | (Tclass ch) -> ch | _ -> failwith "Heisenbug.\n") tl)) ;
					let (new_tmethod : tdemeths) = Hashtbl.create 10 and (new_tmember : tdemems) = Hashtbl.create 10 in
					Hashtbl.add table_c_meth s new_tmethod ;
					Hashtbl.add table_c_member s new_tmember;
					let rec auxdeclc envi l = match l with
						| [] -> [], envi, 0
						| x::l -> let tx, envir, off = typmembre s envi lvl x in
								let reste, renvir, offs = auxdeclc envir l in
									(tx::reste), renvir, (off + offs)
					in
					let (lm, envir, off) = auxdeclc env l in
						let renv = Smap.add s {rep = s ; typ = Tclassdecl ; lvl = 0 ; offset = 0 ; byref = false } env in
						Hashtbl.add table_c_env s envir ;
                        Hashtbl.add table_c_size s off;
						if is_meth s (chcons ^ s) then
							(TClass(s, (TSuper tl), lm)), renv
						else begin
                            let i = add_meth s (chcons ^ s) (Tclass s) (false, false) [] in
							(TClass(s, (TSuper tl), lm)), renv	
						end


let rec typexpr expr env lvl = match expr.v with
    | Eint i -> { c = TEint i ; typ = Tint }
    | Ethis -> begin try 
		let tid = Smap.find "this" env in 
  			begin match tid.typ with 
  				| Tpointeur (Tclass s) -> { c = TEthis ; typ = (tid.typ) }
  				| _ -> erreur expr.loc "this est un pointeur vers un objet\n"
			end
		with Not_found -> erreur expr.loc "Utilisation de this en dehors d'une méthode.\n" end
    | Ebool b -> { c = TEint (if b then 1 else 0) ; typ = Tint }
    | Enull-> { c = TEnull ; typ = Tnull }
    | Eqident q -> let tq = typqident q env lvl false in { c = TEqident tq ; typ = (type_of_tqident tq)}
    | Epointeur e -> let te = typexpr e env lvl in 
			if is_left_tvalue te env then 
				begin match te.typ with 
					| Tpointeur t -> {c = TEpointeur te ; typ = t }
					| _ -> erreur expr.loc "Déférencement d'une expression qui n'est pas un pointeur.\n"
				end
		   	else not_left expr.loc 
    | Eattr (e,s)-> let te = typexpr e env lvl in
			begin match te.typ with 
				| Tclass nc -> if is_member nc s then 
							let tidm = List.hd (find_all_member nc s) in
								{ c = TEattr (te, tidm) ; typ = tidm.typ }
					       else if is_meth nc s then
								{ c = TEattr (te,  {rep = s ; typ = Fonc ; lvl = lvl ; offset = 0 ; byref = false }) ; typ = Fonc }  (* il faut sûrement rajouter des constructeurs de type, pour prendre en compte les méthodes, et leurs applications !! *)
						else erreur expr.loc (s ^ " n'est pas un membre de la classe " ^ nc)
				| _ -> erreur e.loc "Cette expression ne représente pas un objet, on ne peut donc utiliser l'opérateur . pour accéder à un de ses membres.\n"
			end

    | Esderef (e,s) -> typexpr { v = Eattr({v = Epointeur(e)  ;loc = e.loc},s) ; loc = expr.loc } env lvl 

    | Eassign (e,f)-> let te = typexpr e env lvl in 
			if is_left_tvalue te env then
				let tf = typexpr f env lvl in
				if is_sub_type tf.typ te.typ then
					if is_num te.typ then
						{ c = TEassign ( te, tf) ; typ = te.typ }
					else
						erreur expr.loc "Le type de la première expression dans l'assignation n'est pas un type numérique.\n"
			else erreur expr.loc "Le type de la deuxième expression dans l'assignation n'est pas un sous-type du type de la première.\n"
		    else not_left expr.loc 
  | Efcall (e, l)-> let te = typexpr e env lvl in begin
			let tl = List.map (fun x -> typexpr x env lvl) l in
			match te.c with
				| TEqident (TQident id) -> if id.typ = Fonc then
                                if Smap.mem "this" env then
                                    let tthis = Smap.find "this" env in
                                        let nc = classe_de tthis.typ in
                                            let lm = find_all_meth nc id.rep in begin
				                                match (scan_lf (List.map (fun (x:texpr) -> x.typ) tl) lm) with
                                                    | None -> let lf = Hashtbl.find_all table_f (id.rep) in begin
				match (scan_lf (List.map (fun (x:texpr) -> x.typ) tl) lf) with 
					| None -> erreur e.loc "Aucune fonction n'a une signature conforme aux types des arguments avec laquelle elle est appelée, ou bien l'ensemble de ces fonctions n'a pas de minimum pour l'inclusion de profils.\n"
					| Some(f,i,tt, b) -> { c = TEfcall(te, tl, i, b) ; typ = tt } 
			  end
                                                    | Some(f,i, tt, b) ->  { c = TEfcall(te, tl, i, (fst b)) ; typ = tt }
         end
                                            else let lf = Hashtbl.find_all table_f (id.rep) in begin
				match (scan_lf (List.map (fun (x:texpr) -> x.typ) tl) lf) with 
					| None -> erreur e.loc "Aucune fonction n'a une signature conforme aux types des arguments avec laquelle elle est appelée, ou bien l'ensemble de ces fonctions n'a pas de minimum pour l'inclusion de profils.\n"
					| Some(f,i,tt, b) -> { c = TEfcall(te, tl, i, b) ; typ = tt } 
			  end
				
							   else erreur e.loc "Cette expression n'est pas une fonction.\n"
				| TEqident (TQmeth(_)) -> ratet "(méthode fonction).\n" 
				| TEattr(te, id) when id.typ = Fonc -> let nc = classe_de te.typ in
                                                            let lm = find_all_meth nc id.rep in begin
                                                                    match (scan_lf (List.map (fun (x:texpr) -> x.typ) tl) lm) with
                                                                    | None -> erreur expr.loc ("Aucune méthode de la classe" ^ nc  ^ " ne correspond à ce profil d'appel.\n")
                                                                    | Some(f, i, tt, b) ->  { c = TEfcall(te, tl, i, (fst b)) ; typ = tt }
                                                            end

               
                        
				| _ -> erreur e.loc "Cette expression n'est pas une fonction et donc ne peut être appliquée.\n"
		end

  | Enew (nc, l) -> if Hashtbl.mem table_c nc then
			let tl = List.map (fun x -> (typexpr x env lvl)) l in
				 let optcons = scan_lf (List.map (fun (x:texpr) -> x.typ) tl) (find_all_meth nc (chcons ^ nc)) in begin
                                                        match optcons with
                                                                | None -> erreur expr.loc "Aucun constructeur de la classe ne correspond au profil d'appel dans cette invocation de new.\n"
                                                                | Some(la, ni, ttt, _) -> { c = (TEnew(nc, tl, ni)) ; typ = Tpointeur (Tclass nc) }
		end
		    else erreur expr.loc "L'opérateur new invoque le constructeur d'une classe qui n'existe pas.\n"
  | Elincr e-> let te = typexpr e env lvl in 
		if is_left_tvalue te env then
			begin match te.typ with 
                                | Tint-> {c = TElincr te ; typ = Tint }
                                | _ -> erreur expr.loc "Incrémentation à gauche d'une expression non entière.\n"
                        end
	       else not_left expr.loc
  | Eldecr e -> let te = typexpr e env lvl in
		if is_left_tvalue te env then
			begin match te.typ with
                                | Tint-> {c = TEldecr te ; typ = Tint }
                                | _ -> erreur expr.loc "Décrémentation à gauche d'une expression non entière.\n"
                        end
                else not_left expr.loc 
  | Erincr e -> let te = typexpr e env lvl in
                if is_left_tvalue te env then
                        begin match te.typ with
                                | Tint-> {c = TErincr te ; typ = Tint }
                                | _ -> erreur expr.loc "Incrémentation à droite d'une expression non entière.\n"
                        end
                else not_left expr.loc
  | Erdecr e -> let te = typexpr e env lvl in
                if is_left_tvalue te env then
                        begin match te.typ with
                                | Tint-> {c = TErdecr te ; typ = Tint }
                                | _ -> erreur expr.loc "Décrémentation à droite d'une expression non entière.\n"
                        end
                else not_left expr.loc
  | Eaddr e -> let te = typexpr e env lvl in
			if is_left_tvalue te env then
				{ c = TEaddr te ; typ = Tpointeur(te.typ) }
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
							if te.typ = tf.typ || (te.typ = Tnull && (is_pointeur tf.typ) || (tf.typ = Tnull && (is_pointeur te.typ))) then
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


let rec typinst i env lvl off = match i.v with
	| Nothing -> TNothing, env, off
	| Iexpr e -> TIexpr (typexpr e env lvl), env, off
	| Idecl (tdef, v)-> let tt = typtypedef tdef in
				if is_bf tt then
			    	let tv, envir = (typvar v env lvl tt off) in (* ajouter application du constructeur s'il y a un constructeur vide et tv est de type Tclass s *)
					(TIdecl(tt, tv)), envir, (off + (size_tvar tv))
				else erreur i.loc "Déclaration d'une variable de type non bien formé.\n"
	| Ideclinit (tdef, v, e) -> (* vérifier si v est une référence, que e est une valeur gauche *) 				let tt = typtypedef tdef in
					if is_bf tt then
                            	    		let tv, envir = (typvar v env lvl tt off) in
						let te = typexpr e env lvl in
							if (is_sub_type te.typ tv.typ) then
								if not(tvar_by_ref tv) then
									(TIdeclinit(tt, tv,te)), envir, (off + (size_tvar tv))
 
								else
									if (is_left_tvalue te env) then
										(TIdeclinit(tt, tv,te)), envir, (off + (size_tvar tv))

									else erreur e.loc "Cette expression n'est pas une valeur gauche, mais est pourtant assignée à une référence.\n"
							else erreur e.loc "Cette expression n'est pas d'un type qui est sous-type du type de déclaration de la variable.\n"
					else erreur i.loc "Déclaration d'une variable de type non bien formé.\n"
	| Ideclobj (tdef, v, s, l)-> let tt = typtypedef tdef in
				     if tt <> (Tclass s) then
					erreur i.loc "Le type de cette déclaration ne correspond pas à la classe du constructeur appelé.\n"
				else
				     let (tv, envir) = typvar v env lvl tt off in
				     let tvid = extract_tvar tv in
				         if tvid.typ <> tt then
						erreur v.loc "Cette variable n'a pas le même type que celui de l'objet retourné par le constructeur.\n"
				else
				     let tl = List.map (fun x -> typexpr x env lvl) l in
						let optcons = scan_lf (List.map (fun (x:texpr) -> x.typ) tl) (find_all_meth s (chcons ^ s)) in begin
							match optcons with
								| None -> erreur i.loc "Aucun constructeur de la classe ne correspond au profil d'appel dans cette instruction.\n"
								| Some(la, ni, ttt, _) -> TIdeclobj(tt, tv, s, tl, ni), envir, (off +(size_tvar tv))  (*failwith "(assignation objet retour constructeur) non implé( )menté"*)
		
						end 
 	| If (e, ins)-> let te = typexpr e env lvl in
				if te.typ = Tint then
					let (tins,envir, offs) = typinst ins env lvl off in
						(TIfelse (te, tins, TNothing)), env, offs
				else erreur e.loc "L'expression à l'intérieur du if n'est pas entière.\n"
	| Ifelse (e, ins1, ins2) -> let te = typexpr e env lvl in
                               				if te.typ = Tint then
                                        			let (tins1,envir1, offs) = typinst ins1 env lvl off in let  (tins2, envir2, offse) = typinst ins2 env lvl offs in
                                                		(TIfelse (te, tins1, tins2)), env, offse
                                			else erreur e.loc "L'expression à l'intérieur du if n'est pas entière.\n"
	| While (e, ins) -> let te = typexpr e env lvl in
                                if te.typ = Tint then
                                        let (tins,envir, offs) = typinst ins env lvl off in
                                                (TWhile (te, tins)), env, offs
                                else erreur e.loc "L'expression à l'intérieur du while n'est pas entière.\n"
	| For (l1, e, l2, ins) -> let tl1 = List.map (fun x -> typexpr x env lvl) l1 in
					let te = typexpr e env lvl in
					if te.typ = Tint then
						let tl2 = List.map (fun x -> typexpr x env lvl) l2 in
							let tins, envir, offs = typinst ins env lvl off in
								(TFor (tl1, te, tl2, tins)), env, offs
					else erreur e.loc "L'expression de contrôle à l'intérieur du for n'est pas entière.\n"
	| Afor (l1, l2, ins) -> let tl1 = List.map (fun x -> typexpr x env lvl) l1 in
                    			let tl2 = List.map (fun x -> typexpr x env lvl) l2 in
                        			let tins, envir, offs = typinst ins env lvl off in
                                			(TFor (tl1, {c = TEint 1; typ = Tint } , tl2, tins)), env, offs
	| Ibloc b -> let tb, offs = (typbloc b env (lvl + 1) off) in (TIbloc tb), env, offs

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
		     in (TCout (auxcout le)), env, off
	| Return e -> begin try let tr = Smap.find chtypereturn env in
			let te = (typexpr e env lvl) in 
				if is_sub_type te.typ (tr.typ) then 
					if tr.byref then 
						if is_left_tvalue te env then
							(TReturn te), env, off
						else erreur e.loc "L'expression retournée n'est pas une valeur gauche, alors que le prototype de la fonction stipule qu'elle renvoie une référence.\n"
					else (TReturn te), env, off
				else erreur e.loc "Le type de l'expression retournée ne correspond pas à un sous-type de retour du prototype de la fonction. (1)\n"
			with Not_found -> erreur i.loc ("Return en dehors d'une fonction ?!! La fonction n'a pas ajouté " ^ chtypereturn ^" au contexte.\n")
		       end
	| Areturn -> begin try let tr = Smap.find chtypereturn env in 
			if tr.typ = Tvoid || is_classe tr.typ then TAreturn, env, off
                                else erreur i.loc "Le type de l'expression retournée ne correspond pas au type de retour du prototype de la fonction. (2)\n"
			with Not_found -> erreur i.loc ("Return en dehors d'une fonction ?!! La fonction n'a pas ajouté " ^ chtypereturn ^" au contexte.\n")
                 end

and typdbloc bl env lvl off = match bl with
	| Bloc [] -> (TBloc ([],0)), off
	| Bloc (i::l) -> let (ti, envir, offs) = typinst i env lvl off in
				let (TBloc (tl,offb)), offr = typdbloc (Bloc l) envir lvl offs in
					(TBloc ((ti::tl),offr)), (offs + offr)
and typbloc bl env lvl off = (typdbloc (bl.v) env lvl off)



let typdecl d env = match d.v with
    | Dv dv -> let (tdv, envir, off) = typdecl_v env 0 dv in ( TDv tdv), envir
    | Dc dc -> let (tdc, envir) = typdecl_c dc env 0 in (TDc tdc), envir    
    | Db (p, bl) -> let (tp, envir, env_hb) = typproto p env None in
				let tbl, off (* bug le rajouter *) = typbloc bl envir 1 0 in
					(TDb (tp, tbl, off)), env_hb 

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
				| [ (_,_,Tint, []) ]-> tf
				| [ (_,_, _, []) ]-> erreur f.loc "L'unique fonction main du fichier n'est pas de type int.\n"
				| [ (_,_ ,Tint, _) ] -> erreur f.loc "L'unique fonction main du fichier possède des arguments dans son prototype, ce qui n'est pas autorisé en Mini-C++.\n"

				| _ -> erreur f.loc "Il y a plusieurs fonctions main déclarées au sein du fichier.\n"


