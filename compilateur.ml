open Mips
open Typing
open Ast

exception Not_implementedc of string
let ratec s =
	raise (Not_implementedc("Compilation non impléméntée : " ^ s))

(* numéro des label *)

let ntest = (ref 0), "_sortielazy"
let nstring = (ref 0), "_chaine"
let nif = (ref 0), "_else", "_sortiecond"
let nloop = (ref 0), "_entreeloop", "_testloop"

let (table_chaine : hashstring) = Hashtbl.create 50

type envchaine = int Smap.t (* ou une table de Hash, cela éviterait de prendre en compte cela partout *)


let next_lab (r, s) =
	incr r;
	s ^ (string_of_int !r)

let next_labd (r, s1, s2) =
	incr r;
	let a = string_of_int !r in
		(s1 ^ a), (s2 ^ a)

let get_lab (r,s) =
	s ^ (string_of_int !r)

let nopp = { text = nop ; data = nop } (* petit raccourci *)

let convp e = { text = e ; data = nop }


let addp a b =
	{ text = a.text ++ b.text ; data = a.data ++ b.data }

(* ***************** *)

let rec concatene l = List.fold_left (++) nop l

let rec conca l = List.fold_left addp nopp l

let rec iter n code = if n=0 then nop 
		      else 
			code ++
			(iter (n - 1) code)



let associe_opar op = match op with
	| Add -> add
	| Sub -> sub
	| Mul -> mul
	| Div -> div
	| Mod -> rem
	| _ -> failwith "Opération arithmétique attendue par associe_opar\n"
let associe_oplog op = match op with
	| And -> and_
	| Or -> or_
	| Le -> sle
	| Ge -> sge
	| Lt -> slt
	| Gt -> sgt
	| Eq -> seq
	| Neq -> sne
	| _ -> failwith "Opération logique attendue par associe_oplog\n"



let rec code_expr lvl texpr = match texpr.c with
    | TEint i -> li a0  i
	| TEop(op, te, tf) when List.mem op [Add ; Sub ; Mul ; Div ; Mod ] -> 
		(code_expr lvl te) ++ (push a0) ++
		(code_expr lvl tf) ++  (pop t1) ++
		((associe_opar op) a0 t1 oreg a0)
	| TEop(op, te, tf) when List.mem op [Or ; And ] -> let lab = next_lab ntest in
		(code_expr lvl te) ++ ((if op = Or then bnez else beqz) a0 lab) ++
		(code_expr lvl tf) ++ (label lab)
        | TEop (op, te, tf) -> 
		(code_expr lvl te) ++ (push a0) ++
		(code_expr lvl tf) ++ (pop t1) ++ 
		((associe_oplog op) a0 t1 a0)
	| TEpar te -> code_expr lvl te
	| TEnot te -> (code_expr lvl te) ++ (or_ a0 a0 zero) ++ (not_ a0 a0) 
	| TEnull -> li a0 0
	| TEuminus te -> (code_expr lvl te) ++ (neg a0 a0)
	| TEuplus te -> (code_expr lvl te)
	| TEfcall(te, tl, i, b) -> begin match te.c with
					| TEqident(TQident id) -> (List.fold_left (fun code e -> code ++ (code_expr lvl e) ++ (push a0)) nop tl) ++ (move t1 fp) ++ (iter (lvl-id.lvl) (lw t1 areg (8, t1))) ++ (push t1) ++ (jal (id.rep ^ (string_of_int i))) ++ (popn (1+ List.length tl))
					| TEqident(_) -> ratec ""
					| _ -> failwith "Heisenbug 2.\n"
				   end
	| TEqident (TQident tid) -> (* il faut corriger les offset dans typage.ml *)
			assert (tid.lvl <= lvl) ;
			(move t1 fp) ++ (iter (lvl-tid.lvl) (lw t1 areg (8, t1))) ++ (if tid.byref then (*add a0 t0 oi tid.offset*)(lw a0 areg (tid.offset, t1)) else lw a0 areg (tid.offset, t1)) ++ (if tid.byref then lw a0 areg (0, a0) else nop)
	| TEassign({ c = TEqident (TQident tid) ; typ = _  }, tf) -> 
			(code_expr lvl tf) ++
			(move t1 fp) ++ (iter (lvl-tid.lvl) (lw t1 areg (8, t1))) ++
			(if tid.byref then lw t1 areg (tid.offset, t1)
			 else add t1 t1 oi tid.offset) ++
			(sw a0 areg (0, t1))				
(* pour l'instant uniquement les variables et non les membres d'objets *)

    | TElincr ({ c = TEqident (TQident tid) ; typ = typ }) -> (code_expr lvl { c = TEqident (TQident tid) ; typ = typ }) ++ add a0 a0 oi 1 ++ (move t1 fp) ++
             (iter (lvl-tid.lvl) (lw t1 areg (8, t1))) ++ add t1 t1 oi tid.offset ++ (sw a0 areg (0, t1))

    | TErincr ({ c = TEqident (TQident tid) ; typ = typ }) -> (code_expr lvl { c = TEqident (TQident tid) ; typ = typ }) ++ (move a1 a0) ++ (add a1 a1 oi 1) ++ (move t1 fp) ++
             (iter (lvl-tid.lvl) (lw t1 areg (8, t1))) ++ add t1 t1 oi tid.offset ++ (sw a1 areg (0, t1))
    | TEldecr ({ c = TEqident (TQident tid) ; typ = typ  }) -> (code_expr lvl { c = TEqident (TQident tid) ; typ = typ }) ++ sub a0 a0 oi 1 ++ (move t1 fp) ++
                              (iter (lvl-tid.lvl) (lw t1 areg (8, t1))) ++ add t1 t1 oi tid.offset ++ (sw a0 areg (0, t1))

    | TErdecr ({ c = TEqident (TQident tid) ; typ = typ }) -> (code_expr lvl { c = TEqident (TQident tid) ; typ = typ }) ++ (move a1 a0) ++ (sub a1 a1 oi 1) ++ (move t1 fp) ++
                              (iter (lvl-tid.lvl) (lw t1 areg (8, t1))) ++ add t1 t1 oi tid.offset ++ (sw a1 areg (0, t1))

	| _ ->  ratec "Compilation de cette partie non encore implémentée.\n"



let code_cout_expr_str lvl (*env*) e = match e with
	| TEsexpr te -> { text = (code_expr lvl te) ++ (li v0 1) ++ (syscall) ; data = nop }
	| TEstring s -> if Hashtbl.mem table_chaine s then 
				let lab = Hashtbl.find table_chaine s in
					{ text = (la a0 alab lab) ++ (li v0 4) ++ (syscall) ; data = nop }
			else let lab = next_lab nstring in 
				Hashtbl.add table_chaine s lab ;
				{ text = (la a0 alab lab) ++ (li v0 4) ++ (syscall) ; 
				data = (label lab) ++ (asciiz s) }

(*
	| TEqident (TQident tid) -> 
			assert (tid.lvl <= lvl) ;
			(move t1 fp) ++ (iter (lvl-tid.lvl) (lw t1 areg (8, t1))) ++ (if tid.byref then (*add a0 t0 oi tid.offset*)(lw a0 areg (tid.offset, t1)) else lw a0 areg (tid.offset, t1)) ++ (if tid.byref then lw a0 areg (0, a0) else nop)
*)
(*
let calc_adresse q lvl = match q with
    | TQident tid -> (move t1 fp) ++ (iter (lvl-tid.lvl) (lw t1 areg (8, t1))) ++ (if tid.byref then add a0 t0 oi tid.offset(*(lw a0 areg (tid.offset, t1)) *) else lw a0 areg (tid.offset, t1)) ++ (if tid.byref then lw a0 areg (0, a0) else nop)

    | _ -> failwith "Erreur 0000\n"
    *)
let rec code_inst lvl ti = match ti with
	| TNothing -> nopp
  	| TIexpr te -> { text = (code_expr lvl te) ; data = nop }
    | TIdecl (tt, tv) -> { text = sub sp sp oi (size_type ((extract_tvar tv).typ )); data = nop} (*Othmane.*)
    | TIdeclinit (tt, tv, te) -> let tid = extract_tvar tv in { text = sub sp sp oi (size_type (tid.typ )) ++ (code_expr lvl te) ++ (move t1 fp) ++ (add t1 t1 oi tid.offset) ++
            (sw a0 areg (0, t1)) ; data = nop}
            (* problème ici pour les références *) 
            (*{text = (code_expr lvl te) ++ (push a0)  ; data = nop}*)
  	| TIdeclobj (tt, tv, s, tl, ni) -> ratec "" 
  	(*| TIf (te, ti) -> ratec ""*)
  	| TIfelse (te, ti, tj) -> let (lab1, lab2) = next_labd nif in
					let ci = code_inst lvl ti in
					let cj = code_inst lvl tj in
						{ text = (code_expr lvl te) ++ (beqz a0 lab1) ++ ci.text ++ (b lab2) ++ (label lab1)  ++ cj.text ++ (label lab2) ; data = ci.data ++ cj.data }
  	| TWhile (te, ti) -> let (lab1, lab2) = next_labd nloop in
				let ci = code_inst lvl ti in
				{ text = (b lab2) ++ (label lab1) ++ ci.text ++  (label lab2) ++ (code_expr lvl te) ++ (bnez a0 lab1) ; data = ci.data }
  	| TFor (tl1, te, tl2, ti) -> let (lab1, lab2) = next_labd nloop in
                                	let ci = code_inst lvl ti in
						{ text = (concatene (List.map (code_expr lvl) tl1)) ++ (b lab2) ++ (label lab1) ++ ci.text ++ (concatene (List.map (code_expr lvl) tl2)) ++  (label lab2) ++ (code_expr lvl te) ++ (bnez a0 lab1) ; data = ci.data }
  	| TIbloc tb -> code_bloc lvl tb
  	| TCout tls -> conca (List.map (code_cout_expr_str lvl) tls)
  	| TReturn te -> ratec ""
  	| TAreturn -> ratec ""

and code_bloc lvl (TBloc (tl, off)) = conca (List.map (code_inst (lvl+1)) tl)
(* rajouter offset ici ? *)

let code_proto tp = match tp with
	| TProto(tt, tqv, tla) -> ratec ""
	| _ -> ratec "Pas d'autres fonctions que main.\n"

(* ne pas gérer le main dans code_proto *)

let code_decl td = match td with
	| TDv tdv -> nopp (* peut-êttre à changer pour la PµOO *)
	| TDc tdc -> ratec ""
    | TDb ( (TProto(Tint, (**){ c =(* *)(TQvar(TQident(ti)))(**); typ = _ }(**), [])), tb, off) when ti.rep = "main" -> conca [ { text = (label "main") ; data = nop } ; {text = (sub sp sp oi (8+off)) ++ (add fp sp oi off) ;  data = nop} ; (code_bloc 0 tb) ; { text = (add sp sp oi off) ++ (li v0 10) ++ (syscall); data = nop } ]
	| TDb (tp, tb, off) -> addp (code_proto tp) (code_bloc 0 tb)

let code_fichier tf =
	List.fold_left (fun x td -> addp x (code_decl td)) nopp tf.tdecls
	(*let rec auxc_fichier l = match l with
		| [] -> nopp
		| td::l -> addp (code_decl td) (auxc_fichier l)
	in auxc_fichier tf.tdecls*)


let compile_fichier tf f =
	let cf = code_fichier tf in
		print_in_file f cf
