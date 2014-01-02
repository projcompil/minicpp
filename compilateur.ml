open Mips
open Typing
open Ast
 (* Ici commence l'assemblage \Production de code\*) 
 (*Une expression entière*)



(* numéro des label *)

let ntest = (ref 0), "_sortielazy"
let nstring = (ref 0), "_chaine"
let nif = (ref 0), "_else", "_sortiecond"
let nloop = (ref 0), "_entreeloop", "_testloop"

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

let rate s =
	failwith ("Compilation non impléméntée : " ^ s)

let addp a b =
	{ text = a.text ++ b.text ; data = a.data ++ b.data }

(* ***************** *)

let rec concatene l = List.fold_left (++) nop l (*function
  | [] -> nop 
  | x::y::[] -> x ++ y
  | x::l -> x ++ (concatene l)*)

let rec conca = function
  | [] -> nopp 
  | x::l -> let reste = conca l in
		{ text = x.text ++ reste.text ; data = x.data ++ reste.data }

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
	(*| TEop (Mod, te, tf) -> 
		(code_expr lvl te) ++ (push a0) ++
		(code_expr lvl tf) ++ (pop t1)++
		(div t2 t1 oreg a0)++(mul t2 t2 oreg a0) ++ (sub a0 t1 oreg t2) *)
	| TEop(op, te, tf) when List.mem op [Add ; Sub ; Mul ; Div] -> 
		(code_expr lvl te) ++ (push a0) ++
		(code_expr lvl tf) ++  (pop t1) ++
		((associe_opar op) a0 t1 oreg a0)
	| TEop(op, te, tf) when List.mem op [Or ; And ] -> let lab = next_lab ntest in
		(code_expr lvl te) ++ ((if op = Or then beqz else bnez) a0 lab) ++
		(code_expr lvl tf) ++ (label lab)
        | TEop (op, te, tf) -> 
		(code_expr lvl te) ++ (push a0) ++
		(code_expr lvl tf) ++ (pop t1) ++ 
		((associe_oplog op) a0 t1 a0)
	| TEpar te -> code_expr lvl te
	| TEnot te -> (code_expr lvl te) ++ (li t1 0) ++ (or_ a0 a0 t1) ++ (not_ a0 a0) 
	| TEnull -> li a0 0
	| TEuminus te -> (code_expr lvl te) ++ (neg a0 a0)
	| TEuplus te -> (code_expr lvl te)
	| _ ->  rate "Compilation de cette partie non encore implémentée.\n"



let code_cout_expr_str lvl (*env*) e = match e with
	| TEsexpr te -> { text = (code_expr lvl te) ++ (li v0 1) ++ (syscall) ; data = nop }
	| TEstring s -> let lab = next_lab nstring in
				{ text = (la a0 alab lab) ++ (li v0 4) ++ (syscall) ; 
				data = (label lab) ++ (asciiz s) }

(*	| (TIdent { rep = s; typ = t ; lvl = l ; offset = ofs }) -> 
		assert (l <= lvl);
   		(move t1 fp) ++ 
   		iter (lvl - l) ++ (lw t1  (8,t1)); 
*)

let rec iter n code = if n=0 then nop 
		      else 
			code ++
			(iter (n - 1) code)


let rec code_inst lvl ti = match ti with
	| TNothing -> nopp
  	| TIexpr te -> { text = (code_expr lvl te) ; data = nop }
  	| TIdecl (tt, tv) -> rate "" 
  	| TIdeclinit (tt, tv, te) -> rate ""
  	| TIdeclobj (tt, tv, s, tl) -> rate "" 
  	(*| TIf (te, ti) -> rate ""*)
  	| TIfelse (te, ti, tj) -> let (lab1, lab2) = next_labd nif in
					let ci = code_inst lvl ti in
					let cj = code_inst lvl tj in
						{ text = (code_expr lvl te) ++ (bnez a0 lab1) ++ ci.text ++ (b lab2) ++ (label lab1)  ++ cj.text ++ (label lab2) ; data = ci.data ++ cj.data }
  	| TWhile (te, ti) -> let (lab1, lab2) = next_labd nloop in
				let ci = code_inst lvl ti in
				{ text = (b lab2) ++ (label lab1) ++ ci.text ++  (label lab2) ++ (code_expr lvl te) ++ (bnez a0 lab1) ; data = ci.data }
  	| TFor (tl1, te, tl2, ti) -> let (lab1, lab2) = next_labd nloop in
                                	let ci = code_inst lvl ti in
						{ text = (concatene (List.map (code_expr lvl) tl1)) ++ (b lab2) ++ (label lab1) ++ ci.text ++ (concatene (List.map (code_expr lvl) tl2)) ++  (label lab2) ++ (code_expr lvl te) ++ (bnez a0 lab1) ; data = ci.data }
  	| TIbloc (TBloc tl) -> conca (List.map (code_inst (lvl+1)) tl)
  	| TCout tls -> conca (List.map (code_cout_expr_str lvl) tls)
  	| TReturn te -> rate ""
  	| TAreturn -> rate ""





let code_fichier tf =
	failwith "Compilation non implémentée.\n"

let compile_fichier tf f =
	let cf = code_fichier tf in
		print_in_file f cf
