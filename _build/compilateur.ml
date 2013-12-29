open Mips
open Typing
open Ast
 (* Ici commence l'assemblage \Production de code\*) 
 (*Une expression entière*)



(* numéro des label *)

let ntest = ref 0
let nstring = ref 0

(* ***************** *)

let rec concatene = function
  | [] -> nop 
  | x::y::[] -> x ++ y
  | x::l -> x ++ (concatene l)

let associe_opar op = match op with
	| Add -> add
	| Sub -> sub
	| Mul -> mul
	| Div -> div
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

let rec code_expr lvl const = match const.c with
        | TEint i -> li a0  i
	| TEop (Mod, te, tf) -> 
		(code_expr lvl te) ++ (push a0) ++
		(code_expr lvl tf) ++ (pop t1)++
		(div t2 t1 oreg a0)++(mul t2 t2 oreg a0) ++ (sub a0 t1 oreg t2) 
	| TEop(op, te, tf) when List.mem op [Add ; Sub ; Mul ; Div] -> 
		(code_expr lvl te) ++ (push a0) ++
		(code_expr lvl tf) ++  (pop t1) ++
		((associe_opar op) a0 t1 oreg a0)
	| TEop(op, te, tf) when List.mem op [Or ; And ] -> let lab = "sortietest" ^ (string_of_int !ntest) in let () = incr ntest in
		(code_expr lvl te) ++ ((if op = Or then beqz else bnez) a0 lab) ++
		(code_expr lvl tf) ++ (label lab)
        | TEop (op, te, tf) -> 
		(code_expr lvl te) ++ (push a0) ++
		(code_expr lvl tf) ++ (pop t1) ++ 
		((associe_oplog op) a0 t1 a0)
	| _ -> failwith "Compilation de cette partie non encore implémentée.\n"



let code_expr_str lvl e = match e with
	| TEsexpr te -> { text = (code_expr lvl te) ++ (li v0 1) ; data = nop }
	| TEstring s -> let lab = "chaine" ^ (string_of_int !nstring) in let () = incr nstring in
			{ text = (la a0 alab lab) ++ (li v0 4) ++ (syscall) ; data = (label lab) ++ (asciiz s) }

(*	| (TIdent { rep = s; typ = t ; lvl = l ; offset = ofs }) ->   begin 
		assert (l <= lvl);
   		 move t1, fp; 
   		iter (lvl - l) lw t1  8(t1); 
   		(*lw a0 {ofs}($t1)*) end 
*)
let rec iter n code = if n=0 then nop 
		      else 
			code ++
			(iter (n - 1) code)

