open Mips
open Typing
open Ast
 (* Ici commence l'assemblage \Production de code\*) 
 (*Une expression entière*)


let rec concatene = fun
  | [] -> Nop
  | x::y::[] -> Cat(x,y)
  | x::l -> Cat(x, (concatene l))

let associe_op op = match op with
	| Add -> add
	| Sub -> sub
	| Mul -> mul
	| Div -> div
	| Mul -> mul
	| And -> and_
	| Or -> or_
	| Le -> sle
	| Ge -> sge
	| Lt -> slt
	| Gt -> sgt

let rec int_expr lvl const = match const.c with
        | TEint i -> li a0  i
        | TEop (op, te, tf) -> concatene [(int_expr lvl te) ; (push a0) ; (int_expr lvl tf) ; (pop t1) ; ((associe_op op) a0 t1 a0) ]


(*
let rec int_expr lvl const = match const.c with 
 	| TEint i -> li a0  i
	| TEop (Add, te, tf) -> begin 
		int_expr lvl te; push a0; 
		int_expr lvl tf; pop t1; 
		add a0 t1 a0 end 
	| 	TEop (Sub, te, tf) -> begin 
		int expr lvl te; push a0; 
		int_expr lvl tf; pop t1; 
		sub a0 t1 a0 end 
	| TEop (Mul, te, tf) -> begin 
		int expr lvl te; push a0; 
		int_expr lvl tf; pop t1; 
		mul a0 t1 a0 end 
	| TEop (Div, te, tf) -> begin 
		int expr lvl te; push a0; 
		int_expr lvl tf; pop t1; 
		div a0 t1 a0 end 
	| TEop (Mod, te, tf) -> begin 
		int expr lvl te; push a0; 
		int_expr lvl tf; pop t1; 
		div t2 t1 a0; mul t2 t2 a0 ; sub a0 t1 t2
	 (* pas sûr que ça marche pour les négatif en gros je dis que a mod b c'est a- (b/a)*a*)end 
	| TEop (And, te, tf) -> begin 
		int expr lvl te; push a0; 
		int_expr lvl tf; pop t1; 
		and_ t2 t1 a0 end
	| TEop (Or, te, tf) -> begin 
		int expr lvl te; push a0; 
		int_expr lvl tf; pop t1; 
		or_ t2 t1 a0 end
	| TEop (Le, te, tf) -> begin 
		int expr lvl te; push a0; 
		int_expr lvl tf; pop t1; 
		sle a0 t1 a0 end (* on met un 1 ou un 0 dans $a0 selon que c'est plus petit ou égal ou pas, 
	petit problème l'expression (a=<b)+c est correctement traitée dans ce cas :x*) 
	| TEop (Ge, te, tf) -> begin 
		int expr lvl te; push a0; 
		int_expr lvl tf; pop t1; 
		sge a0 t1 a0 end
	| TEop (Lt, te, tf) -> begin 
		int expr lvl te; push a0; 
		int_expr lvl tf; pop t1; 
		slt a0 t1 a0 end
	| TEop (Gt, te, tf) -> begin 
		int expr lvl te; push a0; 
		int_expr lvl tf; pop t1; 
		sgt a0 t1 a0 end
*)
(*	| (TIdent { rep = s; typ = t ; lvl = l ; offset = ofs }) ->   begin 
		assert (l <= lvl);
   		 move t1, fp; 
   		iter (lvl - l) lw t1  8(t1); 
   		(*lw a0 {ofs}($t1)*) end 
*)
let rec iter n code = if n=0 then () 
		      else begin 
			code; 
			iter (n - 1) code 
		      end

