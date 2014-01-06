let ( table : (string, unit) Hashtbl.t ) = Hashtbl.create 17  (* la table du lexer hack *)

let () = Hashtbl.add table "void" ()

let () = Hashtbl.add table "int" () 
