let ( table : (string, unit) Hashtbl.t ) = Hashtbl.create 17 ;; (* la table du lexer hack *)

Hashtbl.add table "void" () ; 

Hashtbl.add table "int" () ;
