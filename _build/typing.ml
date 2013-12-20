open Ast

exception Error of loc * string

type typ =
    | Tnull
    | Tint
    | Tvoid
    | Tpointeur of typ
    | Tclass of string


module Smap = Map.Make(String)

type env = typ Smap.t

let table_f = Hashtbl.create 17 ;; (* on enregistre les fonctions en clé et les listes des arguments possibles pour prendre en compte la surcharge *)

Hashtbl.add table_f "" [""] ;;

let table_c = Hashtbl.create 17 ;; (* on enregistre ici les classes en clé, leurs super classes en champ, toujours avec le chamo "" pour pouvoir enregistrer les classes sans super classes *)

Hashtbl.add table_c "" "";;

type tfichier =
    | Tfichier


let tinst p =
    ()

let tfichier p =
    let rec auxf p envi =
	Tfichier
    in auxf p Smap.empty
