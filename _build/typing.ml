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

type tfichier =
    | Tfichier

let tfichier p =
    let rec auxf p envi =
	Tfichier
    in auxf p Smap.empty
