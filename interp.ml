
(* interpréteur de mini-Pascal *)

open Ast

exception Error of string

let unbound_var s = raise (Error ("unbound variable " ^ s))
let unbound_procedure f = raise (Error ("unbound procedure " ^ f))
let bad_arity x = raise (Error ("bad arity for " ^ x))

(* primitives *)

(*open Graphics

let is_open = ref false

let drawline x1 y1 x2 y2 =
  if not !is_open then begin is_open := true; open_graph " 800x600" end;
  moveto x1 y1; lineto x2 y2

let writeln v =
  Format.printf "%d@." v

(* table des variables globales *)
let (globals : (string, int) Hashtbl.t) = Hashtbl.create 17

(* structure de données pour les variables locales *)
module Smap = Map.Make(String)
type env = int Smap.t

(* expressions arithmétiques *)

let rec expr env = function
  | Econst n -> n
  (* À COMPLÉTER *)

(* expressions booléennes (conditions) *)

let rec condition env = function
  | Bnot c -> not (condition env c)
  (* À COMPLÉTER *)

(* instructions *)

(* table des procédures *)
let (procs : (string, procedure) Hashtbl.t) = Hashtbl.create 17

let rec stmt env = function
  | Sblock sl ->
      List.fold_left stmt env sl
  | Scall ("writeln", [e]) ->
      writeln (expr env e); env
  | Scall ("writeln", _) ->
      bad_arity "writeln"
  | Scall ("drawline", [e1;e2;e3;e4]) ->
      drawline (expr env e1) (expr env e2) (expr env e3) (expr env e4); env
  | Scall ("drawline", _) ->
      bad_arity "drawline"
  (* À COMPLÉTER *)

(* interprétation du programme p *)

let prog p =
  (* À COMPLÉTER *)
  ignore (stmt Smap.empty p.main);
  if !is_open then begin ignore (read_key ()); close_graph () end*)




