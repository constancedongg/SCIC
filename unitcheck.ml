(* Unit Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)
module SS = Set.Make(String);;
test 
let check (globals, functions) =
  let rec resemble lst = 
    match lst with
  |  [] -> []
  | (t, u, n)::tl -> (t, n)::(resemble tl)
  in
  (resemble globals, functions)

  (* let check (globals, functions) =
    (globals, functions) *)
