(* Unit Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)
module SS = Set.Make(String);;



let check (globals, functions) =
  (* some base unit *)
  let base_units = 
    List.fold_right SS.add ["m"; "s"; "1"] SS.empty
  in

  let units = 
    StringMap.add "cm" ("m", 100) StringMap.empty
  in 


  let nonbase_unit_check u table = 
    match StringMap.find_opt u table with
      Some (bu, l) -> ()
    | None ->  raise (Failure ("units cannot found in table " ^ u)) 
  in 
  
  let unit_check u set table = 
    match SS.find_opt u set with
      Some bu -> ()  
    | None -> nonbase_unit_check u table
           (* raise (Failure ("units cannot found in set " ^ u))  *)
  in
  

  (* unit lookup and check together*)
  let unit_check_exists (ubinds : ubind list) =
    List.iter (function (_, u, _) ->ignore(unit_check u base_units units)
                                           (* ignore(let _ = SS.find u base_units          *)
                                              (* in 
                                              nonbase_unit_check u units) *)
                            (* with Not_found -> () *)
                              (* nonbase_unit_check u units *)
                        (* |_ -> () *)
                ) ubinds
  in
  
 
  let _ = unit_check_exists globals
  in
  
  let rec resemble lst = 
    match lst with
  |  [] -> []
  | (t, u, n)::tl -> (t, n)::(resemble tl)
  in
  (resemble globals, functions)

  (* let check (globals, functions) =
    (globals, functions) *)
