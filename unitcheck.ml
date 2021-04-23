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
  

  (* check global variable unit exists*)
  let unit_check_exists (ubinds : ubind list) =
    List.iter (function (_, u, _) ->ignore(unit_check u base_units units)
                ) ubinds
  in
  
 (* check other variable unit exists, base_units set and units table*)

  (* expressions: unit lookup - left == right?
    lookup table similar to id->type, id->unit
  *)

 (* function formals, return unit *)

 (* convertion: scaling, assign*)

 (* unit declaration *)

  (***** build function id lookup table *****)
  (* Add function name to symbol table *)
   let add_func map fd = 
      let n = fd.func_identifier (* Name of the function *)
      in StringMap.add n fd map
    in
    (* Collect all function names into one symbol table *)
    let function_decls = List.fold_left add_func StringMap.empty functions
    in
    
    (* Return a function from our symbol table *)
    let find_func s = 
      try StringMap.find s function_decls
      with Not_found -> raise (Failure ("unrecognized function " ^ s))
    in

  (* func is current function scope *)
  let check_function func = 

    (*  *)
    let convert u = 
    in

    (* 
      string/bool/other -> none
      float/int -> exist? -> convertable? -> convert 
    *)
    
    (* 
      bool m boo = true // -> skip
      int m x = 10 // 
      int cm y = 10
      int mm z = y
      (int, 10, int mm z = y)
      print(z) // 10 * 10
      int mm z = 100 * expr(y {m})
      int mm z = expr(y->mm)
                  y m -> mm * a's /b's

    *)

    (* Raise an exception if the given rvalue unit cannot be assigned to
    the given lvalue type *)
    (*  *)
    let check_assign lvalueu rvalueu err = 
      (* lvalueu int m z *)
      let lvalueu' = convert lvalueu
      (* rvalueu: int m || int "1"*)
      and rvalueu' 
      if lvalueu = rvalueu || StringMap.find 
        then lvalueu else raise (Failure err)
    in
   
    
    (* Build local symbol table of variables for this function*)
    let symbols = List.fold_left (fun m (_, unt, name) -> StringMap.add name unt m)
      StringMap.empty (globals @ func.func_formals )
    in

    (* Return a variable from our local symbol table *)
    let unit_of_identifier s table =
      try StringMap.find s table
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in
    
    (* check *)
  in




  let _ = unit_check_exists globals
  in
  
  let rec resemble lst = 
    match lst with
  |  [] -> []
  | (t, u, n)::tl -> (t, n)::(resemble tl)
  in


  (resemble globals, list.map check_function functions)

  (* let check (globals, functions) =
    (globals, functions) *)
