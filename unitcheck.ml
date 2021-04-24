(* Unit Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)
module SS = Set.Make(String);;



let check (udecls, globals, functions) =
  (* some base unit *)
  let base_units = 
    List.fold_right SS.add ["m"; "s"; "1"] SS.empty
  in

  let units = 
    StringMap.add "cm" ("m", 100.0) StringMap.empty
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

  let base_unit_check u set = 
    match SS.find_opt u set with
    Some bu -> ()
  | None -> raise (Failure ("units cannot found in set " ^ u)) 
  in

  let nonbase_unit_reverse_check u table = 
    match StringMap.find_opt u table with
      None -> ()
    | Some (bu, l) ->  raise (Failure ("units already in table " ^ u)) 
  in 

  (* raise failure if the unit already exists in table/set*)
  let unit_reverse_check u set table = 
    match SS.find_opt u set with
     Some bu -> raise(Failure ("unit already exists in set " ^ u))
    | None -> nonbase_unit_reverse_check u table 
  in

  let check_udecls (kind : string) (unit_decls : unit_decl list) =
    List.iter (function (u1, u2, _) -> ignore(base_unit_check u2 base_units); 
                                        ignore(unit_reverse_check u1 base_units units);
    ) unit_decls;
    (* List.iter (function
    (Void, _, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b)) 
    | _ -> ()) ubinds; *)
    let rec dups = function
        [] -> ()
      |	((u1, _, _) :: (u1',_, _) :: _) when u1 = u1' -> raise (Failure ("duplicate unit definition " ^ kind ^ " " ^ u1))
      | _ :: t -> dups t
    in dups (List.sort (fun (a,_,_) (b,_,_) -> compare a b) unit_decls)
  in

  let add_unit (u1, u2, c) table = 
    StringMap.add u1 (u2, c) table
  in

  let add_unit_decls (unit_decls: unit_decl list) table = 
    check_udecls "new unit" unit_decls;
    List.fold_left (fun m (k,u,v) -> StringMap.add k (u, float_of_string v) m) table unit_decls
  in

  let units = add_unit_decls udecls units

in



  (* check global variable unit exists*)
  let var_unit_check (ubinds : ubind list) =
    List.iter (function (_, u, _) ->ignore(unit_check u base_units units)
                ) ubinds
  in
  var_unit_check globals;



  let rec resemble lst = 
    match lst with
  |  [] -> []
  | (t, u, n)::tl -> (t, n)::(resemble tl)
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
      let n = fd.sfunc_identifier (* Name of the function *)
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

    var_unit_check func.sfunc_formals;

    (* 
      string/bool/other -> none
      float/int -> exist? -> convertable? -> convert 
    *)

    (* Raise an exception if the given rvalue unit cannot be assigned to
    the given lvalue type *)
    (*  *)
    let check_assign lvalueu rvalueu err = 
      (* lvalueu int m z *)
      if lvalueu = rvalueu then lvalueu else raise (Failure err)
    in
   
    
    (* Build local symbol table of variables for this function*)
    let symbols = List.fold_left (fun m (_, unt, name) -> StringMap.add name unt m)
      StringMap.empty (globals @ func.sfunc_formals )
    in

    (* Return a variable from our local symbol table *)
    let unit_of_identifier s table =
      try StringMap.find s table
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* let rec check_stmt table = function
      
  in *)
  (* body of check_function *)
  { sreturn_type = func.sreturn_type;
  sfunc_identifier = func.sfunc_identifier;
  sfunc_formals = func.sfunc_formals;
  sfunc_stmts = func.sfunc_stmts;
  (* sfunc_stmts = match check_stmt symbols (Block func.func_stmts) with
    (_, SBlock(sl)) -> sl
    | _ -> raise (Failure ("internal error: block didn't become a block?")) *)
  }
in (resemble globals, functions)
