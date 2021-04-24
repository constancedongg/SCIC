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
    let check_unit_assign lvalueu rvalueu err = 
      (* lvalueu int m z *)

      (* rvalueu: int m || int "1"*)
      if lvalueu = rvalueu 
        then (10, lvalueu) else raise (Failure err)
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
    
    (* check *)
    let _ = unit_check_exists func.sfunc_formals
  in


  let _ = unit_check_exists globals
  in
  
  (* what is blow ?? *)
  

  let convert e2' scale = e2' in
  (* in this layer, we still use SAST, BUT ALL TYPE IS UNIT TYPE*)
  let rec expr table (t, e) = match e with
    SIntLit  l   -> ("1", (t, SIntLit l))
  | SFloatLit l  -> ("1", (t, SFloatLit l))
  | SBoolLit l   -> ("1", (t, SBoolLit l))
  | SStringLit l -> ("1", (t, SStringLit l))
  | SNoexpr      -> ("1", (t, SNoexpr))
  | SId s        -> (unit_of_identifier s table, (t, SId s))
  | SAssign(e1, e2) as ex -> 
      let (lu, (t1, e1')) = expr table e1
    (* find unit of e1*)
    and (ru, (t2, e2')) = expr table e2 in
      let err = "illegal assignment found in unit check " in
    let (scale, lu) = check_unit_assign lu ru err in
    let scale_e2' = convert e2' scale
    in (lu, (t, SAssign((t1, e1'), (t2, scale_e2'))))
  | SFunctionCall(fname, args) as call -> 
    let fd = find_func fname in
    (* check each of the args, to see if it can scale*)
    let check_args_unit (_,fu,_) e =
      let (eu, (t, e')) = expr table e in
      let err = "illegal argument found in unit check" in
      let (scale, fu) = check_unit_assign fu eu err in
      let scale_e' = convert e' scale in
      (t, scale_e')
    in
    let args' = List.map2 check_args_unit fd.func_formals args
    in (fd.sreturn_unit, (fd.sreturn_type, SFunctionCall(fname, args')))
  | SUnop(op, e) as ex -> 
      let (eu, e') = expr table e in
     (eu, (t, SUnop(op, e')))
  | SBinop(e1, op, e2) as e -> 
    let (eu, e1') = expr table e1 in
    (eu, (t, SBinop(e1',op,e2)))
  | SArray(el) -> 
    ("1", (t, SArray(el)))
  | SArrayAccess(e1, e2) -> 
    let (u, e1') = expr table e1 in
    (u, (t, SArrayAccess(e1', e2)))

in

  (* in this layer, we still use SAST, BUT ALL TYPE IS UNIT TYPE*)
  (* I guess -> so no need to write USAT *)
  let rec check_stmt table = function 
   SExpr e -> let (u, e') = expr table e in (table, SExpr(e'))
  | SDAssign (lt, unt, var, e) ->
      (* get e's unit in recursion*)
      let (ru, (t, e')) = expr table e in
      let err = "illegal assignment" in
      (* check if e's unit can assign to variable unit *)
      let (scale, lu) = check_unit_assign unt ru err in
      (* convert e expression with scaler 
         such as
         m y = 5
         cm x = 100 * y
         after convert
         x = 10 * y
         then we can have no unit along with
      *)
      let scale_e' = convert e' scale in
      let new_table = StringMap.add var lu table in
      (* here all the expr must be (unit, expr) according to usat *)
      (new_table, SDAssign(lt, unt, var, (t, scale_e')))
  | SIf(p, b1, b2) ->
    let (table_b1, st_b1) = check_stmt table b1 in
    let (table_b2, st_b2) = check_stmt table_b1 b2 in 
    (table_b2, SIf(p, st_b1, st_b2))
  | SFor(e1, e2, e3, st) -> 
    let (new_table, new_st) = check_stmt table st in
    let (eu1, e1') = expr new_table e1 in
    let (eu3, e3') = expr new_table e3 in
    (new_table, SFor(e1', e2, e3', new_st)) 
  | SWhile(p, st) -> 
    let (new_table, new_st) = check_stmt table st in 
    (new_table, SWhile(p, new_st))
  | SReturn e -> let (eu, e') = expr table e in
      (* check if return e's unit can be convert to function return unit*)
        (* question, where is func.return unit??? *)
      let err = "illegal return check" in 
      let (scale, ru) = check_unit_assign eu func.sreturn_unit err in
      let scale_e' = convert e' scale in 
      (table, SReturn (scale_e'))
  | SBlock sl -> 
    let rec check_stmt_list table = function
        [SReturn _ as s] -> let (new_table, st) = check_stmt table s in (new_table, [st])
      | SReturn _ :: _   -> raise (Failure "nothing may follow a return")
      | SBlock sl :: ss  -> check_stmt_list table (sl @ ss) (* Flatten blocks *)
      | s :: ss         -> let (one_table, one_s) = check_stmt table s in 
                            let (list_table, list_s) = check_stmt_list one_table ss in 
                              (list_table, one_s :: list_s)
      | []              -> (table, [])
    in let (new_table, listS) = check_stmt_list table sl in (new_table, SBlock(listS))


   in
    { sreturn_type = func.sreturn_type;
    sreturn_unit = func.sreturn_unit;
    sfunc_identifier = func.sfunc_identifier;
    sfunc_formals = func.sfunc_formals;
    sfunc_stmts = match check_stmt symbols (SBlock func.sfunc_stmts) with
	    (_, SBlock(sl)) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
in
let rec resemble lst = 
  match lst with
|  [] -> []
| (t, u, n)::tl -> (t, n)::(resemble tl)
in
  (resemble globals, List.map check_function functions)

  (* let check (globals, functions) =
    (globals, functions) *)
