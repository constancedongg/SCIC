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

  let units = add_unit_decls udecls units in



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

  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      sreturn_type = Void;
      sreturn_unit = "1";
      sfunc_identifier = name; 
      sfunc_formals = [(ty,"1","x")];
      sfunc_stmts = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", Int); ("printl", String);
                              ("printb", Bool);
                              ("printf", Float);
                              ("printbig", Int) ]
  in
  

  (***** build function id lookup table *****)
  (* Add function name to symbol table *)
   let add_func map fd = 
      let n = fd.sfunc_identifier (* Name of the function *)
      in StringMap.add n fd map
    in
    (* Collect all function names into one symbol table *)
    let function_decls = List.fold_left add_func built_in_decls functions
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

    (* if right unit is "1", does not multiply anything *)
    let check_right_unit runit =
      if runit = "1" then true
      else false
    in 

    (* get conversion rate between two untis *)
    let get_scale lunit runit map =
        if lunit = runit then 1.0
        (* else if runit = "1" then 1.0 *)
        else try let (u, r) = StringMap.find lunit map in
                if u = runit then r
                else raise (Failure (lunit ^ " and " ^ runit ^ " is not defined in the conversion rule"))
              with Not_found -> 
                try let (u, r) = StringMap.find runit map in
                  if u = lunit then 1.0 /. r
                  else raise (Failure (lunit ^ " and " ^ runit ^ " is not defined in the conversion rule"))
              with Not_found -> raise (Failure ("unit not defined"))
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
    (* let _ = unit_check_exists func.sfunc_formals
  in


  let _ = unit_check_exists globals
  in *)
  
  (* what is blow ?? *)
  

  (* let convert e2' scale = e2' in *)
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
        and (ru, (t2, e2')) = expr table e2 in
      if check_right_unit ru then (lu, (t, SAssign((t1, e1'), (t2, e2'))))
      else let scale = get_scale lu ru units
            in (lu, (t, SAssign((t1, e1'), (t2, SBinop((t2, e2'), Mult, (Float, SFloatLit (Float.to_string scale)))))))

  | SFunctionCall(fname, args) as call -> 
    let fd = find_func fname in
    (* check each of the args, to see if it can scale*)
    let check_args_unit (_,fu,_) e =
      let (eu, (t, e')) = expr table e in
        if StringMap.mem fname built_in_decls  || check_right_unit eu 
          then (t, e')
        else let scale = get_scale fu eu units in
            (t, SBinop((t, e'), Mult, (Float, SFloatLit (Float.to_string scale))))
      in
    let args' = List.map2 check_args_unit fd.sfunc_formals args
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

   (* | SAssign(e1, e2) as ex -> 
    let (lu, (t1, e1')) = expr table e1
      and (ru, (t2, e2')) = expr table e2 in
    (* let err = "illegal assignment found in unit check " in *)
    let scale = get_scale lu ru units
  in (lu, (t, SAssign((t1, e1'), (t2, SBinop((t2, e2'), Mult, (Float, SFloatLit (Float.to_string scale))))))) *)

  | SDAssign (lt, unt, var, e) ->
      (* get e's unit in recursion*)
      let (ru, (t, e')) = expr table e in
      let res = check_right_unit ru  in 
      let new_table = StringMap.add var unt table in
      (* here all the expr must be (unit, expr) according to usat *)
      if res then  (new_table, SDAssign(lt, unt, var, (t, e')))
      else let scale = get_scale unt ru units in 
        (new_table, SDAssign(lt, unt, var, (t, SBinop((t, e'), Mult, (Float, SFloatLit (Float.to_string scale))))))
  | SIf(p, b1, b2) ->
    let (table_b1, st_b1) = check_stmt table b1 in
    let (table_b2, st_b2) = check_stmt table_b1 b2 in 
    (table_b2, SIf(p, st_b1, st_b2))
  | SFor(e1, e2, e3, st) -> 
    let (new_table, new_st) = check_stmt table st in
    let (_, e1') = expr new_table e1 in
    let (_, e3') = expr new_table e3 in
    (new_table, SFor(e1', e2, e3', new_st)) 
  | SWhile(p, st) -> 
    let (new_table, new_st) = check_stmt table st in 
    (new_table, SWhile(p, new_st))
  | SReturn e -> let (eu, (t, e')) = expr table e in
      if check_right_unit eu then  (table, SReturn(t, e'))
      else let scale = get_scale func.sreturn_unit eu units in
          (table, SReturn ((t, SBinop((t, e'), Mult, (Float, SFloatLit (Float.to_string scale))))))
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



    (* let rec check_stmt table = function
      
  in *)
  (* body of check_function *)
  (* { sreturn_type = func.sreturn_type;
  sfunc_identifier = func.sfunc_identifier;
  sfunc_formals = func.sfunc_formals;
  sfunc_stmts = func.sfunc_stmts;
  (* sfunc_stmts = match check_stmt symbols (Block func.func_stmts) with
    (_, SBlock(sl)) -> sl
    | _ -> raise (Failure ("internal error: block didn't become a block?")) *)
  } *)
in (resemble globals, List.map check_function functions)
