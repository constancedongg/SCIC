(* Unit Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)
module SS = Set.Make(String);;



let check (udecls, globals, functions) =
  (* base unit set - static*)
  let base_units = 
    List.fold_right SS.add ["m"; "s"; "1"; "kg"] SS.empty
  in

  (* unit maping key: non-base unit, value: (base unit, scale)*)
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
    List.iter (function (u1, u2, _) -> ignore(unit_check u2 base_units units); 
                                        ignore(unit_reverse_check u1 base_units units);
    ) unit_decls;
    (* List.iter (function
    (Void, _, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b)) 
    | _ -> ()) ubinds; *)
    (* check duplication in new units *)
    let rec dups = function
        [] -> ()
      |	((u1, _, _) :: (u1',_, _) :: _) when u1 = u1' -> raise (Failure ("duplicate unit definition " ^ kind ^ " " ^ u1))
      | _ :: t -> dups t
    in dups (List.sort (fun (a,_,_) (b,_,_) -> compare a b) unit_decls)
  in

  let add_unit table (u1, u2, c)  = 
    match SS.find_opt u2 base_units with
      Some bu -> StringMap.add u1 (u2, float_of_string c) table
    | None -> match StringMap.find_opt u2 table with
               Some (bu, c2) -> StringMap.add u1 (bu, c2 *. (float_of_string c)) table
              | None -> raise (Failure ("The reference unit not existed " ^ u2)) 
  in

  let add_unit_decls (unit_decls: unit_decl list) table = 
    ignore(check_udecls "new unit" unit_decls);
    List.fold_left add_unit table unit_decls
  in

  let units = add_unit_decls udecls units in

  let decompose_unit u set table = 
    let l = Str.split (Str.regexp "/") u in
    let (ln, ld) = match l with
              [e1] ->  (Str.split (Str.regexp "*") e1, [])
            | [e1; e2] -> (Str.split (Str.regexp "*") e1, Str.split( Str.regexp "*") e2)
            | _ -> ([], [])
    in 
    List.iter (fun s -> unit_check s set table) ln;
    List.iter (fun s -> unit_check s set table) ld;
    let sn = List.fold_left (fun s u -> SS.add u s) SS.empty ln
      and sd = List.fold_left (fun s u -> SS.add u s) SS.empty ld
    in (sn, sd)
      (* Printf.eprintf  "Debugging: Numerator %s denominator %s" e1 e2; *)
  in

  let derived_unit_check u set table = 
    let l = Str.split (Str.regexp "/") u in
    let (ln, ld) = match l with
              [e1] ->  (Str.split (Str.regexp "*") e1, [])
            | [e1; e2] -> (Str.split (Str.regexp "*") e1, Str.split( Str.regexp "*") e2)
            | _ -> ([], [])
    in 
    List.iter (fun s -> unit_check s set table) ln;
    List.iter (fun s -> unit_check s set table) ld;
  in

  let all_unit_check u set table = 
    match SS.find_opt u set with
    Some bu -> ()
  | None -> match StringMap.find_opt u table with 
            Some (bu, c) -> ()
          | None -> derived_unit_check u set table
  in
         (* raise (Failure ("units cannot found in set " ^ u))  *)

  (* decompose_unit "kg*m/s" base_units units; *)

  let derived_unit_comp u1 u2 = 
    let (sn, sd) = decompose_unit u1 base_units units 
      and (sn', sd') = decompose_unit u2 base_units units
    in 
    SS.equal sn sn' && SS.equal sd sd'; 
  in





  (* check global variable unit exists*)
  let var_unit_check (ubinds : ubind list) =
    List.iter (function (_, u, _) ->ignore(all_unit_check u base_units units)
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

    (* if right unit is "1", does not multiply anything *)
    let check_right_unit runit =
      if runit = "1" then true
      else false
    in 

    let lookup_base u = 
      match SS.find_opt u base_units with
      Some bu -> bu
      | None -> match StringMap.find_opt u units with
               Some (bu, c) -> bu
              | None -> raise (Failure ("Cannot find conversion rule for unit " ^ u))
    in

    let lookup_base_scale u = 
      match SS.find_opt u base_units with
      Some bu -> 1.0
      | None -> match StringMap.find_opt u units with
               Some (bu, c) -> c
              | None -> raise (Failure ("Cannot find conversion rule for unit " ^ u))
    in
    (* let reduce_to_base uset =
      let l = [] in
       SS.fold (fun buset u -> let (bu, c) = lookup_base u in l @ [bu])
       l uset;
       (* (fun buset u -> SS.add "a" buset) SS.empty uset; let (bu, c) = StringMap.find u units in *)
                            
    in *)

    let reduce_to_base uset =
      let buset = SS.fold (fun u buset -> SS.add (lookup_base u) buset) uset SS.empty
      in
      let scale = SS.fold (fun u scale -> scale *. (lookup_base_scale u)) uset 1.0
      in
      (buset, scale)
    in

    let derived_get_scale lunit runit = 
      let (sn, sd) = decompose_unit lunit base_units units and (sn', sd') = decompose_unit runit base_units units
      in
      let (snb, cn) = reduce_to_base sn and (snb',cn') = reduce_to_base sn' and (sdb,cd) = reduce_to_base sd and (sdb',cd') = reduce_to_base sd' in
      if SS.equal snb snb' && SS.equal sdb sdb' then (cn /. cd )/. (cn' /. cd')   (* raise( Failure( Float.to_string ((cn /. cd ) /. (cn' /. cd')) ) )*)
      else raise( Failure("No conversion rules between unit " ^ lunit ^ " and " ^ runit))
    in

    (* get conversion rate between two units *)
    let get_scale lunit runit map =
        if lunit = runit then 1.0
        (* else if runit = "1" then 1.0 *)
        else try let (u, r) = StringMap.find lunit map in
                if u = runit then r
                else let (u', r') = StringMap.find runit map in
                      if u' = u then r /. r'
                      else  raise (Failure (lunit ^ " and " ^ runit ^ " is not defined in the conversion rule")) 
              with Not_found -> derived_get_scale lunit runit; (* raise (Failure ("unit not defined")) *)
                try let (u, r) = StringMap.find runit map in
                  if u = lunit then 1.0 /. r
                  else let (u', r') = StringMap.find lunit map in
                        if u' = u then r' /. r
                        else raise (Failure (lunit ^ " and " ^ runit ^ " is not defined in the conversion rule"))
              with Not_found -> derived_get_scale lunit runit; (* raise (Failure ("unit not defined")) *)
      in 

    let scale_expr scaler e t = 
      let e' = (t, SBinop((t, e), Mult, (Float, SFloatLit (Printf.sprintf "%.5f" scaler)))) in e'
    in
    
    (* Build local symbol table of variables for this function. key: variable name, value: unit.*)
    let symbols = List.fold_left (fun m (_, unt, name) -> StringMap.add name unt m)
      StringMap.empty (globals @ func.sfunc_formals )
    in

    (* Return a variable from our local symbol table *)
    let unit_of_identifier s table =
      try StringMap.find s table
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in
  

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
      else let scale = get_scale lu ru units in
      let e2_scale = scale_expr scale e2' t2 in
      (lu, (t, SAssign((t1, e1'), e2_scale)))

  | SFunctionCall(fname, args) as call -> 
    let fd = find_func fname in
    (* check each of the args, to see if it can scale*)
    let check_args_unit (_,fu,_) e =
      let (eu, (t, e')) = expr table e in
        if StringMap.mem fname built_in_decls  || check_right_unit eu 
          then (t, e')
        else let scale = get_scale fu eu units in
        let e_scale = scale_expr scale e' t in
            e_scale
      in
    let args' = List.map2 check_args_unit fd.sfunc_formals args
    in (fd.sreturn_unit, (fd.sreturn_type, SFunctionCall(fname, args')))
  | SUnop(op, e) as ex -> 
      let (eu, e') = expr table e in
     (eu, (t, SUnop(op, e')))
  | SBinop(e1, op, e2) as e -> 
    let (eu1, (t1, e1')) = expr table e1 in
    let (eu2, (t2, e2')) = expr table e2 in
    let same = t1 = t2 in
    let check_binop_unit op e1 e2 = match op with
      | Add | Sub | Equal | Neq | Less | Leq | Greater | Geq | And | Or ->
        let (eu1, (t1, e1')) = expr table e1 in
        let (eu2, (t2, e2')) = expr table e2 in 
        if check_right_unit eu1 || check_right_unit eu2 then 
          e2
        else
          let scale = get_scale eu1 eu2 units in
          let e2_scale = scale_expr scale e2' t2 in
          e2_scale
      | _ -> e2 in
    let e2_scale = check_binop_unit op e1 e2 
    in (eu1, (t1, SBinop(e1, op, e2_scale)))
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
      let new_table = StringMap.add var unt table in
      (* here all the expr must be (unit, expr) according to usat *)
      if check_right_unit ru then 
        (new_table, SDAssign(lt, unt, var, e))
      else 
        let scale = get_scale unt ru units in 
        let e_scale = scale_expr scale e' t in
          (new_table, SDAssign(lt, unt, var, e_scale))
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
      let e_scale = scale_expr scale e' t in 
          (table, SReturn (e_scale))
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
