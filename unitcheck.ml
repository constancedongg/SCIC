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
    | None ->  raise (Failure ("units " ^ u ^ " not found in table")) 
  in 
  
  let unit_check u set table = 
    match SS.find_opt u set with
      Some bu -> ()
    | None -> nonbase_unit_check u table
           (* raise (Failure ("units cannot found in set " ^ u))  *)
  in
  

  (* check global variable unit exists*)
  let unit_check_exists (ubinds : ubind list) =
    List.iter (function (_, u, _) -> ignore(unit_check u base_units units)
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

    (* Raise an exception if the given rvalue unit cannot be assigned to
    the given lvalue type *)
    (* let check_assign lvalueu rvalueu err = 
      (* lvalueu int m z *)
      let lvalueu' = convert lvalueu
      (* rvalueu: int m || int "1"*)
      and rvalueu' 
      if lvalueu = rvalueu || StringMap.find 
        then lvalueu else raise (Failure err)
    in *)

    (* let check_assign lunit runit err = 
      if lunit = runit then lunit 
      else if runit = "1" then "1"
      else 
        try let (u, _) = StringMap.find lunit units 
      in 
        with Not_found -> raise (Fialure ("left unit " ^ lunit ^ "not defined"))
    in  *)


    (* Build local symbol table of variables for this function*)
    let symbols = List.fold_left (fun m (_, unt, name) -> StringMap.add name unt m)
      StringMap.empty (globals @ func.func_formals )
    in

    (* find unit of identifier *)
    let unit_of_identifier s table =
      try StringMap.find s table
      with Not_found -> raise (Failure ("cannot find unit for identifier " ^ s))
    in
    
    (* check if data type with unit is float*)
    let type_of_identifier s table =
        try StringMap.find s table
        with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* get conversion rate between two untis *)
    let get_multipler lunit runit map =
      if lunit = runit then 1
      else if runit = "1" then 1
      else try let (u, r) = StringMap.find lunit map in
              if u = runit then r
              else raise (Failure (lunit ^ " and " ^ runit ^ " is not defined in the conversion rule"))
            with Not_found -> 
              try let (u, r) = StringMap.find runit map in
                if u = lunit then 1 / r
                else raise (Failure (lunit ^ " and " ^ runit ^ " is not defined in the conversion rule"))
            with Not_found -> raise (Failure ("unit not defined"))
    in 

    let rec expr table = function
      SIntLit  l   -> ("1", SIntLit l)
    | SFloatLit l  -> ("1", SFloatLit l)
    | SBoolLit l   -> ("1", SBoolLit l)
    | SStringLit l -> ("1", SStringLit l)
    | SNoexpr      -> ("1", SNoexpr)
    | SId s        -> (unit_of_identifier s table, SId s)
    | Binop(e1, op, e2) as e -> 
      let (t1, e1') = expr table e1 
      and (t2, e2') = expr table e2 in
      (* All binary operators require operands of the same type *)
      let same = t1 = t2 in
      (* Determine expression type based on operator and operand types *)
      let ty = match op with
                Add | Sub | Mult | Div  when same && t1 = Int   -> Int
              | Add | Sub | Mult | Div | Pow when same && t1 = Float -> Float
              | Pow when (t1 = Float && t2 = Int) -> Float
              | Equal | Neq            when same               -> Bool
              | Less | Leq | Greater | Geq
                        when same && (t1 = Int || t1 = Float) -> Bool
              | And | Or when same && t1 = Bool -> Bool
              | _ -> raise (Failure ("illegal binary operator " ^
                                    string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                                    string_of_typ t2 ^ " in " ^ string_of_expr e))
      in (ty, SBinop((t1, e1'), op, (t2, e2')))

    |  Assign(e1, e2) as ex -> 
      let lunit = unit_of_identifier e1 table
        and runit = unit_of_identifier e2 table
        and err = "illegal unit assignment " ^ lunit ^ "=" ^ runit
      in 
      (* ignore check_assign lunit runit err ;  *)
      Assign(e1, Binop(expr e2 table, Mult, get_multipler(lunit, runit, units)))

    (* 
      string/bool/other -> none
      float/int -> exist? -> convertable? -> convert 
    *)
    
    (* 
      bool m boo = true // -> skip
      int m x = 10 // 
      int cm y = x;
      print(y) = 1000
      int cm y = x + 2 * x ;   // 
    

      int mm z = y * 10
      (int, 10, int mm z = y)
      print(z) // 10 * 10
      int mm z = 100 * expr(y {m})
      int mm z = expr(y->mm)
                  y m -> mm * a's /b's

    *)


   
    


    
    
    (* check *)
  in




  (* let _ = unit_check_exists globals
  in
   *)

  (globals, list.map check_function functions)

  (* let check (globals, functions) =
    (globals, functions) *)
