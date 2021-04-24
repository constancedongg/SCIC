(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)
module SS = Set.Make(String)
(* module Mut = Set.Make(struct type t = unt;; let compare = compare end);; *)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.
 
   Check each global variable, then check each function *)
 
let check (globals, functions) =
  
  (* SCIC does not have void type, which here only check for duplicate names *)
  (* Verify a list of bindings has no void types or duplicate names *)
  let check_ubinds (kind : string) (ubinds : ubind list) =
    List.iter (function
    (Void, _, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b)) 
    | _ -> ()) ubinds;
    let rec dups = function
        [] -> ()
      |	((_, _, n1) :: (_,_, n2) :: _) when n1 = n2 -> raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,_,a) (_,_,b) -> compare a b) ubinds)
  in

  (**** Check global variables ****)

  check_ubinds "global" globals;

  (**** Check functions ****)

  (* Here is what for hello world : build-in function*)
  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      return_type = Void;
      return_unit = "1";
      func_identifier = name; 
      func_formals = [(ty,"1","x")];
      func_stmts = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", Int); ("printl", String);
			                         ("printb", Bool);
                               ("printf", Float);
			                         ("printbig", Int) ]
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.func_identifier ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.func_identifier
    and make_err er = raise (Failure er)
    and n = fd.func_identifier (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  (* Verify a list of bindings has no void types or duplicate names *)
  (* let check_binds (kind : string) (binds : bind list) =
    List.iter (function
  (Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
    raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in *)

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    check_ubinds "formal" func.func_formals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    
    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, _, name) -> StringMap.add name ty m)
	                StringMap.empty (globals @ func.func_formals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s table =
      try StringMap.find s table
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* type_to_array converts type to corresponding array type for array handling *)
    let type_to_array = function
      | Int -> IntArr
      (* | Bool -> BoolArr *)
      (* | String -> StringArr *)
      | Float -> FloatArr
      | _ as x -> x
    in 
    (* array_to_type converts array types to their corresponding non-array types *)
    let array_to_type = function
      | IntArr -> Int
      (* | BoolArr -> Bool *)
      | FloatArr -> Float
      (* | StringArr -> String *)
      | _ as x -> x
    in 
    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr table = function
        IntLit  l -> (Int, SIntLit l)
      | FloatLit l -> (Float, SFloatLit l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | StringLit l -> (String, SStringLit l)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s table, SId s)
      (* | DAssign(lt, var, e) as ex -> (* y == int x = 3*)
          (* must put variable name into symbols?*)
          let symbols = StringMap.add var lt symbols in
          let (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ "=" ^ string_of_typ rt ^ " in " ^ string_of_expr ex in
          let lt2 = check_assign lt rt err in
          (check_assign lt2 rt err, SDAssign(lt2, var, (rt, e'))) *)
      | Assign(e1, e2) as ex -> 
          let (lt, e1') = match e1 with 
                Id(s) -> (type_of_identifier s table, SId s)
              | _ -> expr table e1 
          and (rt, e2') = expr table e2 in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign((lt, e1'), (rt, e2')))
      | Unop(op, e) as ex -> 
          let (t, e') = expr table e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
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
      | FunctionCall(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.func_formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _,_) e = 
            let (et, e') = expr table e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.func_formals args
          in (fd.return_type, SFunctionCall(fname, args'))

      | Array(el) -> 
        let rec helper typ out  = function 
          | [] -> (type_to_array typ, SArray(List.rev out))
          | a :: tl -> 
            let (t, e) = expr table a in
            if t = typ then helper t ((t, e) :: out) tl 
            else raise (Failure ("multiple types in array not allowed"))
        in (match el with 
            | a :: tl ->  let (t,e) = expr table a in helper t [(t, e)] tl 
            | [] -> raise (Failure ("empty array init not allowed")))

      | ArrayAccess(e1, e2) ->
        let (t1, e1') = expr table e1 in 
        let (t2, e2') = expr table e2 in 
        if (t1 = IntArr || t1 = FloatArr) && t2 = Int 
          then (array_to_type t1, SArrayAccess((t1, e1'), (t2, e2')))
        else raise (Failure ("invalid type for array access")) 

    in

    let check_bool_expr table e = 
      let (t', e') = expr table e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* let print_map key value =
      let new_value = string_of_typ value in
      print_string(key ^ " " ^ new_value ^ "\n") in  *)

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt table = function
        Expr e -> (table, SExpr (expr table e))
      | DAssign(lt, unt, var, e) -> 
        let (rt, e') = expr table e in
        let err = "illegal assignment" in
        let lt2 = check_assign lt rt err in
        let new_table = StringMap.add var lt2 table in
        (new_table, SDAssign(lt2, unt, var, (rt, e')))
      | If(p, b1, b2) -> let (table_b1, st_b1) = check_stmt table b1 in
                          let (table_b2, st_b2) = check_stmt table_b1 b2 in 
                          (table_b2, SIf(check_bool_expr table p, st_b1, st_b2))
      | For(e1, e2, e3, st) -> let (new_table, new_st) = check_stmt table st in 
                                (new_table, SFor(expr new_table e1, check_bool_expr new_table e2, expr new_table e3, new_st)) 
      | While(p, st) -> let (new_table, new_st) = check_stmt table st in 
                (new_table, SWhile(check_bool_expr new_table p, new_st))
      | Return e -> let (t, e') = expr table e in
        if t = func.return_type then (table, SReturn (t, e'))
        else raise (
	        Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		      string_of_typ func.return_type ^ " in " ^ string_of_expr e))
	    
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list table = function
              [Return _ as s] -> let (new_table, st) = check_stmt table s in (new_table, [st])
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list table (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> let (one_table, one_s) = check_stmt table s in 
                                  let (list_table, list_s) = check_stmt_list one_table ss in 
                                    (list_table, one_s :: list_s)
            | []              -> (table, [])
          in let (new_table, listS) = check_stmt_list table sl in (new_table, SBlock(listS))

    in (* body of check_function *)
    { sreturn_type = func.return_type;
    sreturn_unit = func.return_unit;
    sfunc_identifier = func.func_identifier;
    sfunc_formals = func.func_formals;
    sfunc_stmts = match check_stmt symbols (Block func.func_stmts) with
	    (_, SBlock(sl)) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
     in (globals, List.map check_function functions)
