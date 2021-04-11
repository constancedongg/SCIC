(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)
module C = Char
module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "scic" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context 
  (* and void_ptr_t = L.pointer_type (L.i8_type context) *)
  and string_t   = L.pointer_type (L.i8_type context)
  in


  (* Return the LLVM type for a SCIC type *)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.String -> string_t
    | A.IntArr -> L.pointer_type (ltype_of_typ A.Int) 
    | A.FloatArr -> L.pointer_type (ltype_of_typ A.Float) 
    | _ -> raise (Failure "type not defined")
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype = 
      L.var_arg_function_type string_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

  let printbig_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let printbig_func : L.llvalue =
      L.declare_function "printbig" printbig_t the_module in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfunc_identifier
      and formal_types = 
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sfunc_formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.sreturn_type) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfunc_identifier function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder
    and str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
        let local_vars =
          let add_formal m (t, n) p = 
            L.set_value_name n p;
          let local = L.build_alloca (ltype_of_typ t) n builder in
                ignore (L.build_store p local builder);
          StringMap.add n local m 
    
          (* Allocate space for any locally declared variables and add the
           * resulting registers to our map *)
          (* and add_local m (t, n) =
      let local_var = L.build_alloca (ltype_of_typ t) n builder
      in StringMap.add n local_var m  *)
          in
    
          List.fold_left2 add_formal StringMap.empty fdecl.sfunc_formals
              (Array.to_list (L.params the_function)) 
          (* in
          List.fold_left add_local formals fdecl.slocals  *)
        in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n table = try StringMap.find n table
                   with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder table ((_, e) : sexpr) = match e with
	      SIntLit i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFloatLit l -> L.const_float_of_string float_t l
      | SStringLit s -> L.build_global_stringptr s "str" builder
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s table) s builder
      | SAssign((_, SArrayAccess(e1, i)), e2) ->
              let e2' = expr builder table e2 in
              let id = expr builder table e1 in
              let idx = expr builder table i in 
              let ptr = L.build_gep id [| idx |] "" builder in 
              ignore(L.build_store e2' ptr builder); e2'
      | SAssign (e1, e2) -> let e' = expr builder table e2 in
                          let s = match (let (_, e) = e1  in e) with 
                                            SId(s)  -> s
                                          |  _ -> raise (Failure "invalid assignment id")
                             in
                         ignore(L.build_store e' (lookup s table) builder); e'
      | SBinop (e1, op, ((A.Float, _) as e2)) ->
        let e1' = expr builder table e1
        and e2' = expr builder table e2 in
          (match op with 
            A.Add     -> L.build_fadd e1' e2' "tmp" builder
          | A.Sub     -> L.build_fsub e1' e2' "tmp" builder
          | A.Mult    -> L.build_fmul e1' e2' "tmp" builder
          | A.Div     -> L.build_fdiv e1' e2' "tmp" builder
          | A.Pow     -> 
            let pow32 = L.declare_function "llvm.pow.f64" (L.function_type float_t [|float_t;float_t;|]) the_module in
            L.build_call pow32 [| e1'; e2' |] "pow32" builder
          | A.Equal   -> L.build_fcmp L.Fcmp.Oeq e1' e2' "tmp" builder
          | A.Neq     -> L.build_fcmp L.Fcmp.One e1' e2' "tmp" builder
          | A.Less    -> L.build_fcmp L.Fcmp.Olt e1' e2' "tmp" builder
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole e1' e2' "tmp" builder
          | A.Greater -> L.build_fcmp L.Fcmp.Ogt e1' e2' "tmp" builder
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge e1' e2' "tmp" builder
          | A.And | A.Or ->
              raise (Failure "internal error: semant should have rejected and/or on float")
          ) 
      | SBinop (e1, op, e2) ->
        let e1' = expr builder table e1
        and e2' = expr builder table e2 in
        (match op with
          A.Add     -> L.build_add e1' e2' "tmp" builder
        | A.Sub     -> L.build_sub e1' e2' "tmp" builder
        | A.Mult    -> L.build_mul e1' e2' "tmp" builder
        | A.Div     -> L.build_sdiv e1' e2' "tmp" builder
        | A.Pow     -> 
          let powi32 = L.declare_function "llvm.powi.f64" (L.function_type float_t [|float_t;i32_t;|]) the_module in
        L.build_call powi32 [| e1'; e2' |] "powi32" builder
        | A.And     -> L.build_and e1' e2' "tmp" builder
        | A.Or      -> L.build_or e1' e2' "tmp" builder
        | A.Equal   -> L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
        | A.Neq     -> L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
        | A.Less    -> L.build_icmp L.Icmp.Slt e1' e2' "tmp" builder
        | A.Leq     -> L.build_icmp L.Icmp.Sle e1' e2' "tmp" builder
        | A.Greater -> L.build_icmp L.Icmp.Sgt e1' e2' "tmp" builder
        | A.Geq     -> L.build_icmp L.Icmp.Sge e1' e2' "tmp" builder
        ) 
      | SUnop(op, ((t, _) as e)) ->
        let e' = expr builder table e in
	        (match op with
	        A.Neg when t = A.Float -> L.build_fneg 
	        | A.Neg                  -> L.build_neg
          | A.Not                  -> L.build_not) e' "tmp" builder
      | SFunctionCall ("print", [e])  |SFunctionCall ("printb", [e]) ->
          L.build_call printf_func [| int_format_str ; (expr builder table e) |]
            "printf" builder
      | SFunctionCall("printl", [e]) -> L.build_call printf_func [| str_format_str ; (expr builder table e) |] "printf" builder
      | SFunctionCall ("printbig", [e]) ->
	        L.build_call printbig_func [| (expr builder table e) |] "printbig" builder
      (* | SFunctionCall("printc", [e]) -> 
        L.build_call printc_func [| (expr builder e) |] "printc" builder  *)
      | SFunctionCall ("printf", [e]) -> 
	        L.build_call printf_func [| float_format_str ; (expr builder table e) |]
	        "printf" builder
      | SFunctionCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (expr builder table) (List.rev args)) in
        let result = (match fdecl.sreturn_type with 
                              A.Void -> ""
                            | _ -> f ^ "_result") in
              L.build_call fdef (Array.of_list llargs) result builder
        
      | SArray (el) -> 
          let ls = List.map (fun e -> expr builder table e) el in
          let typ = L.type_of (List.hd ls) in 
          let n = List.length ls in 
          let ptr = L.build_array_malloc typ (L.const_int i32_t n) "" builder  in 
          ignore (List.fold_left (fun i elem ->
                                    let idx = L.const_int i32_t i in
                                    let eptr = L.build_gep ptr [|idx|] "" builder in
                                    let cptr = L.build_pointercast eptr 
                                        (L.pointer_type (L.type_of elem)) "" builder in
                                    let _ = (L.build_store elem cptr builder) in i + 1) 0 ls); ptr
        
      | SArrayAccess(e1, e2) ->
          let id = expr builder table e1 in
          let i = expr builder table e2 in 
          let ptr =  L.build_load (L.build_gep id [| i |] "" builder) "" builder 
          in ptr

      in
    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	      Some _ -> ()
      | None -> ignore (instr builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

    let rec stmt builder table = function
      	SBlock sl -> List.fold_left (fun (builder, table) s -> stmt builder table s) (builder, table) sl
      | SExpr e -> ignore(expr builder table e); (builder, table)
      | SDAssign (t, s, e) -> let add_local m (t, n) =
                                let local_var = L.build_alloca (ltype_of_typ t) n builder
                                  in StringMap.add n local_var m in
                              let new_table = add_local table (t, s) in
                              let e' = expr builder new_table e in
                              let _ = L.build_store e' (lookup s new_table) builder in
                              ignore(expr builder table e); 
                              (builder, new_table)
      | SReturn e -> ignore(match fdecl.sreturn_type with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder table e) builder );
                     (builder, table)
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = expr builder table predicate in
        let merge_bb = L.append_block context "merge" the_function in
        let build_br_merge = L.build_br merge_bb in (* partial function *)

	      let then_bb = L.append_block context "then" the_function in
        let (builder, table) = stmt (L.builder_at_end context then_bb) table then_stmt in
	        add_terminal builder build_br_merge;

	      let else_bb = L.append_block context "else" the_function in
        let (builder, table) = stmt (L.builder_at_end context else_bb) table else_stmt in
	        add_terminal builder build_br_merge;

	 ignore(L.build_cond_br bool_val then_bb else_bb builder);
	 (L.builder_at_end context merge_bb, table)
      in
      (* | SWHILE (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore(L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt builder
	    ( SBlock [SExpr e1 ; while_func (e2, SBlock [body ; SExpr e3]) ] )
    in *)

    (* Build the code for each statement in the function *)
    let (builder, _) = stmt builder local_vars (SBlock fdecl.sfunc_stmts) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.sreturn_type with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
