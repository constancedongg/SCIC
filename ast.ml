(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Pow

type uop = Neg | Not

type typ = Int | Bool | Float | String | Void | IntArr | FloatArr 

(* type unt = Meter | Centimeter | Second | Nounit *)

(* type uexpr = 
  Unit of string
  | Nounit *)
(* type unt = Nounit of string *)

type bind = typ * string

type ubind = typ * string * string 

type expr =
  IntLit of int
  | FloatLit of string
  | StringLit of string
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | FunctionCall of string * expr list
  | Array of expr list 
  | ArrayAccess of expr * expr 
  | Noexpr 


type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt  
  | DAssign of typ * string * string * expr

type func_decl = {
    return_type : typ;
    func_identifier : string;
    func_formals : ubind list;
    func_stmts : stmt list;
  }
  (* retun_unit: unt; *)



type program = ubind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Pow -> "^"
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

  let string_of_typ = function
  Int -> "int"
| Bool -> "bool"
| Float -> "float"
| String -> "string"
| Void -> "void"
| IntArr -> "int[]"
| FloatArr -> "float[]"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

(* let string_of_unit = function
  Unit(s) -> s *)


let rec string_of_expr = function
  IntLit(l) -> string_of_int l
  | FloatLit(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StringLit(l) -> l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | FunctionCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Array(el) -> String.concat ", " (List.map string_of_expr el) 
  | ArrayAccess(s, e) -> string_of_expr s ^ "[" ^ string_of_expr e ^ "]"
      
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | DAssign(t, u, v, e) -> string_of_typ t ^ " " ^ u ^ " " ^ v ^ " = " ^ string_of_expr e ^ ";\n"

let string_of_vdecl (t, u, id) = string_of_typ t ^ " " ^ u ^ " " ^ id ^ ";\n"

let get_3_3 t = match t with 
    (_, _, e) -> e;;

let string_of_fdecl fdecl =
  string_of_typ fdecl.return_type ^ " " ^ "func " ^ 
  fdecl.func_identifier ^ "(" ^ String.concat ", " (List.map get_3_3 fdecl.func_formals) ^
  ")\n{\n" ^
  (* String.concat "" (List.map string_of_vdecl fdecl.locals) ^ *)
  String.concat "" (List.map string_of_stmt fdecl.func_stmts) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
  