open Ast
open Sast

let string_of_decl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_usprogram (vars, funcs) =
  String.concat "" (List.map string_of_decl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)