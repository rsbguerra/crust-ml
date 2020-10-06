open Ast
open Format

let rec string_of_expr_list = function
  | [hd] -> string_of_expr hd
  | hd::tl -> (string_of_expr hd)^", "^(string_of_expr_list tl)
  | _ -> ""

and string_of_expr = function
  | Ecst (n, _)              -> "Ecst("^(string_of_crust_consts n)^")"
  | Ebinop (Band, e1, e2, _) -> "Ebinop(Band, "^(string_of_expr e1)^", "^(string_of_expr e2)^")"
  | Ebinop (Bor, e1, e2, _)  -> "Ebinop(Bor, "^(string_of_expr e1)^", "^(string_of_expr e2)^")"
  | Ebinop (_, e1, e2, _)    -> "Ebinop(Op, "^(string_of_expr e1)^", "^(string_of_expr e2)^")"
  | Eunop (_ , e1, _)        -> "Eunop(Unot, "^(string_of_expr e1)^")"
  | Ecall ("size", [e1], _)  -> "Ecall(size, "^(string_of_expr e1)^")"
  | Ecall (f, el, _)         -> "Ecall("^f^", "^(string_of_expr_list el)^")"  
  | Eident (id, _)           -> "Eident("^id^")"

and print_stmt = function
  | Sif (e, s1, _, _)    -> printf "Sif(%s" (string_of_expr e); printf ", "; print_stmt s1; printf ", )"
  | Sreturn (e, _)       -> printf "Sreturn(%s)" (string_of_expr e)
  | Sassign (id, e1, _)  -> printf "Sassign(%s, %s)" id (string_of_expr e1)
  | Sdeclare (id, t ,e1, _) -> printf "Sdeclare(%s, %s, %s)" id (string_of_crust_types t) (string_of_expr e1)
  | Sprint (e, _)        -> printf "Sprint(%s)" (string_of_expr e)
  | Sprintn (e, _)       -> printf "Sprintn(%s)" (string_of_expr e)
  | Sscanf (id, _)       -> printf "Sscanf(%s)" id
  | Sblock (bl, _)       -> interpret_block_stmt  bl
  | Swhile(e, bl, _)     -> printf "Swhile(%s, \n"(string_of_expr e); print_stmt bl; printf ")"
  | Sloop(bl, _)         -> printf "Sloop( "; print_stmt bl; printf ")"
  | _ -> assert false

and print_argument_list = function
  | [arg1] -> let id, t = arg1 in printf " %s : %s" id (string_of_crust_types t)
  | arg1 :: tl -> let id, t = arg1 in printf " %s : %s" id (string_of_crust_types t); printf ", "; print_argument_list tl
  | [] -> ()

and string_of_crust_consts = function 
  | Cu8   c -> "Cu8( "^(Stdint.Uint8.to_string c)^" )"
  | Cu16  c -> "Cu16( "^(Stdint.Uint16.to_string c)^" )"
  | Cu32  c -> "Cu32( "^(Stdint.Uint32.to_string c)^" )"
  | Cu64  c -> "Cu64( "^(Stdint.Uint64.to_string c)^" )"
  | Cu128 c -> "Cu128( "^(Stdint.Uint128.to_string c)^" )"
  | Ci8   c -> "Ci8( "^(Stdint.Int8.to_string c)^" )"
  | Ci16  c -> "Ci16( "^(Stdint.Int16.to_string c)^" )"
  | Ci32  c -> "Ci32( "^(Stdint.Int32.to_string c)^" )"
  | Ci64  c -> "Ci64( "^(Stdint.Int64.to_string c)^" )"  
  | Ci128 c -> "Ci128( "^(Stdint.Int128.to_string c)^" )"
  | Cbool c -> "Cbool( "^(string_of_bool c)^" )"

and string_of_crust_types = function 
  | Tu8  -> "Tu8"
  | Tu16 -> "Tu16"
  | Tu32 -> "Tu32"
  | Tu64 -> "Tu64"
  | Tu128-> "Tu128"
  | Ti8  -> "Ti8"
  | Ti16 -> "Ti16"
  | Ti32 -> "Ti32"
  | Ti64 -> "Ti64"
  | Ti128-> "Ti128"
  | Tbool-> "Tbool"

and print_stmts = function  
  | Stfunction (f, args, return, body, _) -> printf "Stfunction( %s, (" f ; print_argument_list args; printf "), %s" (string_of_crust_types return); printf ", "; print_stmt body
  | Stblock (bl, _) -> interpret_block_stmts bl
  | Stmt (s, _) -> print_stmt s

and interpret_block_stmt  = function
  | [] -> printf "\n"
  | s :: sl -> print_stmt s; printf "\n";interpret_block_stmt sl

and interpret_block_stmts = function
  | [] -> printf "\n"
  | s :: sl -> print_stmts s; printf "\n"; interpret_block_stmts sl

let print_file s = print_stmts s
