open Ast
open Format

let rec print_expr_list = function
  | [hd] -> print_expr hd
  | hd::tl -> print_expr hd; printf ", " ; print_expr_list tl
  | _ -> ()

and print_expr = function
  | Ecst (n, _)              -> printf " Ecst( %s )"(string_of_crust_consts n)
  | Eset (e1, e2, _)         -> printf " Eset( "; print_expr e1; print_expr e2; printf ") "
  | Ebinop (Band, e1, e2, _) -> printf " Ebinop( Band, "; print_expr e1; printf ", "; print_expr e2; printf ") "
  | Ebinop (Bor, e1, e2, _)  -> printf " Ebinop( Bor, "; print_expr e1; printf ", "; print_expr e2; printf ") "
  | Ebinop (_, e1, e2, _)    -> printf " Ebinop( Op, "; print_expr e1; printf ", "; print_expr e2; printf ") "
  | Eunop (_ , e1, _)        -> printf " Eunop( Unot, "; print_expr e1; printf ") "
  | Ecall ("size", [e1], _)  -> printf " Ecall( size, "; print_expr e1; printf ") "
  | Ecall (f, el, _)         -> printf " Ecall( %s, " f; print_expr_list el; printf " ) "  
  | Eident (id, line)        -> printf " Eident(%s, %d) " id line
  | _ -> assert false 

and print_stmt = function
  | Sif (e, s1, _, _)    -> printf "Sif("; print_expr e; printf ", "; print_stmt s1; printf ","; printf ")"
  | Sreturn (e, _)       -> printf "Sreturn("; print_expr e; printf ")"
  | Sassign (id, e1, _)  -> printf "Sassign(%s, " id; print_expr e1; printf ")"
  | Sdeclare (id, t ,e1, _) -> printf "Sdeclare(%s, %s, " id (string_of_crust_types t); print_expr e1; printf ")"
  | Sprint (e, _)        -> printf "Sprint("; print_expr e; printf ")"
  | Sprintn (e, _)       -> printf "Sprintn("; print_expr e; printf ")"
  | Sscanf (id, _)       -> printf "Sscanf( %s )" id
  | Sblock (bl, _)       -> interpret_block_stmt  bl
  | Swhile(e, bl, _)     -> printf "Swhile( "; print_expr e; printf ",\n"; print_stmt bl; printf ")"
  | Sloop(bl, _)         -> printf "Sloop( "; print_stmt bl; printf ")"
  | _ -> assert false

and print_argument_list = function
  | [arg1] -> let id, t = arg1 in printf " %s : %s" id (string_of_crust_types t)
  | arg1 :: tl -> let id, t = arg1 in printf " %s : %s" id (string_of_crust_types t); printf ", "; print_argument_list tl
  | [] -> ()

and string_of_crust_consts = function 
  | Cu8   c -> "Cu8( "^(Stdint.Uint8.to_string c) ^ " )"
  | Cu16  c -> "Cu16( "^(Stdint.Uint16.to_string c)^ " )"
  | Cu32  c -> "Cu32( "^(Stdint.Uint32.to_string c)^ " )"
  | Cu64  c -> "Cu64( "^(Stdint.Uint64.to_string c)^ " )"
  | Cu128 c -> "Cu128( "^(Stdint.Uint128.to_string c)^ " )"
  | Ci8   c -> "Ci8( "^(Stdint.Int8.to_string c)^ " )"
  | Ci16  c -> "Ci16( "^(Stdint.Int16.to_string c)^ " )"
  | Ci32  c -> "Ci32( "^(Stdint.Int32.to_string c)^ " )"
  | Ci64  c -> "Ci64( "^(Stdint.Int64.to_string c)^ " )"  
  | Ci128 c -> "Ci128( "^(Stdint.Int128.to_string c)^ " )"
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
