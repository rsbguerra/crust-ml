open Ast
open Format

let rec print_expr_list = function
  | [hd] -> print_expr hd
  | hd::tl -> print_expr hd; printf ", " ; print_expr_list tl
  | _ -> ()

and print_expr = function
  | Ecst (n, _)              -> printf " Ecst( "; print_crust_consts  n; printf " )"
  | Eset (e1, e2, _)         -> printf " Eset( "; print_expr e1; print_expr e2; printf ") "
  | Ebinop (Band, e1, e2, _) -> printf " Ebinop( Band, "; print_expr e1; printf ", "; print_expr e2; printf ") "
  | Ebinop (Bor, e1, e2, _)  -> printf " Ebinop( Bor, "; print_expr e1; printf ", "; print_expr e2; printf ") "
  | Ebinop (_, e1, e2, _)    -> printf " Ebinop( Op, "; print_expr e1; printf ", "; print_expr e2; printf ") "
  | Eunop (_ , e1, _)        -> printf " Eunop( Unot, "; print_expr e1; printf ") "
  | Ecall ("size", [e1], _)  -> printf " Ecall( size, "; print_expr e1; printf ") "
  | Ecall (f, el, _)         -> printf " Ecall( %s, " f; print_expr_list el; printf " ) "  
  | Eident (id, line)     -> printf " Eident(%s, %d) " id line
  | _ -> assert false 

and print_stmt = function
  | Sif (e, s1, _, _) -> printf "Sif("; print_expr e; printf ", "; print_stmt s1; printf ","; printf ")"
  | Sreturn (e, _)       -> printf "Sreturn("; print_expr e; printf ")"
  | Sassign (id, e1, _)-> printf "Sassign(%s, " id; print_expr e1; printf ")"
  | Sdeclare (id, _ ,e1, _) -> printf "Sdeclare(%s, t, " id ; print_expr e1; printf ")"
  | Sprint (e, _)        -> printf "Sprint("; print_expr e; printf ")"
  | Sprintn (e, _)       -> printf "Sprintn("; print_expr e; printf ")"
  | Sscanf (id, _)       ->printf "Sscanf( %s )" id
  | Sblock (bl, _)       -> interpret_block_stmt  bl
  | Swhile(e, bl, _) -> printf "Swhile( "; print_expr e; printf ",\n"; print_stmt bl; printf ")"
  | _ -> assert false

and print_argument_list = function
  | [arg1] -> let id, t = arg1 in printf " %s : " id; print_crust_types t
  | arg1 :: tl -> let id, t = arg1 in printf " %s : " id; print_crust_types t; printf ", "; print_argument_list tl
  | [] -> ()

and print_crust_consts = function 
  | Cu8   c -> printf "Cu8( %s )"   (Stdint.Uint8.to_string c)
  | Cu16  c -> printf "Cu16( %s )"  (Stdint.Uint16.to_string c)
  | Cu32  c -> printf "Cu32( %s )"  (Stdint.Uint32.to_string c)
  | Cu64  c -> printf "Cu64( %s )"  (Stdint.Uint64.to_string c)
  | Cu128 c -> printf "Cu128( %s )" (Stdint.Uint128.to_string c)
  | Ci8   c -> printf "Ci8( %s )"   (Stdint.Int8.to_string c)
  | Ci16  c -> printf "Ci16( %s )"  (Stdint.Int16.to_string c)
  | Ci32  c -> printf "Ci32( %s )"  (Stdint.Int32.to_string c)
  | Ci64  c -> printf "Ci64( %s )"  (Stdint.Int64.to_string c)  
  | Ci128 c -> printf "Ci128( %s )" (Stdint.Int128.to_string c)  
  | Cbool c -> printf "Cbool( %b )" c

and print_crust_types = function 
  | Tu8  -> printf "Tu8" 
  | Tu16 -> printf "Tu16" 
  | Tu32 -> printf "Tu32" 
  | Tu64 -> printf "Tu64" 
  | Tu128-> printf "Tu128" 
  | Ti8  -> printf "Ti8" 
  | Ti16 -> printf "Ti16" 
  | Ti32 -> printf "Ti32" 
  | Ti64 -> printf "Ti64" 
  | Ti128-> printf "Ti128" 
  | Tbool-> printf "Tbool" 


and print_stmts = function  
  | Stfunction (f, args, return, body, _) -> printf "Stfunction( %s, (" f ; print_argument_list args; printf "), "; print_crust_types return; printf ", "; print_stmt body
  | Stblock (bl, _) -> interpret_block_stmts bl
  | Stmt (s, _) -> print_stmt s

and interpret_block_stmt  = function
  | [] -> printf "\n"
  | s :: sl -> print_stmt s; printf "\n";interpret_block_stmt sl

and interpret_block_stmts = function
  | [] -> printf "\n"
  | s :: sl -> print_stmts s; printf "\n"; interpret_block_stmts sl

let print_file s = print_stmts s
