open Ast

let rec string_of_expr_list = function
  | [hd]   -> string_of_expr hd
  | hd::tl -> (string_of_expr hd)^", "^(string_of_expr_list tl)
  | _      -> ""

and string_of_expr = function
  | Ecst (n, _)              -> Format.sprintf "@[<1>%s@ %s@ %s@]@." "Ecst(" (string_of_crust_consts n) ")"
  | Ebinop (Band, e1, e2, _) -> "Ebinop(Band, "^(string_of_expr e1)^", "^(string_of_expr e2)^")"
  | Ebinop (Bor, e1, e2, _)  -> "Ebinop(Bor, "^(string_of_expr e1)^", "^(string_of_expr e2)^")"
  | Ebinop (_, e1, e2, _)    -> "Ebinop(Op, "^(string_of_expr e1)^", "^(string_of_expr e2)^")"
  | Eunop (_ , e1, _)        -> "Eunop(Unot, "^(string_of_expr e1)^")"
  | Ecall ("size", [e1], _)  -> "Ecall(size, "^(string_of_expr e1)^")"
  | Ecall (f, el, _)         -> "Ecall("^f^", "^(string_of_expr_list el)^")"  
  | Eident (id, _)           -> "Eident("^id^")"

and string_of_stmt = function
  | Sif (e, s1, elifs, _)-> "Sif("^(string_of_expr e)^", "^(string_of_stmt s1)^", "^(string_of_elif elifs)^")"
  | Sreturn (e, _)       -> "Sreturn("^(string_of_expr e)^")"
  | Sassign (id, e1, _)  -> "Sassign("^id^", "^(string_of_expr e1)^")"
  | Sdeclare (id,t,e1,_) -> "Sdeclare("^id^", "^(string_of_crust_types t)^", "^(string_of_expr e1)^")"
  | Sprint (e, _)        -> "Sprint("^(string_of_expr e)^", "^")"
  | Sprintn (e, _)       -> Format.sprintf "@[<1>%s%s%s]@." "Sprintln(" (string_of_expr e) ")"
  | Sblock (bl, _)       -> string_of_block_stmt  bl
  | Swhile(e, bl, _)     -> "Swhile("^(string_of_expr e)^"\n"^(string_of_stmt bl)^")"
  | _ -> assert false

and string_of_elif l = 
  let out = ref "" in
  List.iter(fun (e, body, line) -> 
    out := !out ^ "Selif("^(string_of_expr e)^", "^(string_of_stmt body)^ ")"
  )l;
  !out

and string_of_argument_list = function
  | [arg1]     -> let id, t = arg1 in id^":"^(string_of_crust_types t)
  | arg1 :: tl -> let id, t = arg1 in id^":"^(string_of_crust_types t)^", "^(string_of_argument_list tl)
  | []         -> ""

and string_of_crust_consts = function 
  | Ci32  c -> "Ci32( "^(Stdint.Int32.to_string c)^" )"
  | Cbool c -> "Cbool( "^(string_of_bool c)^" )"

and string_of_crust_types = function 
  | Tvoid  -> "Tvoid"
  | Ti32 -> "Ti32"
  | Tbool-> "Tbool"

and string_of_binop = function
  | Badd -> "+"
  | Bsub -> "-"
  | Bdiv -> "/"
  | Bmod -> "%"
  | Bmul -> "*"
  | Beq -> "=="
  | Bneq -> "!="
  | Blt -> "<" 
  | Ble -> "<="
  | Bgt -> ">" 
  | Bge -> ">="
  | Band -> "&&"
  | Bor -> "||"

and string_of_block_stmt  = function
  | []      -> "\n"
  | s :: sl -> (string_of_stmt s)^"\n"^(string_of_block_stmt sl)

and string_of_block_global_stmt = function
  | []      -> "\n"
  | s :: sl -> (string_of_global_stmt s)^"\n"^(string_of_block_global_stmt sl)

and string_of_global_stmt = function
  | GSblock (bl, _) -> string_of_block_global_stmt bl
  | GSfunction (f, args, return, body, _) -> "GSfunction("^f^", ("^(string_of_argument_list args)^"), "^(string_of_crust_types return)^", \n    "^(string_of_stmt body)
  | GSstruct (id, elements, _)-> "GSstruct("^id^"(,"^(string_of_argument_list elements)^")"
   
let print_file s = 
  Printf.printf "%s\n" (string_of_global_stmt s)
