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
  | Sloop(bl, _)         -> "Sloop("^(string_of_stmt bl)^")"
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
  | Cu8   c -> "Cu8("^(Stdint.Uint8.to_string c)^")"
  | Cu16  c -> "Cu16( "^(Stdint.Uint16.to_string c)^" )"
  | Cu32  c -> "Cu32( "^(Stdint.Uint32.to_string c)^" )"
  | Cu64  c -> "Cu64( "^(Stdint.Uint64.to_string c)^" )"
  | Cu128 c -> "Cu128( "^(Stdint.Uint128.to_string c)^" )"
  | Ci8   c -> "Ci8("^(Stdint.Int8.to_string c)^")"
  | Ci16  c -> "Ci16( "^(Stdint.Int16.to_string c)^" )"
  | Ci32  c -> "Ci32( "^(Stdint.Int32.to_string c)^" )"
  | Ci64  c -> "Ci64("^(Stdint.Int64.to_string c)^")"  
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
  | _ -> assert false  

and string_of_block_stmt  = function
  | []      -> "\n"
  | s :: sl -> (string_of_stmt s)^"\n"^(string_of_block_stmt sl)

and string_of_block_global_stmt = function
  | []      -> "\n"
  | s :: sl -> (string_of_global_stmt s)^"\n"^(string_of_block_global_stmt sl)

and string_of_global_stmt = function
  | GSblock (bl, _) -> string_of_block_global_stmt bl
  | GSuse (id, _)   -> "GSuse("^id^")"
  | GSfunction (f, args, return, body, _) -> "GSfunction("^f^", ("^(string_of_argument_list args)^"), "^(string_of_crust_types return)^", \n    "^(string_of_stmt body)
  | GSstruct (id, _)-> "GSstruct("^id^")"
  | GSimpl (id, _)  -> "GSimpl("^id^")"
   
let print_file s = 
  Printf.printf "%s\n" (string_of_global_stmt s)
