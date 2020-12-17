open Ast

let string_of_crust_types = function 
  | Tvoid -> "Tvoid"
  | Ti32  -> "Ti32"
  | Tbool -> "Tbool"

let string_of_unop = function
  | Uneg -> "-"
  | Unot -> "!"

let string_of_binop = function
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

let string_of_crust_consts = function 
  | Ci32  c -> "Ci32("^(Stdint.Int32.to_string c)^")"
  | Cbool c -> "Cbool("^(string_of_bool c)^")"

let rec string_of_expr_list acc = function
  | []      -> acc
  | hd::tl -> string_of_expr_list (acc^(string_of_expr hd)^", ") tl

and string_of_expr = function
  | Ecst (n, _)              -> "Ecst("^(string_of_crust_consts n)^")"
  | Eident (id, _)           -> "Eident("^id^")"
  | Ebinop (binop, e1, e2, _)-> "Ebinop("^(string_of_binop binop)^", "^(string_of_expr e1)^", "^(string_of_expr e2)^")"
  | Eunop (unop , e1, _)     -> "Eunop("^(string_of_unop unop)^", "^(string_of_expr e1)^")"
  | Ecall (f, el, _)         -> "Ecall("^f^", "^(string_of_expr_list "" el)^")"  

and string_of_stmt = function
  | Sif (e, s1, elifs, _)-> "Sif("^(string_of_expr e)^", "^(string_of_stmt s1)^", "^(string_of_elif elifs)^")"
  | Sreturn (e, _)       -> "Sreturn("^(string_of_expr e)^")"
  | Sassign (id, e1, _)  -> "Sassign("^id^", "^(string_of_expr e1)^")"
  | Sdeclare (id,t,e1,_) -> "Sdeclare("^id^", "^(string_of_crust_types t)^", "^(string_of_expr e1)^")"
  | Sprint (e, _)        -> "Sprint("^(string_of_expr e)^", "^")"
  | Sprintn (e, _)       -> "Sprintln("^(string_of_expr e)^")"
  | Sblock (bl, _)       -> string_of_block_stmt "" bl
  | Swhile(e, bl, _)     -> "Swhile("^(string_of_expr e)^"\n"^(string_of_stmt bl)^")"
  | _ -> assert false

and string_of_elif l = 
  let out = ref "" in
  List.iter(fun (e, body, line) -> 
    out := !out ^ "Selif("^(string_of_expr e)^", "^(string_of_stmt body)^ ")"
  )l;
  !out

and string_of_block_stmt acc = function
  | []      -> acc^"\n"
  | s :: sl -> string_of_block_stmt (acc^(string_of_stmt s)^"\n") sl

and string_of_block_global_stmt acc = function
  | []      -> acc^"\n"
  | s :: sl -> string_of_block_global_stmt (acc^(string_of_global_stmt s)^"\n") sl

and string_of_global_stmt = function
  | GSblock (bl, _) -> string_of_block_global_stmt "" bl
  | GSfunction (f, args, return, body, _) -> 
    "GSfunction("^f^", ("^(string_of_pairs "" args)^"), "^(string_of_crust_types return)^", \n    "^(string_of_stmt body)
  | GSstruct (id, el, _)-> "GSstruct("^id^"(,"^(string_of_pairs "" el)^")"

and string_of_pairs acc = function
  | []           -> acc
  | (id, t):: tl -> string_of_pairs (acc^id^":"^(string_of_crust_types t)^", ") tl

let print_file s = 
  Printf.printf "AST:\n\n\n%s\n\n" (string_of_global_stmt s)
