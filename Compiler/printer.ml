open Ast

let rec string_of_crust_types = function 
  | Tunit -> "Tunit"
  | Ti32  -> "Ti32"
  | Tbool -> "Tbool"
  | Tstruct s -> "Tstruct( " ^ s ^ " )"
  | Tvec (s, sz) -> "Tvec( " ^ string_of_crust_types s ^ ", "^string_of_int sz^" )"
  | Tref (t, id) -> "Tref( " ^ string_of_crust_types t ^  ", " ^ id ^ " )"
  | Tmut t -> "Tmut( " ^ string_of_crust_types t ^ " )"

let string_of_unop = function
  | Uneg -> "-"
  | Unot -> "!"
  | Uptr -> "*"
  | Umut -> "&mut"

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
  | Cunit   -> "Cunit( () )"

let rec string_of_expr_list acc = function
  | []      -> acc
  | hd::tl -> string_of_expr_list (acc^(string_of_expr hd)^", ") tl

and string_of_struct_pair_list acc = function
  | []      -> acc
  | (id, e)::tl -> string_of_struct_pair_list (acc^id^" : "^(string_of_expr e)^", ") tl


and string_of_expr = function
  | Ecst (n, _)              -> "Ecst("^(string_of_crust_consts n)^")"
  | Eident (id, _)           -> "Eident("^id^")"
  | Eref (id, _)             -> "Eref("^id^")"
  | Ebinop (binop, e1, e2, _)-> "Ebinop("^(string_of_binop binop)^", "^(string_of_expr e1)^", "^(string_of_expr e2)^")"
  | Eunop (unop , e1, _)     -> "Eunop("^(string_of_unop unop)^", "^(string_of_expr e1)^")"
  | Estrc_access (id, el, _) -> "Estrc_access("^ id^", "^el^")"
  | Estrc_decl (id, el, _)   -> "Estrc_decl("^ id^", "^(string_of_struct_pair_list "" el)^")"
  | Ecall (f, el, _)         -> "Ecall("^f^", "^(string_of_expr_list "" el)^")"  
  | Elen (id, _)                 -> "Elen("^id^")"
  | Evec_access(id, e, _)   -> "Evec_access("^id^", "^(string_of_expr e)^")"
  | Evec_decl(el, _)         -> "Evec_decl("^(string_of_expr_list "" el)^")" 



and string_of_stmt = function
  | Sif (e, s1, elifs, _)-> "Sif("^(string_of_expr e)^", "^(string_of_stmt s1)^", "^(string_of_elif elifs)^")"
  | Swhile(e, bl, _)     -> "Swhile("^(string_of_expr e)^"\n"^(string_of_stmt bl)^")"
  | Sdeclare (id,t,e1,_) -> "Sdeclare("^id^", "^(string_of_crust_types t)^", "^(string_of_expr e1)^")"
  | Sassign (id, e1, _)  -> "Sassign("^id^", "^(string_of_expr e1)^")"
  | Sprintn (e, _)       -> "Sprintln("^(string_of_expr e)^")"
  | Sprint (e, _)        -> "Sprint("^(string_of_expr e)^", "^")"
  | Sblock (bl, _)       -> string_of_block_stmt "" bl
  | Scontinue _          -> "Scontinue"
  | Sbreak _             -> "Sbreak"
  | Sreturn (e, _)       -> "Sreturn("^(string_of_expr e)^")"
  | Snothing _           -> "Snothing"
  | Sexpr(e, _)          -> "Sexpr("^(string_of_expr e)^")"

and string_of_elif l = 
  let out = ref "" in
  List.iter(fun (e, body, _) -> 
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
