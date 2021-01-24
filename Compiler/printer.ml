open Ast
open Int32

let rec string_of_type = function
  | Tid id -> "Tid(" ^ id ^ ")"
  | Tid_typed (id, t) -> "Tid(" ^ id ^ string_of_type t ^ ")"
  | Tref t -> "Tref(" ^ string_of_type t ^ ")"
  | Trefmut t -> "Trefmut(" ^ string_of_type t ^ ")"

let string_of_unop = function
  | Uneg -> "-"
  | Unot -> "!"
  | Uref -> "&"
  | Urefmut -> "&mut"
  | Uderef -> "*"

let string_of_binop = function
  | Badd -> "+"
  | Bsub -> "-"
  | Bdiv -> "/"
  | Bmod -> "%"
  | Bmul -> "*"
  | Beq  -> "=="
  | Bneq -> "!="
  | Blt  -> "<" 
  | Ble  -> "<="
  | Bgt  -> ">" 
  | Bge  -> ">="
  | Band -> "&&"
  | Bor  -> "||"
  | Bassign -> "="

let rec string_of_expr_list exprs = 
  let rec aux acc = function
    | []      -> acc
    | hd::tl -> aux (acc^(string_of_expr hd)^", ") tl
  in aux "" exprs

and string_of_block (stmts, exp) = 
  let stmts = List.fold_left (fun acc s -> (string_of_stmt s) ^ ", " ^ acc) "" stmts in
  match exp with
  | Some exp -> stmts ^ string_of_expr exp
  | None -> stmts

and string_of_pair_list pairs = 
  let rec aux acc = function
    | []           -> acc
    | (id, t)::tl  -> aux (acc^id^" : "^(string_of_type t)^", ") tl
  in aux "" pairs

and string_of_struct_decl_pair_list pairs = 
  let rec aux acc = function
    | []          -> acc
    | (id, e)::tl -> aux (acc ^id^" : "^(string_of_expr e)^", ") tl
  in aux "" pairs

and string_of_arg_list args =
  let rec aux acc = function
    | []          -> acc
    | (mut, id, t)::tl -> aux (acc ^ (if mut then "mut " else "") ^ id ^ " : "^(string_of_type t)^", ") tl
  in aux "" args

and string_of_expr = function
  | Eint (n, _)               -> "Eint("^(Int32.to_string n)^")"
  | Ebool (b, _)              -> "Ecst("^(string_of_bool b)^")"
  | Eident (id, _)            -> "Eident("^id^")"
  | Ebinop (binop, e1, e2, _) -> "Ebinop("^(string_of_binop binop)^", "^(string_of_expr e1)^", "^(string_of_expr e2)^")"
  | Eunop (unop , e1, _)      -> "Eunop("^(string_of_unop unop)^", "^(string_of_expr e1)^")"
  | Estruct_access (e, id, _) -> "Estrc_access("^ (string_of_expr e) ^ ", " ^ id ^")"
  | Elen (e, _)               -> "Elen("^ (string_of_expr e) ^")"
  | Evec_access(e1, e2, _)    -> "Evec_access("^ string_of_expr e1 ^", "^(string_of_expr e2)^")"
  | Ecall (f, el, _)          -> "Ecall("^ f ^", "^(string_of_expr_list el)^")"  
  | Evec_decl(el, _)          -> "Evec_decl("^(string_of_expr_list el)^")" 
  | Eprint (s, _)             -> "Eprint(" ^ s ^ ")"
  | Eblock (b, _)             -> "Eblock(" ^ (string_of_block b) ^ ")"

and string_of_stmt = function
  | Snothing _            -> "Snothing"
  | Sexpr(e, _)           -> "Sexpr("^(string_of_expr e)^")"
  | Sdeclare (mut,id,e,_) -> "Sdeclare(" ^ (if mut then "mut " else "") ^ id ^", " ^(string_of_expr e)
  | Sdeclare_struct (mut,id,t,el,_) -> "Sdeclare_struct(" ^ (if mut then "mut " else "") ^ id ^", " ^ t ^ ", "^(string_of_struct_decl_pair_list el)^")"
  | Swhile(e, bl, _)      -> "Swhile("^(string_of_expr e)^"\n"^(string_of_block bl)^")"
  | Sreturn (Some e, _)   -> "Sreturn("^(string_of_expr e)^")"
  | Sreturn (None, _)     -> "Sreturn("^")"
  | Sif (e, b1, b2, _)    -> "Sif("^ (string_of_expr e) ^ ", "^(string_of_block b1)^", "^(string_of_block b2)^")"

and string_of_decl = function
  | Dstruct (id, pairs, _)-> "Dstruct("^id^"(,"^(string_of_pair_list pairs)^")"
  | Dfun (f,args,t,body,_) -> 
    match t with
    | Some t -> "Dfun("^f^", ("^(string_of_arg_list args)^"), "^(string_of_type t)^", \n    "^(string_of_block body)
    | None   -> "Dfun("^f^", ("^(string_of_arg_list args)^")"^", \n    "^(string_of_block body)

and string_of_program p = 
  List.fold_left (fun acc d -> acc ^ string_of_decl d) "" p

let print_file s = 
  Printf.printf "AST:\n\n\n%s\n\n" (string_of_program s)
