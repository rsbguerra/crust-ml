open Tast
open Int32
let rec string_of_program p = 
  List.fold_left (fun acc d -> acc ^ string_of_typed_decl d) "" p

and string_of_typed_decl = function
  | TDstruct (id, p, t) -> 
    "TDstruct("^id^",("^(string_of_pairs "" p)^")" ^ (string_of_prust_types t)^ ")"
  | TDfun (f, args, return, body, t) -> 
    "TDfun("^f^", ("^(string_of_args args)^"), "^
    (string_of_prust_types return)^", "^
    (string_of_typed_block body) ^
    (string_of_prust_types t)

and string_of_pairs acc = function
  | []         -> acc
  | (id, t):: tl -> string_of_pairs (acc^id^":"^(string_of_prust_types t)^", ") tl

and string_of_args args = 
  List.fold_left (fun acc (mut, id, t) -> acc ^ (if mut then "mut " else "") ^ id ^ ":" ^ (string_of_prust_types t)^", ") "" args

and string_of_prust_types = function 
  | Tunit -> "Tunit"
  | Ti32  -> "Ti32"
  | Tbool -> "Tbool"
  | Tstruct s    -> "Tstruct( " ^ s ^ " )"
  | Tvec t -> "Tvec( " ^ (string_of_prust_types t) ^ " )"
  | Tref t -> "Tref( " ^ (string_of_prust_types t) ^ " )"
  | Trefmut t -> "Trefmut( " ^ (string_of_prust_types t) ^ " )"

and string_of_typed_expr_list acc = function
  | []      -> acc
  | hd::tl -> (string_of_typed_expr_list (acc^(string_of_typed_expr hd)^", ") tl)

and string_of_struct_pair_list acc = function
  | []      -> acc
  | (id, e)::tl -> string_of_struct_pair_list (acc^id^" : "^(string_of_typed_expr e)^", ") tl

and string_of_call_pair_list pair = 
  List.fold_left (fun acc e -> acc ^ ", " ^ string_of_typed_expr e) "" pair

and string_of_struct_decl_pair_list acc = function
  | []      -> acc
  | (id, e, t)::tl -> string_of_struct_decl_pair_list (acc^id^" : "^(string_of_typed_expr e)^": "^(string_of_prust_types t)^", ") tl

and string_of_typed_expr = function
  | TEint (n, t)            -> "TEint("^(Int32.to_string n)^", "^(string_of_prust_types t)^")"
  | TEbool (b, t)           -> "TEbool("^ (string_of_bool b) ^ ", " ^(string_of_prust_types t)^")"
  | TEident (id, t)         -> "TEident("^ id ^", "^(string_of_prust_types t)^")"
  | TEunop (op, e1, t)      -> "TEunop("^(Printer.string_of_unop op)^", "^(string_of_typed_expr e1)^","^(string_of_prust_types t)^")"
  | TEbinop (op, e1, e2, t) -> "TEbinop("^(Printer.string_of_binop op)^", "^(string_of_typed_expr e1)^", "^(string_of_typed_expr e2)^","^(string_of_prust_types t)^")"
  | TEstruct_access (e,id,t1,t2)  -> "TEstrc_access(" ^ (string_of_typed_expr e) ^ ", " ^ id ^ ", " ^ (string_of_prust_types t1) ^ (string_of_prust_types t1) ^ ")"
  | TElen (e, t)              -> "TElen(" ^ (string_of_typed_expr e) ^ (string_of_prust_types t)^ ")"
  | TEvec_access(e1, e2, tid) -> "TEvec_access(" ^(string_of_typed_expr e1)^", "^(string_of_typed_expr e1)^", "^(string_of_prust_types tid)^")"
  | TEcall (f, el, t)            -> "TEcall("^ f ^ ", " ^ (string_of_call_pair_list el) ^ "," ^(string_of_prust_types t)^")"
  | TEvec_decl (els, t)          -> "TEvec_decl("^ (List.fold_left(fun a e -> a ^ ", " ^ (string_of_typed_expr e)) "" els) ^ ", " ^ (string_of_prust_types t)^ ")"
  | TEprint (s, t)  -> "TEprint(" ^ s ^ ", " ^ (string_of_prust_types t) ^ ")"
  | TEblock (bl, t) -> "TEblock(" ^ (string_of_typed_block bl) ^ ", " ^ (string_of_prust_types t) ^ ")"

and string_of_typed_block (stmts, exp, t) = 
  let stmts = List.fold_left (fun acc s -> acc ^ "," ^ (string_of_typed_stmt s) ^ "\n") "" stmts in
  match exp with
    | Some e -> stmts ^( string_of_typed_expr e) ^ ", " ^ (string_of_prust_types t)
    | None -> stmts ^ ", " ^ (string_of_prust_types t)


and string_of_typed_stmt = function
  | TSnothing t            -> "TSnothing"^(string_of_prust_types t)^")"
  | TSexpr (e,t)           -> "TSexpr("^(string_of_typed_expr e)^", "^(string_of_prust_types t)^")"
  | TSdeclare(mut,id,e,t) -> "TSdeclare(" ^ (if mut then "mut " else "") ^id^", "^(string_of_typed_expr e)^","^(string_of_prust_types t)^")"
  | TSdeclare_struct(mut, id, tid, el, t) -> "TSdeclare_struct(" ^ (if mut then "mut " else "") ^ id^", "^tid^", "^(string_of_struct_decl_pair_list "" el)", "^(string_of_prust_types t)^")"
  | TSwhile(e, bl, t)     -> "TSwhile("^(string_of_typed_expr e)^"\n"^(string_of_typed_block bl) ^ ", " ^(string_of_prust_types t)^")"
  | TSreturn (None, t)    -> "TSreturn("^(string_of_prust_types t)^")"
  | TSreturn (Some e, t)  -> "TSreturn("^(string_of_typed_expr e)^", "^(string_of_prust_types t)^")"
  | TSif (e, bl1, bl2, t) -> 
  "TSif("^(string_of_typed_expr e)^", "^(string_of_typed_block bl1)^", "^(string_of_typed_block bl2)^", "^(string_of_prust_types t)^")"

let print_typed_ast s = 
  Printf.printf "TYPED AST:\n\n%s\n\n" (string_of_program s)
