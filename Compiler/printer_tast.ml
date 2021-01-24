open Tast

let string_of_program p = 
  List.fold_left (fun acc d -> acc ^ string_of_decl d) "" p

and string_of_typed_decl = function
  | TDstruct (id, pairs, t) -> 
    "GSstruct("^id^",("^(string_of_pairs pairs)^")" ^ string_of_prust_types t^ ")"
  | TDfun (f, args, return, body, t) -> 
    "TGSfunction("^f^", ("^(string_of_args args)^"), "^
    (string_of_prust_types return)^", \n    "^
    (string_of_typed_block body)
    (string_of_prust_types t)

and string_of_pairs acc = function
  | []         -> acc
  | (id, t):: tl -> string_of_pairs (acc^id^":"^(string_of_prust_types t)^", ") tl

and string_of_args args = 
  List.fold_left (fun acc (mut, id, t) -> acc ^ (if mut then "mut " else "") ^ id ^ ":" (string_of_prust_types t)^", ") "" args

and string_of_prust_types = function 
  | Tunit -> "Tunit"
  | Ti32  -> "Ti32"
  | Tbool -> "Tbool"
  | Tstruct s    -> "Tstruct( " ^ s ^ " )"
  | Tvec (s, sz) -> "Tvec( " ^ string_of_prust_types s ^ ", " ^ string_of_int sz ^ " )"
  | Tref (t, id) -> "Tref( " ^ string_of_prust_types t ^ ", " ^ id ^ " )"

and string_of_typed_expr_list acc = function
  | []      -> acc
  | hd::tl -> (string_of_typed_expr_list (acc^(string_of_typed_expr hd)^", ") tl)

and string_of_struct_pair_list acc = function
  | []      -> acc
  | (id, e)::tl -> string_of_struct_pair_list (acc^id^" : "^(string_of_typed_expr e)^", ") tl

and string_of_call_pair_list pair = 
  let rec aux acc = function
  | []      -> acc
  | (e, t)::tl -> aux ((string_of_typed_expr e)^": "^(string_of_prust_types t)^", ") tl
  in aux "" pair
  
and string_of_struct_decl_pair_list acc = function
  | []      -> acc
  | (id, e, t)::tl -> string_of_struct_decl_pair_list (acc^id^" : "^(string_of_typed_expr e)^": "^(string_of_prust_types t)^", ") tl

and string_of_typed_expr = function
  | TEint (n, t)            -> "TEint("^(string_of_int n)^", "^(string_of_prust_types t)^")"
  | TEbool (b, t)           -> "TEbool("^ (string_of_bool b) ^ ", " ^(string_of_prust_types t)^")"
  | TEident (id, t)         -> "TEident("^ id ^", "^(string_of_prust_types t)^")"
  | TEunop (op, e1, t)      -> "TEunop("^(Printer.string_of_unop op)^", "^(string_of_typed_expr e1)^","^(string_of_prust_types t)^")"
  | TEbinop (op, e1, e2, t) -> "TEbinop("^(Printer.string_of_binop op)^", "^(string_of_typed_expr e1)^", "^(string_of_typed_expr e2)^","^(string_of_prust_types t)^")"
  | TEstrc_access (e,id,t)  -> "TEstrc_access(" ^ (string_of_typed_expr e) ^ ", " ^ id ^ ", " ^ (string_of_prust_types t) ^ ")"
  | TElen (id)              -> "TElen(" ^ id ^ ")"
  | TEvec_access(id, e, te, tid) -> "TEvec_access("^id^", "^(string_of_typed_expr e)^", "^(string_of_prust_types te)^", "^(string_of_prust_types tid)^")"
  | TEcall (f, el, t)            -> "TEcall("^ f ^ ", " ^ (string_of_call_pair_list el) ^ "," ^(string_of_prust_types t)^")"
  | TEvec_decl (els, t)          -> "TEvec_decl("^ (List.fold_left(fun a e -> a ^ ", " ^ (string_of_typed_expr e)) "" els) ^ ", " ^ (string_of_prust_types t)^ ")"
  | TEprint (s, t)  -> "TEprint(" ^ s ^ ", " ^ (string_of_typed_expr t) ^ ")"
  | TEblock (bl, t) -> "TEblock(" ^ (string_of_typed_block bl) ^ ", " ^ (string_of_typed_expr t) ^ ")"

and string_of_typed_block bl = 
  List.fold_left (fun acc stmt -> acc ^ ", " string_of_typed_stmt stmt) "" bl

and string_of_typed_stmt = function
  | TSnothing t            -> "TSnothing"^(string_of_prust_types t)^")"
  | TSexpr (e,t)           -> "TSexpr("^(string_of_typed_expr e)^", "^(string_of_prust_types t)^")"
  | TSdeclare(mut,id,e,ts) -> 
  "TSdeclare(" ^ (if mut then "mut " else "") ^ id ^ ", " ^(string_of_prust_types t)^", "^(string_of_typed_expr e)^","^(string_of_prust_types ts)^")"
  | TEdeclare_struct(id, el, t) -> "TSdeclare_struct(" ^ (if mut then "mut " else "") ^ id^", "^(string_of_struct_decl_pair_list "" el)^", "^(string_of_prust_types t)^")"
  | TSwhile(e, bl, t)     -> "TSwhile("^(string_of_typed_expr e)^"\n"^(string_of_typed_block bl) ^ ", " ^(string_of_prust_types t)^")"
  | TSreturn (None, t)    -> "TSreturn("^(string_of_prust_types t)^")"
  | TSreturn (Some e, t)  -> "TSreturn("^(string_of_typed_expr e)^", "^(string_of_prust_types t)^")"
  | TSif (e, bl1, bl2, t) -> 
  "TSif("^(string_of_typed_expr e)^", "^(string_of_typed_block bl1)^", "^(string_of_typed_block bl2)^", "^(string_of_prust_types t)^")"

let print_typed_ast s = 
  Printf.printf "TYPED AST:\n\n\n%s\n\n" (string_of_typed_decl s)
