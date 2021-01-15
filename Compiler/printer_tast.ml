open Tast

let rec string_of_typed_expr_list acc = function
  | []      -> acc
  | hd::tl -> (string_of_typed_expr_list (acc^(string_of_typed_expr hd)^", ") tl)

and string_of_struct_pair_list acc = function
  | []      -> acc
  | (id, e)::tl -> string_of_struct_pair_list (acc^id^" : "^(string_of_typed_expr e)^", ") tl

and string_of_call_pair_list acc = function
  | []      -> acc
  | (e, t)::tl -> string_of_call_pair_list ((string_of_typed_expr e)^": "^(Printer.string_of_crust_types t)^", ") tl


and string_of_struct_decl_pair_list acc = function
  | []      -> acc
  | (id, e, t)::tl -> string_of_struct_decl_pair_list (acc^id^" : "^(string_of_typed_expr e)^": "^(Printer.string_of_crust_types t)^", ") tl


and string_of_typed_expr = function
  | TEcst (n, t)              -> "TEcst("^(Printer.string_of_crust_consts n)^","^(Printer.string_of_crust_types t)^")"
  | TEident (id, t)           -> "TEident("^id^","^(Printer.string_of_crust_types t)^")"
  | TEbinop (binop, e1, e2, t)-> "TEbinop("^(Printer.string_of_binop binop)^", "^(string_of_typed_expr e1)^", "^(string_of_typed_expr e2)^","^(Printer.string_of_crust_types t)^")"
  | TEunop (unop, e1, t)      -> "TEunop("^(Printer.string_of_unop unop)^", "^(string_of_typed_expr e1)^","^(Printer.string_of_crust_types t)^")"
  | TEstrc_access (id, el, ts, tel)-> "TEstrc_access("^ id^", "^el^", "^(Printer.string_of_crust_types ts)^", "^(Printer.string_of_crust_types tel)^")"
  | TEstrc_decl(id, el, t)    -> "TEstrc_decl("^ id^", "^(string_of_struct_decl_pair_list "" el)^", "^(Printer.string_of_crust_types t)^")"
  | TEvec_decl(els, t)        -> "TEvec_decl("^ (List.fold_left(fun a e -> a ^ ", " ^ (string_of_typed_expr e)) "" els) ^ ", " ^ (Printer.string_of_crust_types t)^ ")"
  | TEvec_access(id, e, te, tid) -> "TEvec_access("^id^", "^(string_of_typed_expr e)^", "^(Printer.string_of_crust_types te)^", "^(Printer.string_of_crust_types tid)^")"
  | TEcall (f, el, t)         -> "TEcall("^f^", "^(string_of_call_pair_list "" el)^","^(Printer.string_of_crust_types t)^")"

and string_of_typed_stmt = function
  | TSif (e, s1, elifs, t)-> "TSif("^(string_of_typed_expr e)^", "^(string_of_typed_stmt s1)^", "^(string_of_elif elifs)^","^(Printer.string_of_crust_types t)^")"
  | TSwhile(e, bl,t)    -> "TSwhile("^(string_of_typed_expr e)^"\n"^(string_of_typed_stmt bl)^","^(Printer.string_of_crust_types t)^")"
  | TSdeclare(id,t,e,ts)-> "TSdeclare("^id^", "^(Printer.string_of_crust_types t)^", "^(string_of_typed_expr e)^","^(Printer.string_of_crust_types ts)^")"
  | TSassign(id, e, t)  -> "TSassign("^id^", "^(string_of_typed_expr e)^","^(Printer.string_of_crust_types t)^")"
  | TSprintn (e, te, t) -> "TSprintln("^(string_of_typed_expr e)^","^(Printer.string_of_crust_types te)^","^(Printer.string_of_crust_types t)^")"
  | TSprint (e, te, t)  -> "TSprint("^(string_of_typed_expr e)^","^(Printer.string_of_crust_types te)^","^(Printer.string_of_crust_types t)^")"
  | TSblock (bl, t)    -> string_of_block_typed_stmt "" bl
  | TScontinue t       -> "TScontinue("^(Printer.string_of_crust_types t)^")"
  | TSbreak t          -> "TSbreak("^(Printer.string_of_crust_types t)^")"
  | TSreturn (e, t)    -> "TSreturn("^(string_of_typed_expr e)^","^(Printer.string_of_crust_types t)^")"
  | TSnothing t        -> "TSnothing"^(Printer.string_of_crust_types t)^")"
  | TSexpr (e,t)       -> "TSexpr("^(string_of_typed_expr e)^","^(Printer.string_of_crust_types t)^")"

and string_of_elif l = 
  let out = ref "" in
  List.iter(fun (e, body) -> 
    out := !out ^ "Selif("^(string_of_typed_expr e)^", "^(string_of_typed_stmt body)^ ")"
  )l;
  !out

and string_of_block_typed_stmt acc = function
  | []      -> acc^"\n"
  | s :: sl -> string_of_block_typed_stmt (acc^(string_of_typed_stmt s)^"\n") sl

and string_of_block_typed_global_stmt acc = function
  | []      -> acc^"\n"
  | s :: sl -> string_of_block_typed_global_stmt (acc^(string_of_typed_global_stmt s)^"\n") sl

and string_of_typed_global_stmt = function
  | TGSblock bl -> string_of_block_typed_global_stmt "" bl
  | TGSfunction (f, args, return, body) -> 
    "TGSfunction("^f^", ("^(string_of_pairs "" args)^"), "^(Printer.string_of_crust_types return)^", \n    "^(string_of_typed_stmt body)
  | TGSstruct (id, elements) -> "GSstruct("^id^",("^(string_of_pairs "" elements)^"))"

and string_of_pairs acc = function
  | []         -> acc
  | (id, t):: tl -> string_of_pairs (acc^id^":"^(Printer.string_of_crust_types t)^", ") tl

let print_typed_ast s = 
  Printf.printf "TYPED AST:\n\n\n%s\n\n" (string_of_typed_global_stmt s)
