open Tast

let string_of_crust_consts = function 
  | Ast.Ci32  c -> "Ci32("^(Stdint.Int32.to_string c)^")"
  | Ast.Cbool c -> "Cbool("^(string_of_bool c)^")"

let string_of_crust_types = function 
  | Ast.Tvoid -> "Tvoid"
  | Ast.Ti32  -> "Ti32"
  | Ast.Tbool -> "Tbool"

let rec string_of_typed_expr_list acc = function
  | []      -> acc
  | hd::tl -> (string_of_typed_expr_list (acc^(string_of_typed_expr hd)^", ") tl)

and string_of_typed_expr = function
  | TEcst (n, t)              -> "TEcst("^(string_of_crust_consts n)^","^(string_of_crust_types t)^")"
  | TEident (id, t)           -> "TEident("^id^","^(string_of_crust_types t)^")"
  | TEbinop (binop, e1, e2, t)-> "TEbinop("^(Printer.string_of_binop binop)^", "^(string_of_typed_expr e1)^", "^(string_of_typed_expr e2)^","^(string_of_crust_types t)^")"
  | TEunop (unop, e1, t)      -> "TEunop("^(Printer.string_of_unop unop)^", "^(string_of_typed_expr e1)^","^(string_of_crust_types t)^")"
  | TEcall (f, el, t)         -> "TEcall("^f^", "^(string_of_typed_expr_list "" el)^","^(string_of_crust_types t)^")"

and string_of_typed_stmt = function
  | TSif (e, s1, elifs)-> "TSif("^(string_of_typed_expr e)^", "^(string_of_typed_stmt s1)^", "^(string_of_elif elifs)^")"
  | TSwhile(e, bl)     -> "TSwhile("^(string_of_typed_expr e)^"\n"^(string_of_typed_stmt bl)^")"
  | TSdeclare (id, t, e1) -> "TSdeclare("^id^", "^(string_of_crust_types t)^", "^(string_of_typed_expr e1)^")"
  | TSassign (id, e1)  -> "TSassign("^id^", "^(string_of_typed_expr e1)^")"
  | TSprintn e        -> "TSprintln("^(string_of_typed_expr e)^")"
  | TSprint e         -> "TSprint("^(string_of_typed_expr e)^")"
  | TSblock bl        -> string_of_block_typed_stmt "" bl
  | TScontinue        -> "TScontinue"
  | TSbreak           -> "TSbreak"
  | TSreturn (e, t)   -> "TSreturn("^(string_of_typed_expr e)^")"
  | TSnothing         -> "TSnothing"
  | TSexpr e          -> "TSexpr("^(string_of_typed_expr e)^")"

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
    "TGSfunction("^f^", ("^(string_of_pairs "" args)^"), "^(string_of_crust_types return)^", \n    "^(string_of_typed_stmt body)
  | TGSstruct (id, elements)-> "GSstruct("^id^"(,"^(string_of_pairs "" elements)^")"

and string_of_pairs acc = function
  | []         -> acc
  | (id, t):: tl -> string_of_pairs (acc^id^":"^(string_of_crust_types t)^", ") tl

let print_typed_ast s = 
  Printf.printf "TYPED AST:\n\n\n%s\n\n" (string_of_typed_global_stmt s)
