open Tast

let rec string_of_typed_expr_list acc = function
  | []      -> acc
  | hd::tl -> (string_of_typed_expr_list ((string_of_typed_expr hd)^", "^acc) tl)

and string_of_typed_expr = function
  | TEcst (n, _)              -> Format.sprintf "@[<1>%s@ %s@ %s@]@." "Ecst(" (string_of_crust_consts n) ")"
  | TEbinop (TBand, e1, e2, _) -> "Ebinop(Band, "^(string_of_typed_expr e1)^", "^(string_of_typed_expr e2)^")"
  | TEbinop (TBor, e1, e2, _)  -> "Ebinop(Bor, "^(string_of_typed_expr e1)^", "^(string_of_typed_expr e2)^")"
  | TEbinop (_, e1, e2, _)    -> "Ebinop(Op, "^(string_of_typed_expr e1)^", "^(string_of_typed_expr e2)^")"
  | TEunop (_ , e1, _)        -> "Eunop(Unot, "^(string_of_typed_expr e1)^")"
  | TEcall (f, el, _)         -> "Ecall("^f^", "^(string_of_typed_expr_list "" el)^")"  
  | TEident (id, _)           -> "Eident("^id^")"

and string_of_typed_stmt = function
  | TSif (e, s1, elifs)-> "Sif("^(string_of_typed_expr e)^", "^(string_of_typed_stmt s1)^", "^(string_of_elif elifs)^")"
  | TSreturn (e, _)       -> "Sreturn("^(string_of_typed_expr e)^")"
  | TSassign (id, e1, _)  -> "Sassign("^id^", "^(string_of_typed_expr e1)^")"
  | TSdeclare (id,t,e1,_) -> "Sdeclare("^id^", "^(string_of_crust_types t)^", "^(string_of_typed_expr e1)^")"
  | TSprint (e, _)        -> "Sprint("^(string_of_expr e)^", "^")"
  | TSprintn (e, _)       -> Format.sprintf "@[<1>%s%s%s]@." "Sprintln(" (string_of_expr e) ")"
  | TSblock (bl, _)       -> string_of_block_stmt  bl
  | TSwhile(e, bl, _)     -> "Swhile("^(string_of_expr e)^"\n"^(string_of_stmt bl)^")"
  | _ -> assert false

and string_of_elif l = 
  let out = ref "" in
  List.iter(fun (e, body, line) -> 
    out := !out ^ "Selif("^(string_of_typed_expr e)^", "^(string_of_typed_stmt body)^ ")"
  )l;
  !out

and string_of_argument_list acc = function
  | [] -> acc
  | arg1 :: tl -> let id, t = arg1 in (string_of_argument_list (id^":"^(string_of_crust_types t)^", "^acc) tl)

and string_of_crust_consts = function 
  | Ci32  c -> "Ci32( "^(Stdint.Int32.to_string c)^" )"
  | Cbool c -> "Cbool( "^(string_of_bool c)^" )"

and string_of_crust_types = function 
  | Ast.Tvoid -> "Tvoid"
  | Ast.Ti32  -> "Ti32"
  | Ast.Tbool -> "Tbool"

and string_of_typed_binop = function
  | TBadd -> "+"
  | TBsub -> "-"
  | TBdiv -> "/"
  | TBmod -> "%"
  | TBmul -> "*"
  | TBeq -> "=="
  | TBneq -> "!="
  | TBlt -> "<" 
  | TBle -> "<="
  | TBgt -> ">" 
  | TBge -> ">="
  | TBand -> "&&"
  | TBor -> "||"
  | _ -> assert false

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
