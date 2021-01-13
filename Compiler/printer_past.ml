open Ast
open Past
open Printer


let rec string_of_precomp_expr = function
  | PEcst n ->
      "PEcst(" ^ 
      string_of_crust_consts n ^ ")"
  | PEident (id, fp) -> "PEident(" ^
       id ^ ", " ^ 
       string_of_int fp ^ ")"
  | PEbinop (binop, e1, e2) ->
      "PEbinop(" ^ 
      Printer.string_of_binop binop ^ ", " ^ 
      string_of_precomp_expr e1 ^ ", " ^ string_of_precomp_expr e2  ^ ")"
  | PEunop (unop, e) ->
      "PEunop(" ^ 
      Printer.string_of_unop unop ^  ", " ^ 
      string_of_precomp_expr e ^ ")"
  | PEcall (f, el) ->
      "PEcall(" ^ f ^ ", " ^ 
      string_of_precomp_expr_list el ^ ", (" ^ ") " ^")"

and string_of_precomp_expr_list exprs = 
    List.map (fun e -> string_of_precomp_expr e) exprs |> 
    List.fold_left (fun a b -> a ^ b) ""
    

and string_of_precomp_stmt = function
  | PSif (e, s1, elifs) ->
      "PSif(" ^ string_of_precomp_expr e ^ ", \n" ^ 
      string_of_precomp_stmt s1 ^ ", " ^ 
      string_of_elif elifs ^ ")"
  | PSwhile (e, bl) ->
      "PSwhile(" ^ 
      string_of_precomp_expr e ^ "\n" ^ 
      string_of_precomp_stmt bl ^ ")"
  | PSdeclare (id, t, e, fp) ->
      "PSdeclare(" ^ id ^ ", " ^
       Printer.string_of_crust_types t ^ ", " ^ 
       string_of_precomp_expr e ^ ", " ^
       (string_of_int fp) ^ ")"
  | PSassign (id, e, pos) -> 
      "PSassign(" ^ id ^ ", " ^ 
      string_of_precomp_expr e ^  ", " ^
      (string_of_int pos) ^ ")"
  | PSprintn (e, t) -> 
      "PSprintln(" ^ 
      string_of_precomp_expr e ^ ","^
      (Printer.string_of_crust_types t)^")"
  | PSprint (e, t) -> 
      "PSprint(" ^ 
      string_of_precomp_expr e  ^ ","^
      (Printer.string_of_crust_types t)^")"
  | PSblock bl -> string_of_block_precomp_stmt bl
  | PScontinue-> "PScontinue"
  | PSbreak-> "PSbreak"
  | PSreturn e -> 
      "PSreturn(" ^ 
      string_of_precomp_expr e ^ ")"
  | PSnothing -> "PSnothing"
  | PSexpr e -> 
      "PSexpr(" ^ 
      string_of_precomp_expr e ^ ")"


and string_of_elif l =
    List.map (fun (e, body) -> (string_of_precomp_expr e , string_of_precomp_stmt body)) l |> 
    List.fold_left (fun str (e, body) -> "PSelif(" ^ e ^ ", " ^ body ^ ")" ^ str) ""


and string_of_block_precomp_stmt bl =
    List.fold_left (fun str s -> str ^ (string_of_precomp_stmt s) ^ "\n") "" bl


and string_of_block_precomp_global_stmt gbl = 
  List.fold_left (fun str s -> str ^ (string_of_precomp_global_stmt s) ^ "\n") "" gbl

and string_of_pairs acc = function
  | []           -> acc
  | (id, t, fp):: tl -> string_of_pairs (acc^id^":"^(string_of_crust_types t)^":"^
    (string_of_int fp) ^ ", ") tl

and string_of_precomp_global_stmt = function
  | PGSblock bl -> string_of_block_precomp_global_stmt bl
  | PGSfunction (f, args, return, body) ->
      "PGSfunction(" ^ f ^ ", (" ^ string_of_pairs "" args ^ "), "
      ^ Printer.string_of_crust_types return
      ^ ", \n    " ^ string_of_precomp_stmt body
  | PGSstruct (id, elements) ->
      "PSstruct(" ^ id ^ "(," ^ string_of_pairs "" elements ^ ")"


let print_precomp_past s =
  Printf.printf "PRE COMPILED PAST:\n\n\n%s\n\n" (string_of_precomp_global_stmt s)
