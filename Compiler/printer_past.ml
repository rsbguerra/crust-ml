open Ast
open Past
open Printer


let rec string_of_precomp_expr = function
  | PEcst n, fp ->
      "PEcst(" ^ 
      string_of_crust_consts n ^ ", " ^ 
      string_of_int fp ^ ")"
  | PEident id, fp -> "PEident(" ^
       id ^ ", " ^ 
       string_of_int fp ^ ")"
  | PEbinop (binop, e1, e2), fp ->
      "PEbinop(" ^ 
      Printer.string_of_binop binop ^ ", " ^ 
      string_of_precomp_expr (e1, fp) ^ ", " ^ string_of_precomp_expr (e2, fp) ^ ", " ^ 
      string_of_int fp ^ ")"
  | PEunop (unop, e1), fp ->
      "PEunop(" ^ 
      Printer.string_of_unop unop ^  ", " ^ 
      string_of_precomp_expr (e1,fp) ^ ", " ^ string_of_int fp ^ ")"
  | PEcall (f, el), fp ->
      "PEcall(" ^ f ^ ", " ^ 
      string_of_precomp_expr_list el fp ^ ", (" ^ ") " ^
      string_of_int fp ^ ")"
  | _ -> assert false


and string_of_precomp_expr_list exprs fp = 
    List.map (fun e -> string_of_precomp_expr (e,fp)) exprs |> 
    List.fold_left (fun a b -> a ^ b) ""
    

and string_of_precomp_stmt = function
  | PSif (e, s1, elifs) ->
      "PSif(" ^ string_of_precomp_expr (e,0) ^ ", \n" ^ 
      string_of_precomp_stmt s1 ^ ", " ^ 
      string_of_elif elifs ^ ")"
  | PSwhile (e, bl) ->
      "PSwhile(" ^ 
      string_of_precomp_expr (e,0) ^ "\n" ^ 
      string_of_precomp_stmt bl ^ ")"
  | PSdeclare (id, t, e) ->
      "PSdeclare(" ^ id ^ ", " ^
       Printer.string_of_crust_types t ^ ", " ^ 
       string_of_precomp_expr (e,0)^ ", " ^ ")"
  | PSassign (id, e) -> 
      "PSassign(" ^ id ^ ", " ^ 
      string_of_precomp_expr (e,0) ^ ", " ^ ")"
  | PSprintn e -> 
      "PSprintln(" ^ 
      string_of_precomp_expr (e,0)^ ")"
  | PSprint e -> 
      "PSprint(" ^ 
      string_of_precomp_expr (e,0) ^ ")"
  | PSblock bl -> string_of_block_precomp_stmt bl
  | PScontinue-> "PScontinue"
  | PSbreak-> "PSbreak"
  | PSreturn e -> 
      "PSreturn(" ^ 
      string_of_precomp_expr (e,0)^ ", " ^ ")"
  | PSnothing -> "PSnothing"
  | PSexpr e -> 
      "PSexpr(" ^ 
      string_of_precomp_expr (e,0)^ ")"


and string_of_elif l =
    List.map (fun (e, body) -> (string_of_precomp_expr (e,0), string_of_precomp_stmt body)) l |> 
    List.fold_left (fun str (e, body) -> "PSelif(" ^ e ^ ", " ^ body ^ ")" ^ str) ""


and string_of_block_precomp_stmt bl =
    List.fold_left (fun str s -> string_of_precomp_stmt s ^ str) "" bl


and string_of_block_precomp_global_stmt gbl = 
  List.fold_left (fun str s -> string_of_precomp_global_stmt s ^ str) "" gbl


and string_of_precomp_global_stmt = function
  | PGSblock bl -> string_of_block_precomp_global_stmt bl
  | PGSfunction (f, args, return, body) ->
      "PGSfunction(" ^ f ^ ", (" ^ Printer.string_of_pairs "" args ^ "), "
      ^ Printer.string_of_crust_types return
      ^ ", \n    " ^ string_of_precomp_stmt body
  | PGSstruct (id, elements) ->
      "PSstruct(" ^ id ^ "(," ^ Printer.string_of_pairs "" elements ^ ")"


let print_precomp_past s =
  Printf.printf "PRE COMPILED PAST:\n\n\n%s\n\n" (string_of_precomp_global_stmt s)
