open Ast
open Past
open Printer

let rec string_of_precomp_expr_list acc = function
  | [] -> acc
  | hd :: tl ->
      string_of_precomp_expr_list (acc ^ string_of_precomp_expr hd ^ ", ") tl

and string_of_precomp_expr = function
  | PEcst (n, fp) ->
      "PEcst(" ^ string_of_crust_consts n ^ "," ^ string_of_int fp ^ ")"
  | PEident (id, fp) -> "PEident(" ^ id ^ "," ^ string_of_int fp ^ ")"
  | PEbinop (binop, e1, e2, fp) ->
      "PEbinop("
      ^ Printer.string_of_binop binop
      ^ ", " ^ string_of_precomp_expr e1 ^ ", " ^ string_of_precomp_expr e2 ^ ","
      ^ string_of_int fp ^ ")"
  | PEunop (unop, e1, fp) ->
      "PEunop("
      ^ Printer.string_of_unop unop
      ^ ", " ^ string_of_precomp_expr e1 ^ "," ^ string_of_int fp ^ ")"
  | PEcall (f, el, fp) ->
      "PEcall(" ^ f ^ ", "
      ^ string_of_precomp_expr_list "" el
      ^ "," ^ string_of_int fp ^ ")"
  | PEaccess _ -> assert false
  | PElen _ -> assert false
  | PEvec_access _ -> assert false
  | PEvec_decl _ -> assert false

and string_of_precomp_stmt = function
  | PSif (e, s1, elifs, _) ->
      "PSif(" ^ string_of_precomp_expr e ^ ", " ^ string_of_precomp_stmt s1 ^ ", "
      ^ string_of_elif elifs ^ ")"
  | PSwhile (e, bl, _) ->
      "PSwhile(" ^ string_of_precomp_expr e ^ "\n" ^ string_of_precomp_stmt bl ^ ")"
  | PSdeclare (id, t, e1, fp) ->
      "PSdeclare(" ^ id ^ ", " ^ Printer.string_of_crust_types t ^ ", "
      ^ string_of_precomp_expr e1 ^ ", " ^ string_of_int fp ^ ")"
  | PSassign (id, e1, fp) -> "PSassign(" ^ id ^ ", " ^ string_of_precomp_expr e1 ^ ", " ^ string_of_int fp ^ ")"
  | PSprintn (e, _) -> "PSprintln(" ^ string_of_precomp_expr e ^ ")"
  | PSprint (e, _) -> "PSprint(" ^ string_of_precomp_expr e ^ ")"
  | PSblock (bl, _) -> string_of_block_precomp_stmt "" bl
  | PScontinue _ -> "PScontinue"
  | PSbreak _ -> "PSbreak"
  | PSreturn (e, fp) -> "PSreturn(" ^ string_of_precomp_expr e ^ ", " ^ string_of_int fp ^ ")"
  | PSnothing _ -> "PSnothing"
  | PSexpr (e, fp) -> "PSexpr(" ^ string_of_precomp_expr e ^ ")"

and string_of_elif l =
  let out = ref "" in
  List.iter
    (fun (e, body) ->
      out :=
        !out ^ "Selif(" ^ string_of_precomp_expr e ^ ", "
        ^ string_of_precomp_stmt body ^ ")")
    l;
  !out

and string_of_block_precomp_stmt acc = function
  | [] -> acc ^ "\n"
  | s :: sl ->
      string_of_block_precomp_stmt (acc ^ string_of_precomp_stmt s ^ "\n") sl

and string_of_block_precomp_global_stmt acc = function
  | [] -> acc ^ "\n"
  | s :: sl ->
      string_of_block_precomp_global_stmt
        (acc ^ string_of_precomp_global_stmt s ^ "\n")
        sl

and string_of_precomp_global_stmt = function
  | PGSblock bl -> string_of_block_precomp_global_stmt "" bl
  | PGSfunction (f, args, return, body) ->
      "PGSfunction(" ^ f ^ ", (" ^ Printer.string_of_pairs "" args ^ "), "
      ^ Printer.string_of_crust_types return
      ^ ", \n    " ^ string_of_precomp_stmt body
  | PGSstruct (id, elements) ->
      "PSstruct(" ^ id ^ "(," ^ Printer.string_of_pairs "" elements ^ ")"

let print_precomp_ast s =
  Printf.printf "PRE COMPILED PAST:\n\n\n%s\n\n" (string_of_precomp_global_stmt s)
