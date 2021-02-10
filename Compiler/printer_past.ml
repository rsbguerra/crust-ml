open Ast
open Past
open Printer
open Int32

let rec string_of_prust_types = function 
  | PTunit -> "PTunit"
  | PTi32  -> "PTi32"
  | PTbool -> "PTbool"
  | PTempty -> "Tempty"
  | PTstruct s -> "PTstruct( " ^ s ^ " )"
  | PTvec t -> "PTvec( " ^ (string_of_prust_types t) ^ " )"
  | PTref t -> "PTref( " ^ (string_of_prust_types t) ^ " )"
  | PTrefmut t -> "PTrefmut( " ^ (string_of_prust_types t) ^ " )"

and string_of_precomp_expr = function
  | PEint n ->
    "PEint(" ^ 
    (Int32.to_string n) ^ ")"

  | PEbool b ->
    "PEbool(" ^ 
    (string_of_bool b) ^ ")"

  | PEident (id, fp) -> "PEident(" ^
    id ^ ", " ^
    (string_of_int fp) ^ ")"

  | PEunop (unop, e) ->
    "PEunop(" ^ 
    Printer.string_of_unop unop ^ 
    (string_of_precomp_expr e)^")"

  | PEbinop (binop, e1, e2, fp) ->
    "PEbinop(" ^ 
    Printer.string_of_binop binop ^ ", " ^ 
    (string_of_precomp_expr e1)^ ", " ^ 
    (string_of_precomp_expr e2) ^ ", " ^ 
    (string_of_int fp) ^ ")"

  | PEstruct_access(e, id, el_pos) ->
    "PEstrc_access(" ^ string_of_precomp_expr e ^ ", " ^ id ^ ", " ^
    string_of_int el_pos ^ ")"

  | PElen e -> "PElen( " ^ (string_of_precomp_expr e) ^ " )"

  | PEvec_access(id, el,el_size, id_pos) ->
    "PEvec_access(" ^ (string_of_precomp_expr id) ^ "[" ^
    (string_of_precomp_expr el) ^ "], " ^ (string_of_int el_size) ^ ", " ^ 
    (string_of_int id_pos) ^ ")"

  | PEcall (f, el, size) ->
    "PEcall(" ^ f ^ ", " ^ 
    (List.map (fun e -> string_of_precomp_expr e) el |> 
    List.fold_left (fun a b -> a ^ b) "") ^ ", " ^ 
    string_of_int size ^ ")"

  | PEvec_decl(el, pos) -> 
    "PEvec_decl(" ^ 
    (List.map (fun (e, pos) -> "( " ^ (string_of_precomp_expr e) ^ ", " ^ (string_of_int pos) ^ " )") el |> 
    List.fold_left (fun a b -> a ^ ", " ^ b) "") ^ ", " ^
    string_of_int pos ^ ")"

  | PEprint (s, fp) ->
    "PSprint(" ^ s ^ ", " ^ (string_of_int fp) ^ ")"

  | PEblock (b, fp) -> 
    "PEblock(" ^ 
    (string_of_precomp_block b) ^ ", " ^
    (string_of_int fp) ^ ")"

and string_of_precomp_stmt = function
  | PSnothing -> "PSnothing"

  | PSexpr e -> "PSexpr(" ^ string_of_precomp_expr e ^ ")"

  | PSdeclare (mut, id, t, e, pos_list) ->
    "PSdeclare(" ^ (if mut then "mut " else "") ^ id ^ ", " ^
    string_of_prust_types t ^ ", " ^ 
    string_of_precomp_expr e ^ ", [" ^
    List.fold_left (fun a b -> a ^ (string_of_int b) ^ ", ") "" pos_list ^ "])"

  | PSdeclare_struct(mut, id, tid, el, fp) -> 
    "TSdeclare_struct(" ^ (if mut then "mut " else "") ^ id ^ ", " ^ tid ^ ", " ^
    (List.fold_left (fun acc (id, e, fp) ->
      (acc^id^":"^(string_of_precomp_expr e) ^ ", " ^ (string_of_int fp))) "" el) ^ ", " ^
    (string_of_int fp) ^ ")"

  | PSwhile (e, bl) ->
    "PSwhile(" ^ 
    string_of_precomp_expr e ^ "\n" ^ 
    string_of_precomp_block bl ^ ")"

  | PSreturn (Some e, pos_list) -> "PSreturn(" ^ string_of_precomp_expr e ^ ", [" ^
    List.fold_left (fun a b -> a ^", " ^(string_of_int b)) "" pos_list ^ "])"

  | PSreturn (None, pos_list) -> "PSreturn([" ^
  List.fold_left (fun a b -> a ^", " ^(string_of_int b)) "" pos_list ^ "])"

  | PSif (e, b1, b2) ->
    "PSif(" ^ (string_of_precomp_expr e) ^ ", \n" ^ 
    (string_of_precomp_block b1) ^ ", " ^ 
    (string_of_precomp_block b2) ^ ")"

and string_of_pairs pairs = 
  List.fold_left (fun acc (id, t, fp) ->
    acc^id^":"^(string_of_prust_types t)^":"^
    (string_of_int fp) ^ ", ") "" pairs

and string_of_args args = 
List.fold_left (fun acc (mut, id, t, fp) ->
  acc ^ (if mut then "mut " else "") ^
  id ^ ":" ^ (string_of_prust_types t)^":"^
  (string_of_int fp) ^ ", ") "" args

and string_of_precomp_block (stmts, exp) =
  let stmts = List.fold_left (fun acc s -> acc ^ "," ^ (string_of_precomp_stmt s) ^ "\n") "" stmts in
  match exp with
    | Some e -> stmts ^ ", " ^ (string_of_precomp_expr e)
    | None -> stmts


and string_of_typed_decl = function
  | PDstruct (id, p) -> 
    "TDstruct("^id^",("^(string_of_pairs p)^")"
  | PDfun (f, args, body, fp) -> 
    "TDfun("^f^", ("^(string_of_args args)^"), \n"^
    (string_of_precomp_block body) ^ ", " ^
    (string_of_int fp) ^ ")"

and string_of_program p = 
  List.fold_left (fun acc d -> acc ^"\n\n"^ (string_of_typed_decl d)) "" p

let print_file s =
  Printf.printf "PRE COMPILED PAST:\n\n\n%s\n\n" (string_of_program s)
