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
  | PEint n, fps ->
    "PEint(" ^ 
    (Int32.to_string n) ^ ", " ^
    (string_of_int fps )^ ")"
  | PEbool b, fps ->
    "PEbool(" ^ 
    (string_of_bool b) ^ ", " ^
    (string_of_int fps) ^ ")"
  | PEident (id, fps) -> "PEident(" ^
    id ^ ", [" ^ 
    (List.fold_left (fun a e -> a ^ ", " ^ (string_of_int e)) "" fps) ^ "] )"
  | PEunop (unop, e) ->
    "PEunop(" ^ 
    Printer.string_of_unop unop ^ 
    (string_of_precomp_expr e)^")"
  | PEbinop (binop, e1, e2) ->
    "PEbinop(" ^ 
    Printer.string_of_binop binop ^ ", " ^ 
    (string_of_precomp_expr e1)^ ", " ^ (string_of_precomp_expr e2) ^ ")"
    (string_of_precomp_expr e) ^ ")"
  | PEstruct_access(e, id, el_pos) ->
    "PEstrc_access(" ^ string_of_precomp_expr e ^ ", " ^ id ^ ", " ^
    string_of_int el_pos ^ ")"
  | PElen id -> "PElen( " ^ (string_of_int id) ^ " )"
  | PEvec_access(id, el,el_size, id_pos, sz) ->
    "PEvec_access(" ^ (string_of_precomp_expr id) ^ "["
    (string_of_precomp_expr el) ^ "], " ^ (string_of_int el_size) ^ ", " ^ 
    (string_of_int id_pos) ^  ", " ^ (string_of_int sz) ^")"
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
    (string_of_precomp_block b) ^ ", "
    (string_of_int fp) ^ ")"

and string_of_precomp_block (stmts, exp, fp) =
  let stmts = List.fold_left (fun acc s -> acc ^ "," ^ (string_of_precomp_stmt s) ^ "\n") "" b in
  match exp with
    | Some e -> stmts ^ (string_of_precomp_expr e) ^ ", " ^ (string_of_int fp)
    | None -> stmts ^ ", " ^ (string_of_int fp)

and string_of_precomp_stmt = function
  | PSnothing -> "PSnothing"
  | PSexpr e -> "PSexpr(" ^ string_of_precomp_expr e ^ ")"
  | PSdeclare (id, t, e, pos_list) ->
    "PSdeclare(" ^ id ^ ", " ^
    Printer.string_of_crust_types t ^ ", " ^ 
    string_of_precomp_expr e ^ ", [" ^
    List.fold_left (fun a b -> a ^", " ^(string_of_int b)) "" pos_list ^ "])"
  | PSdeclare_struct(mut, id, tid, el, fp) -> 
    "TSdeclare_struct(" ^ (if mut then "mut " else "") ^ id^", "^tid^", "^
    List.fold_left (fun acc (id, e) ->
      (acc^id^":"^(string_of_precomp_expr e) ^", ")) "" el ^ ", " ^
    (string_of_int fp) ^ ")"
  | PSwhile (e, bl) ->
    "PSwhile(" ^ 
    string_of_precomp_expr e ^ "\n" ^ 
    string_of_precomp_stmt bl ^ ")"
  | PSreturn (e, pos_list) -> "PSreturn(" ^ string_of_precomp_expr e ^ ", [" ^
    List.fold_left (fun a b -> a ^", " ^(string_of_int b)) "" pos_list ^ "])"
  | PSif (e, b1, b2) ->
    "PSif(" ^ (string_of_precomp_expr e) ^ ", \n" ^ 
    (string_of_precomp_block b1) ^ ", " ^ 
    (string_of_precomp_block b2) ^ ")"

and string_of_pairs acc = function
  | []           -> acc
  | (id, t, fp):: tl -> string_of_pairs (acc^id^":"^(string_of_crust_types t)^":"^
    (string_of_int fp) ^ ", ") tl

and string_of_struct_pairs acc = function
  | []           -> acc
  | (id, e, fp):: tl -> string_of_struct_pairs (acc^id^":"^(string_of_precomp_expr e)^":"^
    (string_of_int fp) ^ ", ") tl


and string_of_precomp_global_stmt = function
  | PGSblock bl -> string_of_block_precomp_global_stmt bl
  | PGSfunction (f, args, return, body, fp) ->
    "PGSfunction(" ^ f ^ ", (" ^ string_of_pairs "" args ^ "), " ^
    Printer.string_of_crust_types return ^ ", \n    " ^ 
    string_of_precomp_stmt body ^
    string_of_int fp ^ ")" 
  | PGSstruct (id, elements, size) ->
    "PSstruct(" ^ id ^ "(," ^ string_of_pairs "" elements ^ ", " ^
    string_of_int size ^ ")"

and string_of_typed_decl = function
  | TDstruct (id, p, t) -> 
    "TDstruct("^id^",("^(string_of_pairs p)^")" ^ (string_of_prust_types t)^ ")"
  | TDfun (f, args, return, body, t) -> 
    "TDfun("^f^", ("^(string_of_args args)^"), "^
    (string_of_prust_types return)^", "^
    (string_of_typed_block body) ^
    (string_of_prust_types t)

and string_of_program p = 
  List.fold_left (fun acc d -> acc ^"\n\n"^ (string_of_typed_decl d)) "" p

let print_precomp_past s =
  Printf.printf "PRE COMPILED PAST:\n\n\n%s\n\n" (string_of_program s)
