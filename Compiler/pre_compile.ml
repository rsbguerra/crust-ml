open Past
open Tast
open Ast

exception Error of string

let error s  = raise (Error s)

(* table_ctx representa um scope, contexto *)
type tbl_variables_ctx = (string, int * int) Hashtbl.t
type tbl_functions_ctx = (string, int) Hashtbl.t
type tbl_structs_ctx = (string, ((string * Ast.crust_types * int) list * int)) Hashtbl.t

type tbl_ctx = tbl_variables_ctx * tbl_functions_ctx * tbl_structs_ctx

let make_ctx() =
  let v = (Hashtbl.create 16 : tbl_variables_ctx) in
  let f = (Hashtbl.create 16 : tbl_functions_ctx) in
  let s = (Hashtbl.create 16 : tbl_structs_ctx) in
  (v, f, s)

let var_ctx    = fun (c, _, _) -> c
let fun_ctx    = fun (_, c, _) -> c
let struct_ctx = fun (_, _, c) -> c

let var_ctx_hd l = var_ctx (List.hd l)
let fun_ctx_hd l = fun_ctx (List.hd l)
let struct_ctx_hd l = struct_ctx (List.hd l)

let rec pcompile_expr ctxs next = function
  | TEint (n, _) ->
    PEint n, next
  | TEbool (b, _) -> 
    PEbool b, next
  | TEident (id, t) -> 
    (* 1 - Posição da variável *)
    let pos, size = Hashtbl.find (find_var_id id ctxs) id in
    (* 2 - Obter lista de posições de variável *)
    let pos_list = get_ident_type_elements ctxs pos size t in
    PEident(id, pos_list), next
  | TEunop (op, e, t) -> 
    let pe, next = pcompile_expr ctxs next e in
    PEunop(op, pe), next
  
  | TEbinop (op, e1, e2, t) -> 
    let e1, fp1 = pcompile_expr ctxs next e1 in
    let e2, fp2 = pcompile_expr ctxs next e2 in
    let fp = max fp1 fp2 in
    PEbinop(op, e1, e2), fp
  
  | TEstruct_access(e, id, tid, tel) ->
    let struct_id = string_of_tstruct tid in
    (* 1. Buscar posição de id na pilha *)
    let id_pos,_ = Hashtbl.find (find_var_id id ctxs) id in
    (* 2. Buscar a posição do el relativo ao primeiro elemento*)
    let struct_els = fst(Hashtbl.find (find_struct_id struct_id ctxs) struct_id) in
    let el_pos = find_struct_element el struct_els in

    (* if id_pos > 0 then is an arg*)
    let final_pos = if id_pos > 0 then (id_pos+abs(el_pos)) else (id_pos+el_pos) in

    PEstrc_access(id, el, final_pos), next
  
  | TElen(id,_) ->
    (* 1 - get id size *)
    let _, size = Hashtbl.find (find_var_id id ctxs) id in
    PElen(size), next
  | TEvec_access (e1, e2, t) ->
    (* 1 - Precompilar exprs e1 e obter primeira pos do vetor a*)
    let pe1, next = pcompile_expr ctxs next e in
    (* 2 - Precompilar e2 - pos a aceder *)
    let pe2, next = pcompile_expr ctxs next e in
    
    (* TODO:
    - PEvec_access(e1, e2, type_size, pos_inicial, n_elem)  
    - porque de passar tamanho de vetor para ve_access?
      -> ver compile.ml, linha 625 - var sz não é usada *)

    (* 3 - Ir buscar a posição: a[5] = pos(a) + (5 * size(a)) *)
    PEvec_access(pe1, pe2, (get_type_size ctxs te), id_pos, size), next

  | TEcall (id, args, t) ->
    let exprs, size, fpmax = 
      List.fold_left (fun (l, sz, fpmax) (el, te) ->
        let e, fp = pcompile_expr ctxs fpmax el in
        (e::l, sz+(get_type_size ctxs te), max fp fpmax)
      ) ([], 0, next) args
      in
    PEcall(id, exprs, size), fpmax 
  | TEvec_decl (els, t) ->
    (* 1. Precompilar lista de expressões *)
    let type_size = (get_type_size ctxs t) / (List.length els)  in
    
    let next, p_els = List.fold_right(fun e (next, l) -> 
      let p_e, next = pcompile_expr ctxs next e in
      (next+type_size), (p_e, (-next))::l 
    )els (next, []) in
    
    PEvec_decl(p_els, snd(List.hd p_els)), next
  | TEprint (s, t) -> 
    let ep, next = (pcompile_expr ctxs next e) in
    PSprint (ep, t), next
  | TEblock (b, t) -> 

let precompile = pcompile_global_stmt [make_ctx()]
