open Past
open Tast
open Ast

exception Error of string

let error s  = raise (Error s)

(* table_ctx representa um scope, contexto *)
type tbl_variables_ctx = (string, int) Hashtbl.t
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

let id_exists id =
  List.exists(fun (v,f,s) -> Hashtbl.mem v id || Hashtbl.mem f id || Hashtbl.mem s id)

let find_id id =
  List.find_map(fun (v,f,s) -> 
      match Hashtbl.find_opt v id with | Some x -> x | None ->
      match Hashtbl.find_opt f id with | Some x -> x | None ->
      match Hashtbl.find_opt s id with | Some x -> x | None -> None)

let rec find_var_id id = function
  | []     -> None
  | (ct,_,_)::tl -> if Hashtbl.mem ct id then (Some ct) else (find_var_id id tl) 

let rec find_fun_id id = function
  | []     -> None
  | (_,ct,_)::tl -> if Hashtbl.mem ct id then (Some ct) else (find_fun_id id tl) 


let rec find_struct_id id = function
  | []     -> None
  | (_,_,ct)::tl -> 
    if Hashtbl.mem ct id then (Some ct) else (find_struct_id id tl) 

let get_type_start ctxs = function
  | Ti32 | Tbool | Tunit -> 0
  | Tstruct s -> begin
    match find_struct_id s ctxs with
    | None -> error s
    | Some ct -> -snd(Hashtbl.find ct s) 
    end


let get_type_size ctxs = function
  | Ti32 | Tbool -> 8
  | Tunit -> 0
  | Tstruct s -> begin
    match find_struct_id s ctxs with
    | None -> error s
    | Some ct -> snd(Hashtbl.find ct s) 
    end

let rec pcompile_expr ctxs next = function
  | TEcst (c, t) -> 
      PEcst c, next
  | TEident (id, t) -> begin
    match find_var_id id ctxs with
    | None -> assert false
    | Some ct -> let fp = Hashtbl.find ct id in
      PEident(id, fp), next
    end
  | TEbinop (op, e1, e2, t) -> 
      let e1, fp1 = pcompile_expr ctxs next e1 in
      let e2, fp2 = pcompile_expr ctxs next e2 in
      let fp = max fp1 fp2 in
      PEbinop(op, e1, e2), fp
  | TEunop (op, e, t) -> 
      let pe, _ = pcompile_expr ctxs next e in
      PEunop(op, pe), next
  | TEcall (id, args, t) ->
    let exprs, fpmax = 
      List.fold_left (
        fun (l, fpmax) el -> 
          let e, fp = pcompile_expr ctxs fpmax el in
          (e::l, max fp fpmax))
        ([], next) args in
    PEcall(id, exprs), fpmax
  (*  
  | PEstrc_decl of ident * (ident * int) list
  *)
  | TEstrc_decl (id, pairs, t) -> 
    (* 1. Precompilação de pares *)
    let next, p_els = List.fold_left_map(
      fun next (el, t_el, t_e) -> 
        (* 3. Precompilar expressão t_el *)
        let p_el, next = pcompile_expr ctxs next t_el in
        (next+(get_type_size ctxs t_e)), (el, p_el, (-next))
    ) next pairs in



    PEstrc_decl(id, p_els), next
  | _ -> assert false

and pcompile_stmt ctxs next = function
  | TSif (e, s, elif, _) ->
      let pe, next  = pcompile_expr ctxs next e in
      let ps, next  = pcompile_stmt ((make_ctx())::ctxs) next s in
      let next, if_list = List.fold_left_map(
        fun next (if_expr, if_stmt) ->
          (* 1. Pre-compilar condição *)
          let p_elif, next = pcompile_expr ctxs next if_expr in
          (* 2. Pre-compilar stmts do corpo *)
          let p_body_elif, next = pcompile_stmt ((make_ctx())::ctxs) next if_stmt in
          (* 3. Devolver corpo tipado e último next *)
          (next, (p_elif, p_body_elif))
      ) next elif in
      PSif(pe, ps, if_list), next

  | TSwhile (e, s, _) -> 
      let pe, next = pcompile_expr ctxs next e in
      let ps, next = pcompile_stmt ((make_ctx())::ctxs) next s in
      PSwhile (pe, ps), next

  | TSdeclare (id, t, e, _) -> 
      let ep, next = (pcompile_expr ctxs next e) in
      Hashtbl.replace (var_ctx_hd ctxs) id (-(next + (get_type_start ctxs t)));
      let new_fp = (get_type_start ctxs t) + (get_type_size ctxs t) in
      PSdeclare (id, t, ep, -(next + (get_type_start ctxs t))), (new_fp+next)

  | TSassign (id, e, _) ->
      let ep, next = (pcompile_expr ctxs next e) in
      let pos = match find_var_id id ctxs with
        | None -> assert false
        | Some ct -> Hashtbl.find ct id in

      PSassign (id, ep, pos), next
  | TSprintn (e, t, _) -> 
      let ep, next = (pcompile_expr ctxs next e) in
      PSprintn (ep, t), next
  | TSprint (e, t, _) ->
       let ep, next = (pcompile_expr ctxs next e) in
      PSprint (ep, t), next
  | TSblock (stmts, _) -> 
    let pblock, next = pcompile_block_stmt ctxs next stmts in
    PSblock pblock, next
  | TSreturn (e, _) ->
    let ep, next = (pcompile_expr ctxs next e) in
    PSreturn ep, next
  | TSexpr (e, _) -> 
      let ep, next = (pcompile_expr ctxs next e) in
      PSexpr ep, next
  | TScontinue _ -> PScontinue, next
  | TSbreak _ -> PSbreak, next
  | TSnothing _ -> PSnothing, next

and pcompile_block_stmt ctxs next block_stmt = 
  let next, p_body = List.fold_left_map (fun next s -> 
    let p_body, next = pcompile_stmt ctxs next s in
    next, p_body) next block_stmt in
    p_body, next

and pcompile_block_global_stmt ctxs acc = function
  | [] -> acc
  | s :: sl -> (pcompile_block_global_stmt ctxs (acc@[pcompile_global_stmt ctxs s]) sl)


and pcompile_global_stmt ctxs = function
  | TGSblock stmts -> 
    
    PGSblock (pcompile_block_global_stmt ctxs [] stmts)
    (* PGSblock (List.map (fun s -> pcompile_global_stmt ((make_ctx())::ctxs) s) stmts) *)
  | TGSfunction (x, args, t, stmt) ->
      let new_ctxs = (make_ctx())::ctxs in

      (* 1- Precompilar argumentos *)
      let p_next, p_args = List.fold_left_map(
        fun next (arg, t_arg) -> 
          Hashtbl.add (var_ctx_hd new_ctxs) arg next;
          (next+8), (arg, t_arg, next)
      ) 16 args in

      (* 2 - Pre compilar corpo *)
      let p_stmt, next = pcompile_stmt new_ctxs 8 stmt in
      Hashtbl.replace (fun_ctx_hd new_ctxs) x next;
      
      PGSfunction(x, p_args, t, p_stmt, next)
  | TGSstruct (id, args) -> 
    (* 1 - Adiocionar identificador da struct ao contexto *)
    (* 2 - Calcular espaço na memória*)
    let next, pcompiled_fields = 
      List.fold_left_map(fun next (id, t) -> 
        (next+8), (id, t, (-next))) 0 args in
    Hashtbl.add (struct_ctx_hd ctxs) id (pcompiled_fields, next);
    PGSstruct(id, pcompiled_fields, next)


let precompile = pcompile_global_stmt [make_ctx()]
