open Past
open Tast
open Ast
(* table_ctx representa um scope, contexto *)
type tbl_variables_ctx = (string, int) Hashtbl.t
type tbl_functions_ctx = (string, int) Hashtbl.t
type tbl_structs_ctx = (string, Ast.pairs list) Hashtbl.t

type tbl_ctx = tbl_variables_ctx * tbl_functions_ctx * tbl_structs_ctx

let make_ctx() =
  let v = (Hashtbl.create 16 : tbl_variables_ctx) in
  let f = (Hashtbl.create 16 : tbl_functions_ctx) in
  let s = (Hashtbl.create 16 : tbl_structs_ctx) in
  (v, f, s)

let var_ctx    = fun (c, _, _) -> c
let fun_ctx    = fun (_, c, _) -> c
let struct_ctx = fun (_, _, c) -> c

let var_ctx_hd    = fun l -> var_ctx (List.hd l)
let fun_ctx_hd    = fun l -> fun_ctx (List.hd l)
let struct_ctx_hd = fun l -> struct_ctx (List.hd l)

let id_exists id =
  List.exists(fun (v,s,f) -> Hashtbl.mem v id || Hashtbl.mem f id || Hashtbl.mem s id ) 

let find_id id =
  List.find_map(fun (v,f,s) -> 
      match Hashtbl.find_opt v id with | Some x -> x | None ->
      match Hashtbl.find_opt f id with | Some x -> x | None ->
      match Hashtbl.find_opt s id with | Some x -> x | None -> None)

let rec pcompile_expr ctxs next = function
  | TEcst (c, t) -> 
      PEcst c, next
  | TEident (id, t) ->
      if id_exists id ctxs then PEident id, next
      else raise Not_found
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

and pcompile_stmt ctxs = function
  | TSif (e, s, elif, _) ->
      let pe, fp = pcompile_expr ctxs 0 e in
      let ps = pcompile_stmt ctxs s in
      let iflist = List.map (fun (if_expr, if_stmt) -> 
        let e, _ = pcompile_expr ctxs fp if_expr in
        let s = pcompile_stmt ((make_ctx())::ctxs) if_stmt in
        (e, s)) elif in 
      PSif(pe, ps, iflist)

  | TSwhile (e, s, _) -> 
      let pe, _ = pcompile_expr ctxs 0 e in
      let ps = pcompile_stmt ((make_ctx())::ctxs) s in
      PSwhile (pe, ps)

  | TSdeclare (id, t, e, _) -> 
      let ep, fp = (pcompile_expr ctxs 0 e) in
      Hashtbl.replace (var_ctx_hd ctxs) id fp;
      PSdeclare (id, t, ep)
  | TSassign (id, e, _) ->
      let ep, fp = (pcompile_expr ctxs 0 e) in
      Hashtbl.replace (var_ctx_hd ctxs) id fp;
      PSassign (id, ep)
  | TSprintn (e, _) -> 
      let ep, _ = (pcompile_expr ctxs 0 e) in
      PSprintn ep
  | TSprint (e, _) ->
       let ep, _ = (pcompile_expr ctxs 0 e) in
      PSprintn ep
  | TSblock (stmts, _) -> 
      PSblock (List.map (fun s -> pcompile_stmt ctxs s) stmts)
  | TSreturn (e, _) ->
      let ep, _ = (pcompile_expr ctxs 0 e) in
      PSreturn ep
  | TSexpr (e, _) -> 
      let ep, _ = (pcompile_expr ctxs 0 e) in
      PSexpr ep
  | TScontinue _ -> PScontinue
  | TSbreak _ -> PSbreak
  | TSnothing _ -> PSnothing

and pcompile_block_stmt ctxs = 
  List.map (fun s -> pcompile_stmt ctxs s)

and pcompile_global_stmt ctxs = function
  | TGSblock stmts -> 
      PGSblock (List.map (fun s -> pcompile_global_stmt ((make_ctx())::ctxs) s) stmts, 0)
  | TGSfunction (x, args, t, stmt) -> 
      let new_ctxs = make_ctx()::ctxs in
      let fpmax = List.fold_left(
        fun fp (arg, _) -> 
          Hashtbl.add (var_ctx_hd new_ctxs) x fp;
          fp+8) 8 args in 
      let s = pcompile_stmt new_ctxs stmt in
      Hashtbl.replace (fun_ctx_hd new_ctxs) x fpmax;
      PGSfunction(x, args, t, s, fpmax)
  | TGSstruct (ident, args) -> assert false

let precompile = pcompile_global_stmt [make_ctx()]
