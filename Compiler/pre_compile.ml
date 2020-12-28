open Past
open Tast
open Ast
(* table_ctx representa um scope, contexto *)
type tbl_variables_ctx = (string, int) Hashtbl.t
type tbl_functions_ctx = (string, int) Hashtbl.t
type tbl_structs_ctx = (string, Ast.pairs list) Hashtbl.t

let make_ctx () =
  let v = (Hashtbl.create 16 : tbl_variables_ctx) in
  let f = (Hashtbl.create 16 : tbl_functions_ctx) in
  let s = (Hashtbl.create 16 : tbl_structs_ctx) in
  (v, f, s)

let var_ctx = fun (c, _, _) -> c
let fun_ctx = fun (_, c, _) -> c
let struct_ctx = fun (_, _, c) -> c

let rec pcompile_expr ctxs next = function
  | TEcst (c, t) -> 
      PEcst c, next
  | TEident (id, t) -> 
      if Hashtbl.mem (var_ctx ctxs) id then PEident id, next
      else raise Not_found
  | TEbinop (op, e1, e2, t) -> 
      let e1, fp1 = pcompile_expr ctxs next e1 in
      let e2, fp2 = pcompile_expr ctxs next e2 in
      let fp = max fp1 fp2 in
      PEbinop(op, e1, e2), fp
  | TEunop (op, e, t) -> 
      let pe, _ = pcompile_expr ctxs next e in
      PEunop(op, pe), next
  | TEcall (id, body, t) -> assert false

and pcompile_stmt ctxs = function
  | TSif (e, s, elif) ->
      (* todo: verificar qual se fp é max fp1 fp2 ou só fp2 *)
      let pe, fp = pcompile_expr ctxs 0 e in
      let ps = pcompile_stmt ctxs s in
      let iflist = List.map (fun (if_expr, if_stmt) -> 
        let e, _ = pcompile_expr ctxs fp if_expr in
        let s = pcompile_stmt ctxs if_stmt in
        (e, s)) elif in 
      PSif(pe, ps, iflist)

  | TSwhile (e, s) -> 
      let pe, _ = pcompile_expr ctxs 0 e in
      let ps = pcompile_stmt ctxs s in
      PSwhile (pe, ps)

  | TSdeclare (id, t, e) -> 
      Hashtbl.replace (var_ctx ctxs) id (pcompile_expr ctxs 0 e);
      PSdeclare (id, t, e)
  | TSassign (id, e) ->
      Hashtbl.replace (var_ctx ctxs) id (pcompile_expr ctxs 0 e);
      PSassign (id, e)
  | TSprintn e ->  
      PSprintn (pcompile_expr ctxs 0 e)
  | TSprint e -> 
      PSprint (pcompile_expr ctxs 0 e)
  | TSblock stmts -> 
      PSblock (List.map (fun s -> pcompile_stmt ctxs s) stmts)
  | TSreturn (e, _) -> 
      PSreturn (pcompile_expr ctxs 0)
  | TSexpr e -> 
      PSExpr (pcompile_expr ctxs 0)
  | TScontinue -> PScontinue
  | TSbreak -> PSbreak
  | TSnothing -> PSnothing

and pcompile_global_stmt ctxs = function
  | TGSblock gl_stmts -> 
    PSblock (List.map (fun s -> pcompile_stmt ctxs s) gl_stmts)
  | TGSfunction (i, args, t, stmt) -> assert false
  | TGSstruct (ident, args) -> assert false

let precompile s = pcompile_global_stmt (make_ctx ()) s
