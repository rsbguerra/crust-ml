open Past
open Tast
open Ast
(* table_ctx representa um scope, contexto *)
type tbl_variables_ctx = (string, int) Hashtbl.t
type tbl_functions_ctx = (string, int) Hashtbl.t
type tbl_structs_ctx = (string, Ast.pairs list) Hashtbl.t

exception Not_found of string

let make_ctx () =
  let v = (Hashtbl.create 16 : tbl_variables_ctx) in
  let f = (Hashtbl.create 16 : tbl_functions_ctx) in
  let s = (Hashtbl.create 16 : tbl_structs_ctx) in
  (v, f, s)

and pcompile_expr (ctxs_v, ctxs_f, ctxs_s) next = function
  | TEcst (c, t) -> 
      PEconst c, next
  | TEident (id, t) -> 
      if Hashtbl.mem id ctxs_v then PEident id, next
      else raise "Pre compiler error: id not found"
  | TEbinop (op, e1, e2, t) -> 
      let e1, fp1 = (ctxs_v, ctxs_f, ctxs_s) next e1 in
      let e2, fp2 = (ctxs_v, ctxs_f, ctxs_s) next e2 in
      let fp = max fp1 fp2 in
      PEbinop(op, e1, e2), fp
  | TEunop (op, e, t) -> 
      PEunop(op, e), next
  | TEcall (id, body, t) -> assert false

and pcompile_stmt ctxs = function
  | TSif (e, s, elif) ->
      (* todo: verificar qual se fp é max fp1 fp2 ou só fp2 *)
      let e1, fp1 = pcompile_expr ctxs 0 e in
      let s1, fp2 = pcompile_stmt ctxs s in
      let fpmax = max fp1 fp2 in
      let iflist = 
        List.map (fun (if_expr, if_stmt) -> 
          let e, _ = pcompile_expr ctxs fpmax if_expr in
          let s, _ = pcompile_stmt ctxs if_stmt in
          (e, s)) elif in 
      PSif(e1, s2, iflist) 
  | TSwhile (e, s) -> assert false
  | TSdeclare (i, d, e) -> assert false
  | TSassign (id, e) -> assert false
  | TSprintn e -> assert false
  | TSprint e -> assert false
  | TSblock stmts -> assert false
  | TSreturn (e, t) -> assert false
  | TSexpr e -> assert false
  | TScontinue -> assert false
  | TSbreak -> assert false
  | TSnothing -> assert false

and pcompile_elif ctxs = function 
[] -> assert false | h :: t -> assert false

and pcompile_global_stmt ctxs = function
  | TGSblock l -> assert false
  | TGSfunction (i, args, t, stmt) -> assert false
  | TGSstruct (ident, args) -> assert false

and pcompile_block_stmt ctxs acc = function
  | [] -> assert false
  | h :: t -> assert false

and pcompile_block_global_stmt ctxs acc = function
  | [] -> assert false
  | h :: t -> assert false

let precompile s = pcompile_global_stmt (make_ctx ()) s
