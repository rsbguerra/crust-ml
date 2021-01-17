open Past
open Tast
open Ast

exception Error of string

let error s  = raise (Error s)

(* table_ctx representa um scope, contexto *)
type tbl_variables_ctx = (string, bool) Hashtbl.t
type tbl_functions_ctx = (string, unit) Hashtbl.t

type tbl_ctx = tbl_variables_ctx * tbl_functions_ctx

let make_ctx() =
  let v = (Hashtbl.create 16 : tbl_variables_ctx) in
  let f = (Hashtbl.create 16 : tbl_functions_ctx) in
  (v, f)

let rec find_var_id id = function
  | []     -> assert false
  | (ct,_)::tl -> if Hashtbl.mem ct id then ct else (find_var_id id tl) 

let rec find_fun_id id = function
  | []     -> assert false
  | (_,ct)::tl -> if Hashtbl.mem ct id then ct else (find_fun_id id tl) 

let rec ownership_expr ctxs = function
  | TEcst _ -> false
  | TEident (id, _) ->
    (* 1 - Verificar se id é o dono *)
    if(not (Hashtbl.find (find_var_id id ctxs) id)) then error ("Invalid use of the variable "^id^", it's not the current owner.");
    false

  | TEref(id, _) ->
    let ct = find_var_id id ctxs in

    (* 1 - Verificar se id é o dono *)
    if not (Hashtbl.find ct id) then error ("Invalid use of the variable "^id^", it's not the current owner.");
    
    Hashtbl.replace ct id false;
    true

  | TErefmut(id, _) ->
    let ct = find_var_id id ctxs in

    (* 1 - Verificar se id é o dono *)
    if not (Hashtbl.find ct id) then error ("Invalid use of the variable "^id^", it's not the current owner.");
    
    Hashtbl.replace ct id false;
    true

  | TEptr(id, _) ->
    let ct = find_var_id id ctxs in

    (* 1 - Verificar se id é o dono *)
    if not (Hashtbl.find ct id) then error ("Invalid use of the variable "^id^", it's not the current owner.");
    
    Hashtbl.replace ct id false;
    false

  | TEbinop (op, e1, e2, t) ->
    (* 1 - Verificar a expressão e1 e e2*)
    let s1 = ownership_expr ctxs e1 in
    let s2 = ownership_expr ctxs e2 in
    s1 || s2

  | TEunop (op, e, t) ->
    (* 1 - Verificar a expressão e *)
    ownership_expr ctxs e
  
  | TElen id ->
    (* 1 - Verificar se id é o dono *)
    if(not (Hashtbl.find (find_var_id id ctxs) id)) then error ("Invalid use of the variable "^id^", it's not the current owner.");
    false
  
  | TEcall (id, args, t) ->

    let argument_ctxs = List.fold_right(fun (v,f) l -> 
      let nv = Hashtbl.copy v in
      let nf = Hashtbl.copy f in
      (nv, nf)::l
    ) ctxs [] in


    (* 1 - Verificar se as expressão são donas *)
    List.iter(fun (e,_) -> ignore(ownership_expr argument_ctxs e)) args;
    false

  | TEstrc_decl (id, pairs, t) ->
    (* 1 - Verificar se id é dono *)
    List.iter(fun (_,e,_) -> ignore(ownership_expr ctxs e)) pairs;
    true

  | TEstrc_access (id, el, tid, tel) ->
    (* 1 - Verificar se id é o dono *)
    if(not (Hashtbl.find (find_var_id id ctxs) id)) then error ("Invalid use of the variable "^id^", it's not the current owner.");
    false

  | TEvec_decl (els, t) ->
    List.iter(fun e -> ignore(ownership_expr ctxs e)) els;
    false

  | TEvec_access(id, e, te, tid) ->
    (* 1 - Verificar se id é o dono *)
    if(not (Hashtbl.find (find_var_id id ctxs) id)) then error ("Invalid use of the variable "^id^", it's not the current owner.");
    ignore(ownership_expr ctxs e);
    false

and ownership_stmt ctxs = function
  | TSif (e, s, elif, _) ->
    (* 1 - Verificar a expressão *)
    ignore(ownership_expr ctxs e);
    
    (* 2 - Verificar corpo do if *)
    ownership_stmt ((make_ctx ())::ctxs) s;
  
    (* 3 - Verificar condição e corpo dos elif *)
    List.iter(fun (e, s) -> ignore(ownership_expr ctxs e); ownership_stmt ((make_ctx ())::ctxs) s) elif

  | TSwhile (e, s, _) ->
    (* 1 - Verificar a expressão *)
    ignore(ownership_expr ctxs e);

    (* 1 - Verificar corpo do while *)
    ownership_stmt ((make_ctx ())::ctxs) s

  | TSdeclare (id, t, e, _) ->
    (* 1 - Adicionar id aos contextos *)
    let v_ctx,_ = (List.hd ctxs) in 

    (* 2 - Verificar se a expressão é válida *)
    ignore(ownership_expr ctxs e);

    (* 3 - Tem que ser sempre dono no ato da declaração *)
    Hashtbl.add v_ctx id true

  | TSassign (id, e, _) ->
    (* 1 - Verificar se id é o dono *)
    let ct = find_var_id id ctxs in
    if(not (Hashtbl.find ct id)) then error ("Invalid use of the variable "^id^", it's not the current owner.");

    (* 2 - Verificar a expressão e se passou a ser dono *)
    if (ownership_expr ctxs e) then Hashtbl.replace ct id true
  
  | TSptr_assign (id, e, _) ->
    (* 1 - Verificar se id é o dono *)
    let ct = find_var_id id ctxs in
    if(not (Hashtbl.find ct id)) then error ("Invalid use of the variable "^id^", it's not the current owner.");

    (* 2 - Verificar a expressão e se passou a ser dono *)
    if (ownership_expr ctxs e) then Hashtbl.replace ct id true
    
  | TSprintn (e, t, _) ->
    (* 1 - Verificar o ownership na expressão *)
    ignore(ownership_expr ctxs e)

  | TSprint (e, t, _) ->
    (* 1 - Verificar o ownership na expressão *)
    ignore(ownership_expr ctxs e)

  | TSblock (bl, _) ->
    (* 1 - Tipar bloco *)
    let ctxs = ((make_ctx ())::ctxs) in
    let block_ctxs = List.fold_right(fun (v,f) l -> 
      let nv = Hashtbl.copy v in
      let nf = Hashtbl.copy f in
      (nv, nf)::l
    ) ((make_ctx ())::ctxs) [] in

    ownership_block_stmt block_ctxs bl

  | TSreturn (e, t) ->
    (* 1 - Verificar o ownership na expressão *)
    ignore(ownership_expr ctxs e)
  
  | TScontinue _ -> ()
  | TSbreak _    -> ()
  | TSnothing _  -> ()
  | TSexpr (e, _) ->
    (* 1 - Verificar o ownership na expressão *)
    ignore(ownership_expr ctxs e)

and ownership_block_stmt ctxs = function
  | [] -> ()
  | s :: sl -> (ownership_stmt ctxs s); (ownership_block_stmt ctxs sl)


and ownership_block_global_stmt ctxs = function
  | [] -> ()
  | s :: sl -> (ownership_global_stmt ctxs s); (ownership_block_global_stmt ((make_ctx ())::ctxs) sl)

and ownership_global_stmt ctxs = function
  | TGSblock bl -> ownership_block_global_stmt ((make_ctx ())::ctxs) bl
  | TGSfunction (id, args, r, body) ->
    (* 1 - Verificar se o id já foi definido *)
    let f = snd(List.hd ctxs) in
    Hashtbl.add f id ();
    
    (* 2 - Adicionar os argumentos *)
    let args_ctxs = ((make_ctx ())::ctxs) in
    let v,_ = List.hd args_ctxs in
    List.iter(fun (arg, _) -> Hashtbl.add v arg true) args;

    (* 3 - Verificar ownership no corpo*)
    ownership_stmt args_ctxs body
  | TGSstruct (id, args) -> ()

let verify_ownership = ownership_global_stmt [make_ctx()]
