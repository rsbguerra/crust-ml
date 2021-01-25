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


let remove_ownership e ctxs = match e with
  | TEident (id, _) -> Hashtbl.replace (find_var_id id ctxs) id false
  | _ -> assert false

let give_ownership e ctxs = match e with
  | TEident (id, _) -> Hashtbl.replace (find_var_id id ctxs) id true
  | _ -> assert false

let rec type_unop_expr op e ctxs = match op with
  | Ast.Uneg | Ast.Unot | Ast.Uderef ->
    (* 1 - Verificar owenership *)
    ownership_expr ctxs e

  | Ast.Uref | Ast.Urefmut ->
    (* Perde o ownership *)
    ignore(ownership_expr ctxs e);
    (* Todo: Tirar ownership a e*)
    remove_ownership e ctxs;
    true

and ownership_expr ctxs = function
  | TEint _  -> false
  | TEbool _ -> false
  | TEident (id, _) ->
    (* 1 - Verificar se id é o dono *)
    if(not (Hashtbl.find (find_var_id id ctxs) id)) then error ("Invalid use of the variable "^id^", it's not the current owner.");
    false

  | TEbinop (Ast.Bassign, e1, e2, t) ->
    (* Todo verificar ambas as expressões *)
    let is_to_give = ownership_expr ctxs e2 in
    let _ = ownership_expr ctxs e1 in
    if is_to_give then give_ownership e1 ctxs;
    false

  | TEbinop (op, e1, e2, t) ->
    (* 1 - Verificar a expressão e1 e e2*)
    let s1 = ownership_expr ctxs e1 in
    let s2 = ownership_expr ctxs e2 in
    s1 || s2

  | TEunop (op, e, t) -> 
    type_unop_expr op e ctxs
  | TElen (e, t) ->
    (* 1 - Verificar a expressão *)
    ownership_expr ctxs e

  | TEcall (id, args, t) ->
   let argument_ctxs = List.fold_right(fun (v,f) l -> 
      let nv = Hashtbl.copy v in
      let nf = Hashtbl.copy f in
      (nv, nf)::l
    ) ctxs [] in


    (* 1 - Verificar se as expressão são donas *)
    List.iter(fun e -> ignore(ownership_expr argument_ctxs e)) args;
    false

  | TEstruct_access (e, el, tid, tel) ->
    (* 1 - Verificar a expressão e *)
    ownership_expr ctxs e

  | TEvec_decl (els, t) ->
    List.iter(fun e -> ignore(ownership_expr ctxs e)) els;
    false

  | TEvec_access (e1, e2, te) ->
    (* 1 - Verificar a expressão e *)
    ignore(ownership_expr ctxs e2);
    (* 1 - Verificar a expressão e *)
    ownership_expr ctxs e1

  | TEprint (s, t) -> false
  | TEblock (b, t) ->
    (* TODO: restaurar contextos *)
    (* 1 - Verificar ownership no corpo *)
    ownership_block ((make_ctx ())::ctxs) b

and ownership_stmt ctxs = function
  | TSif (e, bif, belse, _) -> 
    (* 1 - Verificar a expressão *)
    ignore(ownership_expr ctxs e);
    
    (* 2 - Verificar corpo do if *)
    let vif = ownership_block ((make_ctx ())::ctxs) bif in

    (* 3 - Verificar corpo do else *)
    let velse = ownership_block ((make_ctx ())::ctxs) belse in
    vif || velse

  | TSwhile (e, b, _) ->
    (* 1 - Verificar a expressão *)
    ignore(ownership_expr ctxs e);

    (* 2 - Verificar corpo do while *)
    ownership_block ((make_ctx ())::ctxs) b
    
  | TSdeclare (ismut, id, e, t) ->
    (* 1 - Adicionar id aos contextos *)
    let v_ctx,_ = (List.hd ctxs) in 

    (* 2 - Verificar se a expressão é válida *)
    ignore(ownership_expr ctxs e);

    (* 3 - Tem que ser sempre dono no ato da declaração *)
    Hashtbl.add v_ctx id true;
    false

  | TSdeclare_struct (ismut, id, idt, els, t) ->
    (* 1 - Adicionar id aos contextos *)
    let v_ctx,_ = (List.hd ctxs) in 

    (* 2 - Verificar se as expressões são válidas *)
    List.iter(fun (_, e) -> ignore(ownership_expr ctxs e)) els;

    (* 3 - Tem que ser sempre dono no ato da declaração *)
    Hashtbl.add v_ctx id true;
    false

  | TSreturn (e, t) ->
   (* 1 - Verificar a expressão *)
   begin match e with
     | None -> false
     | Some e -> ownership_expr ctxs e
   end

  | TSnothing t -> false
  | TSexpr (e, t)   ->
    (* 1 - Verificar a expressão *)
    ownership_expr ctxs e

and ownership_block ctxs b = 
  let stmts, e, _ = b in
  List.iter(fun s -> ignore(ownership_stmt ctxs s)) stmts;

  begin match e with
    | None   -> false
    | Some e -> ownership_expr ctxs e
  end

and ownership_dec ctxs = function
  | TDstruct (id, args, t) -> ()
  | TDfun (id, args, r, body, t) ->
    (* 1 - Verificar se o id já foi definido *)
    let f = snd(List.hd ctxs) in
    Hashtbl.add f id ();
    
    (* 2 - Adicionar os argumentos *)
    let args_ctxs = ((make_ctx ())::ctxs) in
    let v,_ = List.hd args_ctxs in
    List.iter(fun (ismut, arg, _) -> Hashtbl.add v arg true) args;

    (* 3 - Verificar ownership no corpo*)
    ignore(ownership_block args_ctxs body)

let verify_ownership f =
  List.fold_left(fun ctxs s ->
    let ctxs = (make_ctx ())::ctxs in
    ignore(ownership_dec ctxs s); ctxs
  ) [] f

