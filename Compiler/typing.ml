(* 
  É utilizada uma lista de tabelas de contexto para representar as declarações das variáveis.
  Parte-se do principio que o contexto atual é o que se encontra na cabeça da lista.

  No programa seguinte aquando da tipagem do corpo da instrução if 
  iremos possuir uma lista de contextos parecida com [ctx3; ctx2; ctx1], 
  em que ctx3 representa o contexto mais local, do if neste caso, ctx2 o contexto do loop
  e ctx3 o contexto mais exterior, o da função main.

     +  fn main(){
     |      +  loop{
     |      |      +  if(true) {
CTX1 | CTX2 | CTX3 |    ...
     |      |      + }
     |      |    ...
     |      +  }
     |    ...
     +  }
*)
open Printer

exception Error of string * int

let error s line = raise (Error (s, line))


(* table_ctx representa um scope, contexto *)
type tbl_variables_ctx = (string,Tast.prust_type) Hashtbl.t
type tbl_functions_ctx = (string, Tast.argument list * Tast.prust_type) Hashtbl.t
type tbl_structs_ctx = (string, Tast.pair list) Hashtbl.t

let make_ctx () = 
  let v = (Hashtbl.create 16 : tbl_variables_ctx) in
  let f = (Hashtbl.create 16 : tbl_functions_ctx) in
  let s = (Hashtbl.create 16 : tbl_structs_ctx) in
  (v, f, s)

let rec find_var_id id = function
  | []     -> None
  | (ct,_,_)::tl -> if Hashtbl.mem ct id then (Some ct) else (find_var_id id tl) 

let rec find_fun_id id = function
  | []     -> None
  | (_,ct,_)::tl -> if Hashtbl.mem ct id then (Some ct) else (find_fun_id id tl) 

let rec find_struct_id id = function
  | []           -> None
  | (_,_,ct)::tl -> if Hashtbl.mem ct id then (Some ct) else (find_struct_id id tl) 

let rec find_struct_element el = function
  | []         -> None
  | (id,t)::tl -> if id = el then Some t else (find_struct_element el tl)

let rec is_id_unique id = function
  | []          -> true
  | (v,f,s)::tl -> if (Hashtbl.mem v id || Hashtbl.mem f id || Hashtbl.mem s id) then false else (is_id_unique id tl)

let type_prust_type = function
  | Ast.Tid id -> 
  begin match id with
    | "i32"  -> Tast.Ti32 
    | "bool" -> Tast.Tbool
    | "()"   -> Tast.Tunit
    | _      -> assert false
  end
  | Tid_typed (id, t) -> assert false
  | Tref t -> assert false
  | Trefmut t -> assert false

let rec type_expr ctxs = function
  | Ast.Eint(v, _) -> Tast.TEint(v, Tast.Ti32), Tast.Ti32
  | Ebool(v, _) -> Tast.TEbool(v, Tast.Tbool), Tast.Tbool
  | Eident(id, line) -> assert false
  | Ebinop (op, e1, e2, line) -> assert false
  | Eunop (op, e, line) -> assert false
  | Estruct_access(id, el, line) -> assert false
  | Elen (id,line) -> assert false
  | Evec_access(id, e, line) -> assert false
  | Ecall(id, args, line) -> assert false
  | Evec_decl(els, line) -> assert false
  | Eprint(s, line) -> assert false
  | Eblock(b, line) -> assert false
  
(* Verificacao de uma instrucao - Instruções nao devolvem um valor *)
and type_stmt ctxs = function
  | Ast.Sif(e1, body, elifs, line) -> assert false
  | Swhile(e, body, line) -> assert false
  | Sdeclare(ismut, id, e, line) -> assert false
  | Sdeclare_struct(ismut, id, idt, e, line) -> assert false
  | Sreturn (e, line) -> assert false
  | Snothing _  -> assert false
  | Sexpr(e, line) -> assert false

and type_block ctxs b = 
 let stmts, e = b in
 let t_stmts = List.map (fun s -> type_stmt ctxs s)stmts in

 let te, t = match e with
  | None   -> None, Tast.Tunit
  | Some e -> 
    let s, t = type_expr ctxs e in 
    (Some s), t
 in
 (t_stmts, te, t), t

and type_decl ctxs = function  
  | Ast.Dstruct(id, els, line) ->
    (* 1 - Verificar que os elementos da estrutura são únicos *)
    let ctx = make_ctx () in
    let tels = List.map(fun (e,t) ->
      (* 1.1 Verificar se é único *)
      if not (is_id_unique e [ctx]) then error ("The struct element with identifier " ^ e ^ " was already defined.") line;
      let v,_,_ = ctx in
      let tt = type_prust_type t in
      Hashtbl.add v e tt;

      (id, tt)

    )els in

    (* 2 - Verificar id *)
    if not (is_id_unique id ctxs) then error ("The struct with identifier " ^ id ^ " was already defined.") line;
    let _,_,s = List.hd ctxs in
    Hashtbl.add s id tels;

    Tast.TDstruct(id, tels, Tast.Tunit)

  | Ast.Dfun(id, args, r, body, line) ->
    (* 1 - tipar argumentos *)
    
    let args_ctxs = (make_ctx ())::ctxs in
    let targs = List.map(fun (ismut, arg, t) -> 
      (* 2.1 - Verificar se o id já foi definido *)
      if not (is_id_unique arg args_ctxs) then error ("The function argument with identifier " ^ arg ^ " was already defined.") line;
      let v,_,_ = List.hd args_ctxs in
      let tt = type_prust_type t in

      Hashtbl.add v arg tt;
      (ismut, arg, tt)
    ) args in 
    
    (* 2 - Verificar se o id já foi definido *)
    if not (is_id_unique id args_ctxs) then error ("The function with identifier " ^ id ^ " was already defined.") line;

    (* 3 - Verificar retorno *)
    let r = match r with
     | None   -> Tast.Tunit
     | Some t -> type_prust_type t
    in
   let _,f,_ = List.hd ctxs in
    Hashtbl.add f id (targs, r);

    (* 3 - Tipar corpo *)
    let typed_body, tb = type_block args_ctxs body in
    (*
    Todo: if not (compare_crust_types (tb,r)) then error ("The function "^id^" has return type "^Printer.string_of_crust_types r^" but is body has return type "^Printer.string_of_crust_types tb^".") line;
    *)
    Tast.TDfun(id, targs, r, typed_body, Tast.Tunit)

    
(* Tipa uma AST *)
let type_file f = 
  List.fold_left_map(fun ctxs s ->  
    let ctxs = (make_ctx ())::ctxs in
    ctxs, (type_decl ctxs) 
  ) [] f
