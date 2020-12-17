(* 
  É utilizada uma lista de tabelas de contexto para representar as declarações das variáveis.
  Parte-se do principio que o contexto atual é o que se encontra na cabeça da lista.

  No programa seguinte aquando da tipagem do corpo da instrução if 
  iremos possuir uma lista de contextos parecida com [ctx1; ctx2; ctx3], 
  em que ctx1 representa o contexto local do if, ctx2 o contexto do loop 
  e ctx3 o contexto da função main.

  fn main(){
     loop{
       if(true)
        { 
          ... 
        }
        ...
     }
     ...
  }
*)
open Ast
open Tast
open Printer

exception Error of string * int

let error s line = raise (Error (s, line))

(* table_ctx representa um scope, contexto *)
type table_ctx = (string, crust_types) Hashtbl.t

let rec find_id id l = 
  match l with
  | ct::tl -> if Hashtbl.mem ct id then ( Some ct) else (find_id id tl) 
  | _ -> None

let crust_types_of_crust_const = function
  | Ast.Ci32  _ -> Ast.Ti32  
  | Ast.Cbool _ -> Ast.Tbool

let compare_crust_types = function 
  | Ast.Ti32 , Ast.Ti32  -> true
  | Ast.Tbool, Ast.Tbool -> true
  | _, _                 -> false

let rec type_expr ctxs = function
  | Ecst(const, _) -> TEcst(const, crust_types_of_crust_const const), crust_types_of_crust_const const
  | Eident(id, line)       -> 
    (* 1 - Ir buscar o CTX em que esta variável está declarada *)
    (* 2 - Retornar o seu tipo *)
    begin match find_id id ctxs with
    | None     -> error ("The identifier " ^ id ^ " was not defined.") line
    | Some ctx -> TEident(id, Hashtbl.find ctx id), Hashtbl.find ctx id end
  | Ebinop (op, e1, e2, line) -> assert false
  | Eunop (op, e, line)       -> assert false
  | Ecall _        -> assert false
 
(* Verificacao de uma instrucao - Instruções nao devolvem um valor *)
and type_stmt ctxs = function
  | Sif(e1, body, elifs, line) ->
    (* 1 - Verificar o tipo de e1 *)
    let te1, t1 = type_expr ctxs e1 in
    if not (compare_crust_types (Tbool, t1)) then error ("Wrong type in the if condition, was given " ^ Printer.string_of_crust_types t1 ^ " but a bool was expected.") line;
    (* 2 - Verificaro corpo do if *)
    let typed_body = type_stmt ((Hashtbl.create 16 : table_ctx)::ctxs) body in
    (* TODO: 3 - Verificar elifs e o else *)

    Tast.TSif(te1, typed_body, [])

  | Swhile _              -> assert false
  | Sdeclare(id, t1, e, line) -> assert false
  | Sprintn (e1, _) ->
    (* 1 - Tipar expressão *)
    let te1, t = type_expr ctxs e1 in
    Tast.TSprintn(te1)
  | Sprint (e1, _) ->
    (* 1 - Tipar expressão *)
    let te1, t = type_expr ctxs e1 in
    Tast.TSprint(te1)
  | Sblock (bl, _) ->
    (* 1 - Tipar bloco*)
    Tast.TSblock(type_block_stmt ctxs [] bl)
  | Sreturn (e1, line)     -> 
    (* 1 - Verificar o tipo de e1 *)
    let te1, t = type_expr ctxs e1 in
    Tast.TSreturn(te1, line, t)
  | Scontinue _ -> Tast.TScontinue
  | Sbreak _    -> Tast.TSbreak
  | Snothing _  -> Tast.TSnothing
  | _ -> assert false
  
and type_global_stmt ctxs = function  
  | Ast.GSblock (bl, _) -> Tast.TGSblock(type_block_global_stmt ctxs [] bl)
  | Ast.GSfunction(id, list, r, body, _) -> Tast.TGSfunction(id, list, r, type_stmt ((Hashtbl.create 16 : table_ctx)::ctxs) body)
  | Ast.GSstruct _      -> assert false

and type_block_stmt ctxs acc = function
  | [] -> acc
  | s :: sl -> (type_block_stmt ctxs ((type_stmt ctxs s)::acc) sl)

and type_block_global_stmt ctxs acc = function
  | [] -> acc
  | s :: sl -> (type_block_global_stmt ctxs ((type_global_stmt ctxs s)::acc) sl)

(* Realiza a analise semantica de um ficheiro *)
let file s = type_global_stmt [(Hashtbl.create 16 : table_ctx)] s
