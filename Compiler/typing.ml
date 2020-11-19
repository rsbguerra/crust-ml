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
  | Cu8   _ -> Tu8 
  | Cu16  _ -> Tu16 
  | Cu32  _ -> Tu32
  | Cu64  _ -> Tu64 
  | Cu128 _ -> Tu128
  | Ci8   _ -> Ti8 
  | Ci16  _ -> Ti16
  | Ci32  _ -> Ti32  
  | Ci64  _ -> Ti64
  | Ci128 _ -> Ti128
  | Cbool _ -> Tbool

let compare_crust_types = function 
  | Tu8  , Tu8   -> true 
  | Tu16 , Tu16  -> true 
  | Tu32 , Tu32  -> true
  | Tu64 , Tu64  -> true 
  | Tu128, Tu128 -> true
  | Ti8  , Ti8   -> true 
  | Ti16 , Ti16  -> true
  | Ti32 , Ti32  -> true  
  | Ti64 , Ti64  -> true
  | Ti128, Ti128 -> true
  | Tbool, Tbool -> true
  | _, _         -> false

let rec type_expr ctxs = function
  | Ecst(const, _) -> crust_types_of_crust_const const
  | Eident(id, line)       -> 
    (* 1 - Ir buscar o CTX em que esta variável está declarada *)
    (* 2 - Retornar o seu tipo *)
    begin match find_id id ctxs with
    | None     -> error ("The identifier " ^ id ^ " was not defined.") line
    | Some ctx -> Hashtbl.find ctx id end
  | Ebinop _       -> assert false
  | Eunop _        -> assert false
  | Ecall _        -> assert false
 

(* Verificacao de uma instrucao - Instruções nao devolvem um valor *)
and type_stmt ctxs = function
  | Sif(e1, body, elifs, line) ->
    (* 1 - Verificar o tipo de e1 *)
    let t1 = type_expr ctxs e1 in
    if not (compare_crust_types (Tbool, t1)) then error ("Wrong type in the if condition, was given " ^ Printer.string_of_crust_types t1 ^ " but a bool was expected.") line;
    (* 2 - Verificaro corpo do if *)
    type_stmt ((Hashtbl.create 16 : table_ctx)::ctxs) body;
    (* 3 - Verificar elifs e o else *)
    List.iter(fun(e1, body, line) ->  
      let t1 = type_expr ctxs e1 in
      if not (compare_crust_types (Tbool, t1)) then error ("Wrong type in the elif condition, was given " ^ Printer.string_of_crust_types t1 ^ " but a bool was expected.") line;
      type_stmt ((Hashtbl.create 16 : table_ctx)::ctxs) body
    ) elifs
  | Sloop(body, _)         -> type_stmt ((Hashtbl.create 16 : table_ctx)::ctxs) body
  | Swhile _              -> assert false
  | Sdeclare(id, t1, e, line) ->
    (* 1 - Computar o tipo de e1 *)
    let t2 = type_expr ctxs e in
    (* 2 - Verificar se o tipo devolvido era o esperado *)
    if not (compare_crust_types (t1, t2)) then error ("Wrong type in the declaration of the variable"^id^", was given " ^ Printer.string_of_crust_types t2 ^ " but type "^Printer.string_of_crust_types t1^" was expected.") line;
    (* 3 - Substituir pela nova declaração*)
    Hashtbl.replace (List.hd ctxs) id t1
  | Sassign _              -> ()
  | Sprintn(e, line)       -> 
    (* 1 - Verificar o tipo de e *)
    ignore(type_expr ctxs e)
    (* 2 - Se não deu erro então é válido *)
  | Sprint _               -> assert false
  | Sblock (bl, _)         -> type_block_stmt ctxs bl
  | Scontinue _ | Sbreak _ -> ()
  | Sreturn _              -> assert false
  | Snothing _             -> ()
  
and type_global_stmt ctxs = function  
  | GSblock (bl, _) -> type_block_global_stmt ctxs bl
  | GSuse _         -> assert false
  | GSfunction(_, _, _, body, _)-> type_stmt ((Hashtbl.create 16 : table_ctx)::ctxs) body
  | GSstruct _      -> assert false
  | GSimpl _        -> assert false

and type_block_stmt ctxs = function
  | [] -> ()
  | s :: sl -> type_stmt ctxs s; type_block_stmt ctxs sl

and type_block_global_stmt ctxs = function
  | [] -> ()
  | s :: sl -> type_global_stmt ctxs s; type_block_global_stmt ctxs sl

(* Realiza a analise semantica de um ficheiro *)
let file s = type_global_stmt [(Hashtbl.create 16 : table_ctx)] s
