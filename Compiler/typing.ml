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
open Ast
open Tast
open Printer

exception Error of string * int

let error s line = raise (Error (s, line))

(* table_ctx representa um scope, contexto *)
type tbl_variables_ctx = (string, crust_types) Hashtbl.t
type tbl_functions_ctx = (string, crust_types) Hashtbl.t
type tbl_structs_ctx = (string, Ast.pairs list) Hashtbl.t

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
  | []     -> None
  | (_,_,ct)::tl -> if Hashtbl.mem ct id then (Some ct) else (find_struct_id id tl) 

let rec is_id_unique id = function
  | []          -> true
  | (v,f,s)::tl -> if (Hashtbl.mem v id || Hashtbl.mem f id || Hashtbl.mem s id) then false else (is_id_unique id tl) 

let crust_types_of_crust_const = function
  | Ast.Ci32  _ -> Ast.Ti32
  | Ast.Cbool _ -> Ast.Tbool

let compare_crust_types = function 
  | Ast.Ti32 , Ast.Ti32  -> true
  | Ast.Tbool, Ast.Tbool -> true
  | _, _                 -> false
  
let rec type_binop_expr op te1 t1 te2 t2 line = match op with
 | Ast.Badd | Ast.Bsub | Ast.Bmul | Ast.Bdiv | Ast.Bmod ->
    (* 1 - Verificar t1 e t2 *)
    if not (compare_crust_types (Ast.Ti32, t1)) then error ("Wrong type given to operand "^(Printer.string_of_binop op)^", was given"^Printer.string_of_crust_types t1^" but a "^Printer.string_of_crust_types Ast.Ti32^" was expected.") line;
    if not (compare_crust_types (Ast.Ti32, t2)) then error ("Wrong type given to operand "^(Printer.string_of_binop op)^", was given"^Printer.string_of_crust_types t2^" but a "^Printer.string_of_crust_types Ast.Ti32^" was expected.") line;
    (* 2 - Retornar operação tipada*)
    TEbinop(op, te1, te2, Ast.Ti32), Ast.Ti32
 | Ast.Beq | Ast.Bneq | Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge ->
    (* 1 - Verificar t1 e t2 *)
    if not (compare_crust_types (Ast.Ti32, t1)) then error ("Wrong type given to operand "^(Printer.string_of_binop op)^", was given"^Printer.string_of_crust_types t1^" but a "^Printer.string_of_crust_types Ast.Ti32^" was expected.") line;
    if not (compare_crust_types (Ast.Ti32, t2)) then error ("Wrong type given to operand "^(Printer.string_of_binop op)^", was given"^Printer.string_of_crust_types t2^" but a "^Printer.string_of_crust_types Ast.Ti32^" was expected.") line;
    (* 2 - Retornar operação tipada*)
    TEbinop(op, te1, te2, Ast.Tbool), Ast.Tbool
  | Ast.Bor | Ast.Band ->
    (* 1 - Verificar t1 e t2 *)
    if not (compare_crust_types (Ast.Tbool, t1)) then error ("Wrong type given to operand "^(Printer.string_of_binop op)^", was given"^Printer.string_of_crust_types t1^" but a "^Printer.string_of_crust_types Ast.Ti32^" was expected.") line;
    if not (compare_crust_types (Ast.Tbool, t2)) then error ("Wrong type given to operand "^(Printer.string_of_binop op)^", was given"^Printer.string_of_crust_types t2^" but a "^Printer.string_of_crust_types Ast.Ti32^" was expected.") line;
    (* 2 - Retornar operação tipada*)
    TEbinop(op, te1, te2, Ast.Tbool), Ast.Tbool
 
and type_expr ctxs = function
  | Ecst(const, _) -> 
    TEcst(const, crust_types_of_crust_const const), crust_types_of_crust_const const
  | Eident(id, line)       -> 
    (* 1 - Ir buscar o CTX em que esta variável está declarada *)
    (* 2 - Retornar o seu tipo *)
    begin match find_var_id id ctxs with
    | None     -> error ("The identifier " ^ id ^ " was not defined.") line
    | Some ctx -> TEident(id, Hashtbl.find ctx id), Hashtbl.find ctx id end
  | Ebinop (op, e1, e2, line) ->
    (* 1 - Tipar e1 e2*) 
    let te1, t1 = type_expr ctxs e1 in
    let te2, t2 = type_expr ctxs e2 in
    (* 2 - Verificar as regras de tipos para cada conjunto de operadores *)
    type_binop_expr op te1 t1 te2 t2 line
  | Eunop (Ast.Uneg, e, line) ->
    (* 1 - Tipar e*)
    let te, t = type_expr ctxs e in
    (* 2 - Verificar t*)
    if not (compare_crust_types (Ast.Ti32, t)) then error ("Wrong type given to operand Ast.Uneg, was given"^Printer.string_of_crust_types t^" but a "^Printer.string_of_crust_types Ast.Ti32^" was expected.") line;
    (* 3 - Retorna a expressão tipada *)
    Tast.TEunop(Ast.Uneg, te, Ast.Ti32), Ast.Ti32
  | Eunop (Ast.Unot, e, line) ->
      (* 1 - Tipar e*)
    let te, t = type_expr ctxs e in
    (* 2 - Verificar t*)
    if not (compare_crust_types (Ast.Tbool, t)) then error ("Wrong type given to operand Ast.Unot, was given"^Printer.string_of_crust_types t^" but a "^Printer.string_of_crust_types Ast.Tbool^" was expected.") line;
    (* 3 - Retorna a expressão tipada *)
    Tast.TEunop(Ast.Unot, te, Ast.Tbool), Ast.Tbool
  | Ecall _ -> assert false
  | _ -> assert false
  
(* Verificacao de uma instrucao - Instruções nao devolvem um valor *)
and type_stmt ctxs = function
  | Sif(e1, body, elifs, line) ->
    (* 1 - Verificar o tipo de e1 *)
    let te1, t1 = type_expr ctxs e1 in
    if not (compare_crust_types (Tbool, t1)) then error ("Wrong type in the if condition, was given " ^ Printer.string_of_crust_types t1 ^ " but a bool was expected.") line;
    (* 2 - Verificaro corpo do if *)
    let typed_body = type_stmt ((make_ctx ())::ctxs) body in
    (*3 - Verificar elifs e o else *)
    let iflist = ref [] in
    List.iter(fun (e2, body, line) -> 
      (* 3.1 - Verificar o tipo de e1 *)
      let te2, t2 = type_expr ctxs e2 in
      if not (compare_crust_types (Tbool, t1)) then error ("Wrong type in the if condition, was given " ^ Printer.string_of_crust_types t2 ^ " but a bool was expected.") line;
      (* 3.2 - Verificaro corpo do if *)
      let typed_elif_body = type_stmt ((make_ctx ())::ctxs) body in
      (* 3.3 - Adicionar aos elifs *)
      iflist := (!iflist)@[te2, typed_elif_body]
    )elifs;
    Tast.TSif(te1, typed_body, !iflist)

  | Swhile(e, body, line)     ->
    (* 1 - Tipar e verificar a condição e *)
    let te1, t1 = type_expr ctxs e in
    if not (compare_crust_types (Tbool, t1)) then error ("Wrong type in the while condition, was given "^Printer.string_of_crust_types t1^" but a bool was expected.") line;
    (* 2 - Tipar corpo do while *)
    let typed_body = type_stmt ((make_ctx ())::ctxs) body in
    Tast.TSwhile(te1, typed_body)

  | Sdeclare(id, t, e, line) ->
    (* 1 - Verificar se id já existe no contexto local *)
    if not (is_id_unique id ctxs) then error ("The identifier " ^ id ^ " was already defined.") line;
    (* 2 - Tipar e verificar a expressão e*)
    let te, t1 = type_expr ctxs e in
    if not (compare_crust_types (t, t1)) then error ("Wrong type in the declaration of variable"^id^", was given "^Printer.string_of_crust_types t1^" but a "^Printer.string_of_crust_types t^" was expected.") line;
    (* 3 - Adicionar variável ao contexto *)
    let v_ctx,_,_ = (List.hd ctxs) in 
    Hashtbl.add v_ctx id t;
    (* 4 - Retornar declaração tipada *)
    Tast. TSdeclare(id, t, te)

  | Sassign(id, e, line)   ->
    (* 1 - Verificar id *)
    let ctx = (match find_var_id id ctxs with
    | Some ctx -> ctx
    | None     -> error ("The identifier " ^ id ^ " was not defined.") line) in
    (* 2 - Extrair tipo do id *)
    let t = Hashtbl.find ctx id in
    (* 3 - Tipar expressão *)
    let te, t1 = type_expr ctxs e in
    if not (compare_crust_types (t, t1)) then error ("Wrong type in the assign of variable"^id^", was given "^Printer.string_of_crust_types t1^" but a "^Printer.string_of_crust_types t^" was expected.") line;
    Tast.TSassign(id, te)

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
  | Scontinue _ -> Tast.TScontinue
  | Sbreak _    -> Tast.TSbreak
  | Sreturn (e1, _)     -> 
    (* 1 - Verificar o tipo de e1 *)
    let te1, t = type_expr ctxs e1 in
    Tast.TSreturn(te1, t)
  | Snothing _  -> Tast.TSnothing
  | Sexpr(e, line) ->
    let te, t = type_expr ctxs e in
    Tast.TSexpr te
  
and type_global_stmt ctxs = function  
  | Ast.GSblock (bl, _) -> Tast.TGSblock(type_block_global_stmt ctxs [] bl)
  | Ast.GSfunction(id, args, r, body, line) -> 
    (* 1 - Verificar se o id já foi definido *)
    if not (is_id_unique id ctxs) then error ("The function with identifier " ^ id ^ " was already defined.") line;
    let _,f,_ = List.hd ctxs in
    Hashtbl.add f id r;
    (* 2 - tipar argumentos *)
    let ctxs = (make_ctx ())::ctxs in
    List.iter(fun (arg,t) -> 
      (* 2.1 - Verificar se o id já foi definido *)
      if not (is_id_unique arg ctxs) then error ("The function argument with identifier " ^ arg ^ " was already defined.") line;
      let v,_,_ = List.hd ctxs in
    
      Hashtbl.add v arg t;

    ) args;
    (* 3 - Tipar corpo *)
    (* TODO: Alterar AST para que instruções também tenham tipo. Adicionar tipo UNIT*)
    let typed_body = type_stmt ctxs body in

  Tast.TGSfunction(id, args, r, typed_body)

  | Ast.GSstruct(id, el, line) ->
    (* 1 - Verificar que os elementos da estrutura são únicos *)
    let tmp_ctx = make_ctx () in
    List.iter(fun (e,t) ->
      if not (is_id_unique e [tmp_ctx]) then error ("The struct element with identifier " ^ e ^ " was already defined.") line;
      let v,_,_ = tmp_ctx in
      Hashtbl.add v e t 
    )el;
    (* 2 - Verificar id *)
    if not (is_id_unique id ctxs) then error ("The struct with identifier " ^ id ^ " was already defined.") line;
    let _,_,s = List.hd ctxs in
    Hashtbl.add s id el;

    Tast.TGSstruct(id, el)
    
and type_block_stmt ctxs acc = function
  | [] -> acc
  | s :: sl -> (type_block_stmt ctxs (acc@[type_stmt ctxs s]) sl)

and type_block_global_stmt ctxs acc = function
  | [] -> acc
  | s :: sl -> (type_block_global_stmt ctxs (acc@[type_global_stmt ctxs s]) sl)

(* Tipa uma AST *)
let type_file s = type_global_stmt [make_ctx ()] s
