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

let function_types = ref []


(* table_ctx representa um scope, contexto *)
type tbl_variables_ctx = (string, crust_types) Hashtbl.t
type tbl_functions_ctx = (string, Ast.pairs list * crust_types) Hashtbl.t
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
  | []           -> None
  | (_,_,ct)::tl -> if Hashtbl.mem ct id then (Some ct) else (find_struct_id id tl) 

let rec find_struct_element el = function
  | []         -> None
  | (id,t)::tl -> if id = el then Some t else (find_struct_element el tl)

let rec is_id_unique id = function
  | []          -> true
  | (v,f,s)::tl -> if (Hashtbl.mem v id || Hashtbl.mem f id || Hashtbl.mem s id) then false else (is_id_unique id tl) 

let crust_types_of_crust_const = function
  | Ast.Ci32  _ -> Ast.Ti32
  | Ast.Cbool _ -> Ast.Tbool
  | Ast.Cunit   -> Ast.Tunit

let rec compare_crust_types = function 
  | Ast.Ti32 , Ast.Ti32  -> true
  | Ast.Tbool, Ast.Tbool -> true
  | Ast.Tunit, Ast.Tunit -> true
  | Ast.Tstruct(t1), Ast.Tstruct(t2) -> t1 = t2
  | Ast.Tvec (t1, _), Ast.Tvec (t2, _) -> compare_crust_types (t1,t2)  
  | t1, Ast.Tref (t2, _) -> compare_crust_types (get_ref_type t1, get_ref_type t2)
  | Ast.Tref (t1, _), t2 -> compare_crust_types (get_ref_type t1, get_ref_type t2)
  | _, _                 -> false

and get_ref_type = function
  | Ast.Tref (t1, _) -> get_ref_type t1
  | _  as t -> t

let is_vec = function 
  | Ast.Tvec _ -> true  
  | _          -> false

let string_of_tstruct = function 
  | Ast.Tstruct(t1) -> Some t1
  | _               -> None
  

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
  | Eref (id, line) ->
   (* 1 - Verificar id *)
    let ctx = (match find_var_id id ctxs with
    | Some ctx -> ctx
    | None     -> error ("The identifier " ^ id ^ " was not defined.") line) in
    (* 2 - Extrair tipo do id *)
    let t = Hashtbl.find ctx id in
    TEref(id, Ast.Tref (t, id)), (Ast.Tref (t, id))
  
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

  | Estrc_access(id, el, line) ->
    (* p.x *)
    (* 1 - ir buscar a estrutura com nome id *)
    let tid = match find_var_id id ctxs with
      | None     -> error ("The identifier " ^ id ^ " was not defined.") line
      | Some ctx -> Hashtbl.find ctx id in

    (* 2 - ir buscar o tipo estrutura *)
    let strct = match string_of_tstruct tid with
      | None   -> error ("The structure " ^ Printer.string_of_crust_types tid ^ " was not defined.") line
      | Some s -> 
        begin match find_struct_id s ctxs with
          | None     -> error ("The structure with the identifier " ^ s ^ " was not defined.") line
          | Some ctx -> Hashtbl.find ctx s 
        end 
      in
    
    (* 3 - verificar se a estrutura tem o elemento el *)
    begin match find_struct_element el strct with
       | None   -> error ("Trying to access element "^ el ^" of struct "^ Printer.string_of_crust_types tid ^" but this structure does not contain it.") line;
       | Some tel -> TEstrc_access(id, el, tid, tel), tel
    end 

  | Estrc_decl(id, el, line) ->
    (* 1 - Verificar se a estrutura id existe *)
    begin match find_struct_id id ctxs with
    | None     -> error ("The structure with the identifier " ^ id ^ " was not defined.") line
    | Some ctx ->
      (* 2 - Verificar se todos os elementos de el existem na estrutura id *)
      let elements = Hashtbl.find ctx id in
      
      if (List.length elements) <> (List.length el) then error ("The structure with the identifier "^id^" has "^(string_of_int (List.length elements))^" elements but were givin "^string_of_int(List.length el)^".") line;
      
      let typed_el = ref [] in
      List.iter2(fun (id1,t1) (id2,e2) -> 
        let te2, t2 = type_expr ctxs e2 in
        (* 2.1 - Verificar se os nomes de e2 estão corretos *)
        if id1 <> id2 then error ("Wrong name given in the declaration of a struct condition, was given "^id1 ^" but "^id2^" was expected.") line;
        (* 2.1 - Verificar se os tipos de e2 estão corretos *)
        if not (compare_crust_types (t1, t2)) then error ("Wrong type in the if condition, was given " ^ Printer.string_of_crust_types t2 ^ " but a " ^ Printer.string_of_crust_types t1 ^ " was expected.") line;
        typed_el := (!typed_el)@[(id2,te2,t2)]
      ) elements el;
      
      TEstrc_decl(id, !typed_el, (Tstruct id)), (Tstruct id)

    end
  | Elen (id,line) ->
    (* 1 - Verificar se o id existe *) 
    let ctx = (match find_var_id id ctxs with
    | Some ctx -> ctx
    | None     -> error ("The identifier " ^ id ^ " was not defined.") line) in
   
    (* 2 - Verificar se o id é do tipo Tvec *)
    let t = Hashtbl.find ctx id in
    if not (is_vec t) then error ("Invoking function len with type "^Printer.string_of_crust_types t^" when a vector is expected.") line;
    TElen id, Ast.Ti32

  | Ecall(id, args, line) ->
    (* 1 - Verificar se a função existe *)
    let params, r = match find_fun_id id ctxs with
      | None -> error ("The function with identifier " ^ id ^ " was not defined.") line
      | Some ctx -> Hashtbl.find ctx id in

    (* 2 - Verificar os tipos dos argumentos *)
    if not ((List.length args) = (List.length params)) then error ("Invalid number of arguments given.") line;
    
    let typed_args = ref [] in
    List.iteri(fun i e ->
      let te, t = type_expr ctxs e in
      let ta = snd (List.nth params i) in
      if not (compare_crust_types (t,ta)) then error ("Invalid number of arguments given.") line;
      typed_args := !typed_args@[(te, t)]
    )args;

    TEcall(id, !typed_args, r), r
  
  | Evec_decl(els, line) ->
    (* 1 - O tipo da primeira expressão manda *)
    let te1, t1 = type_expr ctxs (List.hd els) in

    (* 1 - Tipar todas as expressões *)
    let l = te1::(List.map(fun e ->  
       let te2, t2 = type_expr ctxs e in
       if not (compare_crust_types (t1,t2)) then error ("Invalid type in vec declaration was expecting "^ Printer.string_of_crust_types t1^" but was given "^Printer.string_of_crust_types t2 ^".") line;
       te2
    ) (List.tl els)) in

   TEvec_decl(l, (Ast.Tvec (t1, List.length l))), (Ast.Tvec (t1, List.length l))
  | Evec_access(id, e, line) ->
    (* 1 - Verificar se o id existe *) 
    let ctx = (match find_var_id id ctxs with
    | Some ctx -> ctx
    | None     -> error ("The identifier " ^ id ^ " was not defined.") line) in
   
    (* 2 - Verificar se o id é do tipo Tvec *)
    let t = Hashtbl.find ctx id in
    if not (is_vec t) then error ("Trying to use "^id^" as a vector when is a "^Printer.string_of_crust_types t^".") line;
    (* 3 - Tipar e *)
    let te, t1 = type_expr ctxs e in
    (* 4 - Verificar se e é um i32 *)
    if not (compare_crust_types (t1,Ast.Ti32)) then error ("Trying to access an element of a vector with a "^Printer.string_of_crust_types t1 ^" when is expected a Ti32.") line;

    TEvec_access(id, te, t1, t), t1
  | _ -> assert false
  
(* Verificacao de uma instrucao - Instruções nao devolvem um valor *)
and type_stmt ctxs = function
  | Sif(e1, body, elifs, line) ->
    (* 1 - Verificar o tipo de e1 *)
    let te1, t1 = type_expr ctxs e1 in
    if not (compare_crust_types (Tbool, t1)) then error ("Wrong type in the if condition, was given " ^ Printer.string_of_crust_types t1 ^ " but a bool was expected.") line;
    (* 2 - Verificaro corpo do if *)
    let typed_body, tb = type_stmt ((make_ctx ())::ctxs) body in
    (*3 - Verificar elifs e o else *)
    let iflist = ref [] in
    List.iter(fun (e2, body, line) -> 
      (* 3.1 - Verificar o tipo de e1 *)
      let te2, t2 = type_expr ctxs e2 in
      if not (compare_crust_types (Tbool, t1)) then error ("Wrong type in the if condition, was given " ^ Printer.string_of_crust_types t2 ^ " but a bool was expected.") line;
      (* 3.2 - Verificaro corpo do if *)
      let typed_elif_body, telb = type_stmt ((make_ctx ())::ctxs) body in
      (* 3.3 - Verificar se todos os elifs tem o mesmo tipo que if *)
      if not (compare_crust_types (tb, telb)) then error ("Incompatible type in if branch, was given "^Printer.string_of_crust_types telb^" but a "^Printer.string_of_crust_types tb^" was expected.") line;
    
      (* 3.3 - Adicionar aos elifs *)
      iflist := (!iflist)@[te2, typed_elif_body]
    )elifs;
    Tast.TSif(te1, typed_body, !iflist, tb), tb

  | Swhile(e, body, line)     ->
    (* 1 - Tipar e verificar a condição e *)
    let te1, t1 = type_expr ctxs e in
    if not (compare_crust_types (Tbool, t1)) then error ("Wrong type in the while condition, was given "^Printer.string_of_crust_types t1^" but a bool was expected.") line;
    (* 2 - Tipar corpo do while *)
    let typed_body, tb = type_stmt ((make_ctx ())::ctxs) body in
    Tast.TSwhile(te1, typed_body, tb), tb

  | Sdeclare(id, t, e, line) ->
    (* 1 - Tipar e verificar a expressão e*)
    let te, t1 = type_expr ctxs e in
    if not (compare_crust_types (t, t1)) then error ("Wrong type in the declaration of variable "^id^", was given "^Printer.string_of_crust_types t1^" but a "^Printer.string_of_crust_types t^" was expected.") line;
    (* 2 - Adicionar variável ao contexto *)
    let v_ctx,_,_ = (List.hd ctxs) in 
    Hashtbl.add v_ctx id t1;
    (* 3 - Retornar declaração tipada *)
    Tast.TSdeclare(id, t1, te, Ast.Tunit), Ast.Tunit

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
    Tast.TSassign(id, te, Ast.Tunit), Ast.Tunit

  | Sprintn (e, line) ->
    (* 1 - Tipar expressão *)
    let te, t = type_expr ctxs e in
    (* 2 - Só conseguimos imprimir Tbool e Ti32 *)
    if not ((compare_crust_types (t, Ti32)) || (compare_crust_types (t, Tbool)))then error ("Wrong type in the print statement, was given "^Printer.string_of_crust_types t^" but a Ti32 or Tbool was expected.") line;

    Tast.TSprintn(te, t, Ast.Tunit), Ast.Tunit

  | Sprint (e, line) ->
    (* 1 - Tipar expressão *)
    let te, t = type_expr ctxs e in
    (* 2 - Só conseguimos imprimir Tbool e Ti32 *)
    if not ((compare_crust_types (t, Ti32)) || (compare_crust_types (t, Tbool)))then error ("Wrong type in the print statement, was given "^Printer.string_of_crust_types t^" but a Ti32 or Tbool was expected.") line;

    Tast.TSprint(te, t, Ast.Tunit), Ast.Tunit

  | Sblock (bl, _) ->
    (* 1 - Tipar bloco*)
    let l, t = type_block_stmt ctxs [] bl in
    Tast.TSblock(l, t), t

  | Scontinue _ -> Tast.TScontinue Ast.Tunit, Ast.Tunit
  | Sbreak _    -> Tast.TSbreak Ast.Tunit, Ast.Tunit
  | Sreturn (e, line)     -> 
    (* 1 - Verificar se estamos dentro de uma função *)
    if List.length !function_types <= 0 then error ("Using the return statement outside of a function") line;
    let function_type = List.hd !function_types in
    
    (* 2 - Verificar o tipo de e *)
    let te1, t = type_expr ctxs e in

    (* 3 - Verificar o tipo do retorno e da função *)
    if not (compare_crust_types (function_type, t)) then error ("Incompatible return type, function has type "^Printer.string_of_crust_types function_type^" but the return has type "^Printer.string_of_crust_types t^".") line;
    
    Tast.TSreturn(te1, t), t

  | Snothing _  -> Tast.TSnothing Ast.Tunit, Ast.Tunit
  | Sexpr(e, line) ->
    let te, t = type_expr ctxs e in
    Tast.TSexpr(te, t), t
  
and type_global_stmt ctxs = function  
  | Ast.GSblock (bl, _) -> Tast.TGSblock(type_block_global_stmt ctxs [] bl)
  | Ast.GSfunction(id, args, r, body, line) -> 
    (* 1 - tipar argumentos *)
    
    let args_ctxs = (make_ctx ())::ctxs in
    List.iter(fun (arg,t) -> 
      (* 2.1 - Verificar se o id já foi definido *)
      if not (is_id_unique arg args_ctxs) then error ("The function argument with identifier " ^ arg ^ " was already defined.") line;
      let v,_,_ = List.hd args_ctxs in
    
      Hashtbl.add v arg t;
    ) args;
    
    (* 2 - Verificar se o id já foi definido *)
    if not (is_id_unique id args_ctxs) then error ("The function with identifier " ^ id ^ " was already defined.") line;
    let _,f,_ = List.hd ctxs in
    Hashtbl.add f id (args, r);

    (* 3 - Tipar corpo *)
    function_types := r::(!function_types);

    let typed_body, tb = type_stmt args_ctxs body in
    if not (compare_crust_types (tb,r)) then error ("The function "^id^" has return type "^Printer.string_of_crust_types r^" but is body has return type "^Printer.string_of_crust_types tb^".") line;
    
    function_types := List.tl !function_types;

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
  | [] -> (List.map fst acc), Ast.Tunit
  | [s] -> let typed_s, t = type_stmt ctxs s in let acc = acc@[(typed_s, t)] in (List.map fst acc), t 
  | s :: sl -> (type_block_stmt ctxs (acc@[type_stmt ctxs s]) sl)

and type_block_global_stmt ctxs acc = function
  | [] -> acc
  | s :: sl -> (type_block_global_stmt ctxs (acc@[type_global_stmt ctxs s]) sl)

(* Tipa uma AST *)
let type_file s = type_global_stmt [make_ctx ()] s
