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
type tbl_variables_ctx = (string, (bool * Tast.prust_type)) Hashtbl.t
type tbl_functions_ctx = (string, Tast.argument list * Tast.prust_type) Hashtbl.t
type tbl_structs_ctx = (string, Tast.pair list) Hashtbl.t

let expected_vec_type = ref None

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

let rec compare_prust_types = function
 | Tast.Tempty, _ -> true
 | _, Tast.Tempty -> true
 | Tast.Tunit, Tast.Tunit -> true
 | Tast.Ti32, Tast.Ti32   -> true
 | Tast.Tbool, Tast.Tbool -> true
 | Tast.Tstruct s1, Tast.Tstruct s2 -> s1 = s2
 | Tast.Tref t1, Tast.Tref t2 -> compare_prust_types (t1, t2)
 | Tast.Trefmut t1, Tast.Trefmut t2 -> compare_prust_types (t1, t2)
 | Tast.Tref t1, Tast.Trefmut t2 -> compare_prust_types (t1, t2)
 | Tast.Tvec t1, Tast.Tvec t2 -> compare_prust_types (t1, t2)
 | _ -> false

let is_bool = function
 | Tast.Tbool  -> true
 | _           -> false

let is_i32 = function
 | Tast.Ti32   -> true
 | _           -> false

let rec is_vec = function
 | Tast.Tvec _ -> true
 | Tast.Tref t -> is_vec t
 | Tast.Trefmut t -> is_vec t
 | _           -> false

let is_refmut = function
 | Tast.Trefmut _ -> true
 | _              -> false

let is_ref = function
 | Tast.Tref _ -> true
 | _           -> false

let get_ref_type = function
 | Tast.Tref t    -> Some t
 | Tast.Trefmut t -> Some t
 | _              -> None

let rec get_vec_type = function
 | Tast.Tvec t    -> Some t
 | Tast.Tref t -> get_vec_type t
 | Tast.Trefmut t -> get_vec_type t
 | _              -> None


let rec string_of_tstruct = function 
  | Tast.Tstruct t  -> Some t
  | Tast.Tref t     -> string_of_tstruct t
  | Tast.Trefmut t  -> string_of_tstruct t
  | _              -> None

and type_prust_type ctxs = function
  | Ast.Tid id -> 
  begin match id with
    | "i32"  -> Tast.Ti32 
    | "bool" -> Tast.Tbool
    | "()"   -> Tast.Tunit
    | id     -> Tast.Tstruct id
  end
  | Tid_typed (id, t) ->
    if id <> "Vec" then error ("Trying to use type " ^ id ^ " as vec.") (-1);
    Tast.Tvec (type_prust_type ctxs t)
  | Tref t -> Tref (type_prust_type ctxs t)
  | Trefmut t -> Trefmut (type_prust_type ctxs t)

and is_value_mut ctxs = function
  | Tast.TEident (id, _) ->
    begin match find_var_id id ctxs with
      | None     -> error ("The identifier " ^ id ^ " was not defined.") (-1)
      | Some ctx -> (fst (Hashtbl.find ctx id))
    end
  | TEunop(Uderef, e,_) ->
    begin match e with
    | Tast.TEident (id, _) ->
      begin match find_var_id id ctxs with
        | None     -> error ("The identifier " ^ id ^ " was not defined.") (-1)
        | Some ctx -> is_refmut (snd (Hashtbl.find ctx id)) end
    | _ -> false
    end
  | TEstruct_access(e,_,_,_) -> is_vec_struct_value_mut ctxs e
  | TEvec_access(e,_,_) -> is_vec_struct_value_mut ctxs e
  | _ -> false

and is_vec_struct_value_mut ctxs = function
  | Tast.TEident (id, _) ->
    begin match find_var_id id ctxs with
      | None     -> error ("The identifier " ^ id ^ " was not defined.") (-1)
      | Some ctx -> (is_refmut (snd(Hashtbl.find ctx id))) || (fst (Hashtbl.find ctx id))
    end
  | TEunop(Uderef, e,_) ->
    begin match e with
    | Tast.TEident (id, _) ->
      begin match find_var_id id ctxs with
        | None     -> error ("The identifier " ^ id ^ " was not defined.") (-1)
        | Some ctx -> is_refmut (snd (Hashtbl.find ctx id)) end
    | _ -> false
    end
  | TEstruct_access(e,_,_,_) -> is_vec_struct_value_mut ctxs e
  | TEvec_access(e,_,_) -> is_vec_struct_value_mut ctxs e
  | _ -> false


and type_binop_expr op te1 t1 te2 t2 line ctxs = match op with
  | Ast.Badd | Ast.Bsub | Ast.Bmul | Ast.Bdiv | Ast.Bmod ->
    (* 1 - Verificar t1 e t2 *)
    if not (is_i32 t1) then error ("Wrong type given to operand "^(Printer.string_of_binop op)^", was given"^ "-----" ^" but a "^ "-----" ^" was expected.") line;
    if not (is_i32 t2) then error ("Wrong type given to operand "^(Printer.string_of_binop op)^", was given"^ "-----" ^" but a " ^ "-----" ^ " was expected.") line;
    (* 2 - Retornar operação tipada*)
    Tast.TEbinop(op, te1, te2, Tast.Ti32), Tast.Ti32
  | Ast.Beq | Ast.Bneq | Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge ->
    (* 1 - Verificar t1 e t2 *)
    if not (is_i32 t1) then error ("Wrong type given to operand "^(Printer.string_of_binop op)^", was given"^"-----"^" but a "^"-----"^" was expected.") line;
    if not (is_i32 t2) then error ("Wrong type given to operand "^(Printer.string_of_binop op)^", was given"^"-----"^" but a "^"-----"^" was expected.") line;
    (* 2 - Retornar operação tipada*)
    TEbinop(op, te1, te2, Tast.Tbool), Tast.Tbool
  | Ast.Bor | Ast.Band ->
    (* 1 - Verificar t1 e t2 *)
    if not (is_bool t1) then error ("Wrong type given to operand "^(Printer.string_of_binop op)^", was given"^"-----"^" but a "^"-----"^" was expected.") line;
    if not (is_bool t2) then error ("Wrong type given to operand "^(Printer.string_of_binop op)^", was given"^"-----"^" but a "^"-----"^" was expected.") line;
    (* 2 - Retornar operação tipada*)
    TEbinop(op, te1, te2, Tast.Tbool), Tast.Tbool
  | Ast.Bassign ->
    (* 1 - Verificar t1 e t2 *)
    if not (compare_prust_types (t1, t2)) then error ("Wrong type given to operand "^(Printer.string_of_binop op)^", was given"^Printer_tast.string_of_prust_types t1^" but a "^Printer_tast.string_of_prust_types t2^" was expected.") line;
    (* Todo: verificar se te1 é mutável *)
    if not (is_value_mut ctxs te1) then error ("Wrong type given to operand "^(Printer.string_of_binop op)^", was given "^Printer_tast.string_of_prust_types t1^" is not mutable.") line;
    (* 2 - Retornar operação tipada*)
    TEbinop(op, te1, te2, Tast.Tunit), Tast.Tunit

and type_unop_expr op te t line ctxs = match op with
  | Ast.Uneg ->
    (* 1 - Verificar t *)
    if not (is_i32 t) then error ("Wrong type given to operand "^(Printer.string_of_unop op)^", was given"^ "-----" ^" but a "^ "-----" ^" was expected.") line;
    (* 2 - Retornar operação tipada*)
    Tast.TEunop(op, te, Tast.Ti32), Tast.Ti32
  | Ast.Unot  ->
    (* 1 - Verificar t *)
    if not (is_bool t) then error ("Wrong type given to operand "^(Printer.string_of_unop op)^", was given"^"-----"^" but a "^"-----"^" was expected.") line;
    (* 2 - Retornar operação tipada*)
    Tast.TEunop(op, te, Tast.Tbool), Tast.Tbool
  | Ast.Uref ->
    (* 1 - Retornar operação tipada*)
    Tast.TEunop(op, te, Tast.Tref(t)), Tast.Tref(t)

  | Ast.Urefmut ->
    (* 1 - Retornar operação tipada *)
    if not (is_value_mut ctxs te) then error ("Wrong type given to operand "^(Printer.string_of_unop op)^", was given "^Printer_tast.string_of_prust_types t^" is not mutable.") line;

    Tast.TEunop(op, te, Tast.Trefmut(t)), Tast.Trefmut(t)
    
  | Ast.Uderef ->
    (* 1 - Retornar operação tipada *)
    if not ((is_refmut t) || (is_ref t)) then error ("Wrong type given to operand "^(Printer.string_of_unop op)^", was given"^"-----"^" but a "^"-----"^" was expected.") line;
    
    let t = match get_ref_type t with
     | None -> assert false
     | Some t -> t
    in

    Tast.TEunop(op, te, t), t

and type_expr ctxs ret = function
  | Ast.Eint(v, _) -> 
    Tast.TEint(v, Tast.Ti32), Tast.Ti32
  | Ebool(v, _) -> 
    Tast.TEbool(v, Tast.Tbool), Tast.Tbool
  | Eident(id, line) ->
    (* 1 - Ir buscar o CTX em que esta variável está declarada *)
    (* 2 - Retornar o seu tipo *)
    let t = match find_var_id id ctxs with
      | None     -> error ("The identifier " ^ id ^ " was not defined.") line
      | Some ctx -> snd (Hashtbl.find ctx id) 
    in
    TEident(id, t), t

  | Ebinop (op, e1, e2, line) ->
    (* 1 - Tipar e1 e2*) 
    let te1, t1 = type_expr ctxs ret e1 in
    let te2, t2 = type_expr ctxs ret e2 in

    (* 2 - Verificar as regras de tipos para cada conjunto de operadores *)
    type_binop_expr op te1 t1 te2 t2 line ctxs

  | Eunop (op, e, line) ->
    (* 1 - Tipar e *) 
    let te, t = type_expr ctxs ret e in
    
    (* 2 - Verificar as regras de tipos para cada conjunto de operadores *)
    type_unop_expr op te t line ctxs

  | Estruct_access(e, el, line) ->
    (* 1 - Tipar expressão *)
    let te, t = type_expr ctxs ret e in

    (* 2 - ir buscar o tipo estrutura *)
    let strct = match string_of_tstruct t with
      | None   -> error ("The structure " ^ Printer_tast.string_of_prust_types t ^ " was not defined.") line
      | Some s -> 
        begin match find_struct_id s ctxs with
          | None     -> error ("The structure with the identifier " ^ s ^ " was not defined.") line
          | Some ctx -> Hashtbl.find ctx s 
        end 
      in
    
    (* 3 - verificar se a estrutura tem o elemento el *)
    begin match find_struct_element el strct with
       | None   -> error ("Trying to access element "^ el ^" of struct "^ "----" ^" but this structure does not contain it.") line;
       | Some tel -> TEstruct_access(te, el, t, tel), tel
    end 

  | Elen (e, line) ->
    (* 1 - Tipar e *)
    let te, t = type_expr ctxs ret e in
    
    (* 2 - Verificar se e é do tipo vec *)
    (* Todo: Call tast printer *)
    if not (is_vec t) then error ("Trying to invoke len method with type  instead of Tvec.") line;

    TElen(te, Tast.Ti32), Tast.Ti32

  | Evec_access(e1, e2, line) ->
    (* 1 - Tipar e *)
    let te1, t1 = type_expr ctxs ret e1 in
    
    (* 2 - Verificar se e é do tipo vec *)
    (* Todo: Call tast printer *)
    if not (is_vec t1) then error ("Trying to use type "^ Printer_tast.string_of_prust_types t1 ^" as Tvec.") line;

    (* 3 - Tipar e2 *)
    let te2, t2 = type_expr ctxs ret e2 in
    if not (is_i32 t2) then error ("Trying to access element of vector with type"^ "----" ^".") line;

    let t = match get_vec_type t1 with 
     | None -> error ("Trying to access vec with no type.") line
     | Some t -> t
     in

    TEvec_access(te1, te2, t), t

  | Ecall(id, args, line) ->
    (* 1 - Verificar se a função existe *)
    let params, r = match find_fun_id id ctxs with
      | None -> error ("The function with identifier " ^ id ^ " was not defined.") line
      | Some ctx -> Hashtbl.find ctx id in

    (* 2 - Verificar os tipos dos argumentos *)
    if not ((List.length args) = (List.length params)) then error ("Invalid number of arguments given.") line;
    
    let typed_args = ref [] in
    let arg_types = ref [] in
    List.iteri(fun i e ->
      let ismut, arg_name, ta = List.nth params i in
      expected_vec_type := (get_vec_type ta);

      let te, t = type_expr ctxs ret e in

      if not (compare_prust_types (ta,t)) then error ("Invalid argumentin function "^id^" type was given "^Printer_tast.string_of_prust_types t^" but was expected a "^Printer_tast.string_of_prust_types ta^".") line;
      
      typed_args := !typed_args@[te];
      arg_types := !arg_types@[ismut, arg_name, ta]
    )args;

    expected_vec_type := None;

    let _ = match find_fun_id id ctxs with
      | None -> error ("The function with identifier " ^ id ^ " was not defined.") line
      | Some ctx -> Hashtbl.replace ctx id ((!arg_types), r)
    in

    TEcall(id, !typed_args, r), r
    
  | Evec_decl(els, line) ->
    (* 1 - O tipo da primeira expressão manda *)
    
    let t1 = match !expected_vec_type with
     | None -> (try snd(type_expr ctxs ret (List.hd els)) with _ -> Tast.Tempty)
     | Some t -> t
    in

    (* 1 - Tipar todas as expressões *)
    let l = (List.map(fun e ->  
       let te2, t2 = type_expr ctxs ret e in
       
       if not (compare_prust_types (t1, t2)) then error ("Invalid type in vec declaration was expecting "^Printer_tast.string_of_prust_types t1^" but was given "^ Printer_tast.string_of_prust_types t2 ^".") line;
       te2
    ) (els)) in

   TEvec_decl(l, (Tast.Tvec t1)), (Tast.Tvec t1)

  | Eprint(s, line) -> 
    Tast.TEprint(s, Tast.Tunit), Tast.Tunit
  | Eblock(b, line) ->
    let tb, t = type_block ((make_ctx ())::ctxs) ret b in

    TEblock(tb, t), t

  
(* Verificacao de uma instrucao - Instruções nao devolvem um valor *)
and type_stmt ctxs ret = function
  | Ast.Sif(e, bif, belse, line) ->
    (* 1 - Verificar o tipo de e *)
    let te, t = type_expr ctxs ret e in
    if not (is_bool t) then error ("Wrong type in the if condition, was given " ^ Printer_tast.string_of_prust_types t ^ " but a bool was expected.") line;

    (* 2 - Verificar o corpo do if *)
    let typed_body_if, tbif = type_block ((make_ctx ())::ctxs) ret bif in

    (* 3 - Verificar o corpo do else *)
    let typed_body_else, tbelse = type_block ((make_ctx ())::ctxs) ret belse in

    let _ = match (snd bif), (snd belse) with 
      | Some _, Some _ -> if not (compare_prust_types (tbif, tbelse)) then error ("Different types in the if branches, if body has return type "^Printer_tast.string_of_prust_types tbif^" and else body has return type "^Printer_tast.string_of_prust_types tbelse^".") line
      | Some _, None -> if not (compare_prust_types (tbif, Tast.Tunit)) then error ("Different types in the if branches, if body has return type "^Printer_tast.string_of_prust_types tbif^" and else body has return type "^Printer_tast.string_of_prust_types tbelse^".") line
      | None, None -> ()
      | _ -> error ("Different types in the if branches, if body has return type "^Printer_tast.string_of_prust_types tbif^" and else body has return type "^Printer_tast.string_of_prust_types tbelse^".") line
    in

    Tast.TSif(te, typed_body_if, typed_body_else, tbif), tbif

  | Swhile(e, body, line) ->
    (* 1 - Tipar e verificar a condição e *)
    let te1, t1 = type_expr ctxs ret e in
    if not (is_bool t1) then error ("Wrong type in the while condition, was given "^ "----" ^" but a bool was expected.") line;
    (* 2 - Tipar corpo do while *)
    let typed_body, tb = type_block ((make_ctx ())::ctxs) ret body in

    Tast.TSwhile(te1, typed_body, Tast.Tunit), Tast.Tunit

  | Sdeclare(ismut, id, e, line) ->
    (* 1 - Tipar e verificar a expressão e *)
    let te, t = type_expr ctxs ret e in
    
    (* 2 - Adicionar variável ao contexto *)
    let v_ctx,_,_ = (List.hd ctxs) in 
    
    Hashtbl.add v_ctx id (ismut, t);

    (* 3 - Retornar declaração tipada *)
    Tast.TSdeclare(ismut, id, te, Tast.Tunit), Tast.Tunit 
   
  | Sdeclare_struct(ismut, id, idt, el, line) ->

    (* 1 - Verificar se a estrutura idt existe *)
    let struct_els = match find_struct_id idt ctxs with
      | None     -> error ("The structure with the identifier "^idt^" was not defined.") line
      | Some ctx -> Hashtbl.find ctx idt
    in

    (* 2 - Tipar elementos *)
    if (List.length struct_els) <> (List.length el) then error ("The structure with the identifier "^id^" has "^(string_of_int (List.length struct_els))^" elements but were givin "^string_of_int(List.length el)^".") line;
      
      let typed_el = ref [] in

      let struct_els = List.sort (fun (id1,_) (id2,_) -> compare id1 id2) struct_els in

      let el = List.sort (fun (id1,_) (id2,_) -> compare id1 id2) el in

      List.iter2(fun (id1,t1) (id2,e2) -> 
        let te2, t2 = type_expr ctxs ret e2 in
        (* 2.1 - Verificar se os nomes de e2 estão corretos *)
        if id1 <> id2 then error ("Wrong name given in the declaration of a struct condition, was given "^id1^" but "^id2^" was expected.") line;
        (* 2.1 - Verificar se os tipos de e2 estão corretos *)
        if not (compare_prust_types (t1, t2)) then error ("Wrong type in the struct declare, was given " ^ Printer_tast.string_of_prust_types t2 ^ " but a " ^ Printer_tast.string_of_prust_types t1 ^ " was expected.") line;
        typed_el := (!typed_el)@[(id2,te2)]
      ) struct_els el;

    (* 3 - Adicionar variável ao contexto *)
    let v_ctx,_,_ = (List.hd ctxs) in 
    
    Hashtbl.add v_ctx id (ismut, (Tstruct idt));

    (* 3 - Retornar declaração tipada *)
    Tast.TSdeclare_struct(ismut, id, idt, !typed_el, Tast.Tunit), Tast.Tunit 

  | Sreturn (e, line) ->
    let te, t = match e with
      | None   -> None, Tast.Tunit
      | Some e -> 
      let te, t = type_expr ctxs ret e in 
      (* 2 - Comparar com tipo de retorno da função *)
      Some te, t
    in
    if not (compare_prust_types (ret, t)) then error ("Wrong type given to return statement was given "^Printer_tast.string_of_prust_types t^" but a "^Printer_tast.string_of_prust_types ret^" was expected.") line;

    Tast.TSreturn(te, t), t

  | Snothing _  -> 
    Tast.TSnothing(Tast.Tunit), Tast.Tunit
  | Sexpr(e, line) ->
    let te, _ = type_expr ctxs ret e in
    Tast.TSexpr(te, Tast.Tunit), Tast.Tunit

and type_block ctxs ret b = 
  let stmts, e = b in
  let r, t_stmts = List.fold_left_map (fun acc s -> let te, t = type_stmt ctxs ret s in t, te) Tast.Tunit stmts in

  let te, t = match e with
    | None   -> None, r
    | Some e -> 
      let s, t1 = type_expr ctxs ret e in 
      (Some s), t1
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
    
      let tt = type_prust_type ctxs t in
    
      if (compare_prust_types (Tstruct id, tt)) then error ("Error defining elemenet of struct "^id^" field "^e^" has infinite size.") line;
      if (compare_prust_types (Tref (Tstruct id), tt)) then error ("Error defining elemenet of struct "^id^" field "^e^" has infinite size.") line;
      if (compare_prust_types (Trefmut (Tstruct id), tt)) then error ("Error defining elemenet of struct "^id^" field "^e^" has infinite size.") line;
    
      Hashtbl.add v e (false, tt);

      (e, tt)

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
      let tt = type_prust_type ctxs t in

      Hashtbl.add v arg (ismut, tt);
      (ismut, arg, tt)
    ) args in 
    
    (* 2 - Verificar se o id já foi definido *)
    if not (is_id_unique id args_ctxs) then error ("The function with identifier " ^ id ^ " was already defined.") line;

    (* 3 - Verificar retorno *)
    let r = match r with
     | None   -> Tast.Tunit
     | Some t -> type_prust_type ctxs t
    in

    let _,f,_ = List.hd ctxs in
    Hashtbl.add f id (targs, r);

    (* 3 - Tipar corpo *)
    let typed_body, tb = type_block args_ctxs r body in
    
    (* 4 - Comparar tipo da expressão do corpo com o retorno *)
    if not (compare_prust_types (tb,r)) then error ("The function "^id^" has return type "^Printer_tast.string_of_prust_types r^" but is body has return type "^Printer_tast.string_of_prust_types tb^".") line;
  
    Tast.TDfun(id, targs, r, typed_body, Tast.Tunit)

    
(* Tipa uma AST *)
let type_file f =
  snd(List.fold_left_map(fun ctxs s ->
    let ctxs = (make_ctx ())::ctxs in
    ctxs, (type_decl ctxs s) 
  ) [] f)
