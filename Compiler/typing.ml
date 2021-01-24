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

let compare_prust_types = function
 | Tast.Ti32, Tast.Ti32 -> true
 | Tast.Tbool, Tast.Tbool -> true
 | Tast.Tstruct s1, Tast.Tstruct s2 -> s1 = s2
 | _ -> false

let is_bool = function
 | Tast.Tbool  -> true
 | _           -> false

let is_i32 = function
 | Tast.Ti32   -> true
 | _           -> false

let is_vec = function
 | Tast.Tvec _ -> true
 | _           -> false

let is_refmut = function
 | Tast.Trefmut _ -> true
 | _           -> false

let get_refmut_type = function
 | Tast.Trefmut t -> Some t
 | _              -> None

let string_of_tstruct = function 
  | Tast.Tstruct t  -> Some t
  | _              -> None

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

let rec is_value_mut ctxs = function
  | Tast.TEident (id, _) ->
    begin match find_var_id id ctxs with
      | None     -> error ("The identifier " ^ id ^ " was not defined.") (-1)
      | Some ctx -> fst (Hashtbl.find ctx id) end
  | TEunop (Uderef, _, t) ->
      begin
        match t with
        | Tast.Trefmut _ -> true
        | _ -> false
      end
  | _ -> false

let rec type_binop_expr op te1 t1 te2 t2 line = match op with
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
    if not (compare_prust_types (t1, t2)) then error ("Wrong type given to operand "^(Printer.string_of_binop op)^", was given"^"-----"^" but a "^"-----"^" was expected.") line;
    (* Todo: verificar se te1 é mutável *)
    (* 2 - Retornar operação tipada*)
    TEbinop(op, te1, te2, t1), t1

let rec type_unop_expr op te t line = match op with
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
    (* Todo: verificar se te é mutável *)
    Tast.TEunop(op, te, Tast.Trefmut(t)), Tast.Trefmut(t)
   | Ast.Uderef ->
    (* 1 - Retornar operação tipada *)
    if not (is_refmut t) then error ("Wrong type given to operand "^(Printer.string_of_unop op)^", was given"^"-----"^" but a "^"-----"^" was expected.") line;
    
    let t = match get_refmut_type t with
     | None -> assert false
     | Some t -> t
    in

    Tast.TEunop(op, te, t), t

let rec type_expr ctxs = function
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
    let te1, t1 = type_expr ctxs e1 in
    let te2, t2 = type_expr ctxs e2 in

    (* 2 - Verificar as regras de tipos para cada conjunto de operadores *)
    type_binop_expr op te1 t1 te2 t2 line

  | Eunop (op, e, line) ->
    (* 1 - Tipar e *) 
    let te, t = type_expr ctxs e in
    
    (* 2 - Verificar as regras de tipos para cada conjunto de operadores *)
    type_unop_expr op te t line

  | Estruct_access(e, el, line) ->
    (* 1 - Tipar expressão *)
    let te, t = type_expr ctxs e in

    (* 2 - ir buscar o tipo estrutura *)
    let strct = match string_of_tstruct t with
      | None   -> error ("The structure " ^ "----" ^ " was not defined.") line
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
    let te, t = type_expr ctxs e in
    
    (* 2 - Verificar se e é do tipo vec *)
    (* Todo: Call tast printer *)
    if not (is_vec t) then error ("Trying to invoke len method with type  instead of Tvec.") line;

    TElen(te, Tast.Ti32), Tast.Ti32

  | Evec_access(e1, e2, line) ->
    (* 1 - Tipar e *)
    let te1, t1 = type_expr ctxs e1 in
    
    (* 2 - Verificar se e é do tipo vec *)
    (* Todo: Call tast printer *)
    if not (is_vec t1) then error ("Trying to use type"^ "----" ^" has Tvec.") line;

    (* 3 - Tipar e2 *)
    let te2, t2 = type_expr ctxs e2 in
    if not (is_i32 t2) then error ("Trying to access element of vector with type"^ "----" ^".") line;

    TEvec_access(te1, te2, Tast.Ti32), Tast.Ti32

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
      let te, t = type_expr ctxs e in
      let ismut, arg_name, ta = List.nth params i in
      if not (compare_prust_types (ta,t)) then error ("Invalid argument type was given "^"----"^" but was expected a "^"------"^".") line;
      typed_args := !typed_args@[te];
      arg_types := !arg_types@[ismut, arg_name, ta]
    )args;

    let _ = match find_fun_id id ctxs with
      | None -> error ("The function with identifier " ^ id ^ " was not defined.") line
      | Some ctx -> Hashtbl.replace ctx id ((!arg_types), r)
    in

    TEcall(id, !typed_args, r), r
    
  | Evec_decl(els, line) ->
    (* 1 - O tipo da primeira expressão manda *)
    let te1, t1 = type_expr ctxs (List.hd els) in

    (* 1 - Tipar todas as expressões *)
    let l = te1::(List.map(fun e ->  
       let te2, t2 = type_expr ctxs e in
       if not (compare_prust_types (t1, t2)) then error ("Invalid type in vec declaration was expecting "^"----"^" but was given "^ "----" ^".") line;
       te2
    ) (List.tl els)) in

   TEvec_decl(l, (Tast.Tvec t1)), (Tast.Tvec t1)

  | Eprint(s, line) -> 
    Tast.TEprint(s, Tast.Tunit), Tast.Tunit
  | Eblock(b, line) ->
    let tb, t = type_block ((make_ctx ())::ctxs) b in

    TEblock(tb, t), t

  
(* Verificacao de uma instrucao - Instruções nao devolvem um valor *)
and type_stmt ctxs = function
  | Ast.Sif(e, bif, belse, line) ->
    (* 1 - Verificar o tipo de e *)
    let te, t = type_expr ctxs e in
    if not (is_bool t) then error ("Wrong type in the if condition, was given " ^ "---" ^ " but a bool was expected.") line;

    (* 2 - Verificar o corpo do if *)
    let typed_body_if, tbif = type_block ((make_ctx ())::ctxs) bif in

    (* 3 - Verificar o corpo do else *)
    let typed_body_else, tbelse = type_block ((make_ctx ())::ctxs) belse in

    Tast.TSif(te, typed_body_if, typed_body_else, Tast.Tunit), Tast.Tunit

  | Swhile(e, body, line) ->
    (* 1 - Tipar e verificar a condição e *)
    let te1, t1 = type_expr ctxs e in
    if not (is_bool t1) then error ("Wrong type in the while condition, was given "^ "----" ^" but a bool was expected.") line;
    (* 2 - Tipar corpo do while *)
    let typed_body, tb = type_block ((make_ctx ())::ctxs) body in

    Tast.TSwhile(te1, typed_body, Tast.Tunit), Tast.Tunit

  | Sdeclare(ismut, id, e, line) ->
    (* 1 - Tipar e verificar a expressão e *)
    let te, t = type_expr ctxs e in
    
    (* 2 - Adicionar variável ao contexto *)
    let v_ctx,_,_ = (List.hd ctxs) in 
    
    Hashtbl.add v_ctx id (ismut, t);

    (* 3 - Retornar declaração tipada *)
    Tast.TSdeclare(ismut, id, te, Tast.Tunit), Tast.Tunit 
   
  | Sdeclare_struct(ismut, id, idt, el, line) -> 
    (* 1 - Verificar se a estrutura idt existe *)
    let struct_els = match find_struct_id id ctxs with
      | None     -> error ("The structure with the identifier "^idt^" was not defined.") line
      | Some ctx -> Hashtbl.find ctx idt
    in

    (* 2 - Tipar elementos *)
    if (List.length struct_els) <> (List.length el) then error ("The structure with the identifier "^id^" has "^(string_of_int (List.length struct_els))^" elements but were givin "^string_of_int(List.length el)^".") line;
      
      let typed_el = ref [] in

      List.iter2(fun (id1,t1) (id2,e2) -> 
        let te2, t2 = type_expr ctxs e2 in
        (* 2.1 - Verificar se os nomes de e2 estão corretos *)
        if id1 <> id2 then error ("Wrong name given in the declaration of a struct condition, was given "^id1^" but "^id2^" was expected.") line;
        (* 2.1 - Verificar se os tipos de e2 estão corretos *)
        if not (compare_prust_types (t1, t2)) then error ("Wrong type in the if condition, was given " ^ "----" ^ " but a " ^ "----" ^ " was expected.") line;
        typed_el := (!typed_el)@[(id2,te2)]
      ) struct_els el;

    (* 3 - Adicionar variável ao contexto *)
    let v_ctx,_,_ = (List.hd ctxs) in 
    
    Hashtbl.add v_ctx id (ismut, (Tstruct idt));

    (* 3 - Retornar declaração tipada *)
    Tast.TSdeclare_struct(ismut, id, idt, !typed_el, Tast.Tunit), Tast.Tunit 

  | Sreturn (e, line) ->
    let te = match e with
      | None   -> None
      | Some e -> Some (fst (type_expr ctxs e))
    in

    Tast.TSreturn(te, Tast.Tunit), Tast.Tunit

  | Snothing _  -> 
    Tast.TSnothing(Tast.Tunit), Tast.Tunit
  | Sexpr(e, line) ->
    let te, _ = type_expr ctxs e in
    Tast.TSexpr(te, Tast.Tunit), Tast.Tunit

and type_block ctxs b = 
 let stmts, e = b in
 let t_stmts = List.map (fun s -> fst (type_stmt ctxs s))stmts in

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
      Hashtbl.add v e (false, tt);

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

      Hashtbl.add v arg (ismut, tt);
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
      TODO: if not (compare_crust_types (tb,r)) then error ("The function "^id^" has return type "^Printer.string_of_crust_types r^" but is body has return type "^Printer.string_of_crust_types tb^".") line;
    *)
    Tast.TDfun(id, targs, r, typed_body, Tast.Tunit)

    
(* Tipa uma AST *)
let type_file f =


  let tdcl = ref [] in
  let ctxs = ref [] in
  
  let out = ref "" in

  List.iter(fun s ->
    ctxs := (make_ctx ())::(!ctxs);
    tdcl := (!tdcl)@[type_decl !ctxs s];

    List.iter(fun (v,f,s) ->
      out := !out ^ "---VALUES: ";
      Hashtbl.iter(fun k v -> out := !out ^ ", " ^ k)v;
      out := !out ^ "---FUNCTIONS: ";
      Hashtbl.iter(fun k v -> out := !out ^ ", " ^ k)f;
      out := !out ^ "---STRUCTS: ";
      Hashtbl.iter(fun k v -> out := !out ^ ", " ^ k)s;
          out := !out ^ "\n";

    )!ctxs;
    
    Printf.eprintf "CTXS: %s\n" !out;
  ) f;
  !tdcl

  
  (*List.fold_left_map(fun ctxs s ->
    let ctxs = (make_ctx ())::ctxs in
    ctxs, (type_decl ctxs s) 
  ) [] f
*)