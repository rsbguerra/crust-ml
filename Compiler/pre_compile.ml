open Past
open Tast
open Ast

exception Error of string

let error s  = raise (Error s)

(* table_ctx representa um scope, contexto *)
type tbl_variables_ctx = (string, int * int) Hashtbl.t
type tbl_functions_ctx = (string, int) Hashtbl.t
type tbl_structs_ctx = (string, ((string * Ast.crust_types * int) list * int)) Hashtbl.t

type tbl_ctx = tbl_variables_ctx * tbl_functions_ctx * tbl_structs_ctx

let make_ctx() =
  let v = (Hashtbl.create 16 : tbl_variables_ctx) in
  let f = (Hashtbl.create 16 : tbl_functions_ctx) in
  let s = (Hashtbl.create 16 : tbl_structs_ctx) in
  (v, f, s)

let var_ctx    = fun (c, _, _) -> c
let fun_ctx    = fun (_, c, _) -> c
let struct_ctx = fun (_, _, c) -> c

let var_ctx_hd l = var_ctx (List.hd l)
let fun_ctx_hd l = fun_ctx (List.hd l)
let struct_ctx_hd l = struct_ctx (List.hd l)

let rec get_struct_type s = function
  | Tast.Tstruct t  -> t
  | Tast.Tref t     -> get_struct_type t
  | Tast.Trefmut t  -> get_struct_type t
  | _               -> assert false


let get_var_id ctxs id = 
  let ctx = List.find(fun ctx -> Hashtbl.mem (var_ctx ctx) id) in
  Hashtbl.find ctx id
let get_fun_id ctxs id = 
  let ctx = List.find(fun ctx -> Hashtbl.mem (fun_ctx ctx) id) in
  Hashtbl.find ctx id
let get_struct_id ctxs id = 
  let ctx = List.find(fun ctx -> Hashtbl.mem (struct_ctx ctx) id) in
  Hashtbl.find ctx id

let rec get_ident_type_elements ctxs pos sz = function
  | Ti32 | Tbool | Tunit | Tempty | Tref _ -> [pos]
  | Tstruct s -> 
    (* 2.1 - Get struct elements *)
    let struct_els, _ = get_var_id ctxs id in
    (* 2.1.2 - Calculate pos of each element *)
    List.map(fun (id, t, r_pos) -> pos + r_pos) struct_els
  | Tvec(t, _) ->
    let curr_pos = ref pos in
    let out = ref [] in 
    for i=0 to (sz-1) do
      out := (!out)@[(!curr_pos)]; 
      curr_pos := !curr_pos + (get_type_size ctxs t)
    done;
    !out
  | Trefmut t -> get_ident_type_elements ctxs pos sz t

and get_type_size ctxs = function
  | Ti32 | Tbool -> 8
  | Tunit -> 0
  | Tstruct s -> snd(get_struct_id ctxs s)
  | Tvec (t, sz) -> (get_type_size ctxs t) * sz
  | Tref (t, id) -> get_type_size ctxs t
  | Tmut t -> get_type_size ctxs t

and get_type_start ctxs = function
  | Ti32 | Tbool | Tunit -> 0
  | Tstruct s -> -snd (get_struct_id ctxs s)
  | Tvec (t, _) | Tref (t, _) | Trefmut t -> get_type_start ctxs t
  

  
let find_struct_element el s = 
  let _,_,pos = List.find (fun (id, _, _) -> id = el) s in pos


(* ---- *)


let rec pcompile_expr ctxs next = function
  | TEint (n, _) ->
    PEint n, next
  | TEbool (b, _) -> 
    PEbool b, next
  | TEident (id, t) -> 
    (* 1 - Posição da variável *)
    let pos, size = get_var_pos id ctxs in
    (* 2 - Obter lista de posições de variável *)
    let pos_list = get_ident_type_elements ctxs pos size t in
    PEident(id, pos_list), next
  | TEunop (op, e, t) -> 
    let pe, next = pcompile_expr ctxs next e in
    PEunop(op, pe), next
  
  | TEbinop (op, e1, e2, t) -> 
    let e1, fp1 = pcompile_expr ctxs next e1 in
    let e2, fp2 = pcompile_expr ctxs next e2 in
    let fp = max fp1 fp2 in
    PEbinop(op, e1, e2), fp
  
  | TEstruct_access(e1, id, tid, tel) ->
    let struct_id = get_struct_type tid in
    (* 1. Buscar posição de id na pilha *)
    let id_pos,_ = get_var_id ctxs id in
    (* 2. Buscar a posição do el relativo ao primeiro elemento*)
    let struct_els, _ = get_struct_id ctxs id in
    let el_pos = find_struct_element el struct_els in

    (* if id_pos > 0 then is an arg*)
    let final_pos = if id_pos > 0 then (id_pos+abs(el_pos)) else (id_pos+el_pos) in

    PEstrc_access(id, el, final_pos), next
  
  | TElen(id,_) ->
    (* 1 - get id size *)
    let _, size = Hashtbl.find (find_var_id id ctxs) id in
    PElen(size), next
  | TEvec_access (e1, e2, t) ->
    (* 1 - Precompilar exprs e1 e obter primeira pos do vetor a*)
    let pe1, next = pcompile_expr ctxs next e in
    (* 2 - Precompilar e2 - pos a aceder *)
    let pe2, next = pcompile_expr ctxs next e in

    (* TODO:
    - PEvec_access(e1, e2, type_size, pos_inicial, n_elem)  
    - porque de passar tamanho de vetor para ve_access?
      -> ver compile.ml, linha 625 - var sz não é usada *)

    (* 3 - Ir buscar a posição: a[5] = pos(a) + (5 * size(a)) *)
    PEvec_access(pe1, pe2, (get_type_size ctxs te), id_pos), next

  | TEcall (id, args, t) ->
    let exprs, size, fpmax = 
      List.fold_left (fun (l, sz, fpmax) (el, te) ->
        let e, fp = pcompile_expr ctxs fpmax el in
        (e::l, sz+(get_type_size ctxs te), max fp fpmax)
      ) ([], 0, next) args
      in
    PEcall(id, exprs, size), fpmax 
  | TEvec_decl (els, t) ->
    let type_size = (get_type_size ctxs t) / (List.length els)  in
    
    (* 1. Precompilar lista de expressões *)
    let next, p_els = List.fold_right(fun e (next, l) -> 
      let p_e, next = pcompile_expr ctxs next e in
      (next+type_size), (p_e, (-next))::l 
    )els (next, []) in
    
    PEvec_decl(p_els, snd(List.hd p_els)), next
  | TEprint (s, t) -> 
    let ep, next = (pcompile_expr ctxs next e) in
    PSprint (ep, t), next
  | TEblock (b, t) -> 
    let pb, next = pcompile_block_stmt ((make_ctx())::ctxs) next stmts in
    PSblock pblock, next

and pcompile_block ctxs next (body, e, _) =
  (* 1 - precompilar stmts do bloco *)
  let next, p_body = List.fold_left_map (fun next s -> 
    let p_body, next = pcompile_stmt ctxs next s in
    next, p_body) next body in
  (* 2 - se existir, precompilar expressão *)
  let e = match e with
    | Some e -> Some (pcompile_expr ctxs next e)
    | None -> None
  (p_body, e), next

and pcompile_stmt ctxs next = function
  | TSnothing _ -> PSnothing, next 
  | TSexpr (e, _) -> 
    let ep, next = (pcompile_expr ctxs next e) in
    PSexpr (ep, next)
  | TSdeclare (mut, id, e, t) -> 
    (* Size of vector: (previus_next + next) / size(t) *) 
    let previus_next = next in
    (* 1 -  Pré compilar expressão*)
    let ep, next = (pcompile_expr ctxs next e) in
    let start_pos = next + (get_type_start ctxs t) in
    
    let new_next = (get_type_start ctxs t) + (get_type_size ctxs t) in

    (* 2 - Adicionar todos os elementos *)
    let pos_list, sz = get_type_elements ctxs start_pos previus_next next t in

    (* 3 - Adicionar inicio ao frame *)
    Hashtbl.add (var_ctx_hd ctxs) id ((List.hd pos_list), sz);

    PSdeclare (id, t, ep, pos_list), (new_next+next)

  | TSdeclare_struct (mut, id, id, el, t) ->
    (* 1. Precompilação de pares *)
    let next, p_els = List.fold_left_map(
      fun next (el, t_el, t_e) -> 
        (* 3. Precompilar expressão t_el *)
        let p_el, next = pcompile_expr ctxs next t_el in        
        (* Calcular a posição da expressão calculada *)

        (next+(get_type_size ctxs t_e)), (el, p_el, (-(next + (get_type_start ctxs t_e) )))
    ) next pairs in

    PEstrc_decl (id, p_els, -(next + (get_type_start ctxs t))), next


  | Twhile(e, b, _) -> 
    let pe, next = pcompile_expr ctxs next e in
    let pb, next = pcompile_stmt ((make_ctx())::ctxs) next s in
    Pswhile(pe, pb), next
    
  | TSreturn (e, t) -> (* 1 - How many values are there to return? *)
    let ep, next = (pcompile_expr ctxs next e) in
    let start_pos = next + (get_type_start ctxs t) in

    (* 2 - Adicionar todos os elementos *)
    let pos_list =  match t with
      | Ti32 | Tbool | Tunit -> [-start_pos]
      | Tstruct s ->
        (* 3.1 - Get struct elements *)
        let struct_els = fst(get_struct_id ctxs s) in
        (* 3.1.2 - Calculate pos of each element *)
        List.filter_map(fun (id, t, r_pos) -> if r_pos <> 0 then Some (-(abs(start_pos) + abs(r_pos))) else None)struct_els
      | Tvec (t, _) -> [-start_pos]
      | Tref (t, _) -> [-start_pos]
      | Tmut t ->      [-start_pos]
    in
    PSreturn (ep, pos_list), next

  | TSif (e, b1, b2, _) ->
    let pe,  next  = pcompile_expr ctxs next e in
    let pb1, next  = pcompile_stmt ((make_ctx())::ctxs) next s in
    let pb2, next  = pcompile_stmt ((make_ctx())::ctxs) next s in
    PSif(pe, pb1, pb2), next
and pcompile_decl ctx next =
  | TDstruct (id, els, _) -> 
    (* 1 - Calcular espaço na memória*)
    let next, pcompiled_fields = 
    List.fold_left_map(fun next (id, t) -> (next+(get_type_size ctxs t)), (id, t, (-(next)))) 0 args in

    (* 2 - Adiocionar identificador da struct ao contexto *)
    Hashtbl.add (struct_ctx_hd ctxs) id (pcompiled_fields, next);

    PGSstruct(id, pcompiled_fields, next)

  | TDfun (f, args, t, b, _) -> 
    (* TODO: por esta função global *)
    let new_ctxs = (make_ctx())::ctxs in

      (* 1- Precompilar argumentos *)
      let p_next, p_args = List.fold_left_map(
        fun next (arg, t_arg) -> 
          Hashtbl.add (var_ctx_hd new_ctxs) arg (next, 1);
          (next+(get_type_size ctxs t_arg)), (arg, t_arg, next)
      ) 16 args in

      (* 2 - Pre compilar corpo *)
      let pb, next = pcompile_block new_ctxs 8 b in
      Hashtbl.replace (fun_ctx_hd new_ctxs) f next;
      
      PGSfunction(f, p_args, t, p_stmt, next)

(* TODO *)
and pcompile_program = ""

let precompile = pcompile_global_stmt [make_ctx()]
