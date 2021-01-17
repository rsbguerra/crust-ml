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

let id_exists id =
  List.exists(fun (v,f,s) -> Hashtbl.mem v id || Hashtbl.mem f id || Hashtbl.mem s id)

let find_id id =
  List.find_map(fun (v,f,s) -> 
      match Hashtbl.find_opt v id with | Some x -> x | None ->
      match Hashtbl.find_opt f id with | Some x -> x | None ->
      match Hashtbl.find_opt s id with | Some x -> x | None -> None)

let rec find_var_id id = function
  | []     -> assert false
  | (ct,_,_)::tl -> if Hashtbl.mem ct id then ct else (find_var_id id tl) 

let rec find_fun_id id = function
  | []     -> assert false
  | (_,ct,_)::tl -> if Hashtbl.mem ct id then ct else (find_fun_id id tl) 

let rec find_struct_id id = function
  | []     -> assert false
  | (_,_,ct)::tl -> 
    if Hashtbl.mem ct id then ct else (find_struct_id id tl) 

let rec find_struct_element el = function
  | []         -> assert false
  | (id, t, pos)::tl -> if id = el then pos else (find_struct_element el tl)

let rec string_of_tstruct = function 
  | Ast.Tstruct(t) -> t
  | Ast.Tref(t, _) -> string_of_tstruct t
  | Ast.Tmut(t)    -> string_of_tstruct t
  | _               -> assert false

let rec get_type_start ctxs = function
  | Ti32 | Tbool | Tunit -> 0
  | Tstruct s -> -snd(Hashtbl.find (find_struct_id s ctxs) s) 
  | Tvec (t, _) | Tref (t, _) | Tmut t -> get_type_start ctxs t
  

let rec get_type_size ctxs = function
  | Ti32 | Tbool -> 8
  | Tunit -> 0
  | Tstruct s -> snd(Hashtbl.find (find_struct_id s ctxs) s)
  | Tvec (t, sz) -> (get_type_size ctxs t) * sz
  | Tref (t, id) -> get_type_size ctxs t
  | Tmut t -> get_type_size ctxs t

let rec get_type_elements ctxs start_pos previus_next next = function
  | Ti32 | Tbool | Tunit -> [-start_pos], 1
  | Tstruct s ->
    (* 2.1 - Get struct elements *)
    let struct_els = fst(Hashtbl.find (find_struct_id s ctxs) s) in
    (* 2.1.2 - Calculate pos of each element *)
    let a, l  = List.fold_left_map(fun a (id, t, r_pos) -> (a+1), -(start_pos + r_pos)) 0 struct_els in
    l, a
  | Tvec (t, _) ->
    let size = (next - previus_next) / (get_type_size ctxs t) in
    let curr_pos = ref previus_next in
    let out = ref [] in 
    for i=0 to (size-1) do
      out := (!out)@[-(!curr_pos)]; 
      curr_pos := !curr_pos + (get_type_size ctxs t)
    done;
    (List.rev !out), size
  | Tref (t, id) ->
    let pos  = try fst(Hashtbl.find (find_var_id id ctxs) id) with _ -> start_pos in
    get_type_elements ctxs (abs pos)  previus_next next t
  | Tmut t -> get_type_elements ctxs start_pos previus_next next t

let rec get_ident_type_elements ctxs pos sz = function
  | Ti32 | Tbool | Tunit | Tref _ -> [pos]
  | Tstruct s -> 
    (* 2.1 - Get struct elements *)
    let struct_els = fst(Hashtbl.find (find_struct_id s ctxs) s) in
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
  | Tmut t -> get_ident_type_elements ctxs pos sz t

let rec pcompile_expr ctxs next = function
  | TEcst (c, t) -> 
    PEcst c, next
  | TEident (id, t) ->
    (* 1 - Posição da variável *)
    let pos, sz = Hashtbl.find (find_var_id id ctxs) id in
    (* 2 - Obter lista de posições de variável *)
    let pos_list = get_ident_type_elements ctxs pos sz t in
    PEident(id, pos_list), next

  | TEref(id, _) -> 
    (* 1 - Posição da variável *)
    let pos, sz = Hashtbl.find (find_var_id id ctxs) id in
    PEref(pos), next

  | TErefmut(id, _) ->
    (* 1 - Posição da variável *)
    let pos, sz = Hashtbl.find (find_var_id id ctxs) id in
    PErefmut(pos), next
  | TEptr(id, _) ->
    (* 1 - Posição da variável *)
    let pos, sz = Hashtbl.find (find_var_id id ctxs) id in
    PEptr(pos), next

  | TEbinop (op, e1, e2, t) -> 
    let e1, fp1 = pcompile_expr ctxs next e1 in
    let e2, fp2 = pcompile_expr ctxs next e2 in
    let fp = max fp1 fp2 in
    PEbinop(op, e1, e2), fp

  | TEunop (op, e, t) -> 
    let pe, next = pcompile_expr ctxs next e in
    PEunop(op, pe), next

  | TElen id ->
    (* 1 - get id size *)
    let _, sz = Hashtbl.find (find_var_id id ctxs) id in
    PElen(sz), next

  | TEcall (id, args, t) ->
    let exprs, size, fpmax = 
      List.fold_left (fun (l, sz, fpmax) (el, te) ->
        let e, fp = pcompile_expr ctxs fpmax el in
        (e::l, sz+(get_type_size ctxs te), max fp fpmax)
      ) ([], 0, next) args
      in
    PEcall(id, exprs, size), fpmax

  | TEstrc_decl (id, pairs, t) -> 
    (* 1. Precompilação de pares *)
    let next, p_els = List.fold_left_map(
      fun next (el, t_el, t_e) -> 
        (* 3. Precompilar expressão t_el *)
        let p_el, next = pcompile_expr ctxs next t_el in
        (next+(get_type_size ctxs t_e)), (el, p_el, (-next))
    ) next pairs in
    
    PEstrc_decl (id, p_els, -(next + (get_type_start ctxs t))), next

  | TEstrc_access (id, el, tid, tel) ->
    let struct_id = string_of_tstruct tid in
    (* 1. Buscar posição de id na pilha *)
    let id_pos,_ = Hashtbl.find (find_var_id id ctxs) id in
    (* 2. Buscar a posição do el relativo ao primeiro elemento*)
    let struct_els = fst(Hashtbl.find (find_struct_id struct_id ctxs) struct_id) in
    let el_pos = find_struct_element el struct_els in

    (* if id_pos > 0 then is an arg*)
    let final_pos = if id_pos > 0 then (id_pos+abs(el_pos)) else (id_pos+el_pos) in
    PEstrc_access(id, el, final_pos), next
  | TEvec_decl (els, t) ->
    (* 1. Precompilar lista de expressões *)
    let type_size = (get_type_size ctxs t) / (List.length els)  in
    
    let next, p_els = List.fold_right(fun e (next, l) -> 
      let p_e, next = pcompile_expr ctxs next e in
      (next+type_size), (p_e, (-next))::l 
    )els (next, []) in
    
    PEvec_decl(p_els, snd(List.hd p_els)), next

  | TEvec_access(id, e, te, tid) ->
    (* 1 - Ir buscar a posição inicial de id *)
    let id_pos, sz = Hashtbl.find (find_var_id id ctxs) id in
    (* 2 - Précompilar el *)
    let pe, next = pcompile_expr ctxs next e in
    (* 3 - Ir buscar a posição: a[5] = pos(a) + (5 * size(a)) *)
    PEvec_access(id, pe, (get_type_size ctxs te), id_pos, sz), next

and pcompile_stmt ctxs next = function
  | TSif (e, s, elif, _) ->
    let pe, next  = pcompile_expr ctxs next e in
    let ps, next  = pcompile_stmt ((make_ctx())::ctxs) next s in
    let next, if_list = List.fold_left_map(
      fun next (if_expr, if_stmt) ->
        (* 1. Pre-compilar condição *)
        let p_elif, next = pcompile_expr ctxs next if_expr in
        (* 2. Pre-compilar stmts do corpo *)
        let p_body_elif, next = pcompile_stmt ((make_ctx())::ctxs) next if_stmt in
        (* 3. Devolver corpo tipado e último next *)
        (next, (p_elif, p_body_elif))
    ) next elif in

    PSif(pe, ps, if_list), next

  | TSwhile (e, s, _) -> 
    let pe, next = pcompile_expr ctxs next e in
    let ps, next = pcompile_stmt ((make_ctx())::ctxs) next s in
    
    PSwhile (pe, ps), next

  | TSdeclare (id, t, e, _) ->
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

  | TSassign (id, e, _) ->
    let ep, next = (pcompile_expr ctxs next e) in
    let pos, _ = Hashtbl.find (find_var_id id ctxs) id in

    PSassign (id, ep, pos), next

  | TSptr_assign (id, e, _) ->
    let ep, next = (pcompile_expr ctxs next e) in
    let pos, _ = Hashtbl.find (find_var_id id ctxs) id in

    PSptr_assign (id, ep, pos), next

  | TSprintn (e, t, _) -> 
      let ep, next = (pcompile_expr ctxs next e) in
      PSprintn (ep, t), next
  | TSprint (e, t, _) ->
       let ep, next = (pcompile_expr ctxs next e) in
      PSprint (ep, t), next
  | TSblock (stmts, _) -> 
    let pblock, next = pcompile_block_stmt ((make_ctx())::ctxs) next stmts in
    PSblock pblock, next
  | TSreturn (e, t) ->
    (* 1 - How many values are there to return? *)
    let ep, next = (pcompile_expr ctxs next e) in
    let start_pos = next + (get_type_start ctxs t) in

    (* 2 - Adicionar todos os elementos *)
    let pos_list =  match t with
      | Ti32 | Tbool | Tunit -> [-start_pos]
      | Tstruct s ->
        (* 3.1 - Get struct elements *)
        let struct_els = fst(Hashtbl.find (find_struct_id s ctxs) s) in
        (* 3.1.2 - Calculate pos of each element *)
        List.filter_map(fun (id, t, r_pos) -> if r_pos <> 0 then Some (-(abs(start_pos) + abs(r_pos))) else None)struct_els
      | Tvec (t, _) -> [-start_pos]
      | Tref (t, _) -> [-start_pos]
      | Tmut t ->      [-start_pos]
    in
    PSreturn (ep, pos_list), next
  | TSexpr (e, _) -> 
      let ep, next = (pcompile_expr ctxs next e) in
      PSexpr ep, next
  | TScontinue _ -> PScontinue, next
  | TSbreak _    -> PSbreak, next
  | TSnothing _  -> PSnothing, next

and pcompile_block_stmt ctxs next block_stmt = 
  let next, p_body = List.fold_left_map (fun next s -> 
    let p_body, next = pcompile_stmt ctxs next s in
    next, p_body) next block_stmt in
    p_body, next

and pcompile_block_global_stmt ctxs acc = function
  | [] -> acc
  | s :: sl -> (pcompile_block_global_stmt ctxs (acc@[pcompile_global_stmt ctxs s]) sl)

and pcompile_global_stmt ctxs = function
  | TGSblock stmts -> 
    
    PGSblock (pcompile_block_global_stmt ctxs [] stmts)
    (* PGSblock (List.map (fun s -> pcompile_global_stmt ((make_ctx())::ctxs) s) stmts) *)
  | TGSfunction (x, args, t, stmt) ->
      let new_ctxs = (make_ctx())::ctxs in

      (* 1- Precompilar argumentos *)
      let p_next, p_args = List.fold_left_map(
        fun next (arg, t_arg) -> 
          Hashtbl.add (var_ctx_hd new_ctxs) arg (next, 1);
          (next+(get_type_size ctxs t_arg)), (arg, t_arg, next)
      ) 16 args in

      (* 2 - Pre compilar corpo *)
      let p_stmt, next = pcompile_stmt new_ctxs 8 stmt in
      Hashtbl.replace (fun_ctx_hd new_ctxs) x next;
      
      PGSfunction(x, p_args, t, p_stmt, next)
  | TGSstruct (id, args) -> 
    (* 1 - Adiocionar identificador da struct ao contexto *)
    (* 2 - Calcular espaço na memória*)
    let next, pcompiled_fields = 
      List.fold_left_map(fun next (id, t) -> 
        (next+(get_type_size ctxs t)), (id, t, (-next))) 0 args in
    Hashtbl.add (struct_ctx_hd ctxs) id (pcompiled_fields, next);

    
    PGSstruct(id, pcompiled_fields, next)


let precompile = pcompile_global_stmt [make_ctx()]
