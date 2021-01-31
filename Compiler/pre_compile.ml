open Past
open Tast
open Ast

exception Error of string

let error s  = raise (Error s)

(* table_ctx representa um scope, contexto *)
type tbl_variables_ctx = (string, int * int) Hashtbl.t
type tbl_functions_ctx = (string, Past.argument list * int) Hashtbl.t
type tbl_structs_ctx   = (string, Past.pair list * int) Hashtbl.t

type tbl_ctx = tbl_variables_ctx * tbl_functions_ctx * tbl_structs_ctx

let make_ctx() =
  let v = (Hashtbl.create 16 : tbl_variables_ctx) in
  let f = (Hashtbl.create 16 : tbl_functions_ctx) in
  let s = (Hashtbl.create 16 : tbl_structs_ctx) in
  (v, f, s)

let var_ctx    = fun (c, _, _) -> c
let fun_ctx    = fun (_, c, _) -> c
let struct_ctx = fun (_, _, c) -> c

and get_fun_arguments_type ctxs id =
  let ctx = List.find(fun ctx -> Hashtbl.mem (fun_ctx ctx) id) ctxs in
  let args,_ = Hashtbl.find (fun_ctx ctx) id in 
  List.map(fun (_,_,t,_) -> t) args

let var_ctx_hd l = var_ctx (List.hd l)
let fun_ctx_hd l = fun_ctx (List.hd l)
let struct_ctx_hd l = struct_ctx (List.hd l)

let rec get_struct_type = function
  | Tast.Tstruct t -> t
  | Tast.Tref t    -> get_struct_type t
  | Tast.Trefmut t -> get_struct_type t
  | _              -> assert false

and find_var_id ctxs id = 
  let ctx,_,_ = List.find(fun (v, _, _) -> Hashtbl.mem v id) ctxs in
  Hashtbl.find ctx id
and find_fun_id ctxs id = 
  let _,ctx,_ = List.find(fun ctx -> Hashtbl.mem (fun_ctx ctx) id) ctxs in
  Hashtbl.find ctx id
and find_struct_id ctxs id = 
  let _,_,ctx = List.find(fun ctx -> Hashtbl.mem (struct_ctx ctx) id) ctxs in
  Hashtbl.find ctx id

and find_struct_element el s = 
  let _,_,pos = List.find (fun (id, _, _) -> id = el) s in pos

and get_type_elements ctxs id start_pos previus_next next = function
  | Ti32 | Tbool | Tunit | Tempty -> [-start_pos], 1
  | Tstruct s ->
    (* 2.1 - Get struct elements *)
    let struct_els = fst(find_struct_id ctxs s) in
    (* 2.1.2 - Calculate pos of each element *)
    
    let l = ref [] in

    List.iter(fun (id, t, r_pos) -> 
       let tmp, _ = (get_type_elements ctxs id (start_pos + abs(r_pos)) previus_next next t) in
       l := !l@tmp;

      ) struct_els;
    
    List.iter(fun pos -> ()
    ) !l;
    !l, (List.length !l)
  | Tvec t ->
    let pt = pcompile_type t in
    let type_size = get_type_size ctxs pt in
    let size = (next - previus_next) / type_size in
    let curr_pos = ref previus_next in
    let out = ref [] in 
    for i=0 to (size-1) do
      out := (!out)@[-(!curr_pos)]; 
      curr_pos := !curr_pos + type_size
    done;
    (List.rev !out), size
  | Tref(Trefmut _) -> 
    let pos, sz = find_var_id ctxs id in
    [pos], sz 
  | Tref t ->
    let pos = try fst(find_var_id ctxs id) with _ -> start_pos in
    get_type_elements ctxs id (abs pos) previus_next next t
  | Trefmut t -> get_type_elements ctxs id start_pos previus_next next t

(* and get_ident_type_elements ctxs pos sz = function
  | Ti32 | Tbool | Tunit | Tempty | Tref _ -> [pos]
  | Tstruct s -> 
    (* 2.1 - Get struct elements *)
    let struct_els, _ = find_struct_id ctxs s in
    (* 2.1.2 - Calculate pos of each element *)
    List.map(fun (id, t, r_pos) -> pos + r_pos) struct_els
  | Tvec t ->
    let pt = pcompile_type t in
    let curr_pos = ref pos in
    let out = ref [] in 
    for i=0 to (sz-1) do
      out := (!out)@[(!curr_pos)]; 
      curr_pos := !curr_pos + (get_type_size ctxs pt)
    done;
    !out
  | Trefmut t -> get_ident_type_elements ctxs pos sz t *)

and get_type_size ctxs = function
  | PTi32 | PTbool -> 8
  | PTunit | PTempty-> 0
  | PTstruct s -> snd(find_struct_id ctxs s)
  | PTvec t | PTref t |PTrefmut t -> get_type_size ctxs t

and get_type_start ctxs = function
  | PTi32 | PTbool | PTunit | PTempty -> 0
  | PTstruct s -> -(snd (find_struct_id ctxs s))
  | PTvec t | PTref t | PTrefmut t -> get_type_start ctxs t

and get_type_from_struct_el (ctxs:tbl_structs_ctx) id = 
  let s_el, _ = find_struct_id ctxs id in
  let (_,_,el_t,_) = List.find (fun (_,s_id,_,_) -> s_id = id) s_el in el_t 

  (* ---- *)
  
and pcompile_type = function
  | Tunit -> PTunit
  | Ti32 -> PTi32
  | Tbool -> PTbool
  | Tempty -> PTempty
  | Tstruct s -> PTstruct s
  | Tvec t -> PTvec (pcompile_type t)
  | Tref t -> PTref (pcompile_type t)
  | Trefmut t -> PTrefmut (pcompile_type t)

and pcompile_expr ctxs next = function
  | TEint (n, _) ->
    PEint (n, next), next
  | TEbool (b, _) -> 
    PEbool (b, next), next
  | TEident (id, _) -> 
    (* 1 - Posição da variável *)
    let pos, size = find_var_id ctxs id in
    (* 2 - Obter lista de posições de variável *)
    (* let pos_list = get_ident_type_elements ctxs pos size t in *)
    PEident(id, pos), next
  | TEunop (op, e, _) -> 
    let pe, next = pcompile_expr ctxs next e in
    PEunop(op, pe), next
  
  | TEbinop (op, e1, e2, _) -> 
    let e1, fp1 = pcompile_expr ctxs next e1 in
    let e2, fp2 = pcompile_expr ctxs next e2 in
    let fp = max fp1 fp2 in
    PEbinop(op, e1, e2), fp
  
  | TEstruct_access(e, id, tid, _) ->
    let pe, next = pcompile_expr ctxs next e in
    let struct_id = get_struct_type tid in

    (* 1. Buscar posição de id na pilha *)
    let id_pos,_ = find_var_id ctxs id in
    (* 2. Buscar a posição do el relativo ao primeiro elemento*)
    let struct_els, _ = find_struct_id ctxs struct_id in
    let el_pos = find_struct_element id struct_els in

    (* if id_pos > 0 then is an arg*)
    let final_pos = if id_pos > 0 then (id_pos+abs(el_pos)) else (id_pos+el_pos) in

    PEstruct_access (pe, id, final_pos), next
  
  | TElen(e,_) ->
    let pe, next = pcompile_expr ctxs next e in
    PElen pe, next
  
  | TEvec_access (e1, e2, t) ->
    (* 1 - Precompilar exprs e1 e obter primeira pos do vetor a*)
    let pe1, next = pcompile_expr ctxs next e1 in
    (* 2 - Precompilar e2 - pos a aceder *)
    let pe2, next = pcompile_expr ctxs next e2 in
    let pt = pcompile_type t in
    (* TODO:
    - porque de passar tamanho de vetor para ve_access?
    -> ver compile.ml, linha 625 - var sz não é usada *)
    
    let id_pos = get_type_start ctxs pt in

    (* PEvec_access(e1, e2, type_size, pos_inicial)   *)
    
    (* 3 - Ir buscar a posição: a[5] = pos(a) + (5 * size(a)) *)
    PEvec_access(pe1, pe2, (get_type_size ctxs pt), id_pos), next

  | TEcall (id, args, t) ->
    let args_type = get_fun_arguments_type ctxs id in
    let pt = pcompile_type t in

    let exprs, size, fpmax = 
      List.fold_left2 (fun (l, sz, fpmax) el t ->
        let e, fp = pcompile_expr ctxs fpmax el in
        (e::l, sz+(get_type_size ctxs pt), max fp fpmax)
      ) ([], 0, next) args args_type
      in
    PEcall(id, exprs, size), fpmax 

  | TEvec_decl (els, t) ->
    let pt = pcompile_type t in
    let type_size = (get_type_size ctxs pt) / (List.length els)  in
    
    (* 1. Precompilar lista de expressões *)
    let next, p_els = List.fold_right(fun e (next, l) -> 
      let p_e, next = pcompile_expr ctxs next e in
      (next+type_size), (p_e, (-next))::l 
    )els (next, []) in
    
    PEvec_decl(p_els, snd(List.hd p_els)), next
  
  | TEprint (s, t) -> 
    PEprint (s, next), next

  | TEblock (b, t) -> 
    let new_ctxs = (make_ctx())::ctxs in
    let pb, next = pcompile_block new_ctxs next b in
    PEblock (pb, next), next

(* ---- *)

and pcompile_block ctxs next (body, e, _) =
  (* 1 - precompilar stmts do bloco *)
  let next, p_body = List.fold_left_map (fun next s -> 
    let p_body, next = pcompile_stmt ctxs next s in
    next, p_body) next body in
  (* 2 - se existir, precompilar expressão *)
  let e = match e with
    | Some e -> Some (pcompile_expr ctxs next e)
    | None -> None in
  (p_body, e), next

(* ---- *)

and pcompile_stmt ctxs next = function
  | TSnothing _ -> PSnothing, next 
  | TSexpr (e, t) -> 
    let ep, next = (pcompile_expr ctxs next e) in
    PSexpr ep, next

  | TSdeclare (mut, id, e, t) -> 
    (* Size of vector: (previus_next + next) / size(t) *) 
    let previus_next = next in
    (* 1 -  Pré compilar expressão*)
    let ep, next = (pcompile_expr ctxs next e) in
    let pt = pcompile_type t in
    let start_pos = next + (get_type_start ctxs pt) in
    
    let pt = pcompile_type t in
    let new_next = (get_type_start ctxs pt) + (get_type_size ctxs pt) in
    (* 2 - Adicionar todos os elementos *)
    let pos_list, sz = get_type_elements ctxs id start_pos previus_next next t in

    (* 3 - Adicionar inicio ao frame *)
    Hashtbl.add (var_ctx_hd ctxs) id ((List.hd pos_list), sz);

    PSdeclare (mut, id, pt, ep, pos_list), (new_next+next)

  | TSdeclare_struct (mut, id, idt, el, t) ->
    let pt = pcompile_type t in
    (* 1. Precompilação de pares *)
    let next, p_els = List.fold_left_map(
      fun next (id, e) -> 
        (* 3. Precompilar expressão t_el *)
        let p_els, next = pcompile_expr ctxs next e in
        (* 4. Obter tipo do id *)
        let pt = get_type_from_struct_el ctxs id in
        let type_size = (get_type_size ctxs pt) in
        (* Calcular a posição da expressão calculada *)
        (next+type_size), (el, p_el, (-(next + type_size )))
    ) next el in

    PSdeclare_struct (mut, id, idt, p_els, -(next + (get_type_start ctxs pt))), next

  | TSwhile(e, b, _) -> 
    let pe, next = pcompile_expr ctxs next e in
    let pb, next = pcompile_block ((make_ctx())::ctxs) next s in
    PSwhile (pe, pb), next
    
  | TSreturn (Some e, t) -> (* 1 - How many values are there to return? *)

    let ep, next = (pcompile_expr ctxs next e) in
    let pt = pcompile_type t in
    let start_pos = next + (get_type_start ctxs pt) in

    (* 2 - Adicionar todos os elementos *)
    let pos_list =  match t with
      | Ti32 | Tbool | Tunit | Tempty -> [-start_pos]
      | Tstruct s ->
        (* 3.1 - Get struct elements *)
        let struct_els = fst(find_struct_id ctxs s) in
        (* 3.1.2 - Calculate pos of each element *)
        List.filter_map(fun (id, _, r_pos) -> if r_pos <> 0 then Some (-(abs(start_pos) + abs(r_pos))) else None)struct_els
      | Tvec _ | Tref _ | Trefmut _  -> [-start_pos]
    in
    PSreturn (Some ep, pos_list), next

    | TSreturn (None, t) -> 
      let pt = pcompile_type t in
      let start_pos = next + (get_type_start ctxs pt) in
      (* 2 - Adicionar todos os elementos *)
      let pos_list =  match t with
      | Ti32 | Tbool | Tunit | Tempty -> [-start_pos]
      | Tstruct s ->
        (* 3.1 - Get struct elements *)
        let struct_els = fst(find_struct_id ctxs s) in
        (* 3.1.2 - Calculate pos of each element *)
        List.filter_map(fun (id, _, r_pos) -> if r_pos <> 0 then Some (-(abs(start_pos) + abs(r_pos))) else None)struct_els
      | Tvec _ | Tref _ | Trefmut _  -> [-start_pos]
    in
    PSreturn (None, pos_list), next

  | TSif (e, b1, b2, _) ->
    let pe,  next  = pcompile_expr ctxs next e in
    let pb1, next  = pcompile_block ((make_ctx())::ctxs) next b1 in
    let pb2, next  = pcompile_block ((make_ctx())::ctxs) next b2 in
    PSif(pe, pb1, pb2), next

(* ---- *)

and pcompile_decl ctxs next = function
  | TDstruct (id, els, _) -> 
    (* 1 - Calcular espaço na memória*)
    let next, pcompiled_fields = 
    List.fold_left_map( fun next (id, t) -> 
      let pt = pcompile_type t in
      (next+(get_type_size ctxs pt)), (id, pt, (-(next)))) 0 els in
    (* 2 - Adiocionar identificador da struct ao contexto *)
    Hashtbl.add (struct_ctx_hd ctxs) id (pcompiled_fields, next);

    PDstruct(id, pcompiled_fields)
    
  | TDfun (f, args, t, b, _) -> 
    let new_ctxs = (make_ctx())::ctxs in
      (* 1- Precompilar argumentos *)
      let next, p_args = List.fold_left_map(
        fun next (mut, arg, t_arg) -> 
          let pt = pcompile_type t_arg in
          Hashtbl.add (var_ctx_hd new_ctxs) arg (next, 1);
          (next+(get_type_size ctxs pt)), (arg, pt, next)
      ) 16 args in

      (* 2 - Pre compilar corpo *)
      let pb, next = pcompile_block new_ctxs 8 b in
      Hashtbl.replace (fun_ctx_hd new_ctxs) f (p_args, next);
      PDfun(f, p_args, pb, next)

and pcompile_file p = 
  List.map (fun dec -> pcompile_decl [make_ctx()] 0 dec) p