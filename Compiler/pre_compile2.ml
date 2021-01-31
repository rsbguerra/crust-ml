open Tast
open Past
open Ast

(* table_ctx representa um scope, contexto *)
type tbl_variables_ctx = (string, int * int) Hashtbl.t
type tbl_functions_ctx = (string, Past.argument list * int) Hashtbl.t
type tbl_structs_ctx   = (string, Past.pair list * int) Hashtbl.t

type tbl_ctx = tbl_variables_ctx * tbl_functions_ctx * tbl_structs_ctx

(* --- Begin aux functions --- *)

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

let rec find_var_id ctxs id = 
  let ctx,_,_ = List.find(fun (v, _, _) -> Hashtbl.mem v id) ctxs in
  Hashtbl.find ctx id
and find_struct_id ctxs id = 
  let _,_,ctx = List.find(fun ctx -> Hashtbl.mem (struct_ctx ctx) id) ctxs in
  Hashtbl.find ctx id

and get_type_size ctxs = function
| PTi32 | PTbool -> 8
| PTunit | PTempty-> 0
| PTstruct s -> snd(find_struct_id ctxs s)
| PTvec t | PTref t |PTrefmut t -> get_type_size ctxs t

and get_type_start ctxs = function
  | PTi32 | PTbool | PTunit | PTempty -> 0
  | PTstruct s -> -(snd (find_struct_id ctxs s))
  | PTvec t | PTref t | PTrefmut t -> get_type_start ctxs t

and get_type_elements ctxs id start_pos previus_next next = function
| Ti32 | Tbool | Tunit | Tempty -> [-start_pos], 1
| Tstruct s ->
  (* 2.1 - Get struct elements *)
  let struct_els = fst(find_struct_id ctxs s) in

  (* 2.1.2 - Calculate pos of each element *)
  let l = ref [] in

  List.iter(fun (id, t, r_pos) -> 
    let pt = pcompile_type t in
    let tmp, _ = (get_type_elements ctxs id (start_pos + abs(r_pos)) previus_next next pt) in
    l := !l@tmp;
  ) struct_els;
  
  List.iter(fun pos -> ()) !l;
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



(* --- End aux functions --- *)


(* --- Begin precompile functions --- *)

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
  | _ -> assert false

and pcompile_stmt ctxs next = function
  | TSnothing _ -> PSnothing, next 
  (* | TSexpr (e, t) -> 
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
       *)
  | _ -> assert false

and pcompile_block ctxs next (body, e, _) =
  (* 1 - precompilar stmts do bloco *)
  let next, p_body = List.fold_left_map (fun next s -> 
    let p_body, next = pcompile_stmt ctxs next s in
    next, p_body) next body in
  (* 2 - se existir, precompilar expressão *)
  let e = match e with
    | Some e -> let e, next = pcompile_expr ctxs next e in Some e
    | None -> None in
  p_body, e, next


let rec pcompile_decl ctxs next = function
| TDstruct (id, els, _) -> 

  (* 1 - Calcular espaço na memória*)
  let next, pfields = 
  List.fold_left_map (fun next (id, t) -> 
    let pt = pcompile_type t in
    (next+(get_type_size ctxs pt)), (id,pt,(-next))) 0 els in

    (* 2 - Adiocionar identificador da struct ao contexto *)
    Hashtbl.add (struct_ctx_hd ctxs) id (pfields, next);
    PDstruct(id, pfields)

  | TDfun (f, args, t, b, _) -> 
    let new_ctxs = (make_ctx())::ctxs in

    (* 1 - Pre compilar argumentos *)
    let next, p_args = 
    List.fold_left_map (fun next (mut, arg, t_arg) -> 
      let pt = pcompile_type t_arg in
      Hashtbl.add (var_ctx_hd new_ctxs) arg (next, 1);
      (next+(get_type_size ctxs pt)), (mut, arg, pt, next)) 16 args in
      
    (* 2 - Pre compilar corpo *)
    let pb, pe, next = pcompile_block new_ctxs 8 b in
    Hashtbl.replace (fun_ctx_hd new_ctxs) f (p_args, next);
    PDfun(f, p_args, (pb,pe,next), next)

and pcompile_file p = 
  List.map (fun dec -> pcompile_decl [make_ctx()] dec) p
  
(* --- End precompile functions --- *)