open Past
open Tast
open Ast
(* table_ctx representa um scope, contexto *)
type tbl_variables_ctx = (string, int) Hashtbl.t
type tbl_functions_ctx = (string, int) Hashtbl.t
type tbl_structs_ctx = (string, Ast.pairs list) Hashtbl.t

type tbl_ctx = tbl_variables_ctx * tbl_functions_ctx * tbl_structs_ctx

val make_ctx : unit -> tbl_ctx

val var_ctx    : 'a * 'b * 'c -> 'a
val fun_ctx    : 'a * 'b * 'c -> 'b
val struct_ctx : 'a * 'b * 'c -> 'c

val id_exists : string -> tbl_ctx list 
  -> bool

val find_id : 'a ->
           (('a, 'b option) Hashtbl.t * ('a, 'b option) Hashtbl.t *
            ('a, 'b option) Hashtbl.t)
           list -> 'b option


val pcompile_expr : tbl_ctx list -> int -> Tast.typed_expr
  -> Past.expr  * int
val pcompile_stmt : tbl_ctx list -> Tast.typed_stmt
  -> Past.stmt
val pcompile_block_stmt : tbl_ctx list -> Tast.typed_stmt list
  -> Past.stmt list
val pcompile_global_stmt : tbl_ctx list -> Tast.typed_global_stmt
  -> Past.global_stmt
val precompile : Tast.program -> Past.program
