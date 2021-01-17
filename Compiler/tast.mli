(*
  Última alteração: 17-01-2021
  Descricao: Typed Abstract Syntax Tree
*)

type ident = string

and typed_expr =
  | TEcst     of Ast.crust_const * Ast.crust_types
  | TEident   of ident * Ast.crust_types
  | TEbinop   of Ast.binop * typed_expr * typed_expr * Ast.crust_types
  | TEref     of ident * Ast.crust_types
  | TErefmut  of ident * Ast.crust_types
  | TEptr     of ident * Ast.crust_types
  | TEunop    of Ast.unop * typed_expr * Ast.crust_types
  | TEstrc_access of ident * ident * Ast.crust_types * Ast.crust_types
  | TEstrc_decl  of ident * (ident * typed_expr * Ast.crust_types) list  * Ast.crust_types
  | TEvec_decl   of typed_expr list * Ast.crust_types
  | TEvec_access of ident * typed_expr * Ast.crust_types * Ast.crust_types
  | TElen     of ident
  | TEcall    of ident * (typed_expr * Ast.crust_types) list * Ast.crust_types
  
and typed_stmt =
  | TSif       of typed_expr * typed_stmt * typed_elif list  * Ast.crust_types
  | TSwhile    of typed_expr * typed_stmt * Ast.crust_types
  | TSdeclare  of ident * Ast.crust_types * typed_expr * Ast.crust_types
  | TSassign   of ident * typed_expr * Ast.crust_types
  | TSptr_assign of ident * typed_expr * Ast.crust_types
  | TSprintn   of typed_expr * Ast.crust_types * Ast.crust_types
  | TSprint    of typed_expr * Ast.crust_types * Ast.crust_types
  | TSblock    of typed_stmt list * Ast.crust_types
  | TScontinue of Ast.crust_types
  | TSbreak    of Ast.crust_types
  | TSreturn   of typed_expr * Ast.crust_types
  | TSnothing  of Ast.crust_types
  | TSexpr     of typed_expr * Ast.crust_types

and typed_elif = typed_expr * typed_stmt

and typed_global_stmt = 
  | TGSblock    of typed_global_stmt list
  | TGSfunction of ident * pairs list * Ast.crust_types * typed_stmt
  | TGSstruct   of ident * pairs list
 
and pairs = ident * Ast.crust_types

and program = typed_global_stmt
