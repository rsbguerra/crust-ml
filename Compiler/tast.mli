(*
  Última alteração: 11-12-2020
  Descricao: Type Abstract Syntax Tree
*)

open Stdint

type ident = string

type typed_unop = 
  | TUneg
  | TUnot
  | TUbitnot

and typed_binop =
  | TBadd | TBsub | TBmul | TBdiv | TBmod
  | TBeq  | TBneq | TBlt  | TBle  | TBgt | TBge
  | TBand | TBor 
  | TBitand | TBitor | TBitxor | TBitls | TBitrs

and typed_expr =
  | TEcst     of Ast.crust_const *Ast.crust_types
  | TEident   of ident *Ast.crust_types
  | TEbinop   of typed_binop * typed_expr * typed_expr *Ast.crust_types
  | TEunop    of typed_unop * typed_expr *Ast.crust_types
  | TEcall    of ident * typed_expr list *Ast.crust_types
  
and typed_stmt =
  | TSif       of typed_expr * typed_stmt * typed_elif list
  | TSloop     of typed_stmt * int
  | TSwhile    of typed_expr * typed_stmt * int
  | TSdeclare  of ident *Ast.crust_types * typed_expr
  | TSassign   of ident * typed_expr * int
  | TSprintn   of typed_expr
  | TSprint    of typed_expr
  | TSblock    of typed_stmt list
  | TSreturn   of typed_expr * int * Ast.crust_types
  | TScontinue
  | TSbreak
  | TSnothing

and typed_elif = typed_expr * typed_stmt * int

and function_argument = ident *Ast.crust_types

and typed_global_stmt = 
  | TGSblock    of typed_global_stmt list
  | TGSfunction of ident * function_argument list *Ast.crust_types * typed_stmt
  | TGSstruct   of ident (* Todo *)
 
and program = typed_global_stmt
