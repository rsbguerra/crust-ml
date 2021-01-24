(*
  Última alteração: 23-01-2021
  Descricao: Typed Abstract Syntax Tree
*)

type program = typed_decl list

and typed_decl = 
  | TDstruct of ident * pair list * prust_type
  | TDfun    of ident * argument list * prust_type * typed_block * prust_type

and pair = ident * prust_type
and argument = bool * ident * prust_type

and prust_type =
  | Tunit | Ti32 | Tbool
  | Tstruct of ident
  | Tvec of prust_type
  | Tref of bool * prust_type

and typed_expr =
  | TEint   of int32 * prust_type
  | TEbool  of bool  * prust_type
  | TEident of ident * prust_type
  | TEunop  of Ast.unop * typed_expr * prust_type
  | TEbinop of Ast.binop * typed_expr * typed_expr * prust_type
  | TEstrc_access of typed_expr * ident * prust_type
  | TElen   of typed_expr * prust_type
  | TEvec_access of typed_expr * typed_expr * prust_type
  | TEcall  of ident * typed_expr list * prust_type
  | TEvec_decl of typed_expr list * prust_type
  | TEprint  of string * prust_type
  | TEblock  of typed_block * prust_type

and typed_block = typed_stmt list * typed_expr option * prust_type

and typed_stmt =
  | TSnothing of prust_type
  | TSexpr    of typed_expr * prust_type
  | TSdeclare of bool * ident * typed_expr * prust_type
  | TSdeclare_struct of bool * ident * ident * (ident * typed_expr) list * prust_type
  | TSwhile   of typed_expr * typed_block * prust_type
  | TSreturn  of typed_expr option * prust_type
  | TSif      of typed_expr * typed_block * typed_block * prust_type

and ident = string
