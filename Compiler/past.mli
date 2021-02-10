(*
  Última alteração: 17-01-2021
  Descricao: Árvore de sintaxe abastrata do CRust
*)
type program = decl list

and decl = 
  | PDstruct of ident * pair list
  | PDfun    of ident * argument list * block * int

and prust_type =
  | PTunit
  | PTi32
  | PTbool
  | PTempty
  | PTstruct of ident
  | PTvec of prust_type
  | PTref of prust_type
  | PTrefmut of prust_type

and pair = ident * prust_type * int
and argument = bool * ident * prust_type * int

and expr =
  | PEint   of int32
  | PEbool  of bool
  | PEident of ident * int
  | PEunop  of Ast.unop * expr
  | PEbinop of Ast.binop * expr * expr * int
  | PEstruct_access of expr * ident * int
  | PElen   of expr 
  | PEvec_access of expr * expr * int * int
  | PEcall  of ident * expr list * int
  | PEvec_decl of (expr * int) list * int
  | PEprint of string * int
  | PEblock of block * int

and block = stmt list * expr option

and stmt =
  | PSnothing
  | PSexpr    of expr
  | PSdeclare of bool * ident * prust_type * expr * int list
  | PSdeclare_struct of bool * ident * ident * (ident * expr * int) list * int
  | PSwhile  of expr * block
  | PSreturn of expr option * int list
  | PSif     of expr * block * block

and ident = string