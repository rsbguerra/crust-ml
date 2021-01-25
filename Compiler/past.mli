(*
  Última alteração: 17-01-2021
  Descricao: Árvore de sintaxe abastrata do CRust
*)
type program = decl list

and decl = 
  | PDstruct of ident * pair list
  | PDfun    of ident * argument list * int

and pairs = ident * Ast.crust_types * int
and argument = bool * ident * int

and prust_type =
  | Tunit of int
  | Ti32  of int
  | Tbool of int
  | Tstruct of ident
  | Tvec of prust_type
  | Tref of prust_type
  | Trefmut of prust_type

and expr =
  | PEint   of int32 * int
  | PEbool  of bool * int
  | PEident of ident * int
  | PEunop  of Ast.unop * expr
  | PEbinop of Ast.binop * expr * expr
  | PEstruct_access of expr * ident * int
  | PElen   of expr * int
  | PEvec_access of expr * expr * int * int * int
  | PEcall  of ident * expr list * int
  | PEvec_decl of (expr * int) list * int
  | PEprint of string * int
  | PEblock of block * int

and block = stmt list * expr option * int

and stmt =
  | PSnothing
  | PSexpr    of expr
  | PSdeclare of bool * ident * Ast.crust_types * expr * int list
  | PSdeclare_struct of bool * ident * ident * (ident * expr) list * int
  | PSwhile  of expr * stmt
  | PSreturn of expr option * int list
  | PSif     of expr * block * block

and ident = string