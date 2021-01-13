(*
  Última alteração: 26-12-2020
  Descricao: Árvore de sintaxe abastrata do Rust
*)
type ident = string

and expr =
  | PEcst of Ast.crust_const
  | PEident of ident * int
  | PEbinop of Ast.binop * expr * expr
  | PEunop of Ast.unop * expr
  | PEcall of ident * expr list
  (*| PEaccess of expr * ident (* S.x (S-> struct, x -> element of struct)*)
  | PElen of expr
  | PEvec_access of expr * expr
  | PEvec_decl of expr list*)

and stmt =
  | PSif of expr * stmt * elif list
  | PSwhile of expr * stmt
  | PSdeclare of ident * Ast.crust_types * expr * int
  | PSassign of ident * expr
  | PSprintn of expr
  | PSprint of expr
  | PSblock of stmt list
  | PScontinue
  | PSbreak
  | PSreturn of expr
  | PSnothing
  | PSexpr of expr

and elif = expr * stmt

and global_stmt =
  | PGSblock of global_stmt list
  | PGSfunction of ident * pairs list * Ast.crust_types * stmt
  | PGSstruct of ident * pairs list

and pairs = ident * Ast.crust_types * int

and program = global_stmt
