(*
  Última alteração: 26-12-2020
  Descricao: Árvore de sintaxe abastrata do Rust
*)
type ident = string

and expr =
  | PEcst of Ast.crust_const
  | PEident of ident * int list
  | PEref of int
  | PEbinop of Ast.binop * expr * expr
  | PEunop of Ast.unop * expr
  | PElen of int
  | PEcall of ident * expr list * int
  | PEstrc_access of ident * ident * int
  | PEstrc_decl of ident * (ident * expr * int) list * int
  | PEvec_decl of (expr * int) list * int
  | PEvec_access of ident * expr * int * int * int
 
and stmt =
  | PSif of expr * stmt * elif list
  | PSwhile of expr * stmt
  | PSdeclare of ident * Ast.crust_types * expr * int list
  | PSassign of ident * expr * int
  | PSprintn of expr * Ast.crust_types
  | PSprint of expr * Ast.crust_types
  | PSblock of stmt list
  | PScontinue
  | PSbreak
  | PSreturn of expr * int list
  | PSnothing
  | PSexpr of expr

and elif = expr * stmt

and global_stmt =
  | PGSblock of global_stmt list
  | PGSfunction of ident * pairs list * Ast.crust_types * stmt * int
  | PGSstruct of ident * pairs list * int

and pairs = ident * Ast.crust_types * int

and program = global_stmt
