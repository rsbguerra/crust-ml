(*
  Última alteração: 28-12-2019
  Descricao: Árvore de sintaxe abastrata do Rust
*)

type ident = string

type unop = 
  | Uneg
  | Unot
  | Ubitnot

and binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod
  | Beq | Bneq | Blt | Ble | Bgt | Bge
  | Band | Bor 
  | Bitand | Bitor | Bitxor | Bitls | Bitrs

and expr =
  | Ecst     of int64 * int
  | Eset     of expr * expr * int
  | Eident   of ident * int
  | Ebinop   of binop * expr * expr * int
  | Eunop    of unop * expr * int
  | Ecall    of ident * expr list * int
  | Eget     of ident * expr * int(* id[e2] *)
  | Eternary of expr * expr * expr * int

and stmt =
  | Sif       of expr * stmt * elif list * int
  | Sbreak    of int
  | Scontinue of int
  | Sassign   of ident * expr * int
  | Sdeclare  of ident * costumtype * expr * int
  | Sprint    of expr * int
  | Sprintn   of expr * int
  | Sscanf    of ident * int
  | Sblock    of stmt list * int
  | Sfor      of ident * costumtype * expr * expr * expr * stmt * int
  | Sreturn   of expr * int
  | Swhile    of expr * stmt * int
  | Sloop     of stmt * int
  | Snothing  of int

(* Para não podermos definir funções dentro de instruções *)
and stmts =                                               
  | Stblock    of stmts list * int
  | Stfunction of ident * argument list * costumtype * stmt * int
  | Stmt       of stmt * int

and elif = expr * stmt * int

and argument = ident * costumtype

and crType = 
  | Tint  of int
  | Tbool of bool
  
and program = stmts
