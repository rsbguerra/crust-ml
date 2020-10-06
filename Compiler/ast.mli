(*
  Última alteração: 05-10-2020
  Descricao: Árvore de sintaxe abastrata do Rust
*)
open Stdint

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
  | Ecst     of crust_conts * int
  | Eident   of ident * int
  | Ebinop   of binop * expr * expr * int
  | Eunop    of unop * expr * int
  | Ecall    of ident * expr list * int
  
and stmt =
  | Sif       of expr * stmt * elif list * int
  | Sbreak    of int
  | Scontinue of int
  | Sassign   of ident * expr * int
  | Sdeclare  of ident * crust_types * expr * int
  | Sprint    of expr * int
  | Sprintn   of expr * int
  | Sscanf    of ident * int
  | Sblock    of stmt list * int
  | Sreturn   of expr * int
  | Swhile    of expr * stmt * int
  | Sloop     of stmt * int
  | Snothing  of int

(* Para não podermos definir funções dentro de instruções *)
and stmts =                                               
  | Stblock    of stmts list * int
  | Stfunction of ident * argument list * crust_types * stmt * int
  | Stmt       of stmt * int

and elif = expr * stmt * int

and argument = ident * crust_types

and crust_conts =
  | Cu8 of uint8 | Cu16 of uint16 | Cu32 of uint32 | Cu64 of uint64 | Cu128 of uint128
  | Ci8 of int8  | Ci16 of int16  | Ci32 of int32  | Ci64 of int64  | Ci128 of int128
  | Cbool of bool

and crust_types =
  | Tu8 | Tu16 | Tu32 | Tu64 | Tu128
  | Ti8 | Ti16 | Ti32 | Ti64 | Ti128
  | Tbool
  
and program = stmts
