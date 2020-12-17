(*
  Última alteração: 17-12-2020
  Descricao: Árvore de sintaxe abastrata do Rust
*)
open Stdint

type ident = string

type unop = 
  | Uneg      (* - *)
  | Unot      (* ! *)

and binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod
  | Beq  | Bneq | Blt  | Ble  | Bgt | Bge
  | Band | Bor 

and expr =
  | Ecst     of crust_const * int
  | Eident   of ident * int
  | Ebinop   of binop * expr * expr * int
  | Eunop    of unop * expr * int
  | Ecall    of ident * expr list * int
  
and stmt =
  | Sif       of expr * stmt * elif list * int
  | Swhile    of expr * stmt * int
  | Sdeclare  of ident * crust_types * expr * int
  | Sassign   of ident * expr * int
  | Sprintn   of expr * int
  | Sprint    of expr * int
  | Sblock    of stmt list * int
  | Scontinue of int
  | Sbreak    of int
  | Sreturn   of expr * int
  | Snothing  of int
  | Sexpr     of expr * int 

and elif = expr * stmt * int

and crust_const =
  | Ci32 of int32
  | Cbool of bool

and crust_types =
  | Tvoid | Ti32 | Tbool

and global_stmt = 
  | GSblock    of global_stmt list * int
  | GSfunction of ident * pairs list * crust_types * stmt * int
  | GSstruct   of ident * pairs list * int

and pairs = ident * crust_types

and program = global_stmt
