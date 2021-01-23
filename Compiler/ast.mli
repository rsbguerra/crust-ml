(*
  Last Modification: 23-01-2020
  Description: Pico-Rust Abstract Syntax Tree
*)

type program = decl list

and decl = 
  | Dstruct of ident * pair list * int
  | Dfun    of ident * argument list * prust_type option * block * int

and pair = ident * prust_type
and argument = bool * ident * prust_type

and prust_type =
  | Tid       of ident
  | Tid_typed of ident * prust_type
  | Tref      of prust_type
  | Trefmut   of prust_type

and unop = 
  | Uneg    (* -    *)
  | Unot    (* !    *)
  | Uref    (* &    *)
  | Urefmut (* &mut *)
  | Uderef  (* *    *)

and binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod
  | Beq  | Bneq | Blt  | Ble  | Bgt | Bge
  | Band | Bor
  | Bassign

and expr =
  | Eint   of int32 * int
  | Ebool  of bool * int
  | Eident of ident * int
  | Ebinop of binop * expr * expr * int
  | Eunop  of unop * expr * int
  | Estruct_access of expr * ident * int
  | Elen           of expr * int
  | Evec_access of expr * expr * int
  | Ecall     of ident * expr list * int
  | Evec_decl of expr list * int
  | Eprint    of string * int
  | Eblock    of block * int

and block = stmt list * expr option

and stmt =
  | Snothing of int
  | Sexpr    of expr * int
  | Sdeclare of bool * ident * expr * int
  | Sdeclare_struct of bool * ident * ident * (ident * expr) list * int
  | Swhile   of expr * block * int
  | Sreturn  of expr option * int
  | Sif      of expr * block * block * int

and ident = string
