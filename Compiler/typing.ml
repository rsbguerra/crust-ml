open Ast
open Printer

exception Error of string * int

let error s line = raise (Error (s, line))

let crust_types_of_crust_const = function
  | Cu8   _ -> Tu8 
  | Cu16  _ -> Tu16 
  | Cu32  _ -> Tu32
  | Cu64  _ -> Tu64 
  | Cu128 _ -> Tu128
  | Ci8   _ -> Ti8 
  | Ci16  _ -> Ti16
  | Ci32  _ -> Ti32  
  | Ci64  _ -> Ti64
  | Ci128 _ -> Ti128
  | Cbool _ -> Tbool

let compare_crust_types = function 
  | Cu8   _, Cu8   _ -> true 
  | Cu16  _, Cu16  _ -> true 
  | Cu32  _, Cu32  _ -> true
  | Cu64  _, Cu64  _ -> true 
  | Cu128 _, Cu128 _ -> true
  | Ci8   _, Ci8   _ -> true 
  | Ci16  _, Ci16  _ -> true
  | Ci32  _, Ci32  _ -> true  
  | Ci64  _, Ci64  _ -> true
  | Ci128 _, Ci128 _ -> true
  | Cbool _, Cbool _ -> true
  | _, _             -> false

let rec type_expr ctxs = function
  | Ecst(const, _) -> crust_types_of_crust_const const
  | Eident _       -> assert false
  | Ebinop _       -> assert false
  | Eunop _        -> assert false
  | Ecall _        -> assert false
 

(* Verificacao de uma instrucao - Instruções nao devolvem um valor *)
let rec type_stmt ctxs = function
  | Sif _                  -> assert false
  | Sloop _                -> assert false
  | Swhile _               -> assert false
  | Sdeclare _             -> ()
  | Sassign _              -> ()
  | Sprintn _              -> assert false  
  | Sprint _               -> assert false
  | Sblock (bl, _)         -> type_block_stmt ctxs bl
  | Scontinue _ | Sbreak _ -> ()
  | Sreturn _              -> assert false
  | Snothing _             -> ()
  
and type_global_stmt ctxs = function  
  | GSblock (bl, _) -> type_block_global_stmt ctxs bl
  | GSuse _         -> assert false
  | GSfunction _    -> assert false
  | GSstruct _      -> assert false
  | GSimpl _        -> assert false

and type_block_stmt ctxs = function
  | [] -> ()
  | s :: sl -> type_stmt ctxs s; type_block_stmt ctxs sl

and type_block_global_stmt ctxs = function
  | [] -> ()
  | s :: sl -> type_global_stmt ctxs s; type_block_global_stmt ctxs sl

(* Realiza a analise semantica de um ficheiro *)
let file s = type_global_stmt () s
