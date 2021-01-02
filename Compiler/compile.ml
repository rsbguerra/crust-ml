(* Produção de código para a linguagem Rust *)
      
open Format
open X86_64
open Tast
      
exception VarUndef of string
exception Error    of string
let error s = raise (Error s)


(*  *)
let (genv: (string, unit) Hashtbl.t) = Hashtbl.create 17

(* Tamanho da frame, em byte (cada variável local ocupa 8 bytes) *)
let frame_size = ref 0

let get_value = function
 | Ast.Ci32 v -> v
 | _ -> assert false

let rec compile_expr ctxs = function
  | TEcst (i, t) ->
    (* 1 - Colocar a constante no topo da pilha *)
    movq (imm32 (get_value i)) (reg rax) ++
    pushq (reg rax)

  | TEident (i, _) -> assert false
  | TEbinop _ -> assert false
  | TEunop _ -> assert false
  | TEcall _ -> assert false


let rec compile_stmt ctxs = function
  | TSif _       -> assert false
  | TSwhile _ ->   assert false
  | TSdeclare _ -> assert false
  | TSassign _ -> assert false
  | TSprintn (e, _) ->
    (* 1 - Vai buscar o valor de e *)  
    compile_expr ctxs e ++
          
    (* 2 - Passa como parametro para a funcao printn_int e chama-a*)
    popq rdi ++
    call "printn_int"
  | TSprint (e, _)  ->
    (* 1 - Vai buscar o valor de e *)
    compile_expr ctxs e ++

    (* 2 - Passa como parametro para a funcao print_int e chama-a*)
    popq rdi ++
    call "print_int" 
  | TSblock (bl, t) -> 
    (* 1 - Compila um bloco de instrucoes *)
    let block = List.rev(compile_block_stmt ctxs bl) in
    List.fold_right (++) block nop
  
  | TScontinue _ -> assert false
  | TSbreak _ ->              assert false
  | TSreturn _ ->  nop
  | TSnothing _ -> nop
  | _ -> assert false
        
and compile_block_stmt ctx = function
  | [] -> [nop]
  | s :: sl -> (compile_block_stmt ctx sl) @ [compile_stmt ctx s]
        
and compile_block_stmts ctx = function
  | [] -> [nop]
  | s::sl -> (compile_block_stmts ctx sl) @ [compile_stmts ctx s]
        
and compile_stmts ctxs = function  
  | TGSblock bl -> 
    let block = List.rev(compile_block_stmts ctxs bl) in
    List.fold_right (++) block nop
  | TGSfunction(_, _, _, body) ->
    compile_stmt () body
  | _ -> assert false

(* Compilação do programa p e grava o código no ficheiro ofile *)
let compile_program p ofile =
  let ctxs = () in
  let code = compile_stmts ctxs p in
  let p =
    { text =
        globl "main" ++ label "main" ++
        subq (imm !frame_size) (reg rsp) ++ (* aloca a frame *)
        leaq (ind ~ofs:(!frame_size - 8) rsp) rbp ++ (* %rbp = ... *)
        code ++
        label "end" ++
        addq (imm !frame_size) (reg rsp) ++ (* desaloca a frame *)
        movq (imm64 0L) (reg rax) ++ (* exit *)
        ret ++
        label "printn_int" ++
        movq (reg rdi) (reg rsi) ++
        leaq (lab ".Sprintn_int") rdi ++
        movq (imm64 0L) (reg rax) ++
        call "printf" ++
        ret ++
        label "print_int" ++
        movq (reg rdi) (reg rsi) ++
        leaq (lab ".Sprint_int") rdi ++
        movq (imm64 0L) (reg rax) ++
        call "printf" ++
        ret ++

        label "scanf_int" ++
        
        leaq (lab ".Sscanf_int") rdi ++
        leaq (lab "input") rsi  ++
        xorq (reg rax) (reg rax) ++
  
        call "scanf" ++
        movq (lab "input") (reg rax) ++
        ret ++

        label "print_error_t" ++
        movq (reg rdi) (reg rsi) ++
        leaq (lab ".Sprint_error_t") rdi ++
        movq (imm64 0L) (reg rax) ++
        call "printf" ++
        jmp "end" ++
        label "print_error_s" ++
        movq (reg rdi) (reg rsi) ++
        leaq (lab ".Sprint_error_s") rdi ++
        movq (imm64 0L) (reg rax) ++
        call "printf" ++
        jmp "end" ++
        label "print_error_z" ++
        movq (reg rdi) (reg rsi) ++
        leaq (lab ".Sprint_error_z") rdi ++
        movq (imm64 0L) (reg rax) ++
        call "printf" ++
        jmp "end" ++
        label "print_error_f" ++
        movq (reg rdi) (reg rsi) ++
        leaq (lab ".Sprint_error_f") rdi ++
        movq (imm64 0L) (reg rax) ++
        call "printf" ++
        jmp "end";
      data = 
        label ".Sprintn_int" ++ string "%ld\n" ++
        label ".Sprint_int" ++ string "%ld" ++
        label ".Sprint_error_z" ++ string "\nErro: Divisao por zero.\n\n" ++
        label ".Sprint_error_t" ++ string "\nRun-time error:\n\n     Value out of bounds.\n\n" ++
        label ".Sprint_error_s" ++ string "\nRun-time error:\n\n     Invalid size of set. A set needs to have atleast the size of one.\n\n" ++
        label ".Sprint_error_f" ++ string "\nFuncao sem retorno\n\n" ++
        label ".Sscanf_int" ++ string "%ld" ++
        label "is_in_function" ++ dquad [0] ++
        label "number_of_loop" ++ dquad [0] ++
        label "input"  ++ dquad [0] ++
        label "shift"  ++ dbyte [0]
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
   (* "flush" do buffer para garantir que tudo foi para aí escrito antes de o fechar *)
  fprintf fmt "@?";
  close_out f
