(* Produção de código para a linguagem Rust *)
      
open Format
open X86_64
open Ast
      
exception VarUndef of string
exception Error    of string
let error s = raise (Error s)

(* Tamanho da frame, em byte (cada variável local ocupa 8 bytes) *)
let frame_size = ref 0

let rec compile_expr ctxs = function
  | Ecst (i, _) ->
      (* 1 - Colocar a constante no topo da pilha *)
      movq (imm64 i) (reg rax) ++
      pushq (reg rax)

  | Eset (e1, e2, _) ->
      (* 1 - Colocar o inicio na pilha *)
      compile_expr ctxs e1 ++
      
      (* 2 - Colocar o fim na pilha *)
      compile_expr ctxs e2 ++

      (* 3 - Retura os valores da pilha *)
      popq rax ++
      popq rbx ++

      (* 4 - Faz as verificacoes *)
      
      (* Verificar se f < i *)
      cmpq (reg rbx) (reg rax) ++
      jle "print_error_s" ++

      (* Coloca os valores *)
      pushq (reg rbx) ++
      pushq (reg rax)

  | Eminint _ -> 
      (* 1 - Colocar a constante minint na pilha *)
      movq (imm64 minint) (reg rax) ++
      pushq (reg rax) 

  | Emaxint _ ->
      (* 1 - Colocar a constante maxint na pilha *)
      movq (imm64 maxint) (reg rax) ++
      pushq (reg rax)

  | Eident(id, _) ->
      (* 1 - Ir buscar  o valor de id *)
      let ctx = List.hd( List.rev (find_id ctxs id )) in
      let v = snd(Hashtbl.find ctx id) in

      (* 2 - Gerar o codigo respondente  ao tipo de dados*)
      extract_to_assembly v
  
  | Ebinop (Bmod | Bdiv as op, e1, e2, _) ->
      
      (* 1 - Dependendo da operacao queremos um registo diferente *)
      let rg = 
        match op with 
        | Bmod -> rdx
        | Bdiv -> rax
        | _ -> assert false
      in

      (* 2 - Colocar e1 e e2 na pilha*)  
      compile_expr ctxs e1 ++
      compile_expr ctxs e2 ++

      (* 3 - Recebe os valores da pilha *)
      popq rbx ++
      popq rax ++
      
      (* 4 - Limpa o registo rdx *)
      movq (imm 0) (reg rdx) ++
      
      (* 5 - Verifica se estamos a dividir por zero *)
      cmpq (imm 0) (reg rbx) ++
      je "print_error_z" ++

      (* 6 - Realiza a operacao e coloca o resultado na pilha *)
      idivq (reg rbx) ++
      pushq (reg rg)

  | Ebinop (Badd | Bsub | Bmul as o , e1, e2, _) ->
      (* 1 - Dependendo da operacao queremos uma operacao diferente *)
      let op = match o with
        | Badd -> addq
        | Bsub -> subq
        | Bmul -> imulq
        | _ -> assert false
      in
      
      (* 2 - Colocar e1 e e2 na pilha*)  
      compile_expr ctxs e1 ++
      compile_expr ctxs e2 ++

      (* 3 - Recebe os valores da pilha *)
      popq rax ++
      popq rbx ++
      
      (* 4 - Realiza a operacao e coloca o resultado na pilha *)
      op (reg rax) (reg rbx) ++
      pushq (reg rbx)

  | Ebinop (Band | Bor as o, e1, e2, _) ->
      number_of_and_or := !number_of_and_or + 1;
      let current_and_or = string_of_int(!number_of_and_or) in
        
      (* 1 - Dependendo da operacao queremos uma operacao diferente *)
      let op, cmp_op = 
        match o with 
        | Band -> andq, jne
        | Bor  -> orq , je
        | _    -> assert false
      in

      (* 2 - Colocar e1 na pilha*)  
      compile_expr ctxs e1 ++
      popq rax ++
      
      (* 3 - Verificar se podemos terminar a expressao *)
      cmpq (imm64 1L) (reg rax) ++
      cmp_op ("lazy_evaluation_" ^ current_and_or) ++

      (* 4 - COlocar e2 no topo da pilha*)
      compile_expr ctxs e2 ++
      popq rax ++

      (* 5 - Realiza a operacao e coloca o resultado na pilha *)
      op  (imm64 1L) (reg rax) ++

      (* 6 - termina *)
      label ("lazy_evaluation_" ^ current_and_or) ++
      pushq (reg rax)


  | Ebinop (Bitand | Bitor | Bitxor as o, e1 , e2, _) ->
      let op = match o with
        | Bitand -> andq
        | Bitor-> orq
        | Bitxor -> xorq
        | _ -> assert false
      in  

      (* 3 - Colocar e1 e e2 na pilha*)  
      compile_expr ctxs e1 ++
      compile_expr ctxs e2 ++
  
      (* 4 - Recebe os valores da pilha *)
      popq rax ++
      popq rcx ++

      movb (reg cl) (lab "shift") ++

      op  (lab "shift") (reg rax) ++
      pushq (reg rax)
  | Ebinop (Bitls | Bitrs as o, e1 , e2, _) ->
      let op = match o with
        | Bitrs -> shrq
        | Bitls -> shlq
        | _ -> assert false
      in  
      (* 1 - Incrementar numero de verificacoes *)
      number_of_shift := !number_of_shift + 1;
      let current_shift = string_of_int(!number_of_shift) in

      (* 3 - Colocar e1 e e2 na pilha*)  
      compile_expr ctxs e1 ++
      compile_expr ctxs e2 ++
  
      (* 4 - Recebe os valores da pilha *)
      popq rbx ++
      popq rax ++

      cmpq (imm64 0L) (reg rbx) ++
      jle ("print_error_s") ++
      label ("bitwise_shift_" ^ current_shift) ++
      
      op (imm 1) (reg rax) ++
      decq (reg rbx) ++
      
      cmpq (imm64 0L) (reg rbx) ++
      jg ("bitwise_shift_" ^ current_shift) ++
      
      pushq (reg rax) 
      
   
  | Ebinop (Beq | Bneq | Blt | Ble | Bgt | Bge as o, e1, e2, _) ->
      (* 1 - Dependendo da operacao queremos uma operacao diferente *)
      let op = match o with
        | Beq -> je
        | Bneq-> jne
        | Blt -> jl
        | Ble -> jle
        | Bgt -> jg
        | Bge -> jge
        | _ -> assert false
      in
    
      (* 2 - Incrementar numero de verificacoes *)
      number_of_bool_tests := !number_of_bool_tests + 1;
      let current_bool_test = string_of_int(!number_of_bool_tests) in
    
      (* 3 - Colocar e1 e e2 na pilha*)  
      compile_expr ctxs e1 ++
      compile_expr ctxs e2 ++

      (* 4 - Recebe os valores da pilha *)
      popq rbx ++
      popq rax ++

      (* 5 - Compara ambos os valores e faz o devido salto *)
      cmpq (reg rbx) (reg rax) ++
      op ("bool_true_" ^ current_bool_test) ++

      (* 6a - Se for falso *)
      movq (imm 0) (reg rax) ++
      pushq (reg rax) ++

      (* 7a - Termina *)
      jmp ("bool_end_" ^ current_bool_test) ++
      
      (* 6b - Se for verdade *)
      label ("bool_true_" ^ current_bool_test) ++
      movq (imm 1) (reg rax) ++
      pushq (reg rax) ++
      
      (* 7a - Termina *)
      label ("bool_end_" ^ current_bool_test)

  | Eunop (Unot, e1, _) ->
      (* 1 - Incrementar numero de verificacoes *)
      number_of_bool_tests := !number_of_bool_tests + 1;
      let current_bool_test = string_of_int(!number_of_bool_tests) in
  
      (* 2 - Colocar e1 na pilha *)  
      compile_expr ctxs e1 ++
      
      (* 3 - Recebe o valor de e1 *)  
      popq rax ++

      (* 4 - Verifica se e falso *)
      cmpq (imm64 0L) (reg rax) ++
      je ("bool_true_" ^ current_bool_test) ++
      
      (* 5a - Se a comparacao falhar entao retornamos 0 *)
      movq (imm64 0L) (reg rax) ++
      pushq (reg rax) ++
      
      (* 6a - Terminamos *)  
      jmp ("bool_end_" ^ current_bool_test) ++
      
      (* 5b - Se a comparacao acertar entao retornamos 1 *)
      label ("bool_true_" ^ current_bool_test) ++
      movq (imm64 1L) (reg rax) ++
      pushq (reg rax) ++

      (* 6b - Terminamos *)  
      label ("bool_end_" ^ current_bool_test)

  | Eunop (Uneg, e1, _) -> 
      (* 1 - Colocar e1 na pilha *)  
      compile_expr ctxs e1 ++
      
      (* 2 - Recebe o valor de e1 *)  
      popq rax ++
      
      negq (reg rax) ++

      pushq (reg rax)
  | Eunop (Ubitnot, e1, _) -> 
      (* 1 - Colocar e1 na pilha *)  
      compile_expr ctxs e1 ++
      
      (* 2 - Recebe o valor de e1 *)  
      popq rax ++
      
      notq (reg rax) ++

      pushq (reg rax)

  | Ecall ("size", [e1], _) ->
      (* 1 - Colocar e1 na pilha *)  
      compile_expr ctxs e1 ++

      (* 2 - Como sabemos que e um conjunto, vamos buscar dois valores *)
      popq rax ++ (* Fim *)
      popq rbx ++ (* Inicio *)

      (* 3 - Calcula o tamanho *)
      subq (reg rbx) (reg rax) ++

      (* 4 - Termina *)
      pushq (reg rax)

  | Ecall (f, el, _) ->
      (* 1 - Vai buscar a funcao f *)
      let ctx, return = Hashtbl.find function_ctx f in
      let ctxs = ctxs@[(Hashtbl.create 17 : table_ctx)] in
      (* 2 - Ref onde vai ficar o codigo dos argumentos *)
      let code = ref nop in
      
      for i = 0 to (List.length el) - 1 do
        (* 4.1 - Vai buscar os dados do argumento i *)
        let _, v = List.nth ctx i in
        let t, ofs = v in
        let ofs = - int_of_vint ofs in
        code := !code ++
          (* 4.2.1 - Vai buscar o valor da expressao no indice i *)  
          compile_expr ctxs (List.nth el i) ++
          popq rax ++

          (* 4.2.2 - *)
          movq (reg rax) (ind ~ofs rbp) ++
          is_in_type_boundaries ctxs (-ofs) t;
      done;
      (* Verificar se o retorno esta nos limites do tipo *)
      !code ++ 
      addq (imm64 1L) (lab "is_in_function") ++
      call ("user" ^ f) ++
      movq (reg rax) (reg rbx) ++
      is_rbx_in_type_boundaries ctxs return ++
      pushq (reg rbx)
      
  | Eternary (cond, e1, e2, _) -> 
      (*1 - Incrementa o numero de ifs realizados ate ao momento *)
      number_of_ternary := !number_of_ternary + 1;
      let current_ternary = string_of_int(!number_of_ternary) in
    
      (* 2 - Calcular o valor da condicao *)
      compile_expr ctxs cond ++
      popq rax ++

      (* 3 - Se vor diferente de entao e verdade *)
      cmpq (imm 0) (reg rax) ++
      jne ("ternary_true_" ^ current_ternary) ++
      
      (* 4a - Se for 0 executa o else*)
      compile_expr ctxs e2 ++

      jmp ("ternary_end_" ^ current_ternary) ++

      (* 4b - Se for 1 executa o then *)
      label ("ternary_true_" ^ current_ternary) ++
      
      compile_expr ctxs e1 ++

      (* 5 - Termina *)
      label ("ternary_end_" ^ current_ternary)
 
  | _ -> error "Not implemented"


let rec compile_stmt ctxs = function
  | Sif _       -> assert false
  | Sloop _ ->              assert false
  | Swhile _ ->     assert false
  | Sdeclare (id, t, e, _) ->
    (* 1 - Calcular o tamanho do frame *)
    let ofs = !frame_size in
    frame_size := 8 + !frame_size;
  
    let ctx = List.hd (List.rev ctxs) in
    let code =
      (* 2 - Calcular o valor da expressao  *)
      compile_expr ctxs e ++
      popq rax ++
    
      (* 3 - Guardar o valor da expressao na posicao ofs *)
      movq (reg rax) (ind ~ofs:(-ofs) rbp) ++
      is_in_type_boundaries ctxs ofs t 
    in

    (* 4 - Adiciona ao contexto atual *)
    Hashtbl.add ctx id (t, Vint ofs);
    code
  | Sassign (id, e1, _)  ->
        (* 1 - Vai buscar o contexto em que o id esta declarado *)        
        let ctx = List.hd (List.rev (find_id ctxs id)) in
  
        (* 2 - Vai buscar o tipo e o ofs de id *)
        let t, ofs = Hashtbl.find ctx id in
        let ofs = int_of_vint ofs in
  
        (* 2 - Atualiza o valor que esta no endereço ofs*)
        compile_expr ctxs e1 ++
        popq rax ++
  
        movq (reg rax) (ind ~ofs:(-ofs) rbp) ++
        is_in_type_boundaries ctxs ofs t
    
  | Sprintn (e, _) ->
    (* 1 - Vai buscar o valor de e *)  
    compile_expr ctxs e ++
          
    (* 2 - Passa como parametro para a funcao printn_int e chama-a*)
    popq rdi ++
    call "printn_int"
  | Sprint (e, _)  ->
    (* 1 - Vai buscar o valor de e *)
    compile_expr ctxs e ++

    (* 2 - Passa como parametro para a funcao print_int e chama-a*)
    popq rdi ++
    call "print_int" 
  | Sblock (bl, _) -> 
    (* 1 - Compila um bloco de instrucoes *)
    let block = List.rev(compile_block_stmt ctxs bl) in
    List.fold_right (++) block nop
  
  | Scontinue _ -> assert false
  | Sbreak _ ->              assert false
  | Sreturn _ ->       assert false
  | Snothing _ -> nop
  | _ -> error "STMT NOT IMPLEMENTED IN THE COMPILER."
        
and compile_block_stmt ctx = function
  | [] -> [nop]
  | s :: sl -> (compile_block_stmt ctx sl) @ [compile_stmt ctx s]
        
and compile_block_stmts ctx = function
  | [] -> [nop]
  | s::sl -> (compile_block_stmts ctx sl) @ [compile_stmts ctx s]
        
and compile_stmts ctxs = function  
  | GSblock (bl, _) -> 
    let block = List.rev(compile_block_stmts ctxs bl) in
    List.fold_right (++) block nop
  | _ -> assert false
(* Compilação do programa p e grava o código no ficheiro ofile *)
let compile_program p ofile =
  let ctxs = [(Hashtbl.create 17 : table_ctx)] in
  let code = compile_stmts ctxs p in
  functions_code := !functions_code; 
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
        jmp "end" ++
        !functions_code;
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
