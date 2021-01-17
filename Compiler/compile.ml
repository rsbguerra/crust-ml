(* Produção de código para a linguagem Rust *)
open Format
open X86_64
open Past
      
exception Error    of string
let error s = raise (Error s)

(* Variáveis para garantir que cada for/if/etc. tem labels com nomes diferentes *)
let number_of_while  = ref 0
let number_of_and_or = ref 0
let number_of_ifs    = ref 0
let number_of_bool_tests = ref 0

(* 
  Variaveis que guardam as labels dos loops e funções em que estamos, 
  utilizado para se saber em que loop ou função o break, continue e return 
  estão a funcionar 
*)
let loop_labels     = ref []
let function_labels = ref []

let get_value = function
  | Ast.Ci32 v  -> v
  | Ast.Cbool v -> if v then Int32.one else Int32.zero
  | Ast.Cunit -> Int32.zero 

let rec get_str_type = function
  | Ast.Ti32 -> "int"
  | Ast.Tbool -> "bool"
  | Ast.Tref (t, _) | Ast.Tmut t -> get_str_type t
  | _ -> assert false

let rec get_elements e pos_list= function
  | Ast.Ti32 | Ast.Tbool -> 
    (* 1 - Calcular o valor da expressao  *)
    compile_expr e ++
    popq rax ++
    (* 2 - Guardar o valor da expressao na posicao ofs *)
    movq (reg rax) (ind ~ofs:(List.hd pos_list) rbp)
  | Ast.Tref (t, _) -> get_elements e pos_list t
  | Ast.Tmut t      -> get_elements e pos_list t
  | _ -> compile_expr e ++ List.fold_left (fun code p -> code ++ popq rax) nop pos_list

and compile_expr = function
  | PEcst i ->
    (* 1 - Colocar a constante no topo da pilha *)
    movq (imm32 (get_value i)) (reg rax) ++
    pushq (reg rax)
    
  | PEident (id, pos_list) ->
    List.fold_left( fun code p ->  
      movq (ind ~ofs:p rbp) (reg rax) ++
      pushq (reg rax) ++ 
      code
    ) nop pos_list
  | PEref (pos) -> 
    movq (imm pos) (reg rax) ++
    pushq (reg rax)
  | PEbinop (Ast.Bmod | Ast.Bdiv as op, e1, e2) ->    
    (* 1 - Dependendo da operacao queremos um registo diferente *)
    let rg = 
      match op with 
      | Bmod -> rdx
      | Bdiv -> rax
      | _ -> assert false
    in

    (* 2 - Colocar e1 e e2 na pilha*)  
    compile_expr e1 ++
    compile_expr e2 ++

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

  | PEbinop (Ast.Badd | Ast.Bsub | Ast.Bmul as o , e1, e2) ->
    (* 1 - Dependendo da operacao queremos uma operacao diferente *)
    let op = match o with
      | Badd -> addq
      | Bsub -> subq
      | Bmul -> imulq
      | _ -> assert false
    in
      
    (* 2 - Colocar e1 e e2 na pilha*)  
    compile_expr e1 ++
    compile_expr e2 ++

    (* 3 - Recebe os valores da pilha *)
    popq rax ++
    popq rbx ++
      
    (* 4 - Realiza a operacao e coloca o resultado na pilha *)
    op (reg rax) (reg rbx) ++
    pushq (reg rbx)

  | PEbinop (Ast.Band | Ast.Bor as o, e1, e2) ->
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
    compile_expr e1 ++
    popq rax ++
      
    (* 3 - Verificar se podemos terminar a expressao *)
    cmpq (imm64 1L) (reg rax) ++
    cmp_op ("lazy_evaluation_" ^ current_and_or) ++

    (* 4 - COlocar e2 no topo da pilha*)
    compile_expr e2 ++
    popq rax ++

    (* 5 - Realiza a operacao e coloca o resultado na pilha *)
    op  (imm64 1L) (reg rax) ++

    (* 6 - termina *)
    label ("lazy_evaluation_" ^ current_and_or) ++
    pushq (reg rax)

  | PEbinop (Ast.Beq | Ast.Bneq | Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge as o, e1, e2) ->
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
    compile_expr e1 ++
    compile_expr e2 ++

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

  | PEunop (Unot, e1) ->
    (* 1 - Incrementar numero de verificacoes *)
    number_of_bool_tests := !number_of_bool_tests + 1;
    let current_bool_test = string_of_int(!number_of_bool_tests) in
  
    (* 2 - Colocar e1 na pilha *)  
    compile_expr e1 ++
      
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

  | PEunop (Uneg, e) -> 
    (* 1 - Compila e *)  
    compile_expr e ++
    popq rax ++
      
    (* 2 - nega o valor de e *)
    negq (reg rax) ++
    pushq (reg rax)
  | PElen sz ->
    movq (imm sz) (reg rax) ++
    pushq (reg rax)
  | PEcall (id, args, size) ->
    List.fold_left (fun code e -> code ++ compile_expr e) nop args ++
    
    call id ++
    popn size ++

    pushq (reg rax)

  | PEstrc_access (id, el, pos) ->
    movq (ind ~ofs:pos rbp) (reg rax) ++
    pushq (reg rax)
  | PEstrc_decl (id, pairs, start) -> 
    List.fold_left(fun code (id, e, pos) -> 
      code ++
      (* 1 - Calcular o valor da expressao  *)
      compile_expr e ++
      popq rax ++
      (* 2 - Guardar o valor da expressao na posicao ofs *)
      movq (reg rax) (ind ~ofs:pos rbp) ++
      pushq (reg rax)
      ) nop pairs
  | PEvec_decl (els, start) ->
      List.fold_left(fun code (e, pos) -> 
      code ++
      (* 1 - Calcular o valor da expressão  *)
      compile_expr e ++
      popq rax ++
      (* 2 - Guardar o valor da expressao na posicao pos *)
      movq (reg rax) (ind ~ofs:pos rbp) ++
      pushq (reg rax)
      ) nop els
  | PEvec_access (id, e, el_size, pos, sz) ->
    compile_expr e ++
    popq rax ++

    movq (ind ~ofs:(pos) ~index:(rax) ~scale:(el_size) rbp) (reg rax) ++
    
    pushq (reg rax)
  | _ -> assert false
let rec compile_stmt = function
  | PSif (e, s1, elifs)-> 
     (*1 - Incrementa o numero de ifs realizados ate ao momento *)
      number_of_ifs := !number_of_ifs + 1;
      let current_if_test = string_of_int(!number_of_ifs) in
     
      let rec compile_elif index = function
        | [hd] ->
            let _, s = hd in 
            let body = compile_stmt s in
       
            label ("if_else_" ^ string_of_int index ^ current_if_test) ++
            body
     
        | hd::tl -> 
            let e, s = hd in 
            let body = compile_stmt s in
            
            label ("if_else_" ^ (string_of_int index) ^ current_if_test) ++
            compile_expr e ++
            popq rax ++
    
            (* 3 - Se vor diferente de 0 entao é verdade *)
            cmpq (imm 0) (reg rax) ++
            je ("if_else_" ^ string_of_int (index + 1) ^ current_if_test) ++
    
            body ++
            
            jmp ("if_end_" ^ current_if_test) ++
            
            compile_elif (index + 1) tl
        | _ -> label ("if_else_" ^ string_of_int index ^ current_if_test)
        in
        
      (* Corpo do if *)
      let body = compile_stmt s1 in
      
      (* 2 - Calcular o valor da condicao *)
      compile_expr e ++
      popq rax ++

      (* 3 - Se vor diferente de 0 entao é verdade *)
      cmpq (imm 0) (reg rax) ++
      je ("if_else_" ^ string_of_int 1 ^ current_if_test) ++
      
      body ++

      jmp ("if_end_" ^ current_if_test) ++

      (* 4a - Se for 0 vai à próxima condição*)
      compile_elif 1 elifs ++
    
      (* 5 - Termina *)
      label ("if_end_" ^ current_if_test)

  | PSwhile(e, body) ->
    (* 1 - Incrementa o numero de whiles existentes *)
    number_of_while := !number_of_while + 1;
    let while_index = string_of_int(!number_of_while) in
    loop_labels := [("while_" ^ while_index)]@(!loop_labels);
       
    let code = 
      (* 2.1 - Cria a label de inicio do while *)
      label ("while_" ^ while_index ^ "_inicio") ++
      
      (* 2.2 - Compila a condição *)
      compile_expr e ++
      popq rax ++
        
      cmpq (imm32 Int32.zero) (reg rax) ++
      je ("while_" ^ while_index ^ "_fim") ++

      (* 2.3 - Compila o corpo do while *)
      compile_stmt body ++

      (* 2.4 - volta à condição do while *)
      jmp ("while_" ^ while_index ^ "_inicio") ++
      label ("while_" ^ while_index ^ "_fim")
    in

    loop_labels := List.tl !loop_labels;
    code

  | PSdeclare(id, t, e, pos_list) -> get_elements e pos_list t
  | PSassign (id, e, pos) -> 
    (* 1 - Atualiza o valor que esta no endereço ofs*)
    compile_expr e ++
    popq rax ++
    movq (reg rax) (ind ~ofs:pos rbp)

  | PSprintn (e, t) ->
    (* 1 - Extrair tipo da expressão e *)
    let expr_type = get_str_type t in
    (* 2 - Vai buscar o valor de e *)  
    compile_expr e ++
    
    (* 3 - Passa como parametro para a funcao printn_int e chama-a*)
    popq rdi ++
    call ("printn_" ^ expr_type)
  | PSprint (e, t) ->
    (* 1 - Extrair tipo da expressão e *)
    let expr_type = get_str_type t in
    (* 2 - Vai buscar o valor de e *)
    compile_expr e ++

    (* 2 - Passa como parametro para a funcao print_int e chama-a*)
    popq rdi ++
    call ("print_" ^ expr_type)
  | PSblock bl  -> 
    (* 1 - Compila um bloco de instrucoes *)
    let block = List.rev(compile_block_stmt bl) in
    List.fold_right (++) block nop
  
  | PScontinue -> 
    if List.length !loop_labels <= 0 then error "Using the continue statement outside of a loop";
    let current_loop = List.hd !loop_labels in
    jmp (current_loop ^  "_inicio")

  | PSbreak    ->
    if List.length !loop_labels <= 0 then error "Using the break statement outside of a loop";
    let current_loop = List.hd !loop_labels in
     
    jmp (current_loop ^  "_fim")
    
  | PSreturn (e, pos_list) -> 
    if List.length !function_labels <= 0 then error "Using the break statement outside of a loop";
    let current_function = List.hd !function_labels in
    let code = ref (compile_expr e) in
    (List.iteri(fun i _ -> if i <> 0 then code := (!code) ++ (popq r9)) pos_list);
    (!code) ++ popq rax ++
    jmp (current_function ^ "_fim")
    
  | PSnothing  -> nop
  | PSexpr e   ->
    (* compile e *)
    compile_expr e
        
and compile_block_stmt = function
  | [] -> [nop]
  | s :: sl -> (compile_block_stmt sl) @ [compile_stmt s]
        
and compile_block_global_stmt = function
  | [] -> [nop]
  | s::sl -> (compile_block_global_stmt sl) @ [compile_global_stmt s]
        
and compile_global_stmt = function  
  | PGSblock bl -> 
    let block = List.rev(compile_block_global_stmt bl) in
    List.fold_right (++) block nop
  | PGSfunction(id, args, t, body, fp) ->
    
    function_labels := id::(!function_labels);

    (* 1 - Inicio da função *)
    let code =
      label id ++
      pushq (reg rbp) ++
      movq (reg rsp) (reg rbp) ++ 
      pushn fp ++

      compile_stmt body ++ 

      label (id ^ "_fim") ++ 
      popn fp ++
      popq rbp ++

      ret in

    function_labels := List.tl !function_labels;

    code

  | PGSstruct _-> nop

(* Compilação do programa p e grava o código no ficheiro ofile *)
let compile_program p ofile =
  let code = compile_global_stmt p in
  let p =
    { text =
        globl "main" ++
        code ++
        label "printn_int" ++
        movq (reg rdi) (reg rsi) ++
        movq (ilab ".Sprintn_int") (reg rdi) ++
        movq (imm 0) (reg rax) ++
        call "printf" ++
        ret ++
        label "print_int" ++
        movq (reg rdi) (reg rsi) ++
        movq (ilab ".Sprint_int") (reg rdi) ++
        movq (imm 0) (reg rax) ++
        call "printf" ++
        ret ++
        (* print bool *)
        label "print_bool" ++
        cmpq (imm 0) (reg rdi) ++
        je ".print_false" ++
        jne ".print_true" ++
        label ".print_true" ++
        movq (reg rdi) (reg rsi) ++
        movq (ilab ".true") (reg rdi) ++      
        movq (imm 0) (reg rax) ++
        call "printf" ++
        ret ++
        label ".print_false" ++
        movq (reg rdi) (reg rsi) ++
        movq (ilab ".false") (reg rdi) ++      
        movq (imm 0) (reg rax) ++
        call "printf" ++
        ret ++
        label "printn_bool" ++
        cmpq (imm 0) (reg rdi) ++
        je ".printn_false" ++
        jne ".printn_true" ++
        label ".printn_true" ++
        movq (reg rdi) (reg rsi) ++
        movq (ilab ".truen") (reg rdi) ++      
        movq (imm 0) (reg rax) ++
        call "printf" ++
        ret ++
        label ".printn_false" ++
        movq (reg rdi) (reg rsi) ++
        movq (ilab ".falsen") (reg rdi) ++      
        movq (imm 0) (reg rax) ++
        call "printf" ++
        ret ++
        label "print_error_z" ++
        movq (reg rdi) (reg rsi) ++
        leaq (lab ".Sprint_error_z") rdi ++
        movq (imm64 0L) (reg rax) ++
        call "printf" ++
        jmp "main_fim" ++
        label "print_error_f" ++
        movq (reg rdi) (reg rsi) ++
        leaq (lab ".Sprint_error_f") rdi ++
        movq (imm64 0L) (reg rax) ++
        call "printf" ++
        jmp "main_fim";
      data = 
        label ".Sprintn_int" ++ string "%ld\n" ++
        label ".Sprint_int" ++ string "%ld" ++
        label ".true" ++ string "true" ++ 
        label ".false" ++ string "false" ++
        label ".truen" ++ string "true\n" ++ 
        label ".falsen" ++ string "false\n" ++
        label ".Sprint_error_z" ++ string "\nError: Division by zero.\n\n" ++
        label ".Sprint_error_f" ++ string "\nFunction without return.\n\n" ++
        label "is_in_function" ++ dquad [0] ++
        label "number_of_loop" ++ dquad [0]
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
   (* "flush" do buffer para garantir que tudo foi para aí escrito antes de o fechar *)
  fprintf fmt "@?";
  close_out f
