(* Produção de código para a linguagem Rust *)
open Format
open X86_64
open Ast
open Past
      
exception Error    of string
let error s = raise (Error s)

(* Variáveis para garantir que cada for/if/etc. tem labels com nomes diferentes *)
let number_of_while  = ref 0
let number_of_and_or = ref 0
let number_of_ifs    = ref 0
let number_of_bool_tests = ref 0
let number_of_while = ref 0
let number_of_print = ref 0

(* 
  Variaveis que guardam as labels dos loops e funções em que estamos, 
  utilizado para se saber em que loop ou função o break, continue e return 
  estão a funcionar 
*)
let (loop_labels:string list ref)           = ref []
let (function_labels:string list ref)       = ref []
let (print_labels:(string*string) list ref) = ref []

let rec get_str_type = function
  | PTunit -> "unit"
  | PTempty -> "empty"
  | PTi32 -> "int"
  | PTbool -> "bool"
  | PTstruct s -> s
  | PTvec t | PTref t | PTrefmut t -> get_str_type t

let rec get_elements e pos_list = function
  | PTi32 | PTbool -> 
    (* 1 - Calcular o valor da expressao  *)
    compile_expr e ++
    popq rax ++
    (* 2 - Guardar o valor da expressao na posicao ofs *)
    movq (reg rax) (ind ~ofs:(List.hd pos_list) rbp)

  | PTref t -> get_elements e pos_list t
  | PTvec t -> get_elements e pos_list t
  | _ -> 
    compile_expr e ++
    popq rax ++
    (* 2 - Guardar o valor da expressao na posicao ofs *)
    movq (reg rax) (ind ~ofs:(List.hd pos_list) rbp)

    (* comment "heuheuheu" ++ 
    compile_expr e ++ 
    comment "begin fold" ++ 
    (* List.fold_left (fun code p -> code ++ popq rax) nop pos_list *)
    List.fold_left (fun code p -> code ++ popq rax ++ (comment "begin end fold")) (nop) pos_list
     *)

and compile_binop op e1 e2 pos = 
match op with
  | Ast.Bassign -> 
    (* 1 - Atualiza o valor que esta no endereço ofs*)
    compile_expr e2 ++
    popq rax ++
    movq (reg rax) (ind ~ofs:pos rbp)

  | Ast.Bmod | Ast.Bdiv ->
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
    je ".print_error_z" ++

    (* 6 - Realiza a operacao e coloca o resultado na pilha *)
    idivq (reg rbx) ++
    pushq (reg rg)

  | Ast.Badd | Ast.Bsub | Ast.Bmul ->
    (* 1 - Dependendo da operacao queremos uma operacao diferente *)
    let op = match op with
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
  | Ast.Band | Ast.Bor -> 

    number_of_and_or := !number_of_and_or + 1;
    let current_and_or = string_of_int(!number_of_and_or) in
        
    (* 1 - Dependendo da operacao queremos uma operacao diferente *)
    let op, cmp_op = 
      match op with 
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

    (* 4 - Colocar e2 no topo da pilha*)
    compile_expr e2 ++
    popq rax ++

    (* 5 - Realiza a operacao e coloca o resultado na pilha *)
    (* Só é necessário analisar 2a expressão se:
      - 1 && b 
      - 0 || b
      Como estes dois casos apenas dependem do valor do b basta retornar
      o valor avaliado*)
    (* op (imm64 1L) (reg rax) ++ *) 

    (* 6 - termina *)
    label ("lazy_evaluation_" ^ current_and_or) ++
    pushq (reg rax)
  | Ast.Beq | Ast.Bneq | Ast.Blt | Ast.Ble | Ast.Bgt | Ast.Bge ->
    (* 1 - Dependendo da operacao queremos uma operacao diferente *)
    let op = match op with
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

and compile_unop op e pos = 
  match op with
  | Ast.Uneg -> 
    (* 1 - Compila e *)  
    compile_expr e ++
    popq rax ++
      
    (* 2 - nega o valor de e *)
    negq (reg rax) ++
    pushq (reg rax)
  
  | Ast.Unot -> 
    (* 1 - Incrementar numero de verificacoes *)
    number_of_bool_tests := !number_of_bool_tests + 1;
    let current_bool_test = string_of_int(!number_of_bool_tests) in
  
    (* 2 - Colocar e1 na pilha *)  
    compile_expr e ++
      
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

  | Ast.Uref -> 
    movq (ind ~ofs:pos rbp) (reg rax) ++
    pushq (reg rax)
  | Ast.Urefmut -> 
    movq (ind ~ofs:pos rbp) (reg rax) ++
    pushq (reg rax)
  | Ast.Uderef -> 
    movq (ind ~ofs:pos rbp) (reg rax) ++
    pushq (reg rax)

and compile_expr = function
  | PEint i -> 
    (* 1 - Colocar a constante no topo da pilha *)
    movq (imm32 i) (reg rax) ++
    pushq (reg rax)
  | PEbool b ->
    (* 1 - Colocar a constante no topo da pilha *)
    movq (imm32 (if b then Int32.one else Int32.zero)) (reg rax) ++
    pushq (reg rax)
  | PEident (id, pos) ->
      movq (ind ~ofs:pos rbp) (reg rax) ++
      pushq (reg rax)

  | PEbinop (op, e1, e2, pos)-> compile_binop op e1 e2 pos
  
  | PEunop (op, e, pos) -> compile_unop op e pos

  | PEstruct_access (id, el, pos) ->
      movq (ind ~ofs:pos rbp) (reg rax) ++
      pushq (reg rax)
  
  | PElen e -> nop
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
  | PEvec_access (e1, e2, el_size, pos) ->
    compile_expr e1 ++
    popq rax ++

    movq (ind ~ofs:(pos) ~index:(rax) ~scale:(el_size) rbp) (reg rax) ++
    
    pushq (reg rax)

  | PEcall (id, args, size) ->
    List.fold_left (fun code e -> code ++ compile_expr e) nop args ++
    
    call id ++
    (* popn size *)
    pushq (reg rax)

  | PEprint (s, pos) ->
    number_of_print := !number_of_print + 1;
    let print_index = string_of_int (!number_of_print) in
    let l = "string_" ^ print_index in
    print_labels := (l, s)::(!print_labels);
    movq (ilab l) (reg rsi) ++
    call (".print_string")

  | PEblock (bl, pos) -> compile_block bl
  
and compile_stmt = function
| PSnothing -> nop
| PSexpr e -> 
    compile_expr e 
    (* ++ popq rax *)
| PSdeclare (mut, id, t, e, pos_list) -> get_elements e pos_list t
| PSdeclare_struct (mut, id, t, pairs, start) -> 
    List.fold_left(fun code (id, e, pos) -> 
    code ++
    (* 1 - Calcular o valor da expressao  *)
    compile_expr e ++
    popq rax ++
    (* 2 - Guardar o valor da expressao na posicao ofs *)
    movq (reg rax) (ind ~ofs:pos rbp) ++
    pushq (reg rax)
    ) nop pairs
| PSwhile (e, body) -> 
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
      compile_block body ++

      (* 2.4 - volta à condição do while *)
      jmp ("while_" ^ while_index ^ "_inicio") ++
      label ("while_" ^ while_index ^ "_fim")
    in

    loop_labels := List.tl !loop_labels;
    code
| PSreturn (e, pos_list) -> 
    if List.length !function_labels <= 0 then error "Using the break statement outside of a loop";
    let current_function = List.hd !function_labels in
    let code = match e with
      | Some exp -> ref (compile_expr exp)
      | None -> ref (nop) in
    (List.iteri(fun i _ -> if i <> 0 then code := (!code) ++ (popq r9)) pos_list);
    (!code) ++ popq rax ++
    jmp (current_function ^ "_fim")

| PSif (e, b1, b2) ->
  (*1 - Incrementa o numero de ifs realizados ate ao momento *)
  number_of_ifs := !number_of_ifs + 1;
  let current_if_test = string_of_int(!number_of_ifs) in
  
    (* 2 - Calcular o valor da condicao *)
    compile_expr e ++
    popq rax ++

    (* 3 - Se for diferente de 0 entao é verdade *)
    cmpq (imm 0) (reg rax) ++
    je ("if_else_" ^ current_if_test) ++
    compile_block b1 ++
    jmp ("if_end_" ^ current_if_test) ++

    (* 4a - Se for 0 vai à próxima condição*)
    label ("if_else_" ^ current_if_test) ++    
    compile_block b2 ++
    jmp ("if_end_" ^ current_if_test) ++
    (* 5 - Termina *)
    label ("if_end_" ^ current_if_test)
    

and compile_block (b, e) = 
  
  let block = List.fold_left (fun code stmt -> code ++ compile_stmt stmt) nop b in
  
  match e with
  | Some e -> block ++ (compile_expr e)
  | None   -> block

and compile_decl = function
| PDstruct (id, pairs) -> nop
| PDfun (id, args, t, body, pos) -> 
  function_labels := id::(!function_labels);
  (* 1 - Inicio da função *)
  let code =
    label id ++
    pushq (reg rbp) ++
    movq (reg rsp) (reg rbp) ++ 
    pushn pos ++

    compile_block body ++ 
    label (id ^ "_fim") ++ 
    popn pos ++
    popq rbp ++
    (match t with
    | PTunit ->  movq (imm 0) (reg rax)
    | _ -> nop) ++
    ret in
    function_labels := List.tl !function_labels;
    code

(* Compilação do programa p e grava o código no ficheiro ofile *)
let compile_program p ofile =
  let code = (List.fold_left (fun acc decl -> acc ++ compile_decl decl) nop p ) in
  let print_str = List.fold_left (fun acc (l, str) -> acc ++ (label l) ++ (string str) ) nop !print_labels in
  let p =
    { text =
        globl "main" ++
        code ++
        
        label ".print_string" ++
        movq (ilab ".Sprint_string") (reg rdi) ++
        movq (imm 0) (reg rax) ++
        call "printf" ++
        ret ++
        label ".printn_int" ++
        movq (reg rdi) (reg rsi) ++
        movq (ilab ".Sprintn_int") (reg rdi) ++
        movq (imm 0) (reg rax) ++
        call "printf" ++
        ret ++
        label ".print_int" ++
        movq (reg rdi) (reg rsi) ++
        movq (ilab ".Sprint_int") (reg rdi) ++
        movq (imm 0) (reg rax) ++
        call "printf" ++
        ret ++
        (* print bool *)
        label ".print_bool" ++
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
        label ".printn_bool" ++
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
        label ".print_error_z" ++
        movq (reg rdi) (reg rsi) ++
        leaq (lab ".Sprint_error_z") rdi ++
        movq (imm64 0L) (reg rax) ++
        ret ++
        label ".print_error_f" ++
        movq (reg rdi) (reg rsi) ++
        leaq (lab ".Sprint_error_f") rdi ++
        movq (imm64 0L) (reg rax) ++
        call "printf" ++
        ret;
      data = 
        label ".Sprintn_int" ++ string "%ld\n" ++
        label ".Sprint_int" ++ string "%ld" ++
        label ".Sprint_string" ++ string "%s" ++
        label ".true" ++ string "true" ++ 
        label ".false" ++ string "false" ++
        label ".truen" ++ string "true\n" ++ 
        label ".falsen" ++ string "false\n" ++
        label ".Sprint_error_z" ++ string "\nError: Division by zero.\n\n" ++
        label ".Sprint_error_f" ++ string "\nFunction without return.\n\n" ++
        label "is_in_function" ++ dquad [0] ++
        label "number_of_loop" ++ dquad [0] ++
        print_str
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
   (* "flush" do buffer para garantir que tudo foi para aí escrito antes de o fechar *)
  fprintf fmt "@?";
  close_out f
