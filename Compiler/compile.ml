(* Produção de código para a linguagem Rust *)
      
open Format
open X86_64
open Past
      
exception VarUndef of string
exception Error    of string
let error s = raise (Error s)

(* Variáveis para garantir que cada for/if/etc. tem labels com nomes diferentes *)
let number_of_while  = ref 0
let number_of_and_or = ref 0
let number_of_ifs    = ref 0

let loops = ref []

(* Tamanho da frame, em byte (cada variável local ocupa 8 bytes) *)
let frame_size = ref 0

let get_value = function
 | Ast.Ci32 v  -> v
 | Ast.Cbool v -> if v then Int32.one else Int32.zero
 | _ -> assert false

let get_str_type = function
  | Ast.Ti32 -> "int"
  | Ast.Tbool -> "bool"
  | _ -> assert false

let rec compile_expr = function
  | PEcst i ->
    (* 1 - Colocar a constante no topo da pilha *)
    movq (imm32 (get_value i)) (reg rax) ++
    pushq (reg rax)

  | PEident (id, pos) -> 
    movq (ind ~ofs:pos rbp) (reg rax) ++
    pushq (reg rax)
  | PEbinop _ -> assert false
  | PEunop _ -> assert false
  | PEcall _ -> assert false


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
    loops := [("while_" ^ while_index)]@(!loops);
       
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

    loops := List.tl !loops;
    code

  | PSdeclare(id, t, e, pos) -> 
    (* 1 - Calcular o valor da expressao  *)
    compile_expr e ++
    popq rax ++
  
    (* 2 - Guardar o valor da expressao na posicao ofs *)
    movq (reg rax) (ind ~ofs:pos rbp)

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
    if List.length !loops <= 0 then error "Using the continue statement outside of a loop";
    let current_loop = List.hd !loops in
    jmp (current_loop ^  "_inicio")

  | PSbreak    ->
    if List.length !loops <= 0 then error "Using the break statement outside of a loop";
    let current_loop = List.hd !loops in
     
    jmp (current_loop ^  "_fim")
    
  | PSreturn _ -> nop
  | PSnothing  -> nop
  | PSexpr _   -> assert false
        
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
  | PGSfunction(_, _, _, body) ->
    compile_stmt body
  | _ -> assert false

(* Compilação do programa p e grava o código no ficheiro ofile *)
let compile_program p ofile =
  let code = compile_global_stmt p in
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
        label ".true" ++ string "true" ++ 
        label ".false" ++ string "false" ++
        label ".truen" ++ string "true\n" ++ 
        label ".falsen" ++ string "false\n" ++
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
