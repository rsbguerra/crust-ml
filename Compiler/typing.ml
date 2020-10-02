open Ast

exception Error of string * int

let error s line = raise (Error (s, line))

type typ =
  | Tint
  | Tset of typ * typ
  | Tarray
  | Tarrayvar

let string_of_typ = function
  | Tint -> "Tint"
  | Tset _-> "Tset"
  | Tarray -> "Tarray"
  | Tarrayvar -> "Tarrayvar"

let compare_typ = function 
  | Tint, Tint -> true
  | Tset _, Tset _-> true
  | Tarray, Tarray -> true
  | Tarrayvar, Tarrayvar -> true 
  | _ -> false

let string_of_binop = function
  | Badd -> "+"
  | Bsub -> "-"
  | Bdiv -> "/"
  | Bmod -> "%"
  | Bmul -> "*"
  | Beq -> "=="
  | Bneq -> "!="
  | Blt -> "<" 
  | Ble -> "<="
  | Bgt -> ">" 
  | Bge -> ">="
  | Band -> "&&"
  | Bor -> "||"
  | _ -> assert false  

let is_int = function
  | Tint -> true
  | _    -> false

let is_set = function
  | Tset _ -> true
  | _      -> false

let is_array = function
  | Tarray -> true
  | _      -> false

let is_arrayvar = function 
  | Tarrayvar -> true
  | _         -> false

(* table_ctx representa um scope, contexto *)
type table_ctx = (string, typ * bool) Hashtbl.t

(* As funções são globais *)
let functions = (Hashtbl.create 17 : (string, table_ctx * costumtype * stmt) Hashtbl.t)

let rec find_id id l = 
  match l with
  | ct::tl -> if Hashtbl.mem ct id then [ct] @ (find_id id tl) else (find_id id tl) 
  | _ -> []

let costumtype_to_typ ctxs id = 
  match id with
  | Int -> Tint
  | CTid t ->
      let ctxs_in = find_id t ctxs in
      if List.length(ctxs_in) == 0 then error ("The type " ^ t ^ " was not defined.") 0;
      let ctx = List.hd (List.rev ctxs_in) in
      let t1,_ = Hashtbl.find ctx t in
      Hashtbl.replace ctx t (t1,true);
      t1

let rec verify_expr ctxs = function
  | Ecst _ -> Tint
  | Eminint _ | Emaxint _ -> Tint
  | Eset (e1, e2, line) ->
      let t1 = verify_expr ctxs e1 in
      let t2 = verify_expr ctxs e2 in
      if not (is_int t1) || not(is_int t2) then error ("The boundaries of a set need to be integers but was given ["^string_of_typ t1^" .. "^string_of_typ t2^"].") line; 
      Tset(t1, t2)

  | Ebinop (op, e1, e2, line) -> 
      let t1 = verify_expr ctxs e1 in
      let t2 = verify_expr ctxs e2 in
      if not (is_int t1) || not (is_int t2) then error ("The operator "^string_of_binop op^" was expecting two integers but was given "^string_of_typ t1^" and "^string_of_typ t2^".") line; 
      Tint
  | Eunop (_, e1, line) ->
      let t1 = verify_expr ctxs e1 in
      if not (is_int t1) then error ("The operator ! was expecting one integer but was given "^string_of_typ t1^".") line; 
      Tint
  | Ecall ("size", [el], line) ->
      let t1 = verify_expr ctxs el in
      if not (is_set t1) && not(is_arrayvar t1) then error ("The function size was expecting a Tset or Tarrayvar but was given "^string_of_typ t1^".") line; 
      Tint

  | Ecall (f, el, line) ->
      (* 1 - Verify if f exists *)
      if not (Hashtbl.mem functions f) then error ("Could not find a function with the identifier "^f^".") line;
      let function_ctx, _, _ = Hashtbl.find functions f in

      (* 2 - Verify if all of the arguments are of the type Tint or Tset *)
      (* TODO: Change the verification of the arguments to compare with the real types of the arguments *)
      let verify_arguments = function
        | hd::_ -> 
          let tp = verify_expr ctxs hd in
          if not(is_int tp) && not(is_set tp) then error ("Invalid argument type in the function call to "^f^", was expecting an Tint or Tset but was given "^string_of_typ tp^ ".") line
        | [] -> ()
      in
      verify_arguments el;
      
      (* 3 - Verify if was given all of the arguments *)
      if (List.length el) != (Hashtbl.length function_ctx)  then error ("Invalid call to the function "^f^", was given "^string_of_int(List.length el)^" arguments but was expecting "^string_of_int(Hashtbl.length function_ctx)^".") line;
      
      (* 4 - Returns the type of the function f *)
      Tint (* TODO: Change Tint to the return type of the function f *)

  | Eident(id, line) ->
      (* 1 - Verify if the id exists *)
      if List.length(find_id id ctxs) == 0 then error("Variable "^id^" not found.") line; 
      let ctx = List.hd (List.rev(find_id id ctxs)) in
      let t1,_ = Hashtbl.find ctx id in
      Hashtbl.replace ctx id (t1, true);
      
      (* 2 - Returns the type of the id *)
      fst(Hashtbl.find ctx id)

  | Eget (id, e1, line) -> 
      (* 1 - Verificar se o id existe *)
      if List.length(find_id id ctxs) == 0 then error ("Variable "^id^" not found.") line; 
      
      (* 2 - Verificar if id is of the type Tarrayvar *)
      let ctx = List.hd (List.rev(find_id id ctxs)) in
      let t1,_ = Hashtbl.find ctx id in
      Hashtbl.replace ctx id (t1, true);
      if not (is_arrayvar t1) then error ("The variable "^id^" has type "^string_of_typ t1^" but is being used as an array.") line;

      (* 3 - Verificar que e2 e do tipo int *)
      let t2 = verify_expr ctxs e1 in
      if not (is_int  t2) then error ("The type " ^ string_of_typ t1 ^ " can\'t be used as an index to access elements of an array.") line;

      (* 4 - Retornar Tint*)
      Tint

  | Eternary(cond, e1, e2, line) ->
    let tcond = verify_expr ctxs cond in 
    let t1 = verify_expr ctxs e1 in
    let t2 = verify_expr ctxs e2 in

    if not (is_int tcond) then error ("The condition of the ternary operator only supports integers but was given a " ^ string_of_typ tcond ^".") line;
    if not (compare_typ (t1, t2)) then error ("Both branches of the ternary operator need return the same type.") line;
    t1

and verify_array_type ctxs e =
  match e with 
  | ATInt -> Tset(Tint, Tint)
  | ATset(e1, e2) -> 
      let t1 =  verify_expr ctxs (Eset(e1, e2, 0)) in
      if not (is_set t1) then error ("Error defining an array. The size of an array needs to be of the type Tint or Tset but was given " ^ string_of_typ t1 ^ ".") 0;
      Tset(Tint, Tint)
  | ATid t -> 
      let t1 =  verify_expr ctxs (Eident(t, 0)) in
      if not (is_set t1) then error ("Error defining an array. The size of an array needs to be of the type Tint or Tset but was given " ^ string_of_typ t1 ^ ".") 0;
      Tset(Tint, Tint)

(* Verificacao de uma instrucao - Instruções nao devolvem um valor *)
let rec verify_stmt ctxs = function
  | Sif (e, s1, elif, line)   ->
      (* 1 - Verificar a expressao e do tipo Tint *)
      let t1 = verify_expr ctxs e in
      if not (is_int t1) then error ("The statement If only accepts conditions of the type Tint but was givin "^string_of_typ t1^".") line;
      let if_ctx = (Hashtbl.create 17 : table_ctx) in
      (* 2 - Verificar os dois ramos do if *)
      verify_stmt (ctxs@[if_ctx]) s1;
      (* 3 - Verificar todos os else if *)
      let rec verify_elif = function
        | hd::tl -> 
          let e, s, line = hd in 
          let t1 = verify_expr ctxs e in
          if not(is_int t1) then error ("The statement Else If only accepts conditions of the type Tint but was givin "^string_of_typ t1^".") line;
          let elif_ctx =  (Hashtbl.create 17 : table_ctx) in
          verify_stmt (ctxs@[elif_ctx]) s;
          verify_elif tl
        | _ -> ()  
        in
      verify_elif elif

  | Sreturn (e, line)         ->
      let t1 = verify_expr ctxs e in
      if not(is_int t1) then error ("The return statement only supports integers.") line
  
  | Snothing _                -> 
      ()
  | Sbreak _ | Scontinue _    -> 
      ()

  | Sdeclare (id, t, e, line) ->
      (* 1 - Verificar se o nome ja nao esta a ser usado *)
      let ctx = List.hd (List.rev ctxs) in
      if (Hashtbl.mem functions id) || (Hashtbl.mem ctx id) then error ("The identifier "^id^" was already defined in this scope.") line;     
      
      (* 2 - Verificar se o tipo existe e é do tipo Tint ou Tset *)
      let tp = costumtype_to_typ ctxs t in
      if not (is_int tp) && not (is_set tp) then error ("Error declaring a variable. Was expecting the type Tint or Tset but was given "^string_of_typ tp^".") line;

      (* 3 - Verificar se e do tipo Tint*)
      let t1 = verify_expr ctxs e in
      if not(is_int t1) then error ("The declaration statement only supports integers but was given a "^string_of_typ t1^".") line;
      Hashtbl.add ctx id (t1, false)

  | Sassign (id, e1, line)    ->
      (* 1 - Verificar se a variavel existe *)
      if List.length(find_id id ctxs) == 0 then error ("The variable "^id^" was not declared.") line;

      (* 2 - Verificar se estamos a lhe dar um Tint *)
      let t1 = verify_expr ctxs e1 in
      if not(is_int t1) then error ("The assignment statement only supports integers but was given a " ^ string_of_typ t1 ^".") line

  | Sdeclarearray (id, ida, e, line) ->
      (* 1 - Verificar se o id e unico *)
      let ctx = List.hd (List.rev ctxs) in
      if (Hashtbl.mem functions id) || (Hashtbl.mem ctx id) then error ("The identifier "^id^" is already defined in this scope.") line;

      (* 2 - Verificar se ida existe e é do tipo Tarray *)
      if List.length(List.rev (find_id ida ctxs)) == 0 then error ("The array type "^ida^" was not defined.") line;
      let array_ctx = List.hd (List.rev (find_id ida ctxs)) in
      let ta,_ = Hashtbl.find array_ctx ida in
      if not (is_array ta) then error ("Error declaring an array. Was expecting the type Tarray but was given "^string_of_typ ta^".") line;

      (* 3 - Verificar se _e_ e do tipo Tint*)
      let t1 = verify_expr ctxs e in
      if not (is_int t1) then error ("Error declaring an array. The elements can only be filled with integers but was given "^string_of_typ t1^".") line;
      Hashtbl.add ctx id (Tarrayvar, false)

  | Sarray (id, sz, t, line)  ->
      (* 1 - Verificar se o id e unico *)
      let ctx = List.hd (List.rev ctxs) in
      if (Hashtbl.mem functions id) || (Hashtbl.mem ctx id) then error ("The identifier "^id^" was already defined in this scope.") line;

      (* 2 - Verificar se o sz é um Tint ou Tset *)
      let t1 = verify_expr ctxs sz in
      if not (is_int t1) && not (is_set t1) then error ("Error defining an array. The size of an array needs to be of the type Tint or Tset but was given "^string_of_typ t1^".") line;

      (*3 - Verificar se t é do tipo Tset*)
      let t2 = verify_array_type ctxs t in
      if not (is_set t2) then error ("Error defining an array. The range of an array needs to be of the type Tset but was given "^string_of_typ t2^".") line;
      Hashtbl.add ctx id (Tarray, false) 

  | Sset (id, set, line) ->
      (* 1 - Verificar se o nome ja nao esta a ser usado *)
      let ctx = List.hd (List.rev ctxs) in
      if (Hashtbl.mem functions id) || (Hashtbl.mem ctx id) then error ("Error defining a set. The identifier "^id^" was already defined in this scope.") line;
  
      (* 2 - Verificar se estamos a lhe dar um set *)
      let t1 = verify_expr ctxs set in
      if not(is_set t1) then error ("Error defining a set. Was expecting a Tset but was given "^string_of_typ t1^ ".") line;
      Hashtbl.add ctx id (t1,false)

  | Sprint(e, line)      ->
      let t1 = verify_expr ctxs e in
      if not (is_int t1) then error ("The print statement only supports Tint but was given a "^string_of_typ t1^".") line;
  
  | Sprintn (e, line)    -> 
      let t1 = verify_expr ctxs e in
      if not (is_int t1) then error ("The printn statement only supports Tint but was given a " ^ string_of_typ t1 ^ ".") line 

  | Sscanf (id, line)    -> 
      (* 1 - Verificar se a variavel existe *)
      if List.length(find_id id ctxs) == 0 then error ("The variable " ^ id ^ " was not defined.") line;

      (* 2 - Verificar se id e do tipo Tint *)
      let ctx = List.hd (List.rev(find_id id ctxs)) in
      let t1 = fst(Hashtbl.find ctx id) in
      Hashtbl.replace ctx id (t1, true);
      if not (is_int t1) then error ("The scanf statement can only read integers but was used to read " ^ string_of_typ t1 ^ ".") line

  | Sblock (bl, _)       -> 
      verify_block_stmt ctxs bl

  | Sfor(id, t, e, cond, incr, bl, line) ->
      (* 1 - Adiciona o contexto do for e declaracao da sua variavel *)
      let ctxs = ctxs@[(Hashtbl.create 17 : table_ctx)] in
      verify_stmt ctxs (Sdeclare(id, Int, Ecst(0L, line), line));

      (* 3 - Verificar se o tipo existe e é do tipo Tint ou Tset *)
      let tp = costumtype_to_typ ctxs t in
      if not (is_int tp) && not (is_set tp) then error ("Error declaring the variable of the for statement. Was expecting the type Tint or Tset but was given "^string_of_typ tp^".") line;

      (* 4 - Verifica se o valor de e é um inteiro *)
      let t1 = verify_expr ctxs e in
      if not (is_int t1) then error ("The assigment of the for statement only accepts Tint but was givin a "^string_of_typ t1^".") line;

      (* 5 - Verifica se o valor de cond é um interiro *)
      let t2 = verify_expr ctxs cond in
      if not (is_int t2) then error ("The condition of the for statement only accepts Tint but was givin a "^string_of_typ t2^".") line;

      (* 6 - Verifica se o valor de incr é um interiro *)
      let t3 = verify_expr ctxs incr in
      if not (is_int t3) then error ("The increment of the for statement only accepts Tint but was givin a "^string_of_typ t3^".") line;

      (* 7 - Verifica o corpo *)
      verify_stmt ctxs bl

  | Sforeach(id, set, bl, line) ->
      (* 1 - Adiciona o contexto do foreach e declaracao da sua variavel *)
      let ctxs = ctxs@[(Hashtbl.create 17 : table_ctx)] in
      verify_stmt ctxs (Sdeclare(id, Int, Ecst(0L, line), line));

      (* 2 - Verifica que foi passado um conjunto *)
      let t1 = verify_expr ctxs set in
      if not (is_set t1) then error ("The foreach statement was expecting a set but was given "^string_of_typ t1^ ".") line;
  
      (* 3 - Verifica o corpo *)
      verify_stmt ctxs bl

  | Swhile(e, body, line) ->
      (* 1 - Adiciona o contexto do while *)
      let ctxs = ctxs@[(Hashtbl.create 17 : table_ctx)] in

      (* 2 - Verifica que foi passado um inteiro *)
      let t1 = verify_expr ctxs e in
      if not (is_int t1) then error ("The condition of the while statement only accepts Tint but was givin a " ^ string_of_typ t1 ^ ".") line;
  
      verify_stmt ctxs body

  | Sdowhile(e, body, line) ->
      (* 1 - Adiciona o contexto do while *)
      let ctxs = ctxs@[(Hashtbl.create 17 : table_ctx)] in
    
      verify_stmt ctxs body;

      (* 2 - Verifica que foi passado um inteiro *)
      let t1 = verify_expr ctxs e in
      if not (is_int t1) then error ("The condition of the do while statement only accepts Tint but was givin a " ^ string_of_typ t1 ^ ".") line
  
  | Saset (id, e1, e2, line) ->
      (* 1 - Verificar que id existe *)
      if List.length(find_id id ctxs) == 0 then error ("The variable "^id^" was not declared.") line;
      
      (* 2 - Verificar se ida existe e é do tipo Tarray *)
      let array_ctx = List.hd (List.rev (find_id id ctxs)) in
      let ta = fst(Hashtbl.find array_ctx id) in
      Hashtbl.replace array_ctx id (ta, true);
      if not (is_arrayvar ta) then error ("The variable"^id^" has type "^string_of_typ ta^" but s being used as an Tarrayvar.") line;

      (* 3 - Verificar que o index é do tipo Tint *)
      let t1 = verify_expr ctxs e1 in
      if not(is_int t1) then error ("The assignment statement of the type array was expecting a index of the type Tint but was given "^string_of_typ t1^".") line;
  
      (* 4 - Verificar que o novo valor e do tipo Tint *)
      let t2 = verify_expr ctxs e2 in
      if not(is_int t2) then error ("The assignment statement only supports integers but was given a "^string_of_typ t2^".") line  

and verify_stmts ctxs = function  
  | Stfunction (f, args, return, body, line) -> 
    (* 1 - Verifica que o o identificador de f ja nao esta em uso *)
    let ctx = List.hd (List.rev ctxs) in
    if (Hashtbl.mem ctx f) || (Hashtbl.mem functions f) then error ("The identifier "^f^" was already in used in this scope.") line;
    
    (* 2 - Verifica se todos os argumentos sao unicos e sao do tipo int ou set *)
    let args_ctx = (Hashtbl.create 17 : table_ctx) in
    
    let rec verify_arguments = function
      | hd::tl ->
        (* 2.1 - Verificar se o nome e unico *)
        let name, t = hd in
        let tp = costumtype_to_typ ctxs t in
        if Hashtbl.mem args_ctx name then error ("Lexical analysis: There are multiple arguments with the same identifier in the function " ^ f ^ ".") line; 
        if name = f then error ("There\'s a parameter with the function name, on the function "^f^".") line; 
        
        (* 2.2 - Verificar do tipo set ou int *)
        if not (is_set tp) && not (is_int tp) then error ("The arguments of a function can only be of the types Tint or Tset but got "^string_of_typ tp^" in the function "^ f ^ ".") line;
        
        (* 2.3 - As variaveis so podem receber inteiros *)
        Hashtbl.add args_ctx name (Tint, false);
        verify_arguments tl

      | [] -> ()
      in
    verify_arguments args;

    (* 3 - Verifica se o tipo do retorno e int ou set *)
    let return_typ = costumtype_to_typ ctxs return in
    if not(is_int return_typ) && not (is_set return_typ) then error ("The return of the function "^f^" needs to be of the type Tint or Tset but got "^string_of_typ return_typ^".") line;
        
    (* 4 - Se estiver tudo em ordem entao guarda a funcao *)
    Hashtbl.add functions f (args_ctx, return, body);

    (* Se verificarmos o corpo depois, deixamos que existam funcoes recursivas*)
    (* 5 - Verifica se o corpo esta bem tipado *)
    let body_context = Hashtbl.copy args_ctx in
    verify_stmt (ctxs@[body_context]) body
    
  | Stblock (bl, _) -> verify_block_stmts ctxs bl
  | Stmt (s, _) -> verify_stmt ctxs s

and verify_block_stmt ctxs = function
  | [] -> ()
  | s :: sl -> verify_stmt ctxs s; verify_block_stmt ctxs sl

and verify_block_stmts ctxs = function
  | [] -> ()
  | s :: sl -> verify_stmts ctxs s; verify_block_stmts ctxs sl

(* Realiza a analise semantica de um ficheiro *)
let file s = verify_stmts [(Hashtbl.create 17 : table_ctx)] s
