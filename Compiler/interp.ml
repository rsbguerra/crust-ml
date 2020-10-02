open Ast
open Format
open Int64

exception Error of string * int

let error s line = raise (Error(s, line))

exception Return of int64 * int
exception Break of int
exception Continue of int

let minint = Int64.min_int
let maxint = Int64.max_int

(* Tipos de dados*)
type value =
  | Vint of int64                (* valor       *)
  | Vset of int64 * int64        (* inicio, fim *)
  | Vlist of value array * value (* lista, range*)

let vint_to_int = function
  | Vint n     -> n
  | _          -> error "THE TYPING IS NOT GETTING ALL OF THE ERRORS - vint_to_int" 0


let vset_to_tuplo = function
  | Vint n     -> (0L , (Int64.sub n 1L))
  | Vset (i,f) -> (i, f)
  | _          -> error "THE TYPING IS NOT GETTING ALL OF THE ERRORS - vset_to_tuplo" 0

let vlist_to_tuple = function
  | Vlist (l,range) -> l, range
  | _               -> error "THE TYPING IS NOT GETTING ALL OF THE ERRORS - vlist_to_tuple" 0 

let string_of_value = function
  | Vint n      -> Int64.to_string n
  | Vset (i, f) -> "[" ^ Int64.to_string i ^ " ," ^ Int64.to_string f ^ "]"
  | _           -> error "THE TYPING IS NOT GETTING ALL OF THE ERRORS - string_of_value" 0

let size_of_value = function
  | Vint n     -> n
  | Vset(i,f)  -> Int64.sub f i
  | Vlist(_,r) -> let i,f = vset_to_tuplo r in Int64.sub f i

(* Funcao print *)
let print_value = function
  | Vint n ->  printf "%s" (Int64.to_string n)
  | _ -> error "THE TYPING IS NOT GETTING ALL OF THE ERRORS - print_value" 0

(* Falso = 0, Verdade tudo o que não é 0 *)
let is_false = function
  | Vint n -> if n = 0L then 1L else 0L
  | _ -> error "THE TYPING IS NOT GETTING ALL OF THE ERRORS - is_false" 0

let is_true v = if is_false v = 0L then 1L else 0L

let binop op v1 v2 line = match op, v1, v2 with
  | Badd, Vint n1, Vint n2 -> Vint (Int64.add n1 n2)  
  | Bsub, Vint n1, Vint n2 -> Vint (Int64.sub n1 n2)
  | Bmul, Vint n1, Vint n2 -> Vint (Int64.mul n1 n2)
  | Bdiv, Vint n, Vint 0L -> error ("Illegal expression: Division by zero, you are trying to do following operation: " ^ Int64.to_string n ^ " / 0.") line  
  | Bmod, Vint n, Vint 0L -> error ("Illegal expression: Division by zero, you are trying to do following operation: " ^ Int64.to_string n ^ " % 0.") line
  | Bmod, Vint n1, Vint n2 -> Vint (Int64.rem n1 n2)
  | Bdiv, Vint n1, Vint n2 -> Vint (Int64.div n1 n2)
  | Beq,  Vint n1, Vint n2 -> if compare n1 n2 = 0  then Vint 1L else Vint 0L
  | Bneq, Vint n1, Vint n2 -> if compare n1 n2 <> 0 then Vint 1L else Vint 0L
  | Blt,  Vint n1, Vint n2 -> if compare n1 n2 < 0  then Vint 1L else Vint 0L
  | Ble,  Vint n1, Vint n2 -> if compare n1 n2 <= 0 then Vint 1L else Vint 0L
  | Bgt,  Vint n1, Vint n2 -> if compare n1 n2 > 0  then Vint 1L else Vint 0L
  | Bge,  Vint n1, Vint n2 -> if compare n1 n2 >= 0 then Vint 1L else Vint 0L
  | Bitand, Vint n1, Vint n2 -> Vint (Int64.logand n1 n2) 
  | Bitor,  Vint n1, Vint n2 -> Vint (Int64.logor n1 n2) 
  | Bitxor, Vint n1, Vint n2 -> Vint (Int64.logxor n1 n2) 
  | Bitls,  Vint n1, Vint n2 -> Vint (Int64.shift_left n1 (Int64.to_int n2)) 
  | Bitrs,  Vint n1, Vint n2 -> Vint (Int64.shift_right n1 (Int64.to_int n2))
  | _ -> assert false

(* As funções são globais *)
let functions = (Hashtbl.create 17 : (string, argument list * value * stmt) Hashtbl.t)

(* table_ctx representa um scope *)
type table_ctx = (string, value * value) Hashtbl.t


let rec find_id id l = 
  match l with
  | ct::tl -> if Hashtbl.mem ct id then [ct] @ (find_id id tl) else (find_id id tl) 
  | _ -> []

let value_of_costumtype ctxs t = 
  match t with
  | Int     -> Vset(minint, maxint)
  | CTid id ->
      let ctx = List.hd (List.rev (find_id id ctxs)) in
      fst(Hashtbl.find ctx id)
        
let value_in_type_limits v t =
  let i,f = vset_to_tuplo t in
  v >= i && v <= f
      

(* Interpretação de uma expressão (devolve um valor) *)
let rec expr ctxs = function
  | Ecst (n, _) ->
      Vint n
  | Eset (e1, e2, line) -> 
      let i = expr_int ctxs e1 in 
      let f = expr_int ctxs e2 in
      if not(i < f) then error ("Invalid size of set. A set needs to have atleast the size of one, but you are trying to initialize a set with " ^Int64.to_string (Int64.sub f i) ^" elements.") line;
      Vset (i, f)

  | Eminint _ -> 
      Vint minint
      
  | Emaxint _ -> 
      Vint maxint

  | Ebinop (Band, e1, e2, _) ->
      let v1 = expr ctxs e1 in
      if is_true v1 = 1L then expr ctxs e2 else v1

  | Ebinop (Bor, e1, e2, _) ->
      let v1 = expr ctxs e1 in
      if is_false v1 = 1L then expr ctxs e2 else v1

  | Ebinop (Badd | Bsub | Bmul | Bdiv | Bmod |
            Beq | Bneq | Blt | Ble | Bgt | Bge | Bitand | Bitor | Bitxor | Bitls | Bitrs as op, e1, e2, line) ->
      binop op (expr ctxs e1) (expr ctxs e2) line
  | Eunop (Uneg, e1, _) -> 
      Vint (Int64.neg (expr_int ctxs e1))

  | Eunop (Unot, e1, _) ->
      Vint (is_false (expr ctxs e1))

  | Eunop (Ubitnot, e1, _) ->
      Vint (Int64.lognot  (expr_int ctxs e1))

  | Ecall ("size", [e1], _) -> 
      Vint (size_of_value (expr ctxs e1))

  | Ecall (f, el, line) ->
    begin try
        (* 1 - Vai buscar a funcao *)
        let args, _, body = Hashtbl.find functions f in

        (* 2 - Combinar os parametros passados com os parametros da funcao *)
        let comb = List.combine args el in

        (* 3 - Cria o contexto da funcao e atribui os valores ao respetivo argumento *)
        let ctxs = ctxs@[(Hashtbl.create 17 : table_ctx)] in
        List.iter (fun (arg, e) -> let aid, at = arg in interpret_stmt ctxs (Sdeclare(aid, at ,e, line))) comb;
        
        (* 4 - Interpreta o corpo da funcao *)
        interpret_stmt ctxs body;

        (* 5 - Se chegar aqui entao nao passou por uma instrucao return *)
        error ("The function " ^ f ^ " has no return statement but was expected one.") line
      with 
        | Return (r, line) ->
            let _,return,_ = Hashtbl.find functions f in
            if value_in_type_limits r return then Vint r 
            else error ("Value out of bounds. The value "^Int64.to_string r ^" can\'t be used as the return of the function " ^ f ^ ", try a value in the range of the type " ^ string_of_value return ^ ".") line;
    end
  | Eident(id, _) ->
      let ctx = List.hd (List.rev (find_id id ctxs)) in
      fst(Hashtbl.find ctx id)
        
  | Eget (id, e1, line) ->
      let index = expr_int ctxs e1 in

      (* 1 - Verificar se o index esta entre os limites *)
      let ctx = List.hd (List.rev (find_id id ctxs)) in
      let l, range = vlist_to_tuple (fst(Hashtbl.find ctx id)) in
      let i, f = vset_to_tuplo range in
      if (Int64.sub index i) < 0L || index > f then error ("Value out of bounds. The value "^Int64.to_string index ^" can\'t be used with the array "^id^", try a value in the range "^string_of_value range ^ ".") line;
      
      (* 2 - Retorna o valor *)
      l.(Int64.to_int(Int64.sub index i))

  | Eternary (cond, e1, e2, _) -> 
      let cond = expr_int ctxs cond in
      let v1 = expr ctxs e1 in
      let v2 = expr ctxs e2 in
  
      if cond = 1L then v1 else v2 

(* interpretação de um valor e verificação de que se trata de um inteiro *)
and expr_int ctxs e = 
  match expr ctxs e with
  | Vint n -> n
  | _ -> error "THE TYPING IS NOT GETTING ALL OF THE ERRORS - expr_int" 0

and expr_array_type ctxs e =
  match e with 
  | ATInt -> (minint, maxint)
  | ATset(e1, e2) -> 
      let v1 = expr_int ctxs e1 in
      let v2 = expr_int ctxs e2 in
      if v1 < v2 then (v1, v2)
      else error ("Invalid size of set. A set needs to have atleast the size of one, you are trying to do: [" ^ Int64.to_string v1 ^ " .. " ^ Int64.to_string v2 ^ "].") 0
  | ATid t -> 
      let v = expr ctxs (Eident(t, 0)) in
      vset_to_tuplo v
      
(* Interpretacao de instrucoes - locais*)
and interpret_stmt ctxs = function
  | Sif (e, s1, selif, _)   -> 
      let rec interpret_elif = function
        | hd::tl -> 
          let e, s, _ = hd in 
          let v1 = expr_int ctxs e in
          if v1 = 1L then interpret_stmt (ctxs@[(Hashtbl.create 17 : table_ctx)]) s
          else interpret_elif tl
        | _ -> ()
      in
      
      (* 1 - Verificar a condicao, se for verdade s1, se for falso s2 *)
      if is_true (expr ctxs e) = 1L 
      then interpret_stmt (ctxs@[(Hashtbl.create 17 : table_ctx)]) s1 
      (* 2 - Vai testar cada um dos else if e o else *)
      else interpret_elif selif
          
  | Snothing _ -> ()
  | Sreturn (e, line) ->
      (* 1 - Retorna a expressao e*)
      raise (Return ((expr_int ctxs e), line))

  | Sbreak line-> 
      raise (Break line) 

  | Scontinue line ->
      raise (Continue line)

  | Sassign (id, e1, line)  ->
      (* 1 - Ir buscar o tipo da variavel *)
      let local_tbl = List.hd (List.rev (find_id id ctxs)) in
      let t1 = snd(Hashtbl.find local_tbl id) in

      (* 2 - Ir buscar o novo valor*)
      let v1 = expr_int ctxs e1 in

      (* 3 - Verificar se o valor esta dentro dos limites *)
      if value_in_type_limits v1 t1 then 
        Hashtbl.replace local_tbl id (Vint v1, t1)
      else error ("Value out of bounds. The value "^Int64.to_string v1^" can\'t be used with the variable "^id^", try a value in the range "^string_of_value t1^".") line

  | Sdeclare (id, t, e1, line) ->
      (* 1 - Ir buscar o tipo da variavel*)
      let tp = value_of_costumtype ctxs t in
    
      (* 2 - Ir buscar o valor da variavel*)
      let local_tbl = List.hd (List.rev ctxs) in
      let v1 = expr_int ctxs e1 in

      (* 3 - Verificar que o valor esta dentro dos limites *)
      if value_in_type_limits v1 tp then
        Hashtbl.add local_tbl id (Vint v1, tp)
      else error ("Value out of bounds. The value "^Int64.to_string  v1^" can\'t be used with the variable " ^id^", try a value in the range " ^ string_of_value tp ^ ".") line

  | Sdeclarearray (id, ida, e, line) ->
      (* 1 - Ir buscar o valor da variavel*)
      let ctx = List.hd (List.rev ctxs) in
      let v1 = expr_int ctxs e in

      (* 2 - Ir buscar a array*)
      let ctx_array = List.hd (List.rev (find_id ida ctxs)) in
      let range, t1 = Hashtbl.find ctx_array ida in (*range * tipo)*)
      
      (* 3 - Verificar se o valor esta contido nos limites do tipo *)
      if not (value_in_type_limits v1 t1) then error ("Value out of bounds. Error declaring an array, the value " ^ Int64.to_string v1 ^ " can\'t be used with the variable " ^ id ^ ", try a value in the range " ^ string_of_value t1 ^ ".") line;
      
      (* 4 - Se tudo correu bem entao vamos adicionar *)
      let sz = size_of_value range in
      let arr = Array.make (Int64.to_int sz) (Vint v1) in
      Hashtbl.add ctx id ((Vlist (arr, range)), t1)

  | Sarray (id, sz, t, line) -> 
      (* 1 - Ir buscar o conjunto que define o tamanho *)
      let size = expr ctxs sz in

      (* 2 - Verificar que a array criada tem mais do que 0 elementos *)
      let sv = size_of_value size in
      if sv <= 0L then error ("Error defining the array type "^id^". An array must have more than 0 elements but was givin "^Int64.to_string sv^" elements.") line; 
      
      (* 3 - Verificar que tp esta contido nos limites do tipo*)
      let ti, tf = expr_array_type ctxs t in
      let t1 = Vset(ti, tf) in
      
      (* 4 - Adiciona ao contexto atual *)
      let ctx = List.hd (List.rev ctxs) in
      Hashtbl.add ctx id (size, t1)

  | Sset (id, set, _) ->
      (* 1 - Ir buscar o conjunto *)
      let ctx = List.hd (List.rev ctxs) in
      let v1 = expr ctxs set in

      (* 2 - Adiciona ao contexto atual *)
      Hashtbl.add ctx id (v1, Vset(minint, maxint))

  | Saset (id, e1, e2, line) ->
      (* 1 - Ir buscar a array *)
      let ctx = List.hd (List.rev (find_id id ctxs)) in
      let arr, t1 = Hashtbl.find ctx id in

      (* 2 - Vai buscar o index *)
      let index = expr_int ctxs e1 in

      (* 3 - Verifica se o valor esta contido no tipo *)
      let v1 = expr_int ctxs e2 in
      if not (value_in_type_limits v1 t1) then error ("Value out of bounds. Error assigning the value "^Int64.to_string v1^" can\'t be used with the array "^id^", try a value in the range "^string_of_value t1^".") line;
      
      (* 4 - Verificar se o index esta entre os limites *)
      let l, range = vlist_to_tuple arr in
      let i, f = vset_to_tuplo range in
      if (Int64.sub index i) < 0L || index > f then error ("Value out of bounds. The value "^Int64.to_string index^" can\'t be used with the array "^id^", try a value in the range "^string_of_value range^".") line;
      
      (* 5 - Atualiza o elemento da  array*)
      l.(Int64.to_int (Int64.sub index i)) <- Vint(v1)
      
  | Sprint (e, _) -> 
      (* 1 - Imprime a expressao _e_ sem \n*)
      print_value (expr ctxs e);

  | Sprintn (e, _) ->
      (* 1 - Imprime a expressao _e_ com \n*)
      print_value (expr ctxs e); printf "@."
  | Sscanf (id, line) -> 
      begin   try
        let x = (Scanf.scanf " %ld" (fun a -> a)) in 
        interpret_stmt ctxs (Sassign (id, Ecst (Int64.of_int32 x, line), line))
        with 
      | _ ->  begin interpret_stmt ctxs (Sassign (id, Ecst(0L, line), line)) end
      end
  
  | Sblock (bl, _) -> 
      interpret_block_stmt ctxs bl

  | Sfor(id, t, e, cond, incr, bl, line) ->
      (* 1 - Cria um contexto para o for *)
      let for_ctxs = ctxs@[(Hashtbl.create 17 : table_ctx)] in
      let v = ref (expr_int for_ctxs e) in
      interpret_stmt ctxs (Sdeclare(id, t, Ecst(!v, line), line));
      
      let i = ref (expr_int for_ctxs cond) in
      
      (* 2 - Iterar o corpo do foreach *)
      begin try 
      while(not (!i <= 0L)) do
        (* 2.1 - Cada iteração representa um contexto único*)
        let ctxs = ctxs@[(Hashtbl.create 17 : table_ctx)] in
        
        (* 2.2 - Atualizamos a variavel i *)
        interpret_stmt ctxs (Sdeclare(id, t, Ecst (!v, line), line));
  
        begin try
        (* 2.3 - Interpretamos o corpo do for*)
          interpret_stmt ctxs bl;
        with 
        | Continue _ -> ()
        | Break line -> raise (Break line) end;

        (* 2.4 - Atualizar o valor de id *)
        v := (expr_int ctxs incr);
        
        interpret_stmt ctxs (Sassign(id, Ecst(!v, line), line));

        (* 2.5 - Verificar a condicao *)
        i := (expr_int ctxs cond)
      done
      with
      | Break _ -> ()
    end
  | Sforeach(x, e, bl, line) ->
      (* 1 - Ir buscar os limites do foreach*)
      let i, f = vset_to_tuplo (expr ctxs e) in
      let i = ref i in
      (* 2 - Iterar o corpo do foreach *)
      begin try 
        while(!i <= f) do
        (* 2.1 - Cada iteração representa um contexto único*)
        let ctxs = ctxs@[(Hashtbl.create 17 : table_ctx)] in
      
        (* 2.2 - Atualizamos a variavel i *)
        interpret_stmt ctxs (Sdeclare(x, Int, Ecst(!i,line), line));

        (* 2.3 - Interpretamos o corpo do for*)
        begin try interpret_stmt ctxs bl; with Continue _ -> () end;
      
        (* 2.4 - Ir buscar o valor de  x *)
        let ctx = List.hd (List.rev ctxs) in
        let x = vint_to_int(fst(Hashtbl.find ctx x))in
      
        i := Int64.add x Int64.one
        done
      with
        | Break _ -> ()
      end 

  | Swhile (e, bl, _) ->
      (* 1 - Ir buscar o valor da expressão*)
      let i = ref 0L in
      i := vint_to_int(expr ctxs e);
      
      (* 2 - Iterar o corpo do while *)
      begin try 
      while not (!i = 0L) do
        (* 2.1 - Cada iteração representa um contexto único*)
        let while_ctxs = ctxs@[(Hashtbl.create 17 : table_ctx)] in
      
        begin try
        (* 2.2 - Interpretamos o corpo do while\*)
          interpret_stmt while_ctxs bl;
        with 
        | Continue _ -> ()
        | Break line -> raise (Break line) end;
  
        (* 2.3 - Verificar a condição do while *)
        i := vint_to_int(expr ctxs e)
      
      done
      with 
      | Break _ -> () 
    end
  | Sdowhile (e, bl, line) ->
    (* 1 - Ir buscar o valor da expressão*)
    let i = ref 0L in
    i := vint_to_int(expr ctxs e);
    
    (* 2 - Iterar o corpo do while *)
    begin try 
    while true do
      (* 2.1 - Cada iteração representa um contexto único*)
      let while_ctxs = ctxs@[(Hashtbl.create 17 : table_ctx)] in
    
      begin try
      (* 2.2 - Interpretamos o corpo do while\*)
        interpret_stmt while_ctxs bl;
      with 
      | Continue _ -> ()
      | Break line -> raise (Break line) end;

      (* 2.3 - Verificar a condição do while *)
      i := vint_to_int(expr ctxs e);
      if(!i = 0L) then raise (Break line);
    done
    with 
    | Break _ -> () 
  end    

(* Interpretacao de instrucoes - globais *)
and interpret_stmts ctxs = function  
  | Stfunction (f, args, return, body, _) ->
      (* 1 - Vamos buscar o retorno *)
      let return = value_of_costumtype ctxs return in

      (* 1 - Adicionamos a funcao a tabela*)
      Hashtbl.add functions f (args, return, body)

  | Stblock (bl, _) -> 
      (* 1 - Interpretamos o bloco de instruções *)
      interpret_block_stmts ctxs bl

  | Stmt(s, _) -> 
      (* 2 - Interpretamos uma instrucao *)
      interpret_stmt ctxs s

and interpret_block_stmt ctx = function
  | [] -> ()
  | s :: sl -> interpret_stmt ctx s; interpret_block_stmt ctx sl

and interpret_block_stmts ctx = function
  | [] -> ()
  | s :: sl -> interpret_stmts ctx s; interpret_block_stmts ctx sl

(* Interpretação de um ficheiro *)
let file s = interpret_stmts [(Hashtbl.create 17 : table_ctx)] s
