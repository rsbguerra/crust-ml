{
  open Lexing
  open Tokens

  exception Lexing_error of string
  exception Lexing_error_comment of string


  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl
  
  let keyword_table =
    create_hashtable 32
    [
      ("val", VAL);
      ("int", INT);
      ("if", IF);
      ("else", ELSE);
      ("foreach", FOREACH);
      ("while", WHILE);
      ("do", DO);
      ("for", FOR);
      ("in", IN);
      ("break", BREAK);
      ("continue", CONTINUE);
      ("type", TYPE);
      ("array", ARRAY);
      ("of", OF);
      ("filled", FILLED);
      ("by", BY);
      ("print", PRINT);
      ("printn",PRINTN);
      ("scanf", SCANF);
      ("function", FUNCTION);
      ("return", RETURN);
      ("maxint", MAXINT);
      ("minint", MININT)
    ]
  let line_num = ref 1

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1;  pos_bol = pos.pos_cnum }

}

let digit      = ['0'-'9']
let letter     = ['a'-'z' 'A'-'Z']
let integer    = digit+
let id         = ('_'|letter)('_'|letter|digit)*
let newline    = ['\n']
let whitespace = [' ' '\t']

rule analisador = parse
  | "//"            { singlecomment lexbuf}
  | "(*"            { multicomment lexbuf }
  | newline         { new_line lexbuf; line_num := !line_num + 1;analisador lexbuf}
  | whitespace      { analisador lexbuf}
  | '='             { [ASSIGN] }
  | '('             { [LPR] }
  | ')'             { [RPR] }
  | '['             { [LBK] }
  | ']'             { [RBK] }
  | '{'             { [LBC] }
  | '}'             { [RBC] }
  | '+'             { [PLUS] }
  | '-'             { [MINUS] }
  | '*'             { [TIMES] }
  | '/'             { [DIV] }
  | '%'             { [MOD] }
  | '?'             { [TERNARY] }
  | "&"             { [BITAND] }
  | "|"             { [BITOR] }
  | "^"             { [BITXOR] }
  | "<<"            { [LSHIFT] }
  | ">>"            { [RSHIFT] }
  | "<"             { [LT] }
  | "<="            { [LET] }
  | ">"             { [GT] }
  | ">="            { [GET] }
  | "=="            { [EQ] }
  | "!="            { [NEQ] }
  | "||"            { [OR] }
  | "&&"            { [AND] }
  | "~"             { [BITNOT] }
  | "!"             { [NOT] }
  | ':'             { [COLON] }
  | ';'             { [DELIMITER] }
  | ".."            { [TO] }
  | ','             { [COMMA] }
  | integer as snum 
    { 
      try
        [CST (Int64.of_string snum)]
      with _ -> raise (Lexing_error ("The constant is too big : " ^ snum)) }
  | id as word
  { try
      let token = Hashtbl.find keyword_table word in  
      [token]
    with Not_found -> [IDENT word]
  }
  | eof       { [EOF] }
  | _ as c    { raise (Lexing_error (Char.escaped c)) }

and singlecomment = parse
  | newline      { newline lexbuf; line_num := !line_num + 1; analisador lexbuf}
  | eof       { [EOF]}
  | _         { singlecomment lexbuf}

and multicomment = parse
  | "*)"      { analisador lexbuf}
  | eof       { raise (Lexing_error_comment "Commentary not closed, you need to close the multi line comments with: *)")}
  | newline   {new_line lexbuf; line_num := !line_num + 1; multicomment lexbuf}
  | _         { multicomment lexbuf}

{
  let next_token =
    let tokens = Queue.create () in
    fun lb ->
      if Queue.is_empty tokens then 
      begin
        let l = analisador lb in
        List.iter (fun t -> Queue.add t tokens) l
      end;
      Queue.pop tokens
}