(*
  References: 
    - https://doc.rust-lang.org/reference/tokens.html
*)

{
  open Lexing
  open Tokens
  open Ast

  exception Lexing_error of string
  exception Lexing_error_comment of string

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl
  
  let keyword_table =
    create_hashtable 16
    [
      "break",    KW_BREAK;
      "continue", KW_CONTINUE;
      "else",     KW_ELSE;
      "false",    KW_FALSE;
      "fn",       KW_FN;
      "if",       KW_IF;
      "len",      KW_LEN;
      "let",      KW_LET;
      "mut",      KW_MUT;
      "print!",   KW_PRINT;
      "println!", KW_PRINTLN;
      "return",   KW_RETURN;
      "struct",   KW_STRUCT;
      "true",     KW_TRUE;
      "vec!",     KW_VEC;
      "Vec",      KW_TVEC;
      "while",    KW_WHILE;
    ]

  let line_num = ref 1

  let comment_level = ref 0

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1;  pos_bol = pos.pos_cnum }

}

(* ======== Rules ========== *)

(** Integer Literals*)
let INTEGER_SUFFIX = "i32"
let HEX_DIGIT      = ['0'-'9' 'a'-'f' 'A'-'F']
let DEC_DIGIT      = ['0'-'9']
let OCT_DIGIT      = ['0'-'7']
let BIN_DIGIT      = ['0'-'1']
let HEX_LITERAL    = ("0x"|"0X") (HEX_DIGIT|'_')*
let OCT_LITERAL    = ("0o"|"0O") (OCT_DIGIT|'_')*
let BIN_LITERAL    = ("0b"|"0B") (BIN_DIGIT|'_')*
let DEC_LITERAL    = DEC_DIGIT(DEC_DIGIT|'_')*
let INTEGER_LITERAL= (DEC_LITERAL|BIN_LITERAL|OCT_LITERAL|HEX_LITERAL)

(** Boolean Literals*)
let BOOLEAN_LITERAL = "true"|"false"

let alpha     = ['a'-'z' 'A'-'Z']
let id         = alpha(('_'|alpha|DEC_DIGIT)*)('!')*
let newline    = ['\n']
let whitespace = [' ' '\t']

rule analisador = parse
  | "//"            { singlecomment lexbuf}
  | "/*"            { comment_level := !comment_level+1; multicomment lexbuf }
  | newline         { new_line lexbuf; line_num := !line_num+1; analisador lexbuf}
  | whitespace      { analisador lexbuf}
  | '='             { [ASSIGN] }
  | '('             { [LPR] }
  | ')'             { [RPR] }
  | '{'             { [LBC] }
  | '}'             { [RBC] }
  | '['             { [LBK] }
  | ']'             { [RBK] }
  | '+'             { [PLUS] }
  | '-'             { [MINUS] }
  | '*'             { [TIMES] }
  | '/'             { [DIV] }
  | '%'             { [MOD] }
  | '&'             { [BITAND] }
  | '|'             { [BITOR] }
  | '^'             { [BITXOR] }
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
  | '!'             { [NOT] }
  | '.'             { [DOT] }
  | ':'             { [COLON] }
  | "->"            { [ARROW] }
  | ';'             { [DELIMITER] }
  | ','             { [COMMA] }
  | "i32"           { [I32] }
  | "bool"          { [BOOL] }
  | "()"            { [UNIT]}
  | INTEGER_LITERAL as snum 
    { (*Todo decide wich type this integer is *)
      try
        [CST ( Ci32 (Int32.of_string snum))]
      with _ -> raise (Lexing_error ("The constant is too big : _" ^ snum^"_")) }
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
  | "*/"      { comment_level := !comment_level-1; if !comment_level = 0 then analisador lexbuf else multicomment lexbuf}
  | "/*"      { comment_level := !comment_level+1; multicomment lexbuf}
  | eof       { raise (Lexing_error_comment "Commentary not closed, you need to close the multi line comments with: */")}
  | newline   { new_line lexbuf; line_num := !line_num + 1; multicomment lexbuf}
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