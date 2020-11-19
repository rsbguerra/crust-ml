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
    create_hashtable 64
    [
      (* === strict === *)
      "as",       KW_AS;
      "break",    KW_BREAK;
      "const",    KW_CONST;
      "continue", KW_CONTINUE;
      "crate",    KW_CRATE;
      "else",     KW_ELSE;
      "enum",     KW_ENUM;
      "extern",   KW_EXTERN;
      "false",    KW_FALSE;
      "fn",       KW_FN;
      "for",      KW_FOR;
      "if",       KW_IF;
      "impl",     KW_IMPL;
      "in",       KW_IN;
      "let",      KW_LET;
      "loop",     KW_LOOP;
      "match",    KW_MATCH;
      "mod",      KW_MOD;
      "move",     KW_MOVE;
      "mut",      KW_MUT;
      "pub",      KW_PUB;
      "ref",      KW_REF;
      "return",   KW_RETURN;
      "self",     KW_SELFVALUE;
      "Self",     KW_SELFTYPE;
      "static",   KW_STATIC;
      "struct",   KW_STRUCT;
      "super",    KW_SUPER;
      "trait",    KW_TRAIT;
      "true",     KW_TRUE;
      "type",     KW_TYPE;
      "unsafe",   KW_UNSAFE;
      "use",      KW_USE;
      "where",    KW_WHERE;
      "while",    KW_WHILE;
      "async",    KW_ASYNC;
      "await",    KW_AWAIT;
      "dyn",      KW_DYN;
      "println!", KW_PRINTLN;
      "print!",   KW_PRINT;
      
      (* === Reserved ===*)
      "abstract", KW_ABSTRACT;
      "become",   KW_BECOME;
      "box",      KW_BOX;
      "do",       KW_DO;
      "final",    KW_FINAL;
      "macro",    KW_MACRO;
      "override", KW_OVERRIDE;
      "priv",     KW_PRIV;
      "try",      KW_TRY;
      "typeof",   KW_TYPEOF;
      "unsized",  KW_UNSIZED;
      "virtual",  KW_VIRTUAL;
      "yield",    KW_YIELD;

      (* === Weak ===*)
      "union",    KW_UNION;
      "'static",  KW_STATICLIFETIME
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
let INTEGER_SUFFIX = "u8"|"u16"|"u32"|"u64"|"u128"|"usize"|"i8"|"i16"|"i32"|"i64"|"i128"|"isize"
let HEX_DIGIT      = ['0'-'9' 'a'-'f' 'A'-'F']
let DEC_DIGIT      = ['0'-'9']
let OCT_DIGIT      = ['0'-'7']
let BIN_DIGIT      = ['0'-'1']
let HEX_LITERAL    = ("0x"|"0X") (HEX_DIGIT|'_')*
let OCT_LITERAL    = ("0o"|"0O") (OCT_DIGIT|'_')*
let BIN_LITERAL    = ("0b"|"0B") (BIN_DIGIT|'_')*
let DEC_LITERAL    = DEC_DIGIT(DEC_DIGIT|'_')*
let INTEGER_LITERAL= (DEC_LITERAL|BIN_LITERAL|OCT_LITERAL|HEX_LITERAL)

(** Floating-point literals*)
let FLOAT_SUFFIX   = "f32"|"f64"
let FLOAT_EXPONENT = ('e'|'E') ('+'|'-')? (DEC_DIGIT|'_')* DEC_DIGIT (DEC_DIGIT|'_')*
let FLOAT_LITERAL  = DEC_LITERAL '.' | DEC_LITERAL FLOAT_EXPONENT | DEC_LITERAL '.' DEC_LITERAL FLOAT_EXPONENT? | DEC_LITERAL ('.' DEC_LITERAL)? FLOAT_EXPONENT? FLOAT_SUFFIX

(** Boolean Literals*)
let BOOLEAN_LITERAL = "true"|"false"

let letter     = ['a'-'z' 'A'-'Z']
let char       = ''' letter '''
let string     = '"' ('_'|letter|DEC_DIGIT)* '"'
let id         = ('_'|letter)(('_'|letter|DEC_DIGIT)*)('!')*
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
  | '~'             { [BITNOT] }
  | '!'             { [NOT] }
  | ':'             { [COLON] }
  | "->"            { [ARROW] }
  | ';'             { [DELIMITER] }
  | ','             { [COMMA] }
  | "u8"            { [U8] }
  | "u16"           { [U16] }
  | "u32"           { [U32] }
  | "u64"           { [U16] }
  | "u128"          { [U128] }
  | "i8"            { [I8] }
  | "i16"           { [I16] }
  | "i32"           { [I32] }
  | "i64"           { [I64] }
  | "i128"          { [I128] }
  | "bool"          { [BOOL] }
  | INTEGER_LITERAL as snum 
    { (*Todo decide wich type this integer is *)
      try
        [CST ( Ci64 (Stdint.Int64.of_string snum))]
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