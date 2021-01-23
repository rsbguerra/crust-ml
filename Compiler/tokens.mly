(*
  Last Modification: 23-12-2020
  Description: Pico-Rust Lexer Tokens
*)

%{
  open Ast
%}

%token <int32> CST
%token <string> STRING
%token <Ast.ident> IDENT

%token KW_IF KW_ELSE
%token KW_WHILE
%token KW_FN KW_RETURN
%token PLUS MINUS TIMES DIV MOD
%token BITAND
%token GT GET LT LET
%token EQ NEQ
%token LPR "(" 
%token RPR ")"
%token LBC "{"
%token RBC "}"
%token LBK "["
%token RBK "]"
%token AND "&&"
%token OR "||"
%token NOT "!"
%token ASSIGN "="
%token COLON ":"
%token DOT "."
%token DELIMITER ";"
%token COMMA ","
%token ARROW "->"
%token EOF

%token KW_FALSE
%token KW_LET
%token KW_STRUCT
%token KW_TRUE
%token KW_PRINT
%token KW_VEC
%token KW_LEN
%token KW_MUT

/* Definição das prioridades e associatividades dos tokens */

%right ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ GT LT GET LET 
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc UNARY_EXPR
%nonassoc LBK
%nonassoc DOT
%%
