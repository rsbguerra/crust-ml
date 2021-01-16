%{
  open Ast
%}

%token <Ast.crust_const>     CST
%token <Ast.ident> IDENT
%token KW_TVEC

%token I32 BOOL 
%token KW_IF KW_ELSE
%token KW_WHILE
%token KW_CONTINUE KW_BREAK
%token KW_FN KW_RETURN
%token PLUS "+"
%token MINUS TIMES DIV MOD
%token BITAND BITOR BITXOR LSHIFT RSHIFT
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
%token EOF
%token ARROW "->"
%token UNIT

%token KW_FALSE
%token KW_LET
%token KW_STRUCT
%token KW_TRUE
%token KW_PRINTLN
%token KW_PRINT
%token KW_VEC
%token KW_LEN

%token KW_MUT

/* Definição das prioridades e associatividades dos tokens */

%right "="
%left OR
%left AND
%left BITOR
%left BITXOR
%left BITAND
%left EQ NEQ GT LT GET LET 
%left PLUS MINUS
%left TIMES DIV MOD
%left NOT PTR BITNOT REF
%left "[" "]"
%nonassoc DOT
%%
