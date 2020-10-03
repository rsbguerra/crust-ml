%{
  open Ast
%}

%token <int64>     CST
%token <Ast.ident> IDENT
%token IF ELSE PRINT PRINTN SCANF INT
%token LOOP WHILE FOR
%token CONTINUE BREAK
%token FUN RETURN
%token TYPE
%token PLUS "+" 
%token MINUS TIMES DIV MOD
%token BITAND BITOR BITXOR LSHIFT RSHIFT BITNOT
%token GT ">"
%token GET LT LET 
%token EQ NEQ
%token LPR "(" 
%token RPR ")"
%token LBC "{"
%token RBC "}"
%token AND "&&"
%token OR "||"
%token NOT "!"
%token ASSIGN "="
%token COLON ":"
%token DELIMITER ";"
%token COMMA ","
%token EOF

/* Definição das prioridades e associatividades dos tokens */

%left COLON
%left OR
%left AND
%left BITOR
%left BITXOR
%left BITAND
%left EQ NEQ
%left GT LT GET LET 
%left LSHIFT RSHIFT
%left PLUS MINUS
%left TIMES DIV MOD
%left NOT BITNOT

%%