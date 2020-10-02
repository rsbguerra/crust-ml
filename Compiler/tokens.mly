%{
  open Ast
%}

%token <int64>     CST
%token <Ast.ident> IDENT
%token IF ELSE PRINT PRINTN SCANF VAL INT
%token FOREACH IN TO WHILE FOR DO
%token CONTINUE BREAK
%token FUNCTION RETURN
%token TYPE
%token ARRAY OF FILLED BY
%token MAXINT MININT
%token PLUS "+" 
%token MINUS TIMES DIV MOD
%token BITAND BITOR BITXOR LSHIFT RSHIFT BITNOT
%token GT ">"
%token GET LT LET 
%token EQ NEQ
%token TERNARY "?"
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
%token DELIMITER ";"
%token COMMA ","
%token EOF

/* Definição das prioridades e associatividades dos tokens */

%left TERNARY COLON
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