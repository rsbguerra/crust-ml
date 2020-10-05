%{
  open Ast
%}

%token <Ast.crust_conts>     CST
%token <Ast.ident> IDENT

%token I8 I16 I32 I64 I128 U8 U16 U32 U64 U128 BOOL
%token KW_IF KW_ELSE PRINT PRINTN SCANF
%token KW_LOOP KW_WHILE KW_FOR
%token KW_CONTINUE KW_BREAK
%token KW_FN KW_RETURN
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
(* === TODO === *)

(* === Strict === *)
%token KW_AS
%token KW_CONST
%token KW_CRATE
%token KW_ENUM
%token KW_EXTERN
%token KW_FALSE
%token KW_IMPL
%token KW_IN
%token KW_LET
%token KW_MATCH
%token KW_MOD
%token KW_MOVE
%token KW_MUT
%token KW_PUB
%token KW_REF
%token KW_SELFVALUE
%token KW_SELFTYPE
%token KW_STATIC
%token KW_STRUCT
%token KW_SUPER
%token KW_TRAIT
%token KW_TRUE
%token KW_TYPE
%token KW_UNSAFE
%token KW_USE
%token KW_WHERE
%token KW_ASYNC
%token KW_AWAIT
%token KW_DYN

(* === Reserved ===*)
%token KW_ABSTRACT
%token KW_BECOME
%token KW_BOX
%token KW_DO
%token KW_FINAL
%token KW_MACRO
%token KW_OVERRIDE
%token KW_PRIV
%token KW_TRY
%token KW_TYPEOF
%token KW_UNSIZED
%token KW_VIRTUAL
%token KW_YIELD

(* === Weak ===*)
%token KW_UNION
%token KW_STATICLIFETIME

%token KW_PRINTLN
%token KW_PRINT

/* Definição das prioridades e associatividades dos tokens */

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
