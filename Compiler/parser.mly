(*
  Última alteração: 28-12-2019
  Descricao: Parser do Natrix
*)

/* Ponto de entrada da gramática */
%start prog

/* Tipo dos valores devolvidos pelo parser */
%type <Ast.program> prog

%%

prog:
| b = list(global_stmt) EOF { GSblock(b, !Lexer.line_num) }
;

global_stmt:
| KW_USE f = ident    { GSuse (f, !Lexer.line_num) }
| KW_STRUCT i = ident { GSstruct (i, !Lexer.line_num) }
| KW_IMPL i = ident   { GSstruct (i, !Lexer.line_num) }
| KW_FN f = ident "(" x = separated_list(",", argument_list) ")" ARROW r = crust_types s = function_suite 
                      { GSfunction(f, x, r, s, !Lexer.line_num)} 
;


argument_list:
|  id = ident ":" t = crust_types {(id, t)}
;

function_suite:
| "{" l = list(stmt) "}"    { Sblock (l, !Lexer.line_num)}
| "="">" e = expr ";"       { Sreturn(e, !Lexer.line_num)}
;


suite:
| l = list(stmt)     { Sblock (l, !Lexer.line_num) }
;

elif:
| KW_ELSE KW_IF e = expr "{" s = suite "}" { (e, s, !Lexer.line_num) }
| KW_ELSE "{" s = suite "}"                { ( Ecst( Cbool true, !Lexer.line_num), s, !Lexer.line_num) }
;

stmt:
| s = simple_stmt                                   { s } 
| KW_IF e = expr "{" s1 = suite "}" l = list(elif)  { Sif(e, s1, l, !Lexer.line_num)}
| KW_WHILE e = expr "{" s = suite "}"               { Swhile(e, s, !Lexer.line_num) }
| KW_LOOP "{" s = suite "}"                         { Sloop(s, !Lexer.line_num)}
;

simple_stmt:
| KW_RETURN e = expr ";"                              { Sreturn(e, !Lexer.line_num) }
| KW_BREAK ";"                                        { Sbreak !Lexer.line_num }
| KW_CONTINUE ";"                                     { Scontinue !Lexer.line_num }
| KW_LET id = ident ":" t = crust_types "=" e = expr ";" { Sdeclare (id, t, e, !Lexer.line_num) }
| id = ident o = binop"=" e = expr ";"             { Sassign (id, Ebinop(o, Eident (id, !Lexer.line_num), e, !Lexer.line_num), !Lexer.line_num) }
| KW_PRINT "(" e = expr ")" ";"                       { Sprint(e, !Lexer.line_num) }
| KW_PRINTLN "(" e = expr ")" ";"                     { Sprintn(e, !Lexer.line_num) }
| ";"                                              { Snothing(!Lexer.line_num) }
;

crust_types:
| I8     { Ti8   }
| I16    { Ti16  }
| I32    { Ti32  }
| I64    { Ti64  }
| I128   { Ti128 }
| U8     { Tu8   }
| U16    { Tu16  }
| U32    { Tu32  }
| U64    { Tu64  }
| U128   { Tu128 }
| BOOL   { Tbool }
;

expr:
| c = CST                           { Ecst (c, !Lexer.line_num) }
| KW_TRUE                           { Ecst ((Cbool true), !Lexer.line_num) }
| KW_FALSE                          { Ecst ((Cbool false), !Lexer.line_num) }
| id = ident                        { Eident (id, !Lexer.line_num) }
| u  = unop e1 = expr               { Eunop (u, e1, !Lexer.line_num) }
| e1 = expr o = binop e2 = expr     { Ebinop (o, e1, e2, !Lexer.line_num) }
| id = ident "(" l = separated_list("," , expr) ")" { Ecall(id, l, !Lexer.line_num)}
| "(" e = expr ")"                  { e }
;

%inline unop:
| MINUS  { Uneg }
| NOT    { Unot }
| BITNOT { Ubitnot }
;

%inline binop:
| PLUS  { Badd }
| MINUS { Bsub }
| TIMES { Bmul }
| DIV   { Bdiv }
| MOD   { Bmod }
| EQ    { Beq }
| NEQ   { Bneq }
| GT    { Bgt }
| LT    { Blt }
| GET   { Bge }
| LET   { Ble }
| AND   { Band }
| OR    { Bor  }
| BITAND{ Bitand } 
| BITOR { Bitor }
| BITXOR{ Bitxor }
| LSHIFT{ Bitls }
| RSHIFT{ Bitrs }
;

ident:
| id = IDENT { id }
;