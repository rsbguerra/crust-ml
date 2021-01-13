(*
  Last Modification: 12-12-2020
  Description: Pico Rust Parser
  Author: Dário Santos (dariovfsantos@gmail.com)
*)

/* Grammar Entry Point */
%start prog

/* Tipo dos valores devolvidos pelo parser */
%type <Ast.program> prog

%%

prog:
| b = list(global_stmt) EOF { GSblock(b, !Lexer.line_num) }
;

global_stmt:
| KW_STRUCT i = ident "{" l = separated_list(",", struct_elements) "}" { GSstruct (i, l, !Lexer.line_num) }
| KW_FN f = ident "(" x = separated_list(",", argument_list) ")" ARROW r = crust_types s = function_suite
                      { GSfunction(f, x, r, s, !Lexer.line_num)} 
;

struct_elements:
|  id = ident ":" t = crust_types {(id, t)}
;

argument_list:
|  id = ident ":" t = crust_types {(id, t)}
;

function_suite:
| "{" l = list(stmt) "}"     {Sblock (l, !Lexer.line_num) }
;

suite:
| "{" l = list(stmt) r = option(expr) "}"     {let r = (match r with | None -> [] | Some e -> [Sexpr(e, !Lexer.line_num)]) in Sblock (l@r, !Lexer.line_num) }
;

elif:
| KW_ELSE KW_IF e = expr s = function_suite  {(e, s, !Lexer.line_num) }
| KW_ELSE s = function_suite                { ( Ecst( Cbool true, !Lexer.line_num), s, !Lexer.line_num) }
;

stmt:
| s = simple_stmt                           { s } 
| KW_IF e = expr s1 = function_suite l = list(elif)  { Sif(e, s1, l, !Lexer.line_num)}
| KW_WHILE e = expr s = function_suite               { Swhile(e, s, !Lexer.line_num) }
;

simple_stmt:
| KW_RETURN e = expr ";"                              { Sreturn(e, !Lexer.line_num) }
| KW_BREAK ";"                                        { Sbreak !Lexer.line_num }
| KW_CONTINUE ";"                                     { Scontinue !Lexer.line_num }
| KW_LET id = ident ":" t = crust_types "=" e = expr ";" { Sdeclare (id, t, e, !Lexer.line_num) }
| id = ident "=" e = expr ";"                         { Sassign (id, e, !Lexer.line_num) }
| KW_PRINT "(" e = expr ")" ";"                       { Sprint(e, !Lexer.line_num) }
| KW_PRINTLN "(" e = expr ")" ";"                     { Sprintn(e, !Lexer.line_num) }
| ";"                                                 { Snothing(!Lexer.line_num) }
;


expr:
| c = CST                           { Ecst (c, !Lexer.line_num) }
| KW_TRUE                           { Ecst ((Cbool true), !Lexer.line_num) }
| KW_FALSE                          { Ecst ((Cbool false), !Lexer.line_num) }
| u  = unop e1 = expr               { Eunop (u, e1, !Lexer.line_num) }
| e1 = expr o = binop e2 = expr     { Ebinop (o, e1, e2, !Lexer.line_num) }
| id = ident                        { Eident (id, !Lexer.line_num) }
| id = ident "(" l = separated_list("," , expr) ")" { Ecall(id, l, !Lexer.line_num)}
| "(" e = expr ")"                  { e }
;

%inline crust_types:
| I32    { Ti32  }
| BOOL   { Tbool }
;

%inline unop:
| MINUS  { Uneg }
| NOT    { Unot }
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
;

%inline ident:
| id = IDENT { id }
;