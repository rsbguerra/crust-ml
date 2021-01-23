(*
  Last Modification: 20-12-2020
  Description: Pico Rust Parser
*)

/* Grammar Entry Point */
%start prog

/* Tipo dos valores devolvidos pelo parser */
%type <Ast.program> prog

%%

prog:
| l = list(decl) EOF { l }
;

decl:
| KW_STRUCT id = ident "{" l = separated_list(",", struct_pair) "}" { Dstruct (id, l, !Lexer.line_num) }
| KW_FN id = ident "(" args = separated_list(",", function_pair) ")" r = option(function_return) s = function_suite
  { Dfun(id, args, r, s, !Lexer.line_num) } 
;

struct_pair:
| id = ident ":" t = prust_types {(id, t)}
;

function_pair:
| id = ident ":" t = prust_types {(false, id, t)}
| KW_MUT id = ident ":" t = prust_types {(true, id, t)}
;

function_return:
| ARROW r = prust_types { r }
;

bloc_body:
| id = stmt b = bloc_body { let l, e = b in (id::l, e) }
| e = option(expr)        { ([], e) }
;

function_suite:
|  "{" b = bloc_body "}" { let l, e = b in (l, e) }
;

if_suite:
| KW_IF e = expr s = function_suite                              { Sif(e, s,  ([], None), !Lexer.line_num) }
| KW_IF e = expr s1 = function_suite KW_ELSE s2 = function_suite { Sif(e, s1, s2, !Lexer.line_num) }
| KW_IF e = expr s1 = function_suite KW_ELSE s2 = if_suite       { Sif(e, s1, ([s2], None), !Lexer.line_num) }
;

stmt:
| ";"                                                    { Snothing(!Lexer.line_num) }
| e = expr ";"                                           { Sexpr(e, !Lexer.line_num) }
| KW_LET m = option(KW_MUT) id = ident "=" e = expr ";"
  { let v = match m with | None -> false | _ -> true in 
    Sdeclare (v, id, e, !Lexer.line_num) }
| KW_LET m = option(KW_MUT) id = ident "=" s = ident "{" l = separated_list("," , expr_pair) "}" ";"
  { let v = match m with | None -> false | _ -> true in 
    Sdeclare_struct(v, id, s, l, !Lexer.line_num) }
| KW_RETURN e = option(expr) ";"                         { Sreturn(e, !Lexer.line_num) }

| KW_WHILE e = expr s = function_suite                   { Swhile(e, s, !Lexer.line_num) }
| ifl = if_suite                                         { ifl }
;

expr_pair:
| id = ident ":" e = expr {(id, e)}
;

expr:
| c = CST                           { Eint (c, !Lexer.line_num) }
| KW_TRUE                           { Ebool (true, !Lexer.line_num) }
| KW_FALSE                          { Ebool (false, !Lexer.line_num) }
| id = ident                        { Eident (id, !Lexer.line_num) }
| u  = unop e1 = expr %prec UNARY_EXPR { Eunop (u, e1, !Lexer.line_num) }
| e1 = expr o = binop e2 = expr     { Ebinop (o, e1, e2, !Lexer.line_num) }
| id = ident "(" l = separated_list("," , expr) ")"      { Ecall(id, l, !Lexer.line_num) }
| e = expr "." id = ident                                { Estruct_access(e, id, !Lexer.line_num) }
| KW_VEC "[" l = separated_list("," , expr) "]"          { Evec_decl(l, !Lexer.line_num) }
| e1 = expr "[" e2 = expr "]"                            { Evec_access(e1, e2, !Lexer.line_num) }
| e = expr "." KW_LEN                                    { Elen(e, !Lexer.line_num) }
| "(" e = expr ")"                                       { e }
| KW_PRINT "(" s = STRING ")"                            { Eprint(s, !Lexer.line_num) }
| b = function_suite                                     { Eblock(b, !Lexer.line_num) }
;

prust_types:
| id = ident                    { Tid id }
| id = ident LT t = prust_types GT { Tid_typed (id, t) }
| BITAND t = prust_types        { Tref t }
| BITAND KW_MUT t = prust_types { Trefmut t }
;

%inline unop:
| MINUS  { Uneg }
| NOT    { Unot }
| BITAND { Uref }
| BITAND KW_MUT { Urefmut }
| TIMES  { Uderef }
;

%inline binop:
| PLUS   { Badd }
| MINUS  { Bsub }
| TIMES  { Bmul }
| DIV    { Bdiv }
| MOD    { Bmod }
| EQ     { Beq }
| NEQ    { Bneq }
| GT     { Bgt }
| LT     { Blt }
| GET    { Bge }
| LET    { Ble }
| AND    { Band }
| OR     { Bor  }
| ASSIGN { Bassign }
;

%inline ident:
| id = IDENT { id }
;
