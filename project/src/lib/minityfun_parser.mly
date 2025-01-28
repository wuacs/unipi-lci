%{
    open Minityfun_ast
%}

%token <string> VAR
%token <int> INT
%token <bool> BOOL
%token EOF
%token IF THEN ELSE
%token LET FUN_ARROW IN LETFUN FUN EQUAL
%token INT_TYPE BOOL_TYPE TYPE_ARROW TYPE_SEP
%token AND LESS MUL MINUS PLUS
%token LEFT_PAR RIGHT_PAR


%start <ast> prog

%%

prog:
    | prg = term ; EOF {prg}
term:
    | LET ; t = VAR ; EQUAL b = term ; IN b1 = term {Let(t, b, b1)} 
    | LETFUN ; fname = VAR ; pname = VAR ; TYPE_SEP ; t = function_type; EQUAL ; body = term IN context = term {LetFun(fname, pname, t, body, context)}
    | IF cond = term THEN then_body = term ELSE else_body = term {If(cond, then_body, else_body)}
    | FUN ; x = VAR ; TYPE_SEP ; t = generic_type FUN_ARROW body = term {Fun(x, t, body)} 
    | o = operation {o}
operation:
    | t = additive_expr {t}
    | t = operation t1 = literal {App(t, t1)}
    | t = operation LEFT_PAR t1 = term RIGHT_PAR {App(t, t1)}
additive_expr:
    | t1 = mulitplicative_expr; o = additive_op; t2 = additive_expr {Op(t1, o, t2)}
    | me = mulitplicative_expr {me}
%inline additive_op:
    | PLUS {Plus}
    | MINUS {Minus}
    | LESS {Less}
    | AND {And}
mulitplicative_expr:
    | me = mulitplicative_expr; MUL; t2 = basic_term {Op(me, Mul, t2)}
    | b = basic_term {b}
basic_term:
    | l = literal {l}
    | i = negative_integer {i}
    | LEFT_PAR t = term RIGHT_PAR {t}
%inline literal:
    | b = BOOL {Val(Boolean b)}
    | v = VAR {Var(v)}
    | i = INT {Val(Integer i)}
%inline negative_integer:
    | MINUS; i = INT {Val(Integer (-i))}
generic_type:
    | ft = function_type {ft}
    | bt = basic_typing {bt}
function_type:
    | domain = basic_typing TYPE_ARROW range = basic_typing {Closure_t(domain, range)}
basic_typing:
    | INT_TYPE {Integer_t}
    | BOOL_TYPE {Boolean_t}
    | LEFT_PAR t = function_type RIGHT_PAR {t}