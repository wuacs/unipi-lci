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
    | t1 = operation; o = op; t2 = basic_term {Op(t1, o, t2)}
    | t = operation t1 = basic_term {App(t, t1)}
    | b = basic_term {b}
%inline op:
    | PLUS {Plus}
    | MINUS {Minus}
    | MUL {Mul}
    | LESS {Less}
    | AND {And}
basic_term:
    | b = BOOL {Val(Boolean b)}
    | v = VAR {Var(v)}  
    | i = INT {Val(Integer i)}
    | i = unary_op {i}
    | LEFT_PAR t = term RIGHT_PAR {t}
unary_op:
    | LEFT_PAR MINUS i = INT RIGHT_PAR {Val(Integer(-i))} 
generic_type:
    | ft = function_type {ft}
    | bt = basic_typing {bt}
function_type:
    | domain = basic_typing TYPE_ARROW range = basic_typing {Closure_t(domain, range)}
basic_typing:
    | INT_TYPE {Integer_t}
    | BOOL_TYPE {Boolean_t}
    | LEFT_PAR t = function_type RIGHT_PAR {t}