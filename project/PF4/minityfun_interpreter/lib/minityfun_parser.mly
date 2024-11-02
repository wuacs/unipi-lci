%{
    open Minityfun_ast
%}

%token <string> VAR
%token <int> INT
%token <bool> BOOL
%token EOF WHITE
%token IF THEN ELSE
%token LET FUN_ARROW IN LETFUN FUN EQUAL
%token INT_TYPE BOOL_TYPE TYPE_ARROW TYPE_SEP
%token AND NOT LESS MUL MINUS PLUS
%token LEFT_PAR RIGHT_PAR

%start <ast> prog

%%

prog:
    | prg = term ; EOF {prg}
term:
    | LET ; t = VAR ; EQUAL b = term ; IN b1 = term {Let(t, b, b1)} 
    | LETFUN ; fname = VAR ; pname = VAR ; TYPE_SEP ; t = typing; body = term IN context = term {LetFun(fname, pname, t, body, context)}
    | LEFT_PAR ; closure = term WHITE par = term RIGHT_PAR {App(closure, par)}
    | t1 = term AND t2 = term {Op(t1, And, t2)}
    | t1 = term LESS t2 = term {Op(t1, Less, t2)}
    | t1 = term MUL t2 = term {Op(t1, Mul, t2)}
    | t1 = term MINUS t2 = term {Op(t1, Minus, t2)}
    | t1 = term PLUS t2 = term {Op(t1, Plus, t2)}
    | IF cond = term THEN then_body = term ELSE else_body = term {If(cond, then_body, else_body)}
    | FUN ; x = VAR ; TYPE_SEP ; t = typing FUN_ARROW body = term {Fun(x, t, body)}
    | i = INT {Val(Integer i)}
    | b = BOOL {Val(Boolean b)}
typing:
    | INT_TYPE {Integer_t}
    | BOOL_TYPE {Boolean_t}
    | t = typing TYPE_ARROW t1 = typing {Closure_t(t, t1)}
