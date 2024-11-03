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
    | LETFUN ; fname = VAR ; pname = VAR ; TYPE_SEP ; t = typing; EQUAL ; body = term IN context = term {LetFun(fname, pname, t, body, context)}
    | IF cond = term THEN then_body = term ELSE else_body = term {If(cond, then_body, else_body)}
    | FUN ; x = VAR ; TYPE_SEP ; t = typing FUN_ARROW body = term {Fun(x, t, body)} 
    | o = operation {o}
operation:
    | t1 = operation AND t2 = basic_term {Op(t1, And, t2)}
    | t1 = operation LESS t2 = basic_term {Op(t1, Less, t2)}
    | t1 = operation MUL t2 = basic_term {Op(t1, Mul, t2)}
    | t1 = operation MINUS t2 = basic_term {Op(t1, Minus, t2)}
    | t1 = operation PLUS t2 = basic_term {Op(t1, Plus, t2)}
    | t = operation t1 = basic_term {App(t, t1)}
    | t = basic_term {t}
basic_term:
    | i = INT {Val(Integer i)}
    | b = BOOL {Val(Boolean b)}
    | v = VAR {Var(v)}
    | LEFT_PAR t = term RIGHT_PAR {t}
typing:
    | t = typing TYPE_ARROW t1 = basic_typing {Closure_t(t, t1)}
    | bt = basic_typing {bt}
basic_typing:
    | INT_TYPE {Integer_t}
    | BOOL_TYPE {Boolean_t}