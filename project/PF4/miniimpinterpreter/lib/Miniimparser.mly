%{
    open MiniImp
%}

(* tokens *)
%token EOF
%token IF THEN ELSE WHILE DO SEQ SKIP ASSIGN 
%token <string * string> MAIN
%token <string> VAR 
%token <int> INT
%token <bool> BOOL
%token MINUS PLUS MUL
%token RIGHTPAR LEFTPAR
%token OR AND LESS NOT

%left SEQ MINUS PLUS MUL AND OR
%left NOT
(* start not terminal *)
%start <program> prg

%%

prg:
    |  t = MAIN ; c = command ; EOF {{input = fst t; output = snd t; command = c}} 
command:
    | SKIP {Skip}
    | t1 = VAR ASSIGN t2 = arit_expr {Assign(t1, t2)}
    | t1  = command SEQ t2 = command {Sequence(t1, t2)}
    | IF bool_expr = bool_expr THEN then_branch = command ELSE LEFTPAR else_branch = command RIGHTPAR {Cond(bool_expr, then_branch, else_branch)}
    | WHILE bool_expr = bool_expr DO LEFTPAR c = command RIGHTPAR {While(bool_expr, c)}
bool_expr:
    | t = BOOL {Bval(t)}
    | t = bool_expr AND t1 = bool_expr {And(t, t1)}
    | t = bool_expr OR t1 = bool_expr {Or(t, t1)}
    | t = arit_expr LESS t1 = arit_expr {Minor(t, t1)}
    | NOT t = bool_expr {Not(t)} 
arit_expr:
    | t = INT {Aval(t)}
    | t = arit_expr PLUS t1 = arit_expr {Plus(t, t1)}
    | t = arit_expr MINUS t1 = arit_expr {Minus(t, t1)}
    | t = arit_expr MUL t1 = arit_expr {Times(t, t1)}
    | t = VAR {Substitue(t)} 