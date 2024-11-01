#1 

SHIFT-REDUCE

WHILE bool_expr DO command

State1 ->  command . SEQ command

State2 -> WHILE bool_expr DO command .

State1 represents placing two commands in the while's body

State2 represents placing a ; After a while

Correction -> Placing parentheses to delimit the body of a WHILE.

#2

SHIFT-REDUCE

Same thing as #1 just for IFs:

State 1: IF bool_expr THEN command ELSE command
                                        command . SEQ command

State 2: IF bool_expr THEN command ELSE
                                        .
State1 represents placing extending the ELSE body with a sequence of commands

State 2 represents placing a command after an IF.

Correction -> Placing parentheses to delimit ELSE body.

#3

SHIFT-REDUCE conflict

```
** In state 33, looking ahead at SEQ, shifting is permitted
** because of the following sub-derivation:

command SEQ command 
            command . SEQ command 

** In state 33, looking ahead at SEQ, reducing production
** command -> command SEQ command
** is permitted because of the following sub-derivation:

command SEQ command // lookahead token appears
command SEQ command . 
```

Correction: we need to give precedence in commands language, clearly we will give left-precedence using:

%left SEQ

#4 

The following conflict has the same source of problems as other conflicts, that being `the problem of arithmetic/boolean precedence`. Thus, we will provide only one example: 

```
** Tokens involved: PLUS MUL MINUS
** The following explanations concentrate on token PLUS.
** This state is reached from prg after reading:

MAIN WHILE arit_expr MINUS arit_expr

...

** In state 18, looking ahead at PLUS, shifting is permitted
** because of the following sub-derivation:

arit_expr MINUS arit_expr 
                arit_expr . PLUS arit_expr 

** In state 18, looking ahead at PLUS, reducing production
** arit_expr -> arit_expr MINUS arit_expr
** is permitted because of the following sub-derivation:

arit_expr PLUS arit_expr // lookahead token appears
arit_expr MINUS arit_expr .
```
Menhir tells us that while, having as a symbol of Look-ahead `PLUS`, we could be shifting
or immediately reduce the `MINUS` operation.
Thus we will give left associativity to arithmetic(and boolean) tokens.