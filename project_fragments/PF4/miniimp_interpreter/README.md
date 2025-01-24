# What?

In this folder you will find in:

- `./programs/` ( mini )programs written in the language [miniImp](https://lceragioli.github.io/pages/Slides/semantics.pdf#Outline0.1).

- `./lib/miniimp_lexer.mll` the input for [ocamllex](https://ocaml.org/manual/5.2/lexyacc.html)

- `./lib/miniim_parser.mly` the input for [menhir](https://gallium.inria.fr/~fpottier/menhir/)

- `./lib/minimp_ast` the module which contains the AST of the language

- `./lib/miniimp_interpreter` the module which allows you to use the interpreter, the parsing and the evaluation is broken in two parts(`parse with errors` which parses the program and `eval` which evaluates it to an integer)

# How to use 

The executable in `./bin/main.ml` is already ready to handle any correct program written in `miniimp`. 

in Ubuntu/Unix distribution run from `PF4/miniimp_interpreter/`

```
dune exec miniimpinterpreter < ./programs/[name of the program]
```

