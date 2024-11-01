# What?

In this folder you will find in:

- `./programs/` programs written in the language [miniImp](https://lceragioli.github.io/pages/Slides/semantics.pdf#Outline0.1).

- `./lib/Miniimplexer.mll` the input for [ocamllex](https://ocaml.org/manual/5.2/lexyacc.html)

- `./lib/Miniimparser.mly` the input for [menhir](https://gallium.inria.fr/~fpottier/menhir/)

- `./lib/MiniImp` the module which contains the AST of the language

- `./lib/miniimpinterpreter` the module which allows you to use the interpreter, the parsing and the evaluation is broken in two parts(`parse with errors` which parses the program and `eval` which evaluates it to an integer)

# How to use 

The executable in `./bin/main.ml` is already ready to handle any correct program written in `miniImp`. 

in Ubuntu/Unix distribution run from `PF4/miniimpinterpreter/`

```
dune exec miniimpinterpreter < ./programs/[name of the program]
```

