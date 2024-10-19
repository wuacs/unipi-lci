`miniInf` is a (really) simple functional language this directory represents a raw implementation in OCAML.

Its specification can be found [here](https://lceragioli.github.io/pages/Slides/semantics.pdf).

Considerations about our implementation: 

- `miniInf` is built with OCAML, which is a strict evaluating language. This is why in the specification, and in our `ast` type, we have the `LetFun` construct. 

- `miniInf` is a functional language, that is, functions are treated as any primitive value. This introduces the question on which is the environment where `closures`(a.k.a first-class representations of functions) are to be called in.
    - `miniInf` adopts a `deep binding` strategy: e.g. when a closure is declared, the environment is the current one. When future applications of that closure are to be evaluated the environment will be the "at declaration one".
    - The point before explains why when evaluating a recursive closure `App(closure, ...)` we need to update `closure`'s envronment with a reference to a closure identical to itself. We note that there is, at least, another way: make references first class values, e.g. making the type `var` just a variant of `value` therefore we can store references to variables in the `environment` which we recall as $\verb|var -> value|$ therefore in the definition of a closure we can add to the environment the mapping $\verb|f -> Var(f)|$

- `type safety` is not made a priority since this implementation only intent is to follow the formal grammar. Type inconsistencies are handled via the `failwith` function.


