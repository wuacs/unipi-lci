open Minityfun_ast
module TyFunAst = Minityfun_ast

exception TypeError of string
(** The exception which is thrown by function {!type_check} *)

val type_check : ast -> tau option
(** This function checks if the given AST is type safe:
    - returns {i Some(t)} if t is the type of the AST
    - returns {i None} if the the AST contains a type error *)

val parse_with_errors : Lexing.lexbuf -> ast option
(** This function takes a {b Lexing.lexbuf} and tries to parse tokens provided,
    returns {b None} if the program contain a syntax error, otherwise returning
    the AST.
    @see <https://github.com/wuacs/unipi-lci/tree/main/project>
      for more information on the syntax of MiniImp *)

val interpret_from_file : string -> value * tau
(** Takes a string representing a filepath of a MiniTyFun program and returns a
    pair of {!Minityfun_ast.value} and {!Minityfun.tau} representing the normal
    form of the program and its type. *)

val eval : ast -> value
(** This function simply evaluates the MiniTyFun represented by this
    {!Minityfun_ast.ast} *)
