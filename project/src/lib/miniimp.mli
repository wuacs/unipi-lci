open Miniimp_ast

exception IllFormedAST of string

module ImpAst = Miniimp_ast

val parse_with_errors : Lexing.lexbuf -> program option
(** This function takes a {b Lexing.lexbuf} and tries to parse tokens provided,
    returns {b None} if the program contain a syntax error.
    @see <https://github.com/wuacs/unipi-lci/tree/main/project>
      for more information on the syntax of MiniImp *)

val eval : program -> int -> int option
(** Evaluates a {!ImpAst.program}, returning an optional containing the result.
    If the execution tries to access a variable which was not previously
    defined, a {! IllFormedAST} is raised, indicating the access to an area of
    memory not defined, and {b None} is returned. *)
