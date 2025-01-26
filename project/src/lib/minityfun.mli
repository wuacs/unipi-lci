open Minityfun_ast

module TyFunAst = Minityfun_ast

(** The exception which is thrown by function {!type_check} *)
exception TypeError of string

val type_check : ast -> tau option
(** 
This function checks if the given AST is type safe:
- returns {i Some(t)} if t is the type of the AST
- returns {i None} if the the AST contains a type error 
*)

val interpret_from_file: string -> value * tau
(**
*)