module ImpAst = Miniimp_ast
exception IllFormedAST of string

(** This function takes a {b Lexing.lexbuf} and tries to parse tokens provided, 
returns {b None} if the program contain a syntax error.
@see <https://github.com/wuacs/unipi-lci/tree/main/project> for more information
on the syntax of MiniImp *)
val parse_with_errors : Lexing.lexbuf -> ImpAst.program option

(** Evaluates a {!ImpAst.program}, returning an optional containing the result.
If the execution tries to access a variable which was not previously defined, a
{! IllFormedAST} is raised, indicating the access to an area of memory not defined, and 
{b None } is returned. *)
val eval : ImpAst.program -> int -> int option
