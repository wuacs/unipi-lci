open Minityfun_interpreter

let () = 
  match interpret (Lexing.from_channel stdin) with
  | Integer x -> print_int x
  | Boolean y -> Format.print_bool y
  | Closure(x, _, _) -> print_string ("closure " ^ x)
  | RecClosure(_, _, _, _) -> print_string "recursive closure" 