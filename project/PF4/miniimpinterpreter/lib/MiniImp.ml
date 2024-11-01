type a_exp = 
  Aval of int 
  | Plus of a_exp * a_exp 
  | Minus of a_exp * a_exp
  | Times of a_exp * a_exp
  | Substitue of string

type b_exp = Bval of bool
  | And of b_exp * b_exp
  | Or of b_exp * b_exp
  | Not of b_exp
  | Minor of a_exp * a_exp

type command = Skip 
| Assign of (string * a_exp)
| Sequence of (command * command)
| Cond of (b_exp * command * command)
| While of (b_exp * command)

type program = {input: string; output: string; command: command} 

let eval p = 
  let memory = Hashtbl.create 10000 
in
  let rec eval_arithmetic expr =
    match expr with 
    | Plus(x, y) -> eval_arithmetic(x) + eval_arithmetic(y)
    | Minus(x, y) -> eval_arithmetic(x) - eval_arithmetic(y)
    | Times(x, y) -> eval_arithmetic(x) * eval_arithmetic(y)
    | Aval(x) -> x
    | Substitue(x) -> match (Hashtbl.find_opt memory x) with
                      | None -> failwith "No good"
                      | Some(y) -> y
in
  let rec eval_bool = function
  | And(x, y) -> eval_bool(x) && eval_bool(y)
  | Or(x, y) -> eval_bool(x) || eval_bool(y)
  | Not(x) -> not (eval_bool(x))
  | Minor(x, y) -> eval_arithmetic(x) < eval_arithmetic(y)
  | Bval(x) -> x 
in
  let assign loc x = Hashtbl.replace memory loc (eval_arithmetic x)
in
  let rec eval_command = function
  | Assign(loc, expr) -> assign loc expr
  | Sequence(c0, c1) -> eval_command c0; eval_command c1
  | Cond(b, t, e) -> if (eval_bool b) then eval_command t else eval_command e  
  | While(b, c) -> if (eval_bool b) then begin eval_command c; eval_command (While(b, c)); end
  | Skip -> ()
in
  begin 
    assign p.input (Aval(2)); assign p.output (Aval(0)); eval_command p.command;
    Hashtbl.find_opt memory p.output
  end;;

let p = {input = "in"; output = "out"; command = Sequence(
                                                  Assign("x", Substitue("in")),
                                                  Sequence(
                                                          Assign("out", Aval(0)),
                                                          While(Not(Minor(Substitue("x"), Aval(1))), 
                                                          Sequence(Assign("out", Plus(Substitue("x"), Substitue("out"))),
                                                                   Assign("x", Minus(Substitue("x"), Aval(1)))))
                                                  ))}
in
match eval p with
| None -> failwith "some error..."
| Some(x) -> Printf.fprintf stdout "Program's output is %d\n" x;
