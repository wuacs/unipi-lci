module MiniFun = 
  struct
    module StringCompare = struct
        type t = string
        let compare = String.compare
    end

    module EnvMap = Map.Make(StringCompare)

    type var = string
      
    type operator = 
      | Plus
      | Minus
      | And
      | Mul
      | Less
        
    type ast =  
      | Fun of var * ast 
      | App of ast * ast 
      | Op of ast * operator * ast
      | If of ast * ast * ast
      | Let of var * ast * ast
      | LetFun of var * var * ast * ast
      | Value of value
      and value = Integer of int
        | Boolean of bool
        | RecClosure of var * var * ast * env 
        | Closure of var * ast * env
      and env = value EnvMap.t


    let eval ast env =
      let updateEnv var value env = EnvMap.add var value env
    in
      let rec rec_eval ast env = 
        match ast with
        | Fun(var, ast) -> Closure(var, ast, env)
        | App(f, par) -> (match f with 
                        | Value(Closure(v, a, e)) -> rec_eval a (updateEnv v (rec_eval par env) env)
                        | _ -> failwith "Application failed")
        | Op(a1, op, a2) ->  
                        let a1 = rec_eval a1 env in
                        let a2 = rec_eval a2 env in       
                        (match op with 
                        | Plus -> (match (a1, a2) with 
                                  | (Integer i , Integer j) -> Integer(i + j)
                                  | _ -> failwith "operands do not match Plus operator")
                        | Minus -> (match (a1, a2) with 
                                  | (Integer i , Integer j) -> Integer(i - j)
                                  | _ -> failwith "operands do not match Minus operator")
                        | Mul -> (match (a1, a2) with 
                                  | (Integer i , Integer j) -> Integer(i * j)
                                  | _ -> failwith "operands do not match Mul operator")
                        | Less -> (match (a1, a2) with 
                                  | (Integer i , Integer j) -> Boolean(i < j)
                                  | _ -> failwith "operands do not match Less operator")
                        | And -> (match (a1, a2) with
                                | (Boolean i, Boolean j) -> Boolean(i && j)
                                | _ -> failwith "operands do not match And operator")  
                        | _ -> failwith "operand not yet supported")
        | If(a1, a2, a3) -> let cond = rec_eval a1 env in
                            (match cond with
                            | Boolean true -> rec_eval a2 env
                            | Boolean false -> rec_eval a3 env 
                            | _ -> failwith "Condition of If is not a Boolean value")
        | Let(var, a1, a2) -> rec_eval a2 (updateEnv var (rec_eval a1 env) env)
        | LetFun(fname, pname, a1, a2) -> rec_eval a2 (updateEnv fname (RecClosure(fname, pname, a1, env)) env) 
        | Value(x) -> x
        | _ -> failwith "Construct not handled"
    in 
      rec_eval ast env
  end;;   
