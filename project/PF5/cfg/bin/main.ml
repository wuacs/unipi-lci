open Cfg
open Miniimp_ast

let () = 
  let prg = 
    Sequence(
      Assign("x", Substitue "in"),
      Sequence(
        Assign("out", Aval 0),
        Cond(
          Not(Minor(Substitue "x", Aval 2)),
          Sequence(
            Assign("x", Minus(Substitue "x", Aval 1)),
            Sequence(
              Assign("out", Aval 1),
              Sequence(
                Assign("second", Aval 0),
                While(
                  Not(Minor(Substitue "x", Aval 2)),
                  Sequence(
                    Assign("temp", Substitue "out"),
                    Sequence(
                      Assign("out", Plus(Substitue "out", Substitue "second")),
                      Sequence(
                        Assign("x", Minus(Substitue "x", Aval 1)),
                        Assign("second", Substitue "temp")
                      )
                    )
                  )
                )
              )
            )
          ),
          Skip
        )
      )
    )

(*let prg = 
    Sequence
    (
      Assign("x", Aval 2),
      Cond(Minor(Substitue "y", Aval 0),
          Sequence
          (
            Assign("y", Plus(Substitue "x", Aval 3)),
            Assign("x", Substitue "y")
          ),
          Assign
          (
            "x", Minus(Aval 1, Substitue "y")
          )
          )
    )*)
  in 
    let prog = {input = "y"; output = "x"; command = prg}
  in 
    let dot_string = miniimp_cfg_to_dot (translate_miniimp prog) 
  in
    let oc = open_out "cfg.dot" 
  in
    Printf.fprintf oc "%s" dot_string;
    close_out oc
  