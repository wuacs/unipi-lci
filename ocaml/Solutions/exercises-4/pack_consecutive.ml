let pack_cons input = 
    let rec helper el list =
      match list with
      | [] -> [el]::[]
      | x ->  if (List.hd (List.hd x)) = el then
                (el::(List.hd x))::(List.tl x)
              else 
                [el]::x
    in 
    List.fold_right (helper) input []