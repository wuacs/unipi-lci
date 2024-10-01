let prefix_sums input = 
  let rec helper list el =
    match list with
    | [] -> [el]
    | x ->  (el+(List.hd x))::list
  in 
  List.rev (List.fold_left (helper) [] input)

