type bst = Node of {value: int; left: bst; right: bst}
          | Leaf of int 

let sum_leaves bst = 
  let rec helper sum node =
    match node with
    | Node(k) ->  let l = helper sum k.left in helper l k.right
    | Leaf(i) ->  sum+i
  in helper 0 bst;;

(* Should print 5+19+33+121=178*)
Printf.fprintf stdout "the sum is %d\n" (sum_leaves (Node(
                                                    {value = 12; 
                                                      left = Node({value = 2; left = Leaf(5); right = Leaf(19)});
                                                      right = Node({value = 5; left = Leaf(33); right = Leaf(121)})})))


