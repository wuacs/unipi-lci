type 'a tree = 
  | Leaf of 'a
  | Node of ('a tree list) * 'a 