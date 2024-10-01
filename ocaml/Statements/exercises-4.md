Exercise 1. Write a pair of functions for evaluating arithmetical
and boolean expressions without the Of bool case

Exercise 2. Same but with the Of bool case

Exercise 3. Write a type for general polymorphic trees (i.e. with
any number of children)

Exercise 4. Write a function that packs consecutive duplicates of
the input list elements into sublists.
For example, the function over [0;0;2;3;3;3;0;2] must return
[[0;0];[2];[3;3;3];[0];[2]]

Exercise 5. Write a function that given two functions f and g
return a function over pairs defined as f on the first element and g
on the second element.

Exercise 6. Write a function that given a list of integers l1 returns
a list l2 of the same length such that each element of l2 in position i
is the sum of all the elements in l1 with position less or equal to i.
E.g. The function over [3,6,10,2] returns [3,9,19,21]

Exercise 7. Define a type for Finite Automata and a function for
checking if a given string is inside the generated language.