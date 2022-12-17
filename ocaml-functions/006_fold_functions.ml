
(* //////////////////////////////
   fold functions
   ////////////////////////////// *)


(* "Function: foldl" : '('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *) 
(* foldl is a function that applies a given function [f] to each element of a list [xl] and an accumulator [a],
 * starting from the left side of the list and moving towards the right. The function returns the final result
 * of applying [f] to the elements of the list and the accumulator.
 *
 * The function takes the following arguments:
 *   - f: the function to apply to each element of the list and the accumulator.
 *   - xl: the list of elements to apply the function [f] to.
 *   - a: the accumulator value.
*)
let rec foldl f xl a = match xl with
  | [] -> a
  | h::tl -> foldl f tl (f h a)
  
(* "Function: foldr" : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *) 
(* foldr applies a function to each element of a list, starting from the rightmost
   element, and combines the results with an accumulator value to produce a final
   result. *)
let rec foldr f xl a = match xl with
  | [] -> a
  | h::tl -> f h(foldr f tl a)