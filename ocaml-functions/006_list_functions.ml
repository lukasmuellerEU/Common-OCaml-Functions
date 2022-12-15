
(* //////////////////////////////
   lists functions
   ////////////////////////////// *) 

(* "Function: length" : 'a list -> int *) 
(* Returns the length of the given list xl *) 
let rec length xl = match xl with
  | [] -> 0 
  | h::tl -> 1 + length tl

(* "Function: rev" : 'a list -> 'a list *) 
(* Reverses the given list xl and returns the result *)
let rec rev xl = match xl with
  | [] -> []
  | h::tl -> rev tl @ [h]

(* "Function: concat" : 'a list -> 'a list -> 'a list *) 
(* Concatenates the given lists xl and yl and returns the result *)
let rec concat xl yl = match xl with
  | [] -> yl
  | h::tl -> h :: concat tl yl

               
(* "Function: flatten" : 'a list list -> 'a list *) 
(* Returns a new list that is the result of flattening the given list of lists xl *)
let rec flatten xl = match xl with
  | [] -> []
  | h::tl -> h @ flatten tl

(* "Function: map" : ('a -> 'b) -> 'a list -> 'b list *) 
(* Applies a given function to each element in a list and returns a new list of the results. *)
let rec map f xl = match xl with
  | [] -> []
  | h::tl -> f h :: map f tl

(* "Function: filter" : ('a -> bool) -> 'a list -> 'a list *) 
(* Returns a new list containing only elements of the original list that satisfy a given predicate. *)
let rec filter p xl = match xl with
  | [] -> []
  | h::tl -> if p h then h :: filter p tl else filter p tl

(* "Function: exists" : ('a -> bool) -> 'a list -> bool *) 
(* Returns true if at least one element in the list satisfies a given predicate, and false otherwise. *)
let rec exists p xl = match xl with
  | [] -> false
  | h::tl -> p h || exists p tl

(* "Function: forall" : ('a -> bool) -> 'a list -> bool *) 
(* Returns true if all elements in the list satisfy a given predicate, and false otherwise. *)
let rec forall p xl = match xl with
  | [] -> true
  | h::tl -> p h && forall p tl 
