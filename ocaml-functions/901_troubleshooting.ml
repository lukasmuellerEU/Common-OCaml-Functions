
(* //////////////////////////////
   functions for troubleshooting
   ////////////////////////////// *)

(* "Function: iterInside" : ('a -> 'a) -> int -> 'a -> 'a list *)
(* Applies the function f to the value x a total of n times, storing the intermediate results in a list. *)
let iterInside f n x =
  map (fun y -> iter f y x) (List.init (n+1) (fun i -> i))
