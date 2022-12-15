(* //////////////////////////////
   basic functions
   ////////////////////////////// *)

(* "Function: max" : int -> int -> int *)
(* Returns the maximum of two int values *)
let max (x:int) (y:int) : int = if x >= y then x else y
    
(* "Function: mid" : int -> int -> int -> int *)
(* Returns the middle value of three int values *)
let mid (x:int) (y:int) (z:int) : int =
  if x <= y
  then if y <= z then y else z
  else if x <= z then x else z
