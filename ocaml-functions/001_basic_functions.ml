(* //////////////////////////////
   basic functions
   ////////////////////////////// *)

(* "Function: fst" : 'a * 'b -> 'a *)
(* Returns the first element of a tuple (a, b). *)
let fst (a,b) = a

(* "Function: snd" : 'a * 'b -> 'b *)
(* Returns the second element of a tuple (a, b). *)
let snd (a,b) = b

(* "Function: max" : 'a -> 'a -> 'a *)
(* Returns the maximum of two values with the same type *)
let max x y = if x >= y then x else y
    
(* "Function: mid" : 'a -> 'a -> 'a -> 'a *)
(* Returns the middle value of three values of the same type *)
let mid x y z =
  if x <= y
  then if y <= z then y else z
  else if x <= z then x else z

(* "Function: sqrt" : float -> float *)
(* Calculates the square root of a positive float value x. *)
let sqrt (x:float) : float =
  let rec helper (k:float) : float =
    if k *. k > x then k -. 1.0 else helper (k +. 1.0)
  in helper 1.0

(* "Function: sqrtf" : float -> float *)
(* Calculates the square root of a float value using a brute force method.
   This function is not very efficient, as it keeps incrementing a value k until k * k is
   greater than the input value x. There are faster algorithms for finding the square root of
   a number, such as the Newton-Raphson method or the bisection method.
*)
let sqrtf (x:float) : float =
  (* Recursive helper function that keeps incrementing k until k * k is greater than x.
     Returns the final value of k.
  *)
  let rec helper (k:float) : float =
    if k *. k > x then k -. 1.0 else helper (k +. 1.0)
  in helper 1.0

(* "Function: sqrti" : int -> int *)
(* Calculates the square root of an integer value using a brute force method.
   This function is not very efficient, as it keeps incrementing a value k until k * k is
   greater than the input value x. There are faster algorithms for finding the square root of
   a number, such as the Newton-Raphson method or the bisection method.
*)
let sqrti (x:int) : int =
  (* Recursive helper function that keeps incrementing k until k * k is greater than x.
     Returns the final value of k.
  *)
  let rec helper (k:int) : int =
    if k * k > x then k - 1 else helper (k + 1)
  in helper 1