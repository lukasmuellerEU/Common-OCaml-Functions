
(* //////////////////////////////
   basic functions with the help of advanced higher order helper functions 
   ////////////////////////////// *)

(* "Function: power" : int -> int -> int *) 
(* Returns the result of raising x to the power of n *)
let power (x:int) (n:int) : int =
  iter (fun (a:int) : int -> a*x) n 1
  
(* "Function: sqrt" : int -> int *) 
(* Returns the largest integer less than or equal to the square root of x *)
let sqrti (x:int) : int =
  first (fun (k:int) : bool -> k * k > x) 1 - 1

(* "Function: gauss" : int -> int *) 
(* Returns the sum of the integers from 1 to n using the Gauss formula (n * (n + 1) / 2) *)
let gauss (n:int) : int =
  let (i,res) = iter (fun ((i:int),(a:int)) : (int*int) -> (i+1, a+i)) n (1,0) in
  res
