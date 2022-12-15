
(* //////////////////////////////
   helper rec funcions
   ////////////////////////////// *)

(* "Function: until" : ('a -> bool) -> ('a -> 'a) -> ('b -> 'b) -> 'a -> 'b -> 'b *) 
(* Applies the function f to a until the function p applied to s is true, then returns a *)
let rec until p n f s a =
  if p s then a
  else until p n f (n s) (f a) 
                               
(* "Function: sum" : (int -> int) -> int -> int *)
(* Returns the sum of the values returned by the function f when called on each integer from 1 to n *)
let rec sum (f:int -> int) (n:int) : int =
  if n < 1 then 0 else sum f (n-1) + f n 
                         
(* "Function: first" : (int -> bool) -> int -> int *)
(* Returns the first integer k for which the function f applied to k is true *)
let rec first f k =
  if f k then k 
  else first f ( k + 1 )
