
(* //////////////////////////////
   changed basic rec functions
   ////////////////////////////// *)

(* "Function: iterc" : ('a -> 'a) -> int -> 'a -> 'a*)    
(*Iter function with the help of the until function*)
let iterc f n s =
  until (fun s -> s < 1) (fun s -> s - 1) f n s

(* "Function: firstc" : ('a -> 'a) -> int -> 'a -> 'a*)    
(*First function with the help of the until function*)
let firstc p s =
  until p (fun s -> s + 1) (fun s -> s + 1) s s
