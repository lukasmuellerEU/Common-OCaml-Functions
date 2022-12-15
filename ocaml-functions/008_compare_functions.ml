
(* //////////////////////////////
   comparison and types
   ////////////////////////////// *)
   
(* This code defines a type 'comparison' that represents the possible results of
   comparing two values, and a function 'comp' that takes two values and returns
   their comparison. The 'comparison' type can take three values: 'LE' if the first
   value is less than the second, 'EQ' if they are equal, and 'GR' if the first
   value is greater than the second. The 'comp' function uses these values to
   compare its arguments and return the appropriate result. *)
(* "Type: comparison" : comparison = LE | EQ | GR *) 
type comparison = LE (* less *) (* x < y *)
                | EQ (* equal *) (* x = y *)
                | GR (* greater *) (* x > y *)

(* "Function: comp" : 'a -> 'a -> comparison *) 
let comp x y = if x = y then EQ else if x < y then LE else GR
