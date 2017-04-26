open Boolast;;

exception Bug;;

let rec eval ex = 
  match ex with 
    True -> True
  | False -> False

  | Not(e) -> 
		(let v = eval e in 
	  match v with
      True -> False
    | False -> True
    | _ -> raise Bug)

  | And(e1,e2) -> 
		(let (v1,v2) = (eval e1, eval e2) in 
			match (v1,v2) with
      (True,True) -> True
    | (True,False) -> False
    | (False,True) -> False
    | (False,False) -> False
    | _ -> raise Bug)
 
  | Or(e1,e2) -> 
		(let (v1,v2) = (eval e1, eval e2) in 
			match (v1,v2) with
      (True,True) -> True
    | (True,False) -> True
    | (False,True) -> True
    | (False,False) -> False
    | _ -> raise Bug)
		
  | Implies(e1,e2) -> 
		(let (v1,v2) = (eval e1, eval e2) in 
			match (v1,v2) with
      (True,True) -> True
    | (True,False) -> False
    | (False,True) -> True
    | (False,False) -> True
    | _ -> raise Bug)
