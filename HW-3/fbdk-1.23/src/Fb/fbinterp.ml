open Fbast;;

exception Wrongtype;;

(*
 * Replace this with your interpreter code.
 *)
let rec eval e =
    match e with
    | Not(e1) -> (match (eval e1) with
                  | Bool true -> Bool false
                  | Bool false -> Bool true
                  | _ -> raise Wrongtype 
                  )
    | And(e1, e2) -> (match (eval e1, eval e2) with
                     | (Bool x, Bool y) -> Bool (x && y)
                     | _ -> raise Wrongtype)
        
    | Or(e1, e2) -> (match (eval e1, eval e2) with
                    | (Bool x, Bool y) -> Bool (x || y)
                    | _ -> raise Wrongtype)
        
    | If(e1, e2, e3) -> (match eval e1 with
                        | Bool(true) -> eval e2
                        | Bool(false) -> eval e3
                        | _ -> raise Wrongtype)
    |Equal(e1, e2) -> (match (eval(e1), eval(e2)) with
                      | (Int x, Int y) -> if x = y then Bool true else Bool false
                      | _ -> raise Wrongtype)
    |Plus(e1, e2) -> (match (eval(e1), eval(e2)) with
                      | (Int x, Int y) -> Int(x + y)
                      | _ -> raise Wrongtype)
    |Minus(e1, e2) -> (match (eval(e1), eval(e2)) with
                      | (Int x, Int y) -> Int(x - y)
                      | _ -> raise Wrongtype)                      
    | _ -> e
;;