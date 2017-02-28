open Fbast;;

exception TypeMismatch;;

(*
 * Replace this with your interpreter code.
 *)
let rec eval e =
    match e with
    | And (e1, e2) ->
        (match (eval e1, eval e2) with
             (Bool x, Bool y) -> Bool (x && y)
            | _ -> raise TypeMismatch
        )
    | Or (e1, e2) ->
        (match (eval e1, eval e2) with
             (Bool x, Bool y) -> Bool (x || y)
            | _ -> raise TypeMismatch
        )
    | _ -> e
;;        