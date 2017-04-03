(* Open the Module containing the expr type *)

open Fbextast;;

exception Wrongtype;;
exception NotClosed;;
exception Invalid;;

(*
 * Replace this with your interpreter code.
 *)
let rec search id_so_far e = 
    match e with
    | Bool _ -> (true)
    | Int _ -> (true)
    | Pair(e1, e2) -> (search id_so_far e1 && search id_so_far e2)
    | Fst(e1) -> (search id_so_far e1)
    | Snd(e1) -> (search id_so_far e1)
    | Variant(n, e1) -> (search id_so_far e1)
    | Match(e1, e2) -> (search id_so_far e1)
    | Not e1 -> (search id_so_far e1)
    | And (e1, e2) -> (search id_so_far e1 && search id_so_far e2)
    | Or (e1, e2) -> (search id_so_far e1 && search id_so_far e2)
    | If (e1, e2, e3) -> (search id_so_far e1 && search id_so_far e2 && search id_so_far e3)
    | Plus (e1, e2) -> (search id_so_far e1 && search id_so_far e2)
    | Minus (e1, e2) -> (search id_so_far e1 && search id_so_far e2)
    | Equal (e1, e2) -> (search id_so_far e1 && search id_so_far e2)
    | Appl (e1, e2) -> (search id_so_far e1 && search id_so_far e2)
    | Function (i, e1) -> (search ([i] @ id_so_far) e1)
    | Let (id, e1, e2) -> ((search id_so_far e1) && (search (id :: id_so_far) e2))
    | Var id -> ((List.mem id id_so_far))
    | LetRec (id_1, id_2, e1, e2) -> (search (id_1 :: id_2 :: id_so_far) e1 && search (id_1 :: id_so_far) e2)
;;

let closure_check e = 
  search [] e
;;

let rec subst id e fn = 
      match fn with
      | Var id_t -> if id_t = id then e else Var id_t
      | And (e1, e2) -> And (subst id e e1, subst id e e2)
      | Or (e1, e2) -> Or (subst id e e1, subst id e e2)
      | Plus (e1, e2) -> Plus (subst id e e1, subst id e e2)
      | Minus (e1, e2) -> Minus (subst id e e1, subst id e e2)
      | If (e1, e2, e3) -> If (subst id e e1, subst id e e2, subst id e e3)
      | Not e1 -> Not (subst id e e1)
      | Equal (e1, e2) -> Equal (subst id e e1, subst id e e2)
      | Int x -> Int x
      | Pair (e1, e2) -> Pair((subst id e e1), (subst id e e2))
      | Fst (e1) -> Fst(subst id e e1)
      | Snd (e1) -> Snd(subst id e e1)
      | Variant(n, e1) -> Variant(n, subst id e e1)
      | Match(e1, e2) -> Match(subst id e e1, e2)
      | Bool x -> Bool x
      | Let(id1, e1, e2) -> raise Wrongtype
      | Appl (e1, e2) -> Appl (subst id e e1, subst id e e2)
      | Function (id_t, f_bod) -> if id_t = id then fn else Function (id_t, subst id e f_bod)
      | LetRec (id_1, id_2, e1, e2) -> if id != id_1 && id != id_2 then 
                                        LetRec (id_1, id_2, subst id e e1, subst id e e2)
                                       else if id = id_2 then 
                                        LetRec (id_1, id_2, e1, subst id e e2)
                                       else 
                                        LetRec (id_1, id_2, e1, e2)
;;

let rec eval e =
    if closure_check e then
        match e with
        | Not(e1) -> (match (eval e1) with
                      | Bool true -> Bool false
                      | Bool false -> Bool true
                      | _ -> raise Wrongtype
                      )
        | And(e1, e2) -> (match (eval e1) with
                         | Bool(true) -> (match (eval e2) with
                                         | Bool(true) -> Bool(true)
                                         | Bool(false) -> Bool(false)
                                         | _ -> raise Wrongtype
                                         )
                         | Bool(false) -> Bool(false)
                         | _ -> raise Wrongtype
                         )

        | Or(e1, e2) ->  (match (eval e1) with
                         | Bool(true) -> Bool(true)
                         | Bool(false) -> (match (eval e2) with
                                         | Bool(true) -> Bool(true)
                                         | Bool(false) -> Bool(false)
                                         | _ -> raise Wrongtype
                                         )
                         | _ -> raise Wrongtype
                         )

        | If(e1, e2, e3) -> (match eval e1 with
                            | Bool(true) -> eval e2
                            | Bool(false) -> eval e3
                            | _ -> raise Wrongtype)
        | Equal(e1, e2) -> (match (eval(e1), eval(e2)) with
                          | (Int x, Int y) -> if x = y then Bool true else Bool false
                          | _ -> raise Wrongtype)
        | Plus(e1, e2) -> (match (eval(e1), eval(e2)) with
                          | (Int x, Int y) -> Int(x + y)
                          | _ -> raise Wrongtype)
        | Minus(e1, e2) -> (match (eval(e1), eval(e2)) with
                          | (Int x, Int y) -> Int(x - y)
                          | _ -> raise Wrongtype)
        | Pair(e1, e2) -> (Pair(eval e1, eval e2))
        | Fst(exPair) -> (match eval exPair with
                          | Pair(e1,e2) -> e1
                          | _ -> raise Wrongtype)
        | Snd(exPair) -> (match eval exPair with
                           | Pair(e1, e2) -> e2
                           | _ -> raise Wrongtype)
        | Variant (n, e) -> (Variant(n, eval e))
        | Match (expr, pos_matches) -> (variant_matcher expr pos_matches)
        | Appl (e1, e2) -> (match eval e1 with
                           | Function (id, fn) -> eval (subst id (eval e2) fn) 
                           | _ -> raise Wrongtype)
        | Var id -> raise NotClosed
        | LetRec (fn, me, e1, e2) -> let excess = 
                                      subst fn (Function (me, (LetRec (fn, me, e1, Appl (Var fn, Var me))))) e1 in
                                        eval (subst fn (Function (me, excess)) e2)
        | Let(i1, e1, e2) -> (eval(subst i1 (eval e1) e2)) 
        | _ -> e
    else raise NotClosed
and variant_matcher exp pos_matches =
    match eval exp with
    | Variant(nv, vv) -> (match pos_matches with
                         | (np, ip, vp)::xs -> if np = nv then
                                                eval vp
                                               else
                                                variant_matcher exp xs
                         | [] -> raise Invalid)
    | _ -> raise Wrongtype
;;
