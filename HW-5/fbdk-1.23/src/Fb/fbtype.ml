open Fbast

exception UnknownExpression
exception InferenceException of string

let typecheck_default_enabled = true


let var_index = ref 65 (* this is the ASCII value of A *)


let left tup =
  match tup with
  | (a, b) -> a

let right tup =
  match tup with
  | (a, b) -> b


let rec union l1 l2 =
  match l1 with
  | hd::tl -> union tl (insert hd l2)
  | [] -> l2
and insert hd tl =
  if List.mem hd tl then
    tl
  else
    hd::tl


let rec type_factory predicates expr =
  match expr with
  | Var x -> ((lookup predicates x), [])
  | Int _ -> (TInt, [])
  | Bool _ -> (TBool, [])
  | Plus (expr1, expr2) ->
      let result_a = type_factory predicates expr1

      and result_b = type_factory predicates expr2
      in
        if (left result_a) = (left result_b)
        then (TInt, (union (union (right result_a) (right result_b)) [ ((left result_b), TInt) ]))
        else
          (TInt,
           (union (union (right result_a) (right result_b))
              [ ((left result_a), TInt); ((left result_b), TInt) ]))
  | Minus (expr1, expr2) ->
      let result_a = type_factory predicates expr1

      and result_b = type_factory predicates expr2
      in
        if (left result_a) = (left result_b)
        then (TInt, (union (union (right result_a) (right result_b)) [ ((left result_b), TInt) ]))
        else
          (TInt,
           (union (union (right result_a) (right result_b))
              [ ((left result_a), TInt); ((left result_b), TInt) ]))
  | Not expr1 ->
      let r = type_factory predicates expr1 in
        (TBool, union (right r) [(left r, TBool)])
  | Equal (expr1, expr2) ->
      let result_a = type_factory predicates expr1

      and result_b = type_factory predicates expr2
      in
        if (left result_a) = (left result_b)
        then (TBool, (union (union (right result_a) (right result_b)) [ ((left result_b), TInt) ]))
        else
          (TBool,
           (union (union (right result_a) (right result_b))
              [ ((left result_a), TInt); ((left result_b), TInt) ]))
  | And (expr1, expr2) ->
      let result_a = type_factory predicates expr1

      and result_b = type_factory predicates expr2
      in
        if (left result_a) = (left result_b)
        then (TBool, (union (union (right result_a) (right result_b)) [ ((left result_b), TBool) ]))
        else
          (TBool,
           (union (union (right result_a) (right result_b))
              [ ((left result_a), TBool); ((left result_b), TBool) ]))
  | Or (expr1, expr2) ->
      let result_a =
        type_factory predicates expr1
      and result_b =
        type_factory predicates expr2
      in
        if (left result_a) = (left result_b)
        then (TBool, (union (union (right result_a) (right result_b)) [ ((left result_b), TBool) ]))
        else
          (TBool,
           (union (union (right result_a) (right result_b))
              [ ((left result_a), TBool); ((left result_b), TBool) ]))
  | If (expr1, expr2, expr3) ->
      let result_a = type_factory predicates expr1 and result_b = type_factory predicates expr2

      and r3 = type_factory predicates expr3 in
      let sub = union (union (right result_a) (right result_b)) (right r3) in
      let m_new_type = make_new_type ()
      in
        (m_new_type,
         (union sub [ ((left result_a), TBool); ((left result_b), m_new_type); ((left r3), m_new_type) ]))
  | Function (x, expr_1) ->
      let m_new_type = make_new_type () in
      let r = type_factory (union predicates [ (x, m_new_type) ]) expr_1
      in ((TArrow (m_new_type, (left r))), (right r))
  | Appl (expr1, expr2) ->
      let result_a = type_factory predicates expr1 and result_b = type_factory predicates expr2 in
      let sub = union (right result_a) (right result_b) in
      let m_new_type = make_new_type ()
      in (m_new_type, (union sub [ ((left result_a), (TArrow ((left result_b), m_new_type))) ]))
  | _ -> raise UnknownExpression
and make_new_type () =
  var_index := !var_index + 1;
  TVar (Char.escaped (Char.chr (!var_index - 1)))
and lookup predicates x =
  match predicates with
  | hd::tl -> if (left hd) = x then right hd else lookup tl x
  | [] -> (match x with
        Ident v -> raise (InferenceException (v ^ "'s type could not be assigned")))


let rec replace_type_with_new_type typ es =
  match es with
  | (type_1, type_2)::tail -> if typ = type_1 then
                                replace_type_with_new_type (replace typ type_2) tail
                              else if typ = type_2 then
                                replace_type_with_new_type (replace typ type_1) tail
                              else
                                replace_type_with_new_type typ tail
  | [] -> typ
and replace typ1 typ2 =
  match (typ1, typ2) with
  | (_, TInt) -> typ2
  | (_, TBool) -> typ2
  | (TVar _, TArrow (_, _)) -> typ2
  | (TVar t1, TVar t2) -> if (get_ascii_precedence t1 t2) then
                          typ2
                        else
                          typ1
  | _ -> typ1
and get_ascii_precedence typvar1 typvar2 =
  if (Char.code typvar1.[0] < Char.code typvar2.[0]) then
    false
  else
    true


let rec substitute typ rest =
  match typ with
  | TBool -> typ
  | TInt -> typ
  | TArrow (t1, t2) -> TArrow (substitute t1 rest, substitute t2 rest)
  | _ -> replace_type_with_new_type typ rest


let rec is_closure_satisfied expr =
  match expr with
  | [] -> true
  | (TInt, TBool)::tl -> false
  | (TBool, TInt)::tl -> false
  | (TInt, TArrow (_, _))::tl -> false
  | (TBool, TArrow (_, _))::tl -> false
  | (TArrow (_, _), TInt)::tl -> false
  | (TArrow (_, _), TBool)::tl -> false
  | _::tl -> is_closure_satisfied tl


let rec typecheck e =
  var_index := 65;
  let type_tuple = type_factory [] e in
    let typ = left type_tuple and eqns = right type_tuple in
        let closure = find_closure eqns in
          if is_closure_satisfied closure then
            if typ = TInt || typ = TBool then
              typ
            else
              get_type typ closure
          else raise (InferenceException "Type Mismatch")
and get_type t tl =
  let t1 = substitute t tl in
    let t2 = substitute t1 tl in
      if t1 = t2 then
        t2
      else
        get_type t2 tl
and find_closure expr =
  let clos_1 = make_equations expr in
    let clos_2 = make_equations clos_1 in
      List.filter (function x ->
        match x with
        | (left, right) -> left != right) ( if (List.length clos_1) = (List.length clos_2) then
                                              clos_1
                                            else
                                              find_closure clos_2 )
and make_equations expr =
  match expr with
  | (type_1, type_2)::eqs ->
      let sub1 =
        (match (type_1, type_2) with
         | (TArrow (left_type_1, right_type_1), TArrow (left_type_2, right_type_2)) ->
             [ (left_type_1, left_type_2); (right_type_1, right_type_2) ]
         | _ -> []) in
      let sub2 =
        let subs = sub1 @ expr in
          let subs1 =
                List.map (function x -> (type_1, right x)) (List.filter (function x -> (left x) = type_2) subs)
          and subs2 =
                List.map (function x -> (right x, type_2)) (List.filter (function x -> (left x) = type_1) subs)
          and subs3 =
                List.map (function x -> (type_1, left x)) (List.filter (function x -> (right x) = type_2) subs)
          and subs4 =
                List.map (function x -> (left x, type_2)) (List.filter (function x -> (right x) = type_1) subs)
        in union (union (union subs1 subs2) subs3) subs4
      in union (union (union sub1 sub2) [ (type_1, type_2) ]) (make_equations eqs)
  | [] -> []