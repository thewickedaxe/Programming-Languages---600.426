type ident = Ide of string

type expr =
	  Var of ident
	| Function of ident * fbtype * expr
	| Appl of expr * expr
	| Plus of expr * expr 
        | Minus of expr * expr
	| Equal of expr * expr 
        | And of expr * expr
	| Or of expr * expr 
        | Not of expr
	| If of expr * expr * expr 
        | Int of int 
        | Bool of bool
  
and fbtype =
	  TInt | TBool  | TArrow of fbtype * fbtype

type envt = (ident * fbtype) list
                                               
exception TypeError ;;

let rec lookup gamma x =
  match gamma with
    [] -> raise TypeError
  | (id,ty)::rest -> if x = id then ty else lookup rest x

let rec typecheck gamma e =
  match e with
    Var x -> lookup gamma x
  | Function(Ide x,t,e1) ->
      let t' = typecheck (((Ide x),t)::gamma) e1 in
      TArrow(t,t')
  | Appl(e1,e2) ->
      let TArrow(t1,t2) = typecheck gamma e1 in
      if typecheck gamma e2 = t1 then t2 else
        raise TypeError
  | Plus(e1,e2) ->
      if typecheck gamma e1 = TInt &&
         typecheck gamma e2 = TInt then TInt else
        raise TypeError

(* ... *)
