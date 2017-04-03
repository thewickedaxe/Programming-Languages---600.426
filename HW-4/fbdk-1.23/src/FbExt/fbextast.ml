type ident = Ident of string

type name = Name of string                        

type expr = 
 Var of ident | Function of ident * expr | Appl of expr * expr |
 LetRec of ident * ident * expr * expr |
 Plus of expr * expr | Minus of expr * expr | Equal of expr * expr | 
 And of expr * expr| Or of expr * expr | Not of expr |  
 If of expr * expr * expr | Int of int | Bool of bool |
 Let of ident * expr * expr | 
 Variant of name * expr | Match of expr * (name * ident * expr) list |
 Pair of expr * expr | Fst of expr | Snd of expr 

type fbtype = TInt | TBool | TArrow of fbtype * fbtype | TVar of string;;

