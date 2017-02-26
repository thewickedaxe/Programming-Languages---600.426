type ident = Ident of string

type name = Name of string

type expr = 
 Var of ident | Function of ident * expr | Appl of expr * expr |
 Plus of expr * expr | Minus of expr * expr | Equal of expr * expr | 
 And of expr * expr| Or of expr * expr | Not of expr |  
 If of expr * expr * expr | Int of int | Bool of bool | String of string |
 Seq of expr * expr | Let of ident * expr * expr | 
 Pair of expr * expr | Fst of expr | Snd of expr |
 Variant of name * expr | Match of expr * (name * ident * expr) list |
 EmptyList | Cons of expr * expr | Head of expr | Tail of expr |
 Actor of name | Send of expr * expr | Create of expr * expr |
 Print of expr

type fbtype = TInt | TBool | TArrow of fbtype * fbtype | TVar of string;;

