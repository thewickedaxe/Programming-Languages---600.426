type ident = Ident of string

type exnid = string

type label = Lab of string

type expr =
	  Var of ident
	| Function of ident * fbtype * expr
	| Letrec of ident * ident * fbtype * expr * fbtype * expr
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
	| Ref of expr 
  | Set of expr * expr 
  | Get of expr
	| Record of (label * expr) list
	| Select of label * expr 
  | Raise of exnid * fbtype * expr
  | Try of expr * exnid * ident * fbtype * expr
  | Cell of int
  
and fbtype =
	  TInt 
  | TBool 
  | TArrow of fbtype * fbtype
	| TRec of (label * fbtype) list 
  | TRef of fbtype
  | TBottom (* Perchance it be useful *)