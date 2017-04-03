type expr = 
    True 
  | False 
  | And of expr * expr 
  | Or of expr * expr 
  | Implies of expr * expr
  | Not of expr 

type fbtype = unit
