open Boolast;;

let pretty_print_fun f e = (f e "") ^ "\n"

let rec pp e pad =
  match e with
    True -> "True"
  | False -> "False"
  | And(e1, e2) -> "(" ^ (pp e1 pad) ^ " And " ^ (pp e2 pad) ^ ")"
  | Or(e1, e2) -> "(" ^ (pp e1 pad) ^ " Or " ^ (pp e2 pad) ^ ")"
  | Implies(e1, e2) -> "(" ^ (pp e1 pad) ^ " Implies " ^ (pp e2 pad) ^ ")"
  | Not e -> "(Not " ^ (pp e pad) ^ ")"

let pretty_print e = pretty_print_fun pp e

let pp_type () pad = "Bool"

let pretty_print_type e = pretty_print_fun pp_type e
