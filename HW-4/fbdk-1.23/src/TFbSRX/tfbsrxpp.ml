open Tfbsrxast;;

let pretty_print_fun f e = (f e "") ^ "\n"

let rec pp_type t pad =
	match t with
	| TInt -> "Int"
	| TBool -> "Bool"
	| TArrow(t1, t2) ->
			(
				match t1 with
					| TArrow(_,_) -> "(" ^ pp_type t1 pad ^ ")"
					| _ -> pp_type t1 pad
			) ^ " -> " ^ pp_type t2 pad
	| TRef(t) -> (pp_type t pad) ^ " Ref"
  | TRec(lst) ->
		"{" ^ (List.fold_left (fun acc -> fun (Lab(lbl),typ) ->
				acc ^ (if String.length acc > 0 then ", " else "") ^
				lbl ^ ":" ^ (pp_type typ pad)
			) "" lst) ^ "}"
  | TBottom -> "BOTTOM"

(* TODO: Old code reused. But the entire thing is better rewritten with sprintf *)

let rec pp e pad =
	match e with
		Bool(true) -> "True"
	| Bool(false) -> "False"
	| Int(x) -> string_of_int x
	| Plus(e1, e2) ->
			pp e1 pad ^ " + " ^ pp e2 pad
	| Minus(e1, e2) ->
			pp e1 pad ^ " - " ^ pp e2 pad
	| Equal(e1, e2) ->
			pp e1 pad ^ " = " ^ pp e2 pad
	| And(e1, e2) ->
			pp e1 pad ^ " And " ^ pp e2 pad
	| Or(e1, e2) ->
			pp e1 pad ^ " Or " ^ pp e2 pad
	| Not(e1) ->
			"Not " ^ pp e1 pad
	| Appl(e1, e2) ->
			"(" ^ pp e1 pad ^ ") (" ^ pp e2 pad ^ ")"
	| Var(Ident(x)) -> x
	| Function(Ident i, t, x) ->
			let newpad = pad ^ "  " in
			"Function " ^ i ^ ":" ^ (pp_type t pad) ^ "->\n" ^ newpad ^ pp x newpad
	| If(e1, e2, e3) ->
			let newpad = pad ^ "  " in
			"If " ^ pp e1 pad ^ " Then\n" ^ newpad ^ pp e2 newpad ^
			"\n" ^ pad ^ "Else\n" ^ newpad ^ pp e3 newpad
	| Letrec(Ident(i1), Ident(i2), t1, e1, t2, e2) ->
			let newpad = pad ^ "  " in
			"Let Rec " ^ i1 ^ " " ^ i2 ^ ":" ^ (pp_type t1 pad) ^ " =\n" ^ newpad ^
			pp e1 newpad ^ ":" ^ (pp_type t2 pad) ^ "\n" ^ pad ^ "In\n" ^ newpad ^
			pp e2 newpad
  | Record body -> "{" ^ pp_record body pad^ "}"
  | Select(Lab l, e) -> pp e pad ^ "." ^ l
  | Get e -> "!" ^ pp e pad
  | Set(e1, e2) -> pp e1 pad ^ " := " ^ pp e2 pad
  | Ref e -> "Ref " ^ pp e pad
  | Raise(n, t, e) -> "Raise " ^ n ^ " " ^ (pp e pad)
  | Try(e1, n, Ident id, t, e2) ->
      "Try\n" ^ (pp e1 pad) ^ "\n" ^ pad ^  "With " ^  id ^ ":" ^ 
      (pp_type t pad) ^ " -> " ^ (pp e2 pad)
  | Cell(i) -> "<CELL #" ^ string_of_int i ^ ">"   
  
and pp_record body pad =
  match body with
    [] -> ""
  | (Lab l, e)::rest ->
      l ^ "=" ^ pp e pad ^
      (if rest = [] then "" else "; ") ^
      pp_record rest pad

let pretty_print e = pretty_print_fun pp e


let pretty_print_type t = pretty_print_fun pp_type t
