open Fbextast;;

exception TypeInferenceFailure of string;;

let typecheck_default_enabled = false;;

(* A module for sets of strings. *)
module StringSet = Set.Make(String);;

(* A module for mappings from strings (for type variables). *)
module StringMap = Map.Make(String);;

(* A module for sets of Fb identifiers *)
module IdentSet = Set.Make(
	struct
		type t = ident
		let compare = compare
	end);;

(* A module for assumption mappings. *)
module AssumptionMap = Map.Make(
	struct
		type t = Fbextast.ident
		let compare = compare
	end);;

(* A module for constraint sets. *)
module ConstraintSet = Set.Make(
	struct
		type t = fbtype * fbtype
		let compare = compare
	end);;

(* A module for a type variable namespace. *)
module type VAR_STORE =
sig
	type var_store
	val new_store: unit -> var_store
	val next: var_store -> string
end;;
module VarStore : VAR_STORE =
struct
	type var_store = int ref
	let new_store () = ref 0
	let next store =
		let this = !store in
		store := !store + 1;
		let type_var_name =
			if this = 0 then "a" else
				let rec makestring n =
					if n = 0 then "" else
						(makestring (n / 26)) ^ (String.make 1 (Char.chr ((n mod 26) + 97)))
				in makestring this
		in
		type_var_name
end;;

let constraint_set_of lst =
	List.fold_left (fun s -> fun e -> ConstraintSet.add e s) ConstraintSet.empty lst;;

let constraint_union_of lst =
	List.fold_left (fun s -> fun s' -> ConstraintSet.union s s') ConstraintSet.empty lst;;

(* Typechecks an Fb AST. *)
let typecheck e =
	let type_var_store = VarStore.new_store () in
	let fresh_type_var () = TVar(VarStore.next type_var_store) in
	let rec derive_type assumptions expr =
		let combine_op_typecheck result_type constraint_type elst =
			let type_and_constraint_set_list =
				List.fold_left (fun l -> fun e -> (derive_type assumptions e):: l) [] elst in
			let (type_list, constraint_set_list) = List.split type_and_constraint_set_list in
			let constraint_list = List.fold_left (fun l -> fun e -> (e, constraint_type):: l) [] type_list in
			result_type , constraint_union_of ((constraint_set_of constraint_list):: constraint_set_list)
		in
		let (t, c) =
			match expr with
			| Var(i) ->
					(
						try
							(AssumptionMap.find i assumptions) , ConstraintSet.empty
						with
						| Not_found ->
								let Ident(s) = i in
								raise (TypeInferenceFailure ("Could not find type for unbound variable "^s))
					)
			| Int(_) -> TInt , ConstraintSet.empty
			| Bool(_) -> TBool , ConstraintSet.empty
			| Plus(e1, e2) -> combine_op_typecheck TInt TInt [e1; e2]
			| Minus(e1, e2) -> combine_op_typecheck TInt TInt [e1; e2]
			| Equal(e1, e2) -> combine_op_typecheck TBool TInt [e1; e2]
			| And(e1, e2) -> combine_op_typecheck TBool TBool [e1; e2]
			| Or(e1, e2) -> combine_op_typecheck TBool TBool [e1; e2]
			| Not(e1) -> combine_op_typecheck TBool TBool [e1]
			| If(e1, e2, e3) ->
					let (t1, c1) = derive_type assumptions e1 in
					let (t2, c2) = derive_type assumptions e2 in
					let (t3, c3) = derive_type assumptions e3 in
					let v = fresh_type_var () in
					v , constraint_union_of [c1; c2; c3; (constraint_set_of [t1, TBool; t2, v; t3, v])]
			| Function(i, eb) ->
					let v = fresh_type_var () in
					let (t, c) = derive_type (AssumptionMap.add i v assumptions) eb in
					(TArrow (v, t)) , c
			| Appl(e1, e2) ->
					let (t1, c1) = derive_type assumptions e1 in
					let (t2, c2) = derive_type assumptions e2 in
					let v = fresh_type_var () in
					v , constraint_union_of [c1; c2; (constraint_set_of [t1, (TArrow (t2, v))])]
			| LetRec(i1, i2, e1, e2) ->
					let v2 = fresh_type_var () in (* for the argument *)
					let v3 = fresh_type_var () in (* for the return type *)
					let v1 = (TArrow(v2, v3)) in  (* the type of the recursive function *)
					let (t1, c1) = derive_type (AssumptionMap.add i1 v1 (AssumptionMap.add i2 v2 assumptions)) e1 in
					let (t2, c2) = derive_type (AssumptionMap.add i1 v1 assumptions) e2 in
					t2 , constraint_union_of [c1; c2; (constraint_set_of [t1, v3])]
                        | _ -> failwith "Not Implemented"
		in
		(t, c)
	in
	let rec compute_closure c =
		let calculate_symmetry c =
			ConstraintSet.fold (fun (t1, t2) -> fun s -> ConstraintSet.add (t2, t1) s) c ConstraintSet.empty
		in
		let calculate_arrow c =
			ConstraintSet.fold (
					fun e -> fun s ->
									match e with
									| (TArrow(t1, t2), TArrow(t1', t2')) ->
											ConstraintSet.union s (constraint_set_of [t1, t1'; t2, t2'])
									| _ -> s
				) c ConstraintSet.empty
		in
		let calculate_transitivity c =
			ConstraintSet.fold (
					fun (t1, t2) -> fun s ->
									ConstraintSet.union s (
											ConstraintSet.fold (
													fun (t1', t2') -> fun s -> if t2 = t1' then (ConstraintSet.add (t1, t2') s) else s
												) c ConstraintSet.empty)
				) c ConstraintSet.empty
		in
		let csym = calculate_symmetry c in
		let carr = calculate_arrow c in
		let ctrans = calculate_transitivity c in
		let c' = constraint_union_of [c; csym; carr; ctrans] in
		if ConstraintSet.equal c c'
		then c
		else compute_closure c'
	in
	let check_consistent c =
		ConstraintSet.fold (fun (t1, t2) -> fun () -> if (
									match (t1, t2) with
									| (TInt, TBool) -> true
									| (TBool, TArrow(_, _)) -> true
									| (TInt, TArrow(_, _)) -> true
									| _ -> false) then raise (TypeInferenceFailure "immediately inconsistent types") else ()
			) c ();
		ConstraintSet.fold (fun (t1, t2) -> fun () ->
								let rec type_contains_var t v =
									match t with
									| TInt -> false
									| TBool -> false
									| TVar(s) -> s = v
									| TArrow(t1, t2) -> (type_contains_var t1 v) || (type_contains_var t2 v)
								in
								match (t1, t2) with
								| (TVar(s), t') ->
										if t1 != t2 && type_contains_var t' s then
											raise (TypeInferenceFailure ("type variable '"^s^" is defined in terms of itself"))
										else ()
								| _ -> ()
			) c ()
	in
	let build_map_from_constraints c =
		ConstraintSet.fold (fun e -> fun m ->
								let mapping =
									match e with
									| (TVar(s), TVar(s')) -> if s > s' then Some(Ident(s), TVar(s')) else None
									| (TVar(s), t) -> Some(Ident(s), t)
									| _ -> None
								in
								let mapping' =
									match mapping with
									| Some(i, t) ->
											if AssumptionMap.mem i m
											then
												let t' = AssumptionMap.find i m in
												match t, t' with
												| (TVar(s), TVar(s')) -> if s > s' then None else mapping
												| (TVar(_), _) -> None
												| _ -> mapping
											else
												mapping
									| None -> None
								in
								match mapping' with
								| Some(i, t) -> AssumptionMap.add i t m
								| None -> m
			) c AssumptionMap.empty
	in
	let rec substitute_type t m =
		let t' =
			match t with
			| TInt -> TInt
			| TBool -> TBool
			| TVar(s) ->
					let i = Ident(s) in
					if AssumptionMap.mem i m then AssumptionMap.find i m else t
			| TArrow(t1, t2) ->
					TArrow(substitute_type t1 m, substitute_type t2 m)
		in
		if t = t' then t else substitute_type t' m
	in
	let canonicalize t =
		let names = VarStore.new_store () in
		let rec var_names_of_type t =
			match t with
			| TInt -> StringSet.empty
			| TBool -> StringSet.empty
			| TArrow(t', t'') -> StringSet.union (var_names_of_type t') (var_names_of_type t'')
			| TVar(s) -> StringSet.singleton s
		in
		let used_vars = StringSet.elements (var_names_of_type t) in
		let sorted_used_vars = List.sort Pervasives.compare used_vars in
		let type_var_remap = List.fold_left (fun acc -> fun elem ->
									StringMap.add elem (VarStore.next names) acc
				) StringMap.empty sorted_used_vars in
		let rec remap_var_names_in_type t =
			match t with
			| TInt -> TInt
			| TBool -> TBool
			| TArrow(t', t'') -> TArrow(remap_var_names_in_type t', remap_var_names_in_type t'')
			| TVar(s) -> TVar(StringMap.find s type_var_remap)
		in
		remap_var_names_in_type t
	in
	let (t, c) = derive_type (AssumptionMap.empty) e in
	let c' = compute_closure c in
	check_consistent c';
	let m = build_map_from_constraints c' in
	let result = substitute_type t m in
	canonicalize result;;
