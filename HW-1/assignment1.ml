(*

POPL Assignment 1

Name                  : Srinivas Suresh Kumar
List of Collaborators : Rohit Ravoori

Please make a good faith effort at listing people you discussed any problems with here, as per the
course academic integrity policy.  TA/CA/Prof need not be listed.

Fill in the function definitions below replacing the

  failwith "Not Implemented"

with your code.  Feel free to add "rec" to any function listed to make
it recursive. In some cases, you may find it helpful to define
auxillary functions, feel free to.  Other than replacing the failwiths
and adding recs, don't edit or remove anything else in the file -- the
autograder will not be happy!  Note you are not allowed to use the
"List.map" etc library functions; this rule holds just for this HW.
You also cannot use mutation (arrays, := etc), which we have not even
covered yet.

 *)

(* ****************************** Problem 1 ****************************** *)
(* ************************ Starting with Lists ************************** *)
(*
Problem 1a. [5 points]

Write a function which accepts a list of integers and returns another list such
that the value of each position in the new list is equal to the sum of all values
up to that position in the old list.
*)

let listsum lst =
  let rec summate lst sum =
    match lst with
    | [] -> []
    | a::b -> [sum+a] @ summate b (sum+a)
  in summate lst 0
;;

(*
Problem 1b. [10 points]

One of the most powerful features of OCaml is the "higher-order function": the
ability to treat functions as first-class citizens (just like objects in Java
or pointers in C).  Functions can be passed as arguments and stored in
variables in the same way as other data.

Write a function which generalizes the previous concept by taking an
accumulator function and a starting accumulator value (rather than assuming
addition for accumulation).  For instance, the behavior of the function defined
in Problem 1a could be recreated by writing

	acclist lst 0 (fun x -> fun y -> x + y)
*)

let acclist lst base f =
  let rec accum lst base f =
    match lst with
    | [] -> []
    | a::b -> let ans = f base a in [ans] @ accum b ans f
  in accum lst base f
;;
(*
# let prod x y = x * y;;
val prod : int -> int -> int = <fun>
# acclist [1;2;3;4] 1 prod;;
- : int list = [1; 2; 6; 24]
# acclist [2;4;6] 1 prod;;
- : int list = [2; 8; 48]
# acclist [true;false;true] true (&&);;
- : bool list = [true; false; false]
*)


(*
Problem 1c. [10 points]

Write a function which removes from a given list all duplicates it contains.
The first element is left in place while all remaining identical elements are
discarded.
*)
let rec revlist lst =
  match lst with
  | [] -> []
  | a::b -> revlist b @ [a]
;;

let rec search lst element =
  match lst with
  | [] -> false
  | a::b -> if a = element then true else search b element
;;

let listuniq lst =
  let revlst = revlist lst in
    let rec getuniq lst =
      match lst with
      | [] -> []
      | a::b -> if search b a then getuniq b else getuniq b @ [a]
    in getuniq revlst
;;

(*
# listuniq [1;2;3;1;2;3];;
- : int list = [1; 2; 3]
# listuniq [1;2;1;3;1;4;1;5];;
- : int list = [1; 2; 3; 4; 5]
# listuniq [2;3;1;2;1;4;6;2;3;4];;
- : int list = [2; 3; 1; 4; 6]
# listuniq [];;
- : 'a list = []
*)


(* ****************************** Problem 2 ****************************** *)
(* ************************* Variants and lists ************************** *)

(*
Problem 2a. [5 points]

OCaml provides a keyword "type" which allows you to define your own variant
data types.  For instance, the Java language allows any reference type to
contain the value null, effectively meaning "no reference here".  OCaml does
not allow this implicitly, but that behavior can be recovered with the
following data type:
*)

type 'a nullable = Null | NotNull of 'a;;

(*
Write a function which accepts a list as input and returns the number of null
elements in that list.
*)

let nullcount lst =
  let rec countnulls lst count =
    match lst with
    | [] -> count
    | a::b -> if a=Null then countnulls b count+1 else countnulls b count
    in countnulls lst 0
;;

(*
# nullcount [];;
- : int = 0
# nullcount [NotNull(5);NotNull(3);NotNull(1)];;
- : int = 0
# nullcount [Null;NotNull(2);Null];;
- : int = 2
# nullcount [Null;NotNull([1;2;3])];;
- : int = 1
*)


(*
Problem 2b. [5 points]

Write a function which takes a list of nullable values and gives back a list of
non-nullable values; values in the original list which were null are skipped.
*)

let rec nullfilter lst =
  match lst with
  | [] -> []
  | a::b -> match a with
            | NotNull(n) -> n::nullfilter b
            | Null -> nullfilter b
;;
(*
# nullfilter [];;
- : 'a list = []
# nullfilter [NotNull(5);NotNull(3);NotNull(1)];;
- : int list = [5; 3; 1]
# nullfilter [Null;NotNull(2);Null];;
- : int list = [2]
# nullfilter [Null;NotNull([1;2;3])];;
- : int list list = [[1; 2; 3]]
*)


(*
Problem 2c. [10 points]

Write a function which merges two sorted lists in order.  Assume that your
input lists are already sorted; the output list should likewise be sorted.
Your function must take a comparison function as an input which is responsible
for determining the order of elements in the list; that function must return
one of the values defined in the "comparison" data type.  (Note that a
variant whose constructors take no arguments is effectively just like an
enumeration in other languages, such as Java.)  That is, "cmp a b" returns
LessThan if a < b.
*)

type comparison = LessThan | EqualTo | GreaterThan;;

let rec merge lst1 lst2 cmp =
  match lst1 with
  | [] -> lst2
  | a::b -> match lst2 with
            | [] -> lst1
            | c::d -> if cmp a c = LessThan then a::merge b lst2 cmp else c::merge lst1 d cmp
;;

(*
# let intcmp x y =
	if x < y then LessThan else
		if x > y then GreaterThan else EqualTo;;
    val intcmp : 'a -> 'a -> comparison = <fun>
# let rintcmp x y = intcmp y x;;
val rintcmp : 'a -> 'a -> comparison = <fun>
# merge [1;3] [2;4] intcmp;;
- : int list = [1; 2; 3; 4]
# merge [1;6;7] [2;3;3;5;6] intcmp;;
- : int list = [1; 2; 3; 3; 5; 6; 6; 7]
# merge [] [2;3] intcmp;;
- : int list = [2; 3]
# merge [5;4;0] [3;1;-1] rintcmp;;
- : int list = [5; 4; 3; 1; 0; -1]
*)


(*
Problem 2d. [10 points]

Write a function which will split an input list in half.  The result should be
a tuple of two lists; half of the elements should be in the first list and half
should be in the second list.  It does not matter which elements go to which
list as long as each list is about the same size.  (For an input of a list of
odd length, make the first list one element larger than the other list.)
*)

let splitlist lst =
  let rec spltlst lst flag =
    match lst with
    | [] -> []
    | a::b -> if flag = 0 then [a]@spltlst b 1 else spltlst b 0
  in (spltlst lst 0, spltlst lst 1)
;;

(*
# splitlist [1;2;3;4;5;6];;
- : int list * int list = ([1; 3; 5], [2; 4; 6])
# splitlist [1;2;7;8;9];;
- : int list * int list = ([1; 7; 9], [2; 8])
# splitlist [true;true;false;true];;
- : bool list * bool list = ([true; false], [true; true])
# splitlist [];;
- : 'a list * 'a list = ([], [])
*)


(*
Problem 2e. [10 points]

Using the merge and splitlist functions you defined in the previous two
problems, write a function which performs a merge sort.
*)
let intcmp x y =
	if x < y then LessThan else
		if x > y then GreaterThan else EqualTo;;

let getlen lst =
  let rec listlen lst cnt =
    match lst with
    | [] -> cnt
    | a::b -> listlen b cnt+1
  in listlen lst 0
;;

let rec mergesort lst cmp =
  let (x, y) = splitlist lst in
    if getlen x > 1 then begin
      let p = mergesort x cmp in
        let q = mergesort y cmp in
          merge p q cmp
    end else merge x y cmp
;;

(*
# mergesort [4;1;3;2;9;4;5] intcmp;;
- : int list = [1; 2; 3; 4; 4; 5; 9]
# mergesort [2;6;4;9;5;2;0] intcmp;;
- : int list = [0; 2; 2; 4; 5; 6; 9]
# mergesort [5] intcmp;;
- : int list = [5]
# mergesort [] intcmp;;
- : 'a list = []
*)


(* ****************************** Problem 3 ****************************** *)
(* ******************** Polynomials and some symbolic algebra ************ *)

(* 3.

A direct, if not exceptionally elegant, way to represent simple univariate
polynomials is as a list of pairs where each pair (c, d) represents the
coefficient and the degree of a term respectively. For example the polynomial
5x^3 + 2x + 1 is represented by the list [ (5, 3) ; (2, 1) ; (1, 0) ] or any
permutation thereof. (There are no ordering guarantees on the list; so the
polynomial [ (1, 0) ; (2, 1) ; (5, 3) ] is equivalent to the above. On the other
hand two terms with the same degree cannot occur in the polynomial. i.e. Pairs
in the list must all have distict second elements - [ (1, 0) ; (2, 1) ; (3, 3) ;
(2, 3) ] ) is NOT a valid polynomial)

 *)


(* 3a. [5 points]
    Write a function to check if a polynomial is valid.
 *)

let rec search lst elem =
  match lst with
  | [] -> false
  | a::b -> if a=elem then true else search b elem
;;

let rec check_for_dups lst =
  match lst with
  | [] -> false
  | a::b -> if search b a then true else check_for_dups b
;;

let rec getpowers poly =
  match poly with
  | [] -> []
  | (x, y)::b -> y::getpowers b
;;

let rec is_valid_polynomial poly =
  let powerlist = getpowers poly in
    let ans = check_for_dups powerlist in
      not ans
;;

(*
# is_valid_polynomial [ (5, 3) ; (2, 1) ; (1, 0) ];;
- : bool = true
# is_valid_polynomial [ (5, 3) ; (2, 1) ; (1, 1) ]
- : bool = false
*)

(*
3b. [10 points] Write a function to add two polynomials.

For this question and the next assume that the coefficients and degree are both
of the integer type.

*)
let rec revlist lst =
  match lst with
  | [] -> []
  | a::b -> revlist b @ [a]
;;

let rec search lst element =
  match lst with
  | [] -> false
  | a::b -> if a = element then true else search b element
;;

let listuniq lst =
  let revlst = revlist lst in
    let rec getuniq lst =
      match lst with
      | [] -> []
      | a::b -> if search b a then getuniq b else getuniq b @ [a]
    in getuniq revlst
;;

let rec find_similar x y poly2 =
  match poly2 with
  | (p,q)::r -> if q=y then [(p+x,y)] else find_similar x y r
  | [] -> [(x, y)]
;;


let rec add_polynomials poly_1 poly_2 =
  let rec prioritizep1 poly1 poly2 =
    match poly1 with
    | [] -> []
    | (x, y)::b -> find_similar x y poly2@prioritizep1 b poly2
  in let ans = prioritizep1 poly_1 poly_2 @ prioritizep1 poly_2 poly_1 in
  listuniq ans
;;

(*
# add_polynomials [ (5, 3) ; (2, 1) ; (1, 0) ] [ (5, 2) ; (2, 3) ; (5, 0) ];;
- : (int * int) list = [(7, 3); (2, 1); (6, 0); (5, 2)]
# add_polynomials [(3, 4) ; (-2, 2) ; (3, 0)] [(2, 3) ; (1, 2) ; (-2, 0)] ;;
- : (int * int) list = [(3, 4); (-1, 2); (1, 0); (2, 3)]
*)

(*
   3c. [10 Points] Write a function to do polynomial multiplication.


*)
let rec revlist lst =
  match lst with
  | [] -> []
  | a::b -> revlist b @ [a]
;;

let rec search lst element =
  match lst with
  | [] -> false
  | a::b -> if a = element then true else search b element
;;

let listuniq lst =
  let revlst = revlist lst in
    let rec getuniq lst =
      match lst with
      | [] -> []
      | a::b -> if search b a then getuniq b else getuniq b @ [a]
    in getuniq revlst
;;

let rec make_bad_list poly1 poly2 =
  let rec pripoly1 a1 a2 poly2 =
    match poly2 with
    | [] -> []
    | (c1,c2)::d -> (c1*a1, c2+a2)::pripoly1 a1 a2 d
  in match poly1 with
  | [] -> []
  | (a1,a2)::b -> pripoly1 a1 a2 poly2 @ make_bad_list b poly2
;;

let rec find_and_sum a1 a2 poly index=
  let rec find_n_sum a1 a2 poly index1 index =
    match poly with
    | [] -> (a1, a2)
    | (c1,c2)::d -> let sumcoeff = c1+a1 in if c2 = a2 then find_n_sum sumcoeff a2 d index1 index else find_n_sum a1 a2 d index1 index
  in find_n_sum 0 a2 poly 0 0
;;

let rec move_over_poly poly1 poly2 =
  let rec mov_poly poly1 poly2 index =
    match poly1 with
    | [] -> []
    | (a1,a2)::b -> let next_index = index+1 in find_and_sum a1 a2 poly2 index::mov_poly b poly2 next_index
  in mov_poly poly1 poly2 0
;;

let rec multiply_polynomials poly_1 poly_2 =
  let badpoly = make_bad_list poly_1 poly_2 in
    let ans = move_over_poly badpoly badpoly in
      listuniq ans
;;
(*
# multiply_polynomials [(3, 4) ; (-2, 2) ; (3, 0)] [(2, 3) ; (1, 2) ; (-2, 0)] ;;
- : (int * int) list = [(6, 7); (-4, 5); (6, 3); (-6, 0); (3, 6); (-8, 4); (7, 2)]
# multiply_polynomials [(1, 2) ; (-2, 1) ; (1, 0)] [(1, 1); (-1, 0)] ;;
- : (int * int) list = [(1, 3); (-3, 2); (3, 1); (-1, 0)]
*)


(* ****************************** Problem 4 ****************************** *)
(* ************************* Vectors and Matrices ************************ *)

(*
  4a. [10 Points]

One way to represent a vector is as a list. Given two vectors

      v1 = [a0; a1; .. an] and v2 = [b0; b1; .. bn],

      the scalar product v1 . v2 = a0 * b0 + a1 * b1 + ... + an * bn.

In a language like OCaml without overloaded operators, directly writing the
expression with arithemetic operators forces us to fix a type for the vector
coordinates. For example if we used operators ( * ) and ( + ), the vector
coordinates can only be of type int. To use vectors with floating point
coordinates we will have to write a new function. The way to generalize this (as
usual) is to hide the base operations behind functions. The above expression has
two basic operations - multiplication of two values and addition of a list of
values. We define two parameters creatively named prod_fn and sum_fn where
prod_fn is a binary function and sum_fn is a function over lists.

Given the two functions and two vectors (represented as lists), write a function
to compute the scalar product. If the vector dimensions differ, raise an
exception using invalid_arg.


 *)
let rec multi prod_fn v1 v2 =
  match (v1, v2) with
  | ([], []) -> []
  | ([], _::_) ->  invalid_arg "invalid args"
  | (_::_, []) -> invalid_arg "invalid args"
  | (a::b, c::d) -> [prod_fn a c] @ multi prod_fn b d
;;


let rec generalized_scalar_product prod_fn sum_fn v1 v2 =
  let ans = multi prod_fn v1 v2 in
  sum_fn ans
;;
(*
let rec sum_int lst = match lst with [] -> 0 | h::t -> h + (sum_int t);;
# generalized_scalar_product ( * ) sum_int [1 ; 2 ;3 ] [4 ; 5 ; 6] ;;
- : int = 32
let rec sum_float lst = match lst with [] -> 0.0 | h::t -> h +. (sum_float t);;
generalized_scalar_product ( *. ) sum_float [1.0 ; 2.0 ;3.0 ] [4.0;5.0;6.0] ;;
- : float = 32.
# generalized_scalar_product ( ^ ) (String.concat ", ") ["a"; "b"; "c"] ["1" ; "2" ; "3"] ;;
- : string = "a1, b2, c3"
*)


(*
  4b. [10 points]

Similar to the Vector representation above, a matrix can be represented as a
list of lists with each list representing a row of the matrix. (Clearly each of
these lists must have the same length).

Define a function to multiply two matrices. The calculation involved is very similar to
the case with vectors. So to maintain generality we once again hide the multiplication and
addition operations behind the two functions (prod_fn and sum_fn).

If the two matrices cannot be multiplied (due to dimension mismatches), raise an exception
(using the built-in invalid_arg function).

*)
let head lst =
	match lst with
	| [] -> failwith "empty"
	| a:: b -> a;;

let tail lst =
	match lst with
	|[]->[]
	| a:: b -> b;;

let rec map func lst =
	match lst with
	[]->[]
	| a::b -> func a :: map func b ;;

let rec transpose list =
  match list with
  | [] -> []
  | []::xss -> transpose xss
  | (x::xs)::xss ->
    (x :: map head xss) :: transpose (xs :: map tail xss);;

let rec vec_mat_mul prod_fn sum_fn a mat2 =
  match mat2 with
  | [] -> []
  | c::d -> generalized_scalar_product prod_fn sum_fn a c::vec_mat_mul prod_fn sum_fn a d
;;

let rec generalized_matrix_multiplication prod_fn sum_fn matrix_1 matrix_2 =
  let tmat2 = transpose matrix_2 in
    let rec mulmat prod_fn sum_fn mat1 mat2 =
      match mat1 with
      | [] -> []
      | a::b -> vec_mat_mul prod_fn sum_fn a mat2::mulmat prod_fn sum_fn b mat2
    in mulmat prod_fn sum_fn matrix_1 tmat2
;;

(*
# let rec sum_int lst = match lst with [] -> 0 | h::t -> h + (sum_int t);;
# let m1 = [ [1;2] ] ;;
# let m2 = [ [3] ; [4] ] ;;
# generalized_matrix_multiplication ( * ) sum_int m1 m2 ;;
- : int list list = [[11]]
(* Multiplication example taken from wikipedia *)
# let m1 = [ [14;9;3] ; [2;11;15] ; [0;12;17] ; [5;2;3] ] ;;
# let m2 = [ [12;25] ; [9;10] ; [8;5] ] ;;
# generalized_matrix_multiplication ( * ) sum_int m1 m2 ;;
- : int list list = [[273; 455]; [243; 235]; [244; 205]; [102; 160]]
*)