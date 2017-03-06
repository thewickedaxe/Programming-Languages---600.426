(*

PoPL Assignment 3 part 2

Your Name             : Srinivas Suresh Kumar
List of Collaborators : Rohit Ravoori

For this part, you will write some programs in Fb.  Your answers for this
section must be in the form of OCaml strings which will load and run in the
standalone interpreter, or that the parse function in fbdktoploop.ml will
successfully turn into Fb ASTs.  If you want a macro for repeated code, Just use
OCaml to put strings together.  Here is an example.

 *)

let pair c1 c2  = "((Function lft -> Function rgt -> Function x -> x lft rgt) ("^c1^") ("^c2^"))";;
let left c =  "(("^c^") (Function x -> Function y -> x))";;
let right c =  "(("^c^") (Function x -> Function y -> y))";;

let my_pair = pair "34" "45";;  (* makes one string what can be parsed into an AST *)

(*

Do realize this is a very primitive macro system, you will want to put () around
any definition you make or when appended the parse order could change.

For questions in this section you are not allowed to use the Let Rec syntax even
if you have implemented it in your interpreter. Any recursion that you use must
entirely be in terms of Functions. Feel free to implement an Fb Y-combinator
here.  For examples and hints, see the file "src/Fb/fbexamples.ml" in the FbDK
project.

Remember to test your code against the standard Fb binaries (and not just your own
implementation of Fb) to ensure that your functions work correctly.

*)


(* 2a. [5 points] Fb is such a minimalistic language that it does not even
include a < operation.  But it is possible to create one of your own.

Hint: a) You can call upon your powers of recursion ; b) We dont care about
efficiency; c) get it to work on positive numbers first then move on to
negatives

 *)

let fblt = "
(
 Function main_fn -> Function det_type -> Function operand_1 -> Function operand_2 -> main_fn main_fn det_type operand_1 operand_2
)
(
 Function self -> Function det_type -> Function op_1 -> Function op_2 ->
  If (op_1 = op_2) Then False Else (
    If (det_type det_type op_1 op_1) = 1 Then (
     If (det_type det_type op_2 op_2) = 1 Then (
      self self det_type (op_1 - 1) (op_2 - 1)
     ) Else (
      False
     )
    ) Else (
       If (det_type det_type op_2 op_2) = 0 Then (
        self self det_type (op_1 + 1) (op_2 + 1)
     ) Else (
      True
     )
    )
  )
)
(
    Function self_check -> Function incre -> Function decre ->
     If (incre = 0) Then 0
     Else (
      If (decre = 0) Then 1
      Else (
       self_check self_check (incre + 1) (decre - 1)
      )
     )
)
";;
(*
# rep ("("^fblt^") 2 3") ;; (*!! convert all other tests below to pure string form - simpler !!*)
==> True
- : unit = ()
# rep ("("^fblt^") (0-3) (0-4)") ;;
==> False
- : unit = ()
*)


(*
Problem 2b. [5 points]

Fb also fails to provide any operations over integers more complex than addition
and subtraction.  Below, define the following operations: multiplication,
integer division, and integer modulus.  (Hint: if you get stuck, try getting
them working for positive numbers first and then dealing with negatives.)  Your
division and modulus functions should diverge when the divisor is zero.
*)

let fbMultiply =
"(
  Function main_fn -> Function det_type -> Function operand_1 -> Function operand_2 -> main_fn main_fn det_type operand_1 operand_2 0
)
(
  Function self_mul -> Function det_type -> Function op_1 -> Function op_2 -> Function product ->
    If op_1 = 0 Then (
      product
    ) Else (
      If (det_type det_type op_1 op_1) = 1 Then (
        self_mul self_mul det_type (op_1 - 1) op_2 (product + op_2)
      ) Else (
        self_mul self_mul det_type (op_1 + 1) op_2 (product - op_2)
      )
    )
)
(
    Function self_check -> Function incre -> Function decre ->
     If (incre = 0) Then 0
     Else (
      If (decre = 0) Then 1
      Else (
       self_check self_check (incre + 1) (decre - 1)
      )
     )
)
";;
let fbDivide = 
"
(
  Function main_fn -> Function det_type -> Function operand_1 -> Function operand_2 -> main_fn main_fn det_type operand_1 operand_2 0
)
(
  Function self_div -> Function det_type -> Function op_1 -> Function op_2 -> Function quotient ->
   If (op_1 = 0) Then quotient
   Else (
    If (det_type det_type op_1 op_1 = 1) Then (
      If (det_type det_type op_2 op_2 = 1) Then
      (
        If ((det_type det_type op_1 op_1) = (det_type det_type (op_1 - op_2) (op_1 - op_2))) Then (
          self_div self_div det_type (op_1 - op_2) (op_2) (quotient + 1)
        ) Else (
          If (op_1 = op_2) Then (
            (quotient + 1)
          ) Else (
            quotient
          )
        )
      ) Else
      (
        If ((det_type det_type op_1 op_1) = (det_type det_type (op_1 + op_2) (op_1 + op_2))) Then (
          self_div self_div det_type (op_1 + op_2) (op_2) (quotient - 1)
        ) Else (
          If (op_1 = (0-op_2)) Then (
            (quotient - 1)
          ) Else (
            quotient
          )
        )
      )
    ) Else (
      If (det_type det_type op_2 op_2 = 1) Then
      (
        If ((det_type det_type op_1 op_1) = (det_type det_type (op_1 + op_2) (op_1 + op_2))) Then (
          self_div self_div det_type (op_1 + op_2) (op_2) (quotient - 1)
        ) Else (
          quotient
        )
      ) Else
      (
        If ((det_type det_type op_1 op_1) = (det_type det_type (op_1 - op_2) (op_1 - op_2))) Then (
          self_div self_div det_type (op_1 - op_2) (op_2) (quotient + 1)
        ) Else (
          quotient
        )
      )
    )
   )
)
(
  Function self_check -> Function incre -> Function decre ->
   If (incre = 0) Then 0
   Else (
    If (decre = 0) Then 1
    Else (
     self_check self_check (incre + 1) (decre - 1)
    )
   )
)
";;
let fbMod = "
(
  Function main_fn -> Function det_type -> Function operand_1 -> Function operand_2 -> main_fn main_fn det_type operand_1 operand_2 
)
(
  Function self_div -> Function det_type -> Function op_1 -> Function op_2 -> 
   If (op_1 = 0) Then 0
   Else (
    If (det_type det_type op_1 op_1 = 1) Then (
      If (det_type det_type op_2 op_2 = 1) Then
      (
        If ((det_type det_type op_1 op_1) = (det_type det_type (op_1 - op_2) (op_1 - op_2))) Then (
          self_div self_div det_type (op_1 - op_2) (op_2) 
        ) Else (
          If (op_1 = op_2) Then (
            0
          ) Else (
            op_1
          )
        )
      ) Else
      (
        If ((det_type det_type op_1 op_1) = (det_type det_type (op_1 + op_2) (op_1 + op_2))) Then (
          self_div self_div det_type (op_1 + op_2) (op_2) 
        ) Else (
          If (op_1 = (0-op_2)) Then (
            0
          ) Else (
            op_1
          )
        )
      )
    ) Else (
      If (det_type det_type op_2 op_2 = 1) Then
      (
        If ((det_type det_type op_1 op_1) = (det_type det_type (op_1 + op_2) (op_1 + op_2))) Then (
          self_div self_div det_type (op_1 + op_2) (op_2) 
        ) Else (
          op_1
        )
      ) Else
      (
        If ((det_type det_type op_1 op_1) = (det_type det_type (op_1 - op_2) (op_1 - op_2))) Then (
          self_div self_div det_type (op_1 - op_2) (op_2) 
        ) Else (
          op_1
        )
      )
    )
   )
)
(
  Function self_check -> Function incre -> Function decre ->
   If (incre = 0) Then 0
   Else (
    If (decre = 0) Then 1
    Else (
     self_check self_check (incre + 1) (decre - 1)
    )
   )
)
";;

(*
#  rep ("("^fbMultiply^") 3 5");;
==> 15
- : unit = ()
#  rep ("("^fbMultiply^") 0 2");;
==> 0
- : unit = ()
# rep ("("^fbMultiply^") (0-3) 5");;
==> -15
- : unit = ()
# rep ("("^fbMultiply^") (0-2) (0-4)");;
==> 8
- : unit = ()
# rep ("("^fbMultiply^") 12 7");;
==> 84
- : unit = ()
#  rep ("("^fbDivide^") 12 4");;
==> 3
- : unit = ()
#  rep ("("^fbDivide^") (0-8) 2");;
==> -4
- : unit = ()
#  rep ("("^fbDivide^") 7 3");;
==> 2
- : unit = ()
#  rep ("("^fbMod^") 12 4");;
==> 0
- : unit = ()
#  rep ("("^fbMod^") 7 3");;
==> 1
- : unit = ()
#  rep ("("^fbMod^") 64 5");;
==> 4
- : unit = ()
*)


(* 2c. [10 points] We can even encode fairly complex data structures in Fb. For
this question we will consider the encoding of a simple functional dictionary /
map. For simplicity we will assume that they keys and values for this data
structure are Ints.

The dictionary data structure is defined by the four functions below.

Hint: This is simpler than it looks. For the simplest answer, you do not even
require the Y-combinator.
*)

(* Write a Fb function that takes a single dummy argument as input and returns a
new empty dictionary *)
let fbDictEmpty = "(0 1)" ;;

(* Write an Fb function that takes a dictionary, a key and a value and returns a
  new dictionary with the key mapped to the specified value.  *)
let fbDictAdd = "(0 1)" ;;

(* Write an Fb function that takes a dictionary and key as input and returns the
  value mapped to the key if any.  Fb does not provide a direct way to report
  errors. So in case the dictionary does not contain the key, your function
  should diverge.  *)

let fbDictGet = "(0 1)" ;;

(* Write an Fb function that takes a dictionary and a key as input and returns a
   new dictionary such that the key is no longer mapped. You are allowed to
   assume that this function is only called with keys known to be present in the
   dictionary.  *)

let fbDictRemove = "(0 1)" ;;

(*
Here are some test examples.  We are going to first parse your code to an AST in these
examples, this is another way to build up Fb programs -- in AST-land and not string-land.

# let empty = Appl(parse fbDictEmpty, Int 0) ;;
# let dict_a = eval ( Appl(Appl(Appl(parse fbDictAdd, empty), Int 1), Int 11) );;
# let dict_b = eval ( Appl(Appl(Appl(parse fbDictAdd, dict_a), Int 2), Int 12) ) ;;
# ppeval ( Appl(Appl(parse fbDictGet, dict_b), Int 1) ) ;;
==> 11
- : unit = ()
# ppeval ( Appl(Appl(parse fbDictGet, dict_b), Int 2) ) ;;
==> 12
- : unit = ()
# ppeval (Appl(Appl(parse fbDictGet, Appl(Appl(Appl(parse fbDictAdd, empty), Int 1), Int 121)), Int 1)) ;;
==> 121
- : unit = ()
# ppeval ( Appl(Appl(parse fbDictGet, dict_b), Int 10) ) ;;
==> Exception: ...
# let dictCreate keyList valList = List.fold_left2 (
    fun res -> fun k -> fun v -> Appl(Appl(Appl(parse fbDictAdd, res), k), v)
  ) empty keyList valList ;;
# let dict_c = eval (dictCreate [Int 4; Int 3; Int 2; Int 1; Int 0] [Int 404; Int 303; Int 202; Int 101; Int 0] ) ;;
# ppeval ( Appl(Appl(parse fbDictGet, dict_c), Int 0) ) ;;
==> 0
- : unit = ()
# ppeval ( Appl(Appl(parse fbDictGet, dict_c), Int 3) ) ;;
==> 303
- : unit = ()
# let dict_d = ( Appl(Appl(parse fbDictRemove, dict_c), Int 3) ) ;;
# ppeval ( Appl(Appl(parse fbDictGet, dict_d), Int 3) ) ;;
==> Exception: ...
# let dict_e = ( Appl(Appl(parse fbDictRemove, dict_d), Int 2) ) ;;
# ppeval ( Appl(Appl(parse fbDictGet, dict_e), Int 2) ) ;;
==> Exception: ...
# let dict_f = ( Appl(Appl(Appl(parse fbDictAdd, dict_e), Int 3), Int 606) ) ;;
# ppeval ( Appl(Appl(parse fbDictGet, dict_f), Int 3) ) ;;
==> 606
- : unit = ()
*)

