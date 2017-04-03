(* ******************************************************************************** *)
(* ************************** Programming In Fb *********************************** *)
(* ******************************************************************************** *)

(* Here are some Fb examples from the book.  Requires fbdktoploop.ml *)
"((Function arg1 -> Function arg2 -> Function arg3 -> arg1 arg2 arg3)
(Function i -> Function j -> If Not(i = j) Then True Else False)
(4)
(5))"
let ex1 = "If Not(1 = 2) Then 3 Else 4";;

let ex2 = "(Function x -> x + 1) 5" ;;

let ex3 = "(Function x -> Function y -> x + y) 4 5" ;;

let ex4 = "Let Rec fib x =
    If x = 1 Or x = 2 Then 1 Else fib (x - 1) + fib (x - 2)
    In fib 6" ;;

let ex5 = "(Function x -> x + 2)(3 + 2 + 5)" ;;

let ex6 = "(Function x -> Function x -> x) 3" ;;

let ex7 = "Function x -> Function y -> x + y + z";;

let ex8 = 
 "Let Rec x1 x2 = 
     If x2 = 1 Then
          (Function x3 -> x3 (x2 - 1)) (Function x4 -> x4)
     Else
          x1 (x2 - 1)
  In x1 100";;


let ex9 = "If 3 = 4 Then 5 Else 4 + 2" ;;
   
let ex10 = "(Function x -> If 3 = x Then 5 Else x + 2) 4 " ;;

let ex11 = "(Function x -> x x)(Function y -> y) " ;;

let ex12 = "(Function f -> Function x -> f(f(x)))
           (Function x -> x - 1) 4" ;;

let ex13 = "(Function x -> Function y -> x + y)
    ((Function x -> If 3 = x Then 5 Else x + 2) 4)
    ((Function f -> Function x -> f (f x))
            (Function x -> x - 1) 4 )" ;;

let ex14 = "Let Rec f x = 
    If x = 1 Then 1 Else x + f (x - 1)
In f 3" ;;


let ex15 = "Let Rec f x = 
    If x = 1 Then 1 Else x + f (x - 1)
  In f" ;;


let diverger = "(Function x -> x x)(Function x -> x x)" ;;

let combI = "Function x -> x";;
let combK = "Function x -> Function y -> x";;
let combS = "Function x -> Function y -> Function z -> (x z) (y z)";;
let combD = "Function x -> x x";;

(* ******************************************************************************** *)
(* ********************************* Macros *************************************** *)
(* ******************************************************************************** *)

(* Pairs.   First lets hack some by hand, then do the general macro. *)

let pair32 = "(Fun d -> d 3 2)";;
let left32 = pair32^"(Fun l -> Fun r -> l)";;  
rep left32;;
let right32 = pair32^"(Fun l -> Fun r -> r)";;  
rep right32;;
    
(* General pair macros.  
   We are going to use OCaml as the macro language and compose strings to make Fb programs.  
   This is similar in spirit how #define and the cpp pre-processor works for C. *)
 
let pr(c1, c2)  =
  "((Function lft -> Function rgt -> 
      Function x -> x lft rgt) ("^c1^")("^c2^"))";;

(* WARNING: in order to avoid parsing precedence problems, make sure when writing macros
to (1) enclose the whole macro you are defining in (...) and (2) for each parameter,
put "("^param^")" parens around it.  If you don't do these things the
parse precedence can change, creating difficult bugs.  *)

(* Also note we are using products for multiple arguments, not Currying - it makes it easier to read the macros if they are not Curried *)
  
let left c =  "("^c^") (Function x -> Function y -> x)";;
let right c =  "("^c^") (Function x -> Function y -> y)";;

let pc = pr("34","45");;  (* construct a string representing the Fb program for this pair *)

let preg = left pc;;

rep preg;;

(* Alternative macro mechanism: pair macros as OCaml functions over ASTs *)
(* This form is harder to read but no parse precedence issues *)
(* We are generally going to use the string form for ease of programming. *)
  
let apr (e1,e2) = Appl(Appl(parse "(Function lft -> Function rgt -> Function x -> x lft rgt)",e1),e2);;

let aleft e = Appl(e,parse "(Function x -> Function y -> x)");;
let aright e = Appl(e,parse "(Function x -> Function y -> y)");;

let p = apr(Int 4, Int 5);;

let expr = aleft p;;

let p = apr(Int 4, Int 5);;

(* Lists.  See the book section 2.3.4 for a discussion of why this works. *)

(* First lets just make lists as pairs of (head,tail), which has a bug *)
  
let cons (e1, e2) = pr(e1,e2);;
let emptylist = pr("0","0");;
let head e = left e;;
let tail e = right e;;

let eglist = cons("0",cons("4",cons("2",emptylist)));;
let eghd = head eglist;;

rep eghd;;
    
let egtl = tail eglist;;

rep egtl;;

let eghdtl = head (tail eglist);;

rep eghdtl;;
    
(* All good so far, but can't test for empty list!  Think about it. *)

(* Solution: do the OCaml variant thing, tag each element with a flag of emptylist or not *)  
  
let cons (e1, e2) = pr(pr("False",e1),e2);; (* tag False means its not emptylist *)
let emptylist = pr(pr("True","0"),"0");; (* tag True --> empty list!  0's are junk filler *)
let head e = right (left e);; (* need to remove tag on elt via right *)
let tail e = right e;;
let isempty e = (left (left e));; (* Pull out the tag: True -> empty list, False -> not *)
                  
let eglist = cons("0",cons("4",cons("2",emptylist)));;
let eghd = head eglist;;

rep eghd;;
    
let egtl = tail eglist;;

rep egtl;;

let eghdtl = head (tail eglist);;

rep eghdtl;;

  (* Now for a real program: length of a list *)
  
let length =
 "(Let Rec len l = 
      If "^isempty "l"^"
      Then 0
      Else
          1 + len ("^tail "l"^")  
   In len)";;

let eglength = length^eglist;;

rep eglength;;

(* Freeze and thaw macros *)
  
let freeze e = "(Fun x -> "^e^")";; (* glitch here -- x can't be free in e *)
let freeze e = "(Fun x_uniq_9282733 -> "^e^")";; (* somewhat better; best would be to inspect e *)
let thaw e = "("^e^" 0)";;

(* Using Freeze and Thaw *)

let lazy_num = freeze "5+2+10922";;
let lazy_double = "(Fun ln -> "^thaw "ln"^" + "^thaw "ln"^")";;
let using_lazy = lazy_double^lazy_num;;

rep using_lazy;;
    
(* The Let macro: its just a function call *)

let fblet (x,e1,e2) = "(Fun "^x^" -> "^e2^")("^e1^")";;
let let_ex = fblet("z","2+3","z + z");; (* Let z = 2 + 3 In z + z *)

rep let_ex;;
  
(* ******************************************************************************** *)
(* ****************************** Y Combinator ************************************ *)
(* ******************************************************************************** *)

(* Recursion via direct self-passing -- "Another route to Y via direct self-passing" in book *)

(* First lets follow Python and pass self/this to the function *)

let summate0 = "(Fun this -> Fun arg ->
    If arg = 0 Then 0 Else arg + this this (arg - 1))";;

let summate0test = (summate0 ^ summate0 ^ "68");;

rep summate0test;;  (* Woo!  Recursion! *)

(* One thing we want to fix here though:
      like Python lets not require passing this explicitly 
      - either inside for recursion (this this) or outside (summate0 summate0) *)

(* Step one: lets write the function like we want to: *)

let summate_bod =
  "(Fun this -> Fun arg ->
      If arg = 0 Then 0 Else arg + this (arg - 1))";;

(* Step two: make a fancy *combinator* to turn summate_bod into summate0-like thing *)  

let this_to_thisthis =
  "(Fun bod -> 
   Fun this -> Fun arg -> bod (this this) arg)";;

let summate_bod_to_summate0 = "("^this_to_thisthis ^ summate_bod^")";;

(* Now lets test to verify this refactoring worked. *)  

let summate_bod_test = (summate_bod_to_summate0 ^ summate_bod_to_summate0 ^ "93");;

rep summate_bod_test;;

(* Yes, it worked!  Now lets compose these two steps to build recursive function in one go. *)

(* First (incorrect) pass: use Let syntax which is not in Fb *)
  
let wrong_combY =
  "(Function body -> 
      Let this_to_thisthis = (Function this -> Function arg -> body (this this) arg)
      In this_to_thisthis this_to_thisthis
   ";;

(* Above has parse error, no Let in Fb grammar -- need to use our let macro *)

let combY =  "(Function body -> 
      "^fblet("this_to_thisthis","(Function this -> Function arg -> body (this this) arg)","this_to_thisthis this_to_thisthis")^")" 

(* Here is the let macro expanded FYI *)

let combY = "(Function body -> 
    (Function this_to_thisthis -> this_to_thisthis this_to_thisthis)
      (Function this -> Function arg -> body (this this) arg))";;

let summate = combY^"(Function this -> Function arg ->
    If arg = 0 Then 0 Else arg + this (arg - 1))";;

rep (summate ^ "8");;

(* Recursion via self passing -- "Encoding Recursion by Passing Self" in the book *)
(* This is another way Y can be built, it may help you understand it better *)
  
(* First, the paradox *)
  
let paradox = "(Function x -> Not(x x))(Function x -> Not(x x))" ;;

(* Next, freeze the "x x" so it doesn't chain forever 
   and repace Not with a macro parameter -- something we can plug in *)
  
let makeFroFs cF = "(Function x -> ("^cF^")(Function _ -> x x)) (Function x -> ("^cF^")(Function _ -> x x))";;

(* Observe cF is getting a parameter which is a frozen version of the cF generator *)  
  
(* A concrete functional we can plug in to do recursion *)                                                                                       
let fF = "(Function froFs -> Function n ->
If n = 0 Then 0 Else n + froFs 33 (n - 1))";;

let fCall = "("^(makeFroFs fF)^") 5";;

rep fCall;;  (* look ma, no Let Rec! *)

(* The hard stuff is done, now we just clean things up *)
  
(* replace dummy parameter _ with actual argument: pun *)
   
let makeFs cF = "(Function x -> ("^cF^")(Function n -> (x x) n)) (Function x -> ("^cF^")(Function n -> (x x) n))"

let fF' = "(Function fs -> Function n ->
If n = 0 Then 0 Else n + fs (n - 1))";;

let fCall' = "("^(makeFs fF')^") 5";;

(* replace macro with a function call - embed the macro in PL *)  
  
let yY = "(Function cF -> (Function x -> cF (Function n -> (x x) n)) (Function x -> cF (Function n -> (x x) n)))"

let yEg = yY^fF';;

let fCall'' = "("^yEg^") 5";;

rep fCall'';;
