(* These are the Fb examples from the book.  Requires fbdktoploop.ml *)

let ex2dot6 = parse "If Not(1 = 2) Then 3 Else 4";;

let ex2dot6 = If(Not(Equal(Int 1, Int 2)), Int 3, Int 4);; (* parsed form of above *)

let ex2dot7 = parse "(Function x -> x + 1) 5" ;;
let ex2dot7 = Appl(Function(Ident "x", Plus(Var(Ident "x"), Int 1)), Int 5) ;;

let ex2dot8 = parse "(Function x -> Function y -> x + y) 4 5" ;;
let ex2dot8 = Appl(Appl(Function(Ident "x", Function(Ident "y", 
  Plus(Var(Ident "x"), Var(Ident "y")))), Int 4), Int 5);;

let ex2dot9 = parse "Let Rec fib x =
    If x = 1 Or x = 2 Then 1 Else fib (x - 1) + fib (x - 2)
    In fib 6" ;;
let ex2dot9 = Letrec(Ident "fib", Ident "x", 
  If(Or(Equal(Var(Ident "x"), Int 1), 
        Equal(Var(Ident "x"), Int 2)), 
    Int 1, 
    Plus(Appl(Var(Ident "fib"), Minus(Var(Ident "x"), Int 1)), 
         Appl(Var(Ident "fib"), Minus(Var(Ident "x"), Int 2)))),
  Appl(Var(Ident "fib"), Int 6));;

let ex2dot3dot2 = parse "(Function x -> x + 2)(3 + 2 + 5)" ;;

let ex2dot3dot2b = parse "(Function x -> Function x -> x) 3" ;;

let ex2dot11 = parse "Function x -> Function y -> x + y + z";;

let ex2dot3dot2c = parse 
 "Let Rec x1 x2 = 
     If x2 = 1 Then
          (Function x3 -> x3 (x2 - 1)) (Function x4 -> x4)
     Else
          x1 (x2 - 1)
  In x1 100";;


let ex2dot3dot3 = parse "If 3 = 4 Then 5 Else 4 + 2" ;;
   
let ex2dot3dot3b = parse "(Function x -> If 3 = x Then 5 Else x + 2) 4 " ;;

let ex2dot3dot3c = parse "(Function x -> x x)(Function y -> y) " ;;

let ex2dot3dot3d = parse "(Function f -> Function x -> f(f(x)))
           (Function x -> x - 1) 4" ;;

let ex2dot3dot3e = parse "(Function x -> Function y -> x + y)
    ((Function x -> If 3 = x Then 5 Else x + 2) 4)
    ((Function f -> Function x -> f (f x))
            (Function x -> x - 1) 4 )" ;;

let ex2dot3dot3f = parse "Let Rec f x = 
    If x = 1 Then 1 Else x + f (x - 1)
In f 3" ;;


let ex2dot3dot3g = parse "Let Rec f x = 
    If x = 1 Then 1 Else x + f (x - 1)
  In f" ;;


let lemma2dot4 = parse "(Function x -> x x)(Function x -> x x)" ;;

let combI = parse "Function x -> x";;
let combK = parse "Function x -> Function y -> x";;
let combS = parse "Function x -> Function y -> Function z -> (x z) (y z)";;
let combD = parse "Function x -> x x";;


(* Pair macros defined as OCaml functions over ASTs *)

let pr (e1,e2) = Appl(Appl(parse "(Function lft -> Function rgt -> Function x -> x lft rgt)",e1),e2);;

let left e = Appl(e,parse "(Function x -> Function y -> x)");;
let right e = Appl(e,parse "(Function x -> Function y -> y)");;

let p = pr(Int 4, Int 5);;

let ex2dot3dot4 = left p;;

let p = pr(Int 4, Int 5);;

(* Pair macros over strings *)

let pair c1 c2  = "((Function lft -> Function rgt -> Function x -> x lft rgt) ("^c1^") ("^c2^"))";;

let leftc c =  "("^c^") (Function x -> Function y -> x)";;
let rightc c =  "("^c^") (Function x -> Function y -> y)";;

let pc = pair "34" "45";;  

let prceg = leftc pc;;
  
(* Lists (using AST macros) *)
let cons (e1, e2) = pr(pr(Bool false, e1), e2)
let emptylist = pr(pr(Bool true, Int 0),Int 0)
let head e = right (left e)
let tail e = right e
let isempty e = (left (left e))
let length = LetRec (Ident "len", Ident "x",
   If (isempty (Var (Ident "x")), Int 0,
    Plus (Int 1,
     Appl (Var (Ident "len"), tail(Var (Ident "x"))))),
   Var (Ident "len"))
(* length pretty prints as 
Let Rec len x = 
 If ((x) (Function x -> Function y -> x)) 
    (Function x -> Function y -> x) Then  0
 Else
  1 + (len) ((x) (Function x -> Function y -> y))
In
 len *)

let eglist = cons(Int 0,cons(Int 4,cons(Int 2,emptylist)));;
let eghd = head eglist;;
let egtl = tail eglist;;
let eghdtl = head (tail eglist);;
let eglength = Appl(length,eglist);;

(* Freeze and thaw macros *)
let freeze e = Function(Ident"x", e);;
let thaw e = Appl(e,Int 0);;

(* Recursion via self passing (using string macros) *)

(* First, the paradox *)
  
let paradox = "(Function x -> Not(x x))(Function x -> Not(x x))" ;;

(* Next, freeze the "x x" so it doesn't chain forever 
   and repace Not with a macro parameter -- something we can plug in *)
  
let makeFroFs cF = "(Function x -> ("^cF^")(Function _ -> x x)) (Function x -> ("^cF^")(Function _ -> x x))";;

(* Observe cF is getting a parameter which is a frozen version of the cF generator *)  
  
(* A concrete functional we can plug in to do recursion *)                                                                                       
let fF = "Function froFs -> Function n ->
If n = 0 Then 0 Else n + froFs 33 (n - 1)";;

let fCall = "("^(makeFroFs fF)^") 5";; (* look ma, no Let Rec! *)

(* The hard stuff is done, now we just clean things up *)
  
(* replace dummy parameter _ with actual argument: pun *)
   
let makeFs cF = "(Function x -> ("^cF^")(Function n -> (x x) n)) (Function x -> ("^cF^")(Function n -> (x x) n))"

let fF' = "Function fs -> Function n ->
If n = 0 Then 0 Else n + fs (n - 1)";;

let fCall' = "("^(makeFs fF')^") 5";;

(* replace macro with a function call - embed the macro in PL *)  
  
let yY = "Function cF -> (Function x -> cF (Function n -> (x x) n)) (Function x -> cF (Function n -> (x x) n))"

let yEg = "("^yY^")("^fF'^")";;

let fCall'' = "("^yEg^") 5";;
  
(* Recursion via direct self-passing  (using AST macros) *)
  
let summate0 = parse "Function this -> Function arg ->
    If arg = 0 Then 0 Else arg + this this (arg - 1)";;

let summate0test = Appl(Appl(summate0, summate0), Int 7);;

(* Let definition is expanded in the below compared to book version *)
let summate =
  parse "(Function summ -> Function arg -> summ summ arg)
          (Function this -> Function arg ->
           If arg = 0 Then 0 Else arg + this this (arg - 1))";;

let summatetest = Appl(summate, Int 7);;

let almosty = parse "Function body -> body body";;


let summate = Appl(almosty, parse "(Function this -> Function arg ->
    If arg = 0 Then 0 Else arg + this this (arg - 1))");;

let summatetest = Appl(summate, Int 7);;


let combY = parse "Function body -> 
    (Function fun -> fun fun)
      (Function this -> Function arg -> body (this this) arg)";;

let summate = Appl(combY, parse "Function this -> Function arg ->
    If arg = 0 Then 0 Else arg + this (arg - 1)");;

let summatetest = Appl(summate, Int 7);;
