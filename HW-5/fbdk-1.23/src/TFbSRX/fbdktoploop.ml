(* First load all the relevant compiled structs.  These commands assume ocaml
   was launched from src/TFbSRX/ directory.  If not, do a 
   # #cd "my/directory/src/TFbSRX";; to get there.  *)
  
#load "tfbsrxast.cmo";;
#load "tfbsrxparser.cmo";;
#load "tfbsrxlexer.cmo";;
#load "tfbsrxpp.cmo";;
#load "tfbsrxinterp.cmo";;
#load "tfbsrxtype.cmo";;

(* Make some structs available at the top for easier use *)

open Tfbsrxast;;
open Tfbsrxinterp;;
open Tfbsrxpp ;;

(* function parse parses FbSR concrete syntax you enter as a string *)

let parse s = 
    let lexbuf = Lexing.from_string (s^";;") in
  	Tfbsrxparser.main Tfbsrxlexer.token lexbuf;;

(* Function pp is a top-loop pretty printer using FbDK's pretty printer *)

let pp e = print_string (Tfbsrxpp.pretty_print e);;

(* ppeval evals then pretty prints the result *)

let ppeval x = print_string "==> ";pp (eval x);;

(* function rep is a read-eval-print function for FbSR programs *)

let rep s = ppeval (parse s);;

(* Do type checking *)
let tc e = Tfbsrxtype.typecheck e ;;

(* Function to parse text and perform type checking *)
let ptc s = tc (parse s) ;; 

(* Function to parse, perform type checking and pretty print a type *)
let ppptc s = print_string ( Tfbsrxpp.pretty_print_type (ptc s) );;

(* Examples. *)

(* Int *)
let s1 = "(Function x:Int -> x + 1) 3" ;;
ppptc s1 ;; 
rep s1 ;;

(* Bool -> Int -> Int *)
let s2 = "Function switch:Bool -> Function x:Int -> If switch Then x Else x - 1" ;;
ppptc s2 ;;
rep s2 ;;

(* Int -> Int *)
let s3 = "Let Rec f x:Int = (If x = 0 Then 0 Else x + f (x - 1)):Int In f";;
ppptc s3 ;;
rep s3 ;;
eval (Appl(parse s3, Int 4)) ;;

(* Int *)
let s4 = "(Function c:Int Ref -> c := (!c + 1)) (Ref 3)" ;;
ppptc s4 ;;
rep s4 ;;

(* Bool *)
let s5 = "{x = 2+1 ; y = (Function x:Int -> x + 1) 3 ; z = False}.z" ;;
ppptc s5 ;;
rep s5 ;;

(* {a:Int, b:Bool} *)
let s6 = "(Function r:{x:Int;y:Int} -> {a = r.x + r.y ; b = False} ) {y = If 0 = 2 Then 1 Else 2 ; x = 1-2}" ;;
ppptc s6 ;;
rep s6 ;;

(* Int *)
let s7 = "Try Raise (#Ex@Bool) False With #Ex@Int x -> x + 1" ;;
ppptc s7;;

(* Int *)
let s8 = "Try Raise #Ex@Int 9 With #Ex@Int x -> x + 1" ;;
ppptc s8;;

(* This one should fail to typecheck *)
let s9 = "1 And False" ;; 
ppptc s9 ;;

(* Int *)
let s10 = "(Function r:{x:Int} -> r.x) ((Function x:Int -> Raise #Ex@Int 0) 0)" ;;
ppptc s10 ;;

(* Int *)
let s11 = "Try (Try Raise (#Ex@Bool) False With #Ex@Int x -> x + 1) With #Ex@Bool b -> (If b Then 1 Else 0)" ;;
ppptc s11;;

(* This one should fail to typecheck *)
let s12 = "Try (Try Raise (#Ex@Bool) False With #Ex@Int x -> x + 1) With #Ex@Bool b -> b" ;;
ppptc s12;;

(* Int -> Bool -> Bool *)
let s13 = "Function x:Int -> Function x:Bool -> Not x" ;;
ppptc s13 ;;

(* Int -> Int *)
let s14 = "Function x:Int -> x + (( Function x:Bool -> If x Then 0 Else 1 ) True)" ;;
ppptc s14 ;;

(* Fails : Expression not closed *)
let s15 = "Function x:Int -> Function y:Int -> x + y + z" ;;
ppptc s15 ;;

(* This one should fail to typecheck *)
let s16 = "Function x:Int -> Function y:Bool -> x + y" ;;
ppptc s16 ;;

(* Int -> Int *)
let s17 = "If 1 = 2 Then (Function x:Int -> x) Else (Function y:Int -> y + 1)" ;;
ppptc s17 ;;

(* This one should fail to typecheck *)
let s18 = "If 1 = 2 Then (Function x:Int -> x) Else (Function y:Int -> y = 1)" ;;
ppptc s18 ;; 

(* Int *)
let s19 = "If 1 = 1 Then Raise #Exn@Int 1 Else 2" ;; 
ppptc s19 ;; 

(* Int *)
let s20 = "(Function x:Int -> x + 1) (Raise #Exn@Int 1)" ;;
ppptc s20 ;; 

(* Int -> BOTTOM *)
let s21 = "Function x:Int -> Raise #Exn@Bool False" ;;
ppptc s21 ;;

(* Int *)
let s22 = "(Function r:{x:Int;y:Int} -> r.x) {x = 1 ; y = (Raise #Foo@Int 3)}" ;;
ppptc s22 ;;

(* Bool *)
let s23 = "(Function a:{r:{m:Int;n:Bool};s:{l:Bool}} -> a.r.n And a.s.l) {s={l=True};r={m=2;n=(1=1)}}" ;;
ppptc s23 ;;
rep s23 ;;

(* Int *)
let s24 = "(Function x:Int Ref -> x := !x) ((Function x:Int -> Ref (Raise #Xn@Int x)) (1+1))" ;;
ppptc s24;;
rep s24 ;;
