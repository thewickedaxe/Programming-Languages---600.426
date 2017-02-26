(* 
   File: fbdktoploop.ml
   Author: Scott Smith

   This file allows interactive testing of your interpreter using the FbDK.

   Before this file can be #use-d you need to execute

     make byte

   from the src/ directory of the FbDK-1.1 distribution.

*)


(* First load all the relevant compiled structs.  These commands assume ocaml
   was launched from src/FbSR/ directory.  If not, do a 
   # #cd "my/directory/src/FbSR";; to get there.  *)

#load "fbsrast.cmo";;
#load "fbsrparser.cmo";;
#load "fbsrlexer.cmo";;
#load "fbsrpp.cmo";;
#load "fbsrinterp.cmo";;

(* Make some structs available at the top for easier use *)

open Fbsrast;;
open Fbsrinterp;;


(* function parse parses FbSR concrete syntax you enter as a string *)

let parse s = 
    let lexbuf = Lexing.from_string (s^";;") in
  	Fbsrparser.main Fbsrlexer.token lexbuf;;

(* Function pp is a top-loop pretty printer using FbDK's pretty printer *)

let pp e = print_string (Fbsrpp.pretty_print e);;

(* ppeval evals then pretty prints the result *)

let ppeval x = print_string "==> ";pp (eval x);;

(* function rep is a read-eval-print function for FbSR programs *)

let rep s = ppeval (parse s);;

(* Examples. *)

let s1 = 
  "Let Rec x1 x2 = 
     If x2 = 1 Then
        (Function x3 -> x3 (x2 - 1)) (Function x4 -> x4)
     Else
        x1 (x2 - 1)
   In x1 100";;

let ex1 = parse s1;;

let result1 = eval ex1;; 

pp result1;;

ppeval ex1;;

rep s1;; 
