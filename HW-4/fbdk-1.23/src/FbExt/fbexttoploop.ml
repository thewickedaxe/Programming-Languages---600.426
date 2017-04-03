(* 
   File: fbexttoploop.ml

   Before this file can be #use-d you need to execute
     make byte
   from the top directory of the fbdk-1.1 distribution.

   OR, if you are using OCaIDE in Eclipse, just edit this file in Eclipse, select all, 
        and from the Ocaml menu, do Eval in Toplevel.

*)


(* First load all the relevant compiled structs.  These commands assume ocaml
   was launched from the .../src/Fb/ directory.  If not, do a 
   # #cd ".../src/Fb";; to get there.  *)

(* #cd "/Users/scott/pl/ocaml/code/FbDK/src/FbExt";; *) (* edit to be your path *)
#load "fbextast.cmo";;
#load "fbextparser.cmo";;
#load "fbextlexer.cmo";;
#load "fbextpp.cmo";;
#load "fbextinterp.cmo";;
(* for typechecker: #load "fbexttype.cmo";; *)

(* Make some structs available at the top for easier use *)

open Fbextast;;
open Fbextinterp;;
(* for typechecker: open Fbexttype;; *)

(* parse parses FbextST concrete syntax you enter as a string *)

let parse s = 
    let lexbuf = Lexing.from_string (s^";;") in
  	Fbextparser.main Fbextlexer.token lexbuf;;

(* unparse is the reverse of parsing: expr to string *)
  
let unparse e = Fbextpp.pretty_print e;;
    
(* pp is a top-loop pretty printer *)

let pp e = print_string (unparse e);;

(* ppeval evaluates an expr and then pretty prints the result *)

let ppeval x = print_string "==> ";pp (eval x);;

(* function rep is a read-eval-print function for Fbextaerse programs: an interpreter *)

let rep s = ppeval (parse s);;

(* res is like rep but just returns the string result - no printing it out. *)  
  
let res s = unparse (eval (parse s));;
  
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

(* For typechecking when we get to that in class:

typecheck ex1;; *)
