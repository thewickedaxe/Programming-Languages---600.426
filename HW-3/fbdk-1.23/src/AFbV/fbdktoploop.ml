(* First load all the relevant compiled structs.  These commands assume ocaml
   was launched from the .../src/Fb/ directory.  If not, do a 
   # #cd ".../src/Fb";; to get there.  *)


#load "afbvast.cmo";;
#load "afbvoptions.cmo";;
#load "afbvparser.cmo";;
#load "afbvlexer.cmo";;
#load "afbvpp.cmo";;
#load "afbvinterp.cmo";;

(* uncomment the following line to turn the debug mode on *)

(* Afbvoptions.set_debug(true);; *)

(* Make some structs available at the top for easier use *)

open Afbvast;;
open Afbvinterp;;


(* function parse parses AFbV concrete syntax you enter as a string *)

let parse s = 
    let lexbuf = Lexing.from_string (s^";;") in
  	Afbvparser.main Afbvlexer.token lexbuf;;

(* Function pp is a top-loop pretty printer using FBDK's pretty printer *)

let pp e = print_string (Afbvpp.pretty_print e);;

(* ppeval evals then pretty prints the result *)

let ppeval x = print_string "==> ";pp (eval x);;

(* function rep is a read-eval-print function for AFbV programs: an interpreter *)

let rep s = ppeval (parse s);;

(* some actor examples *)

let s1 = "
Let ycomb = (Function body -> Let wrapper = Function this -> Function arg -> body (this this) arg In Function arg -> wrapper wrapper arg) In
Let count = ycomb (Function this -> Function data -> Function msg ->
  Match msg With 
    | `count(d) -> 
         Let dir = Fst data In Let c = Snd data In 
         Let newc = If dir Then c+1 Else c-1 In
           (Print newc); this (dir, newc)
    | `up(dx) -> (Print dx); this (dx, Snd data))
In
  Let f = Function myaddr -> Function data -> count (True, 0) 
In
  Let sendn = ycomb (Function this -> Function a -> Function n -> Function msg ->
    If n = 0 Then True Else (a <- msg) ; this a (n-1) msg)   
  In
  Let a = Create(f, 0) In
      sendn a 3 (`count 0) ; ( a <- `up(False) ) ; sendn a 3 (`count 0) 
  ";;
rep s1 ;;


(* Counting down example *)

let s2 = "
Let y = (Function body -> Let wrapper = Function self -> Function msg -> 
          body (self self) msg In wrapper wrapper) In
Let actorBeh = Function me -> y (Function this -> Function data -> Function msg ->
                                    Match msg With
                                        `count(n) ->
                                            (Print n);
                                            (If n = 0 Then 0 Else (me <- (`count (n-1))));
                                            this 0
                               ) In
Let actor = Create(actorBeh, 0) In
	  actor <- `count 4";;

rep s2;;

(* ping pong example *)

let s3 = "
Let y = (Function body -> Let wrapper = Function self -> Function msg -> 
          body (self self) msg In wrapper wrapper) In
Let behPong = Function me -> y (Function this -> Function data -> Function msg ->
                                    Match msg With
                                        `pong(n) ->
                                            (data <- (`ping (n+1))); (* invariant: data is a1 *)
                                            this data (* Use the same behavior for the next message received *)
                               ) In
Let behPing = Function me -> Function dummy -> Function msg0 ->
	 (* First message should be `init; create pong actor and get it going *)
     Match msg0 With
		    `init(n) -> Let a2 = Create(behPong, me) In (* tell ponger about me when its made *)
						(a2 <- `pong(n)); (* send pong an n-ball to start the game *)
							(* Now set behavior for rest of ping/pong game: get a ping, send a pong *)														   
            (y (Function this -> Function msg ->
               Match msg With `ping(n) ->
                 (Print (n));
                 (If n = 0 Then 0 Else (a2 <- (`pong (n-2))));
                 this 
            )) In
Let a1 = Create(behPing, 0) In
	  a1 <- `init(4)";;

rep s3;;
