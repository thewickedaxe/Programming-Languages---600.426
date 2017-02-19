(*

PoPL Assignment 2

Name                  : Srinivas Suresh Kumar
List of Collaborators : Myself and Rohit Ravoori

Please make a good faith effort at listing people you discussed any problems
with here, as per the course academic integrity policy. TA/CA/Prof need not be
listed.

Fill in the function definitions below replacing the

  failwith "Not Implemented"

with your code.  In some cases, you may find it helpful to define auxillary
functions, feel free to.  Other than replacing the failwiths and adding recs,
don't edit or remove anything else in the file -- the autograder will not be
happy! You cannot use any mutation (arrays, :=, or any mutable data structure)
on this homework unless explicitly allowed. You can use core library functions
such as List.map that are not mutating. Note that the Queue, Stack, and Hashtbl
modules are mutating so you are not allowed to use them. *)


(* ****************************** Problem 1 ****************************** *)
(* ************************ The Game of Types **************************** *)

(*

For this problem, you must produce an expression which has a given type.  It
does not matter what expression you provide as long as it has that type; there
may be numerous (or even infinite) answers for each question.  Your answer may
*not* produce a compiler warning.  You are *not* permitted to use explicit type
annotations using ":" (such as "fun x:'a -> x;;").  Also please do not use any
libraries for this question such as List.map etc. You *may* use mutable state to
define x3.

[20 Points]

 *)


let x1 a = a,a;;

 (* val x1 : 'a -> 'a * 'a = ... *)

let x2 a b =
    a
;;

 (* val x2 : 'a -> 'b -> 'a = ... *)

let x3 () =
    let myref = { contents = [1;2;3;4] } in
    let myfun myrefarg =
        myrefarg;
    in
    myfun myref
;;


 (* val x3 : unit -> int list ref = ... *)

let rec x4 func lst =
    match lst with
    | [] -> []
    | hd::tl -> func 5 hd:: x4 func tl
;;

 (* val x4 : (int -> 'a -> 'b) -> 'a list -> 'b list = ... *)

let rec x5 funclst lst =
    match (funclst, lst) with
    | ([], []) -> []
    | ([], _::_) ->  invalid_arg "invalid args"
    | (_::_, []) -> invalid_arg "invalid args"
    | (a::b, c::d) -> a c::x5 b d
;;

 (* val x5 : ('a -> 'b) list -> 'a list -> 'b list = ... *)

let rec x6 lst1 lst2 func =
    match (lst1, lst2) with
    | ([], []) -> []
    | ([], _::_) ->  invalid_arg "invalid args"
    | (_::_, []) -> invalid_arg "invalid args"
    | (hd1::tl1, hd2::tl2) -> func hd1 hd2::x6 tl1 tl2 func
;;

(* val x6 : 'a list -> 'b list -> ('a -> 'b -> 'c) -> 'c = ... *)

let x7 a =
    Obj.magic a
;;

(* val x7 : 'a -> 'b option *)


(* ****************************** Problem 2 ****************************** *)
(* ***************************** Post-fixing ***************************** *)

(*

Write a simple postfix calculator that operates over integers and floats and
supports the mathematical operations +, -, * and /. If you are not familiar
with postfix notation have a look at
e.g. https://en.wikipedia.org/wiki/Reverse_Polish_notation

Numbers are defined as the type "Int of int | Float of float"

The input is expressed as a list of tokens. The numeric and token types are
defined below.

The output should be a number type. When floats and integers occur in the same
expression, the result should be a float type. i.e. Mul(Float 3.2, Int 2)
evaluates to Float(6.4). When both arguments are of the same type, the result
should also be of that type. So, division with two integer arguments should
perform C-style integer division.

If the input is ill-formed, or the computations result in divide by zero error,
raise an exception using invalid_arg. Note that this division by zero error
should occur for diving by both Int(0) and Float(0.0).

HINT: It is worth considering the design of this function carefully - What are
the sub-operations that you want to perform? Can the repetitive code be
abstracted away in to functions?. A few well-written utility functions would
serve to simplify the problem quite a lot.

[20 Points]

*)

type number = Int of int | Float of float ;;

type token = Number of number | Plus | Minus | Mul | Div ;;

let smart_min a b =
    match (a, b) with
    | (Float(c), Float(d)) -> Float(c -. d)
    | (Int(c), Int(d)) -> Int(c-d)
    | (Float(c), Int(d)) -> Float(c -. float_of_int(d))
    | (Int(c), Float(d)) -> Float(float_of_int(c) -. d)
;;

let smart_mul a b =
    match (a, b) with
    | (Float(c), Float(d)) -> Float(c *. d)
    | (Int(c), Int(d)) -> Int(c*d)
    | (Float(c), Int(d)) -> Float(c *. float_of_int(d))
    | (Int(c), Float(d)) -> Float(float_of_int(c) *. d)
;;

let smart_div a b =
    match (a, b) with
    | (Float(c), Float(d)) -> Float(c /. d)
    | (Int(c), Int(d)) -> Int(c/d)
    | (Float(c), Int(d)) -> Float(c /. float_of_int(d))
    | (Int(c), Float(d)) -> Float(float_of_int(c) /. d)
;;

let smart_add a b =
    match (a, b) with
    | (Float(c), Float(d)) -> Float(c +. d)
    | (Int(c), Int(d)) -> Int(c+d)
    | (Float(c), Int(d)) -> Float(c +. float_of_int(d))
    | (Int(c), Float(d)) -> Float(float_of_int(c) +. d)            
;;

let calculate tokenlist =
    let rec act_cal tokenlist operand_stack =
        match tokenlist with
        | [] -> List.hd operand_stack
        | a::b -> match a with
                | Number(n) -> let new_operand_stack = n::operand_stack in  act_cal b new_operand_stack
                | Plus -> let t1 = List.hd operand_stack in 
                          let rest_temp = List.tl operand_stack in
                          let t2 = List.hd rest_temp in
                          let rest = List.tl rest_temp in
                          let ans = smart_add t2 t1 in
                          let new_operand_stack = ans::rest in
                          act_cal b new_operand_stack
                | Minus -> let t1 = List.hd operand_stack in 
                          let rest_temp = List.tl operand_stack in
                          let t2 = List.hd rest_temp in
                          let rest = List.tl rest_temp in
                          let ans = smart_min t2 t1 in
                          let new_operand_stack = ans::rest in
                          act_cal b new_operand_stack
                | Mul -> let t1 = List.hd operand_stack in 
                          let rest_temp = List.tl operand_stack in
                          let t2 = List.hd rest_temp in
                          let rest = List.tl rest_temp in
                          let ans = smart_mul t2 t1 in
                          let new_operand_stack = ans::rest in
                          act_cal b new_operand_stack
                | Div -> let t1 = List.hd operand_stack in 
                          let rest_temp = List.tl operand_stack in
                          let t2 = List.hd rest_temp in
                          let rest = List.tl rest_temp in
                          let ans = smart_div t2 t1 in
                          let new_operand_stack = ans::rest in
                          act_cal b new_operand_stack
    in act_cal tokenlist []
;;

(*
# calculate [ Number (Int 3) ; Number (Int 5) ; Minus ] ;;
- : number = Int (-2)
# calculate [
  Number (Int 2); Number (Int 3); Number (Int 8);
  Mul; Plus; Number (Int 4); Number (Int 48);
  Number (Int 4); Number (Int 2); Plus; Div;
  Number (Int 6); Mul; Plus; Minus
] ;;
- : number = Int (-26)
# calculate [ Number (Int 2) ; Number (Float 3.14) ; Mul ] ;;
- : number = Float (6.28)
# calculate [ Number (Int 1) ; Number (Int 0) ; Div ] ;;
Exception: Invalid_argument "division by zero".
# calculate [ Number (Float 1.0) ; Number (Float 0.0) ; Div ] ;;
Exception: Invalid_argument "division by zero".
# calculate [ Number (Int 42) ; Number (Int 2) ; Minus ; Plus ] ;;
Exception: Invalid_arugment "ill-formed expression".
*)

(* ****************************** Problem 3 ****************************** *)
(* ***************************** State-Full ****************************** *)

(*

For this question you can use the built-in state of OCaml: ref, :=, arrays, and
records with mutable fields. You can also use stateful standard library data
structures if you so choose.

One case where side-effects are particularly useful is in accumulating log
information. In pure functional programming each function would need to pass
and return the log in case it updated it, which is very cumbersome.

For this question you are to implement a very simple log data structure. You can
make a log, add entries to the log, and dump the current contents. You get to
come up with your own type for the log; feel free to add any type declarations
to your file.

 *)

(* 3a. [5 points]

This function should return a new (mutable) log structure. *)

let make_log () = failwith "Not Implemented";;

(* 3b. [5 points]

This function should add a new entry (string s) to the end of a log. It should
just return () since all it is doing is a side-effect.  *)

let add_entry log s = failwith "Not Implemented";;

(* 3c. [5 points]

This function should dump the current log in string format. The format needs to
be exactly as in the example (concatenate the entries in order with one newline
after every entry). Don't use print_string to print the output, just return a
string result.  *)

let dump log = failwith "Not Implemented";;

(*

let lg = make_log () ;;
val lg : ... = ...
add_entry lg "Here is a log entry." ;;
- : unit = ()
add_entry lg "Here is a second log entry." ;;
- : unit = ()
add_entry lg "" ;;
- : unit = ()
add_entry lg "Here is an entry\nwith embedded newlines." ;;
- : unit = ()
add_entry lg "Here is a third log entry." ;;
- : unit = ()
dump lg ;;
- : string = "Here is a log entry.\nHere is a second log entry.\n\nHere is an entry\nwith embedded newlines.\nHere is a third log entry.\n"

*)

(* ****************************** Problem 4 ***************************** *)
(* ************************** Fun with JSON  **************************** *)

(* For this question you will write some basic JSON data manipulation functions.
OCaml in fact comes with a module to do this, but to get some experience with
tree data we are going to start from the ground up here. If you are not familiar
with JSON it is a simple string format for writing structured data which looks a
lot like JavaScript syntax.

 *)

(* Please use the following OCaml type for your JSON data. *)

type json =
  | Assoc of (string * json) list
  | Bool of bool
  | Float of float
  | Int of int
  | List of json list
  | Null
  | String of string
;;

(* 4a. [5 points]

Write a function string_of_json to convert any OCaml json data item into a legal
JSON string. If you want to verify your string output is valid JSON you can use
a tool like http://jsonlint.com.

Note that whitespace between tokens is irrelevant. That means, the following are
all equivalent:

    {"grades":[90,95,88],"undergrad":false}

    {"grades": [90, 95, 88], "undergrad": false}

    {
        "grades": [
            90,
            95,
            88
        ],
        "undergrad": false
    }

We will be ignoring this whitespace when we test this function. Feel free to
include any whitespace in the output you need to make debugging easier.

Note that JSON has some rules for escape sequences in strings (that is, ways of
expressing control characters, the quote character itself, and other characters
which you may not want to verbatim include in your JSON). For example, "\n" in
a string is JSON that contains a newline character. Another way of representing
this is "\u000A". You do not need to worry about this (unless you want the
extra challenge!). We will only be testing you on strings that contain ASCII
characters excluding control characters (newline and friends), the blackslash,
or double quotes.
*)


let string_of_json jsn = failwith "Not Implemented";;

(*

# string_of_json(Assoc[]);;
- : string "{}"

# let person = Assoc([
    ("id", Int(42));
    ("attributes", Assoc([
        ("name", String("John"));
        ("age", Int(80));
        ("gender", String("male"))
    ]));
    ("favorite-colors", List([
        String("red");
        String("blue")
    ]));
    ("rating",Float(4.5))
]) ;;
val person : json = ...

# string_of_json person ;;
- : string = "{\"id\":42,\"attributes\":{\"name\":\"John\",\"age\":80,\"gender\":\"male\"},\"favorite-colors\":[\"red\",\"blue\"],\"rating\":4.5}"

*)

(* 4b. [10 points]

Write a function to look up a particular field's value in a OCaml json object.
The function should return the OCaml json object that is the value. This
function only needs to work on json data that at the top level is an Assoc.
Invoke invalid_arg in other cases. Additionally, invoke invalid_arg if key
s isn't one of the keys in the top level Assoc. *)

let lookup jsn s = failwith "Not Implemented";;

(*

# let person = Assoc([
    ("id", Int(42));
    ("attributes", Assoc([
        ("name", String("John"));
        ("age", Int(80));
        ("gender", String("male"))
    ]));
    ("favorite-colors", List([
        String("red");
        String("blue")
    ]));
    ("rating", Float(4.5))
]) ;;
val person : json = ...

# lookup person "id" ;;
- : json = Int(42)

# lookup person "attributes" ;;
- : json = Assoc([("name", String("John"));("age", Int(80));("gender", String("male"))])

# lookup person "name" ;;
Exception: Invalid_argument "key not found".

# lookup (String "Lambda") "id" ;;
Exception: Invalid_argument "Assoc not top level".

*)


(* 4c. [10 points]

Write a function to deeply look up a particular field's value in a OCaml json
object. This will look for the named field arbitrarily deeply inside of any
Assoc, e.g. Assocs within Assocs and Assocs within Lists.

This function only needs to work on JSON data that at the top level is an
Assoc or a List. Invoke invalid_arg in other cases. Additionally, invoke
invalid_arg if key s isn't one of the keys in any of the Assoc's in jsn.
You should traverse Assoc's and List's in order, returning the first value
that matches the key being search for.
*)


let deep_lookup jsn s = failwith "Not Implemented";;

(*

# let person = Assoc([
    ("id", Int(42));
    ("attributes", Assoc([
        ("name", String("John"));
        ("age", Int(80));
        ("gender", String("male"))
    ]));
    ("school", Assoc([
        ("name", String("Johns Hopkins"));
        ("city", String("Baltimore"))
    ]));
    ("favorite-colors", List([
        Assoc([
            ("name", String("red"));
            ("rgb", List([Int(255); Int(0); Int(0)]))
        ]);
        Assoc([
            ("name", String("blue"));
            ("rgb", List([Int(0); Int(0); Int(255)]))
        ])
    ]));
    ("rating", Float(4.5))
]) ;;
val person : json = ...

# deep_lookup person "id" ;;
- : json = Int(42)

# deep_lookup person "name" ;;
- : json = String("John")

# deep_lookup person "rgb" ;;
- : json = List([Int(255); Int(0); Int(0)])

# deep_lookup person "state" ;;
Exception: Invalid_argument "key not found".

# deep_lookup (String "Lambda") "id" ;;
Exception: Invalid_argument "Assoc or List not top level".

*)


(* 4d. [10 points]

Write a function json_filter to deeply filter out (remove) any Assoc fields
whose names match a filter predicate supplied by the user, filter : string ->
bool. The json object should be idential other than the indicated fields being
removed.

As with deep_lookup, json_filter should traverse into both Assoc's and List's.
Calling the function with a json value that isn't either should return the
value unchanged.
*)

let json_filter jsn filter = failwith "Not Implemented";;

(*

# let person = Assoc([
    ("id", Int(42));
    ("attributes", Assoc([
        ("name", String("John"));
        ("age", Int(80));
        ("gender", String("male"))
    ]));
    ("school", Assoc([
        ("name", String("Johns Hopkins"));
        ("city", String("Baltimore"))
    ]));
    ("favorite-colors", List([
        Assoc([
            ("name", String("red"));
            ("rgb", List([Int(255); Int(0); Int(0)]))
        ]);
        Assoc([
            ("name", String("blue"));
            ("rgb", List([Int(0); Int(0); Int(255)]))
        ])
    ]));
    ("rating", Float(4.5))
]) ;;
val person : json = ...

# json_filter person (fun x -> x = "name") ;;
- : json = Assoc([
    ("id", Int(42));
    ("attributes", Assoc([
        ("age", Int(80));
        ("gender", String("male"))
    ]));
    ("school", Assoc([
        ("city", String("Baltimore"))
    ]));
    ("favorite-colors", List([
        Assoc([
            ("rgb", List([Int(255); Int(0); Int(0)]))
        ]);
        Assoc([
            ("rgb", List([Int(0); Int(0); Int(255)]))
        ])
    ]));
    ("rating", Float(4.5))
])

# json_filter (Int 10) (fun x -> true) ;;
- : json = Int(10)

*)


(* 4e. [10 points]

Port your JSON library above to a module: make a file json.ml which is a
standalone module implementation of the json type, and the four functions
defined above (just copy your final code into that file, you are submitting two
versions of the same question answer).

We will test it by #load-ing your compiled module into the top loop and invoking
it using OCaml module syntax:

#load "json.cmo";;
Json.string_of_json ...

You probably want to test your module that way as well just to be sure it is working correctly.

*)