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


let rec complicated_type_matcher jon =
    match jon with
    | Assoc stuff -> "{"^(list_tuple_extractor stuff)
    | Int z -> ""
    | Float z -> ""
    | String z -> ""
    | Null  -> ""
    | List z -> ""
    | Bool z -> ""
and simple_type_matcher key value =
    match value with
    | Int z -> "\""^key^"\""^":"^string_of_int(z)
    | Float z -> "\""^key^"\""^":"^string_of_float(z)
    | String z -> "\""^key^"\""^":"^"\""^z^"\""
    | Null  -> "\""^key^"\""
    | List z -> "\""^key^"\""^":"^"["^iterative_concat z^"]"
    | Bool z -> "\""^key^"\""^":"^string_of_bool(z)
    | Assoc z -> "\""^key^"\""^":"^"{"^(list_tuple_extractor z)^"}"
and list_tuple_extractor daval =
    match daval with
    | [] -> ""
    | a::b -> match a with
             | (c, d) -> match b with
                         | [] -> simple_type_matcher c d^list_tuple_extractor b
                         | p::q -> simple_type_matcher c d^","^list_tuple_extractor b

and iterative_concat str_list =
    match str_list with
    | [] -> ""
    | a::b -> if b = [] then simp_print a else match a with
                                  | Int z -> string_of_int(z)^","^iterative_concat b
                                  | Float z -> string_of_float(z)^","^iterative_concat b
                                  | String z -> "\""^z^"\""^","^iterative_concat b
                                  | List z -> "["^iterative_concat z^","^iterative_concat b^"]"
                                  | Bool z -> string_of_bool(z)^","^iterative_concat b
                                  | Null -> ""
                                  | Assoc z -> complicated_type_matcher a^"}"^","^iterative_concat b
and simp_print foo =
                    match foo with
                    | Int z -> string_of_int(z)
                    | Float z -> string_of_float(z)
                    | String z -> "\""^z^"\""
                    | List z -> "]"
                    | Bool z -> string_of_bool(z)
                    | Null -> ""
                    | Assoc z -> complicated_type_matcher foo^"}"
;;

let string_of_json jsn =
    complicated_type_matcher jsn^"}"
;;

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

let rec percolate top_level s =
	match top_level with
	| []-> invalid_arg "key not found"
	| a::b -> sift s a b
and
	sift key s b=
	match s with
	| (k,l) -> if (k=key) then l else percolate b key;;

let lookup jsn s=
	match jsn with
	| Assoc a -> percolate a s
    | _ -> invalid_arg "Assoc not top level"
;;

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

let rec complicated_finder jon search_key =
    match jon with
    | Assoc stuff -> list_tuple_searcher stuff search_key
    | _ -> [Assoc([("importance", Int 0)])]
and simple_finder key value search_key=
    match value with
    | List z -> iterative_search z search_key
    | Assoc z -> list_tuple_searcher z search_key
    | _ -> [Assoc([("importance", Int 0)])]
and list_tuple_searcher daval search_key =
    match daval with
    | [] -> [Assoc([("importance", Int 0)])]
    | a::b -> match a with
             | (c, d) -> if c = search_key then [d] else
                         match b with
                         | [] -> simple_finder c d search_key @list_tuple_searcher b search_key
                         | p::q -> simple_finder c d search_key@list_tuple_searcher b search_key

and iterative_search str_list search_key=
    match str_list with
    | [] -> [Assoc([("importance", Int 0)])]
    | a::b -> if b = [] then simp_print a search_key else match a with
                                  | Assoc z -> complicated_finder a search_key@iterative_search b search_key
                                  | _ -> [Assoc([("importance", Int 0)])]
and simp_print foo search_key=
                    match foo with
                    | Assoc z -> complicated_finder foo search_key
                    | _ -> [Assoc([("importance", Int 0)])]
;;

let sanity_check jon skey =
    match jon with
    | Assoc stuff -> list_tuple_searcher stuff skey
    | List z -> complicated_finder jon skey
    | _ -> invalid_arg "Assoc or List not top level"


let rec json_list_result_lookup lst =
    match lst with
    | [] -> invalid_arg "key not found"
    | hd::tl -> match hd with
                | Assoc z -> json_list_result_lookup tl
                | _ -> hd

let deep_lookup jsn key =
    let qualifying_json = sanity_check jsn key in
        json_list_result_lookup qualifying_json
;;

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
let rec complicated_filter lst filter =
    match lst with
    | [] -> []
    | hd::tl -> match hd with
                | (p1, p2) -> if (filter p1) then
                                complicated_filter tl filter
                              else
                                match p2 with
                                | Assoc inner -> (p1, Assoc(complicated_filter inner filter))::complicated_filter tl filter
                                | List inner_list -> (p1, List(handle_lists inner_list filter))::complicated_filter tl filter
                                | _ -> hd::complicated_filter tl filter
and handle_lists lst filter=
    match lst with
    | [] -> lst
    | a::b -> match a with
              | Assoc list_inner -> Assoc(complicated_filter list_inner filter)::handle_lists b filter
              | _ -> lst

let sanity_filter jon filter =
    match jon with
    | Assoc stuff -> Assoc(complicated_filter stuff filter)
    | a -> a
;;

let json_filter jsn filter =
    sanity_filter jsn filter
;;