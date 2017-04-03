(*

PoPL Assignment 4B Question 2
 
Your Name:  Srinivas Suresh Kumar
Collaborators: Rohit Ravoori

 *)

(* 2a. *)

(* See the HW for the details - fill in your FbVPL point class code below. *)

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

let pointClass = 
"
(Function x -> Function y -> Function z ->

    `Point(
            (
                Function a -> Function b -> If ((a =0) And (b = 0)) Then True Else False), 
                (
                    (
                        Function pt -> Match pt With
                                       | `Point(cntnts) -> 
                                       
                                       (
                                           Function a -> Function b ->
                                           ((
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
)) (a) (a)

                                           +

                                           ((
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
)) (b) (b)
                                       )
                                       
                                       
                                       (Fst(Snd(Snd(cntnts)))) (Snd(Snd(Snd(cntnts))))
                    ), 
                    (
                        Fst(x), Snd(x)
                    )
                )
            )

)";;

(* Here is an example of exactly how your code should run - don't edit the below.
   Notice how we are also using the pairs of FbVPL to pass in the initial point coords. *)
  
let pointExample1 = "Let p = ("^pointClass^")(2,3) In p p (`magnitude(0))";; (* should return 13 *)
let pointExample2 = "Let p = ("^pointClass^")(2,3) In p p (`iszero(0))";; (* should return False *)


  
(* 2b. *)

let send (ob,msg) = "0 1"  (* replace with macro *)

(* Here is a test that should work exactly as written below with no editing *)
                      
let messengerTest = "Let p = ("^pointClass^")(1,2) In ("^send("p","`iszero(0)")^")";; (* False *)
  
  
