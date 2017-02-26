{
  open Boolparser;;
} 

let blank = [' ' '\t' '\n' '\r']

rule token = parse
| blank+               { token lexbuf }
| "And"                { AND }
| "Or"                 { OR }
| "Not"                { NOT }
| "Implies"            { IMPLIES }
| "False"              { FALSE }
| "True"               { TRUE }
| "("                  { LPAREN }
| ")"                  { RPAREN }
| ";;"                 { EOEX }
| eof                  { raise Exit }

{} 



