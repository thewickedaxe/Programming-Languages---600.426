{
  open Afbvparser;;
  
  exception StringSyntaxError of string
} 

let blank = [' ' '\t' '\n' '\r']
let decimal_literal = ['0'-'9']+
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

rule token = parse
  ['(']['*']([^'*']|['*'][^')'])*['*'][')'] 
    {token lexbuf} (* Ignore comments *)
| blank+               { token lexbuf }
| '"'                  { STRING (read_string lexbuf) }
| "And"                { AND }
| "Or"                 { OR }
| "Not"                { NOT }
| "Fun"                { FUNCTION }
| "Function"           { FUNCTION }
| "If"                 { IF }
| "Then"               { THEN }
| "Else"               { ELSE }
| "Let"                { LET }
| "In"                 { IN }
| "->"                 { GOESTO }
| "<-"                 { SENDTO }
| "False"              { BOOL false }
| "True"               { BOOL true }
| ";;"                 { EOEX }
| '+'                  { PLUS }
| '-'                  { MINUS }
| '='                  { EQUAL }
| '('                  { LPAREN }
| ')'                  { RPAREN }
| ';'                  { SEMICOLON }
| ','                  { COMMA }
| '|'                  { PIPE }
| '['                  { LBRACKET }
| ']'                  { RBRACKET }
| "::"                 { CONS }
| "Match"              { MATCH }
| "With"               { WITH }
| "Fst"                { FST }
| "Snd"                { SND }
| "Create"             { CREATE }
| "Print"              { PRINT }
| "Head"               { HEAD }
| "Tail"               { TAIL } 
| decimal_literal      { INT (int_of_string(Lexing.lexeme lexbuf)) }
| '`' identchar identchar* { VARIANT (Lexing.lexeme lexbuf) }
| lowercase identchar* { IDENT (Lexing.lexeme lexbuf) }
| eof                  { raise Exit }

and read_string =
  parse
  | [^ '"' '\\']+ { 
                    (* Must _explicitly_ force ordering due to side effects *)
                    let text = Lexing.lexeme lexbuf in
                    let rest = read_string lexbuf in
                    text ^ rest
                  }
  | '"'           { "" }
  | '\\' '/'      { "/" ^ read_string lexbuf }
  | '\\' '\\'     { "\\" ^ read_string lexbuf }
  | '\\' '"'      { "\"" ^ read_string lexbuf }
  | '\\' 'n'      { "\n" ^ read_string lexbuf }
  | '\\' 'r'      { "\r" ^ read_string lexbuf }
  | '\\' 't'      { "\t" ^ read_string lexbuf }
  | _             { raise (StringSyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof           { raise (StringSyntaxError ("String is not terminated")) }


{} 



