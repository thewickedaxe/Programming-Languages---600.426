{
  open Tfbsrxparser;;
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
| "And"                { AND }
| "Or"                 { OR }
| "Not"                { NOT }
| "Fun"                { FUNCTION }
| "Function"           { FUNCTION }
| "If"                 { IF }
| "Then"               { THEN }
| "Else"               { ELSE }
| "In"                 { IN }
| "Ref"                { REF }
| "Let"                { LET }
| "Rec"                { REC }
| ":="                 { SET }
| "->"                 { GOESTO }
| "False"              { BOOL false }
| "True"               { BOOL true }
| ";;"                 { EOEX }
| '+'                  { PLUS }
| '-'                  { MINUS }
| '='                  { EQUAL }
| '('                  { LPAREN }
| ')'                  { RPAREN }
| '{'                  { LCURLY }
| '}'                  { RCURLY }
| ';'                  { SEMI }
| '.'                  { DOT }
| '!'                  { GET }
| ':'                  { COLON }
| "Int"                { INTTYPE }
| "Bool"               { BOOLTYPE }
| "Try"                { TRY }
| "Raise"              { RAISE }
| "With"               { WITH }
| ['#'] (identchar+ as id) ['@']   { EXN id } 
| decimal_literal      { INT (int_of_string(Lexing.lexeme lexbuf)) }
| lowercase identchar* { IDENT (Lexing.lexeme lexbuf) }
| eof                  { raise Exit }

{} 





