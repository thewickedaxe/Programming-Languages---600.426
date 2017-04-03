%{

open Boolast;;

%}

%token AND
%token OR
%token NOT
%token IMPLIES
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token EOEX

%right IMPLIES                          /* Implies */
%right OR                               /* Or */
%right AND                              /* And */
%right NOT                              /* Not */
%nonassoc prec_paren                    /* (e) */

%start main
%type <Boolast.expr> main

%%

main:
  expr EOEX { $1 }
;

expr:
  TRUE
    { True }
| FALSE
    { False }
| expr AND expr
    { And($1, $3) }
| expr OR expr
    { Or($1, $3) }
| expr IMPLIES expr
    { Implies($1, $3) }
| NOT expr
    { Not($2) }
| LPAREN expr RPAREN
    { $2 }
;

%%





