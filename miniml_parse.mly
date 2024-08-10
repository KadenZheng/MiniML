(*
                         CS 51 Final Project
                           MiniML -- Parser

   This grammar is processed by menhir. See
   http://gallium.inria.fr/~fpottier/menhir/manual.html and
   https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html
   for documentation of the various declarations used here.  *)
                  
%{
  open Expr ;;
%}

(* Tokens *)
%token EOF
%token OPEN CLOSE
%token LET DOT IN REC
%token NEG
%token PLUS MINUS FPLUS FMINUS
%token TIMES FTIMES FDIVIDE
%token LESSTHAN EQUALS
%token IF THEN ELSE 
%token FUNCTION
%token RAISE
%token <string> ID
%token <int> INT 
(* Added a token for float support *)
%token <float> FLOAT
(* Added a token for string support *) 
%token <string> STRING 
%token TRUE FALSE

(* Associativity and precedence *)
%nonassoc IF
%left LESSTHAN EQUALS
%left PLUS MINUS
%left TIMES
%nonassoc NEG

(* Start symbol of the grammar and its type *)
%start input
%type <Expr.expr> input

(* takes a list of identifiers and converts to a nested function structure  *)
%{
let convert_to_fun idlist exp = 
match idlist with
| [] -> exp
| _ -> List.fold_right (fun x acc -> Fun(x, acc)) idlist exp
%}

(* Grammar rules with actions to build an `expr` value as defined in
   the `Expr` module. *)
%%
input:  exp EOF                 { $1 }

idlist:  ID idlist              { $1 :: $2 }
        | ID                    { [$1] }
(* expressions *)
exp:    exp expnoapp            { App($1, $2) }
        | expnoapp              { $1 }

(* expressions except for application expressions *)
expnoapp: INT                   { Num $1 }
        | FLOAT                 { Float $1 }
        | STRING                { String $1 }
        | TRUE                  { Bool true }
        | FALSE                 { Bool false }
        | ID                    { Var $1 }
        | exp PLUS exp          { Binop(Plus, $1, $3) }
        | exp MINUS exp         { Binop(Minus, $1, $3) }
        | exp TIMES exp         { Binop(Times, $1, $3) }
        | exp FPLUS exp       { Binop(FPlus, $1, $3) }
        | exp FMINUS exp      { Binop(FMinus, $1, $3) }
        | exp FTIMES exp      { Binop(FTimes, $1, $3) }
        | exp FDIVIDE exp      { Binop(FDivide, $1, $3) }
        | exp EQUALS exp        { Binop(Equals, $1, $3) }
        | exp LESSTHAN exp      { Binop(LessThan, $1, $3) }
        | NEG exp               { Unop(Negate, $2) }
        | IF exp THEN exp ELSE exp      { Conditional($2, $4, $6) }
        | LET ID EQUALS exp IN exp      { Let($2, $4, $6) }
        | LET REC ID EQUALS exp IN exp  { Letrec($3, $5, $7) }
        | LET ID idlist EQUALS exp IN exp   { Let($2, convert_to_fun $3 $5, $7) }
        | LET REC ID idlist EQUALS exp IN exp { Letrec($3, convert_to_fun $4 $6, $8) }
        | FUNCTION ID idlist DOT exp   { Fun($2, convert_to_fun $3 $5) }
        | FUNCTION ID DOT exp   { Fun($2, $4) } 
        | RAISE                 { Raise }
        | OPEN exp CLOSE        { $2 }
;

%%
