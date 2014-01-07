%{
open Regexp
open Utils
%}

%token LPAR RPAR PLUS STAR ONE ZERO
%token <char> SYMB
%token HASH SLASH TILDE COMMA D
%token QUESTION
%token <string> STR
%token NEWLINE EOF

%start toplevel
%type <Utils.action> toplevel
%type <Regexp.regexp> regexp

%%

toplevel:
    | QUESTION                                      { Help }
    | regexp NEWLINE                                { Simplify($1) }
    | HASH regexp NEWLINE                           { PrintRaw($2) }
    | regexp SLASH STR                              { Derivative($1,$3) }
    | STR TILDE regexp NEWLINE                      { Match($1, $3) }
    | D regexp NEWLINE                              { AllDerivatives($2) }

    | EOF                                           { raise Exit }
    | NEWLINE                                       { raise (Invalid_argument "empty") }

regexp:
    | sum_regexp { $1 }

sum_regexp:
    | product_regexp { $1 }
    | product_regexp PLUS sum_regexp { Sum($1, $3) }

product_regexp:
    | atomic_regexp { $1 }
    | atomic_regexp product_regexp { Product($1, $2) }

atomic_regexp:
    | ZERO { Zero }
    | ONE { One }
    | SYMB { Symb($1) }
    | LPAR regexp RPAR { $2 }
    | atomic_regexp STAR { Star($1) }

