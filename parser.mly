%{
open Regexp
open Regexp2dfa

let verbose = ref true

let do_help () =
    List.iter print_endline
    [
"Commands:";
"  > regexp                     simplified form of the regexp";
"  > (#regexp)                  raw form of the regexp";
"  > automaton                  print the table of the automaton";
"";
"  > regexp / \"string\"          print the word derivative of the regexp wrt to the string";
"  > \"string\" ~ regexp          matches the string against the regexp";
"  > automaton == automaton     test if the two automata are equal";
"  > automaton > automaton      test if the first automaton has a larger language than the second one";
"  > automaton < automaton      test if the second automaton has a larger language than the first one";
"";
"  > Q                      quit";
"  > V                      toggle printing labels of states in automata";
"  > ?                      help message";
"";
"Regexp are obtained from 0, 1, lowercase letters, +, * and concatenation.";
"";
"Automata are obtained from:";
"     <regexp>                  automaton of the derivatives of the regexp";
"     <#regexp>                 automaton of the derivatives of the raw regexp";
"     automaton & automaton     intersection of the two automata";
"     automaton | automaton     union of the two automata";
"     ~automaton                complement of the automaton";
"     !automaton                minimization of the automaton";
"";
    ]


let do_derivative r s =
    let rd = simplify (word_derivative r s) in
    print_string (s^" derivative: "); print_regexp rd; print_newline ()

let do_match s r =
    if match_regexp s r
    then print_string "True\n"
    else print_string "False\n"

let do_equal d1 d2 =
    if DFA_Regexp.equal d1 d2
    then print_endline "True"
    else print_endline "False"

let do_subset d1 d2 =
    if DFA_Regexp.subset d1 d2
    then print_string "True\n"
    else print_string "False\n"

%}

%token LPAR RPAR PLUS STAR ONE ZERO
%token <char> SYMB

%token <string> STR

%token HASH SLASH TILDE COMMA DOUBLE_EQUAL QUESTION BANG V
%token RBR LBR LT GT
%token AMPER PIPE
%token NEWLINE EOF

%right PIPE PLUS
%right AMPER
%right TILDE BANG
%left STAR

%start toplevel
%type <unit> toplevel
%type <Regexp.regexp> regexp

%%

toplevel:
    | QUESTION                                      { do_help() }
    | regexp NEWLINE                                { print_regexp (simplify $1) ; print_newline () }
    | LPAR HASH regexp RPAR                         { print_raw_regexp $3; print_newline() }

    | automata NEWLINE                              { DFA_Regexp.print ~show_labels:!verbose $1 ; print_newline () }

    | regexp SLASH STR NEWLINE                      { do_derivative $1 $3 }
    | STR TILDE regexp NEWLINE                      { do_match $1 $3 }
    | automata DOUBLE_EQUAL automata NEWLINE        { do_equal $1 $3 }
    | automata LT automata NEWLINE                  { do_subset $1 $3 }
    | automata GT automata NEWLINE                  { do_subset $3 $1 }

    | V NEWLINE                                     { verbose := not !verbose ;
                                                      raise (Invalid_argument "set verbosity")}

    | EOF                                           { raise Exit }
    | NEWLINE                                       { raise (Invalid_argument "empty") }


automata:
    | LPAR automata RPAR            { $2 }
    | LBR regexp RBR                { dfa_from_regexp (simplify $2) }
    | LBR HASH regexp RBR           { dfa_from_regexp $3 }
    | TILDE automata                { DFA_Regexp.complement $2 }
    | BANG automata                 { DFA_Regexp.minimize $2 }
    | automata PIPE automata        { DFA_Regexp.union $1 $3 }
    | automata AMPER automata        { DFA_Regexp.intersection $1 $3 }


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

