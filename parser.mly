%{
open Misc
open Regexp
open Regexp2dfa

let verbose = ref false

let do_help () =
    List.iter print_endline
    [
"Commands:";
"  > regexp                     simplified form of the regexp";
"  > (#regexp)                  raw form of the regexp";
"  > dfa                        print the table of the automaton";
"";
"  > regexp / \"string\"          print the word derivative of the regexp wrt to the string";
"  > \"string\" ~ regexp          matches the string against the regexp";
"  > \"string\" ~ dfa             matches the string against the automaton";
"  > \"string\" ~ nfa             matches the string against the automaton";
"";
"  > dfa == dfa                 test if the two automata are equal";
"  > dfa > dfa                  test if the first automaton has a larger language than the second one";
"  > dfa < dfa                  test if the second automaton has a larger language than the first one";
"";
"  > Q                      quit";
"  > V                      toggle printing labels of states in automata";
"  > ?                      help message";
"";
"Regexp are obtained from 0, 1, lowercase letters, +, * and concatenation.";
"";
"dfa are obtained from:";
"     <regexp>                  automaton of the derivatives of the regexp";
"     <#regexp>                 automaton of the derivatives of the raw regexp";
"     dfa & dfa                 intersection of the two automata";
"     dfa | dfa                 union of the two automata";
"     ~dfa                      complement of the automaton";
"     !dfa                      minimization of the automaton";
"";
"nfa are obtained from:";
"     {regexp}                  automaton inductively obtained from the regexp";
"     {#regexp}                 automaton inductively obtained from the raw regexp";
"     nfa | nfa                 union of the two automata";
"     nfa*                      star of the automaton";
"     nfa . nfa                 concatenation of the automata";
"     REV nfa                   reversal of the automaton";
"";
    ]


let do_derivative r s =
    let rd = simplify (word_derivative r s) in
    print_string (s^" derivative: "); print_regexp rd; print_newline ()

let do_match_regexp s r =
    if match_regexp s r
    then print_string "True\n"
    else print_string "False\n"

let do_match_dfa s d =
    if DFA_Regexp.accepts d (explode s)
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

let do_nfa r = 
    let d = nfa_from_regexp (simplify r) in
    NFA_Regexp.print ~show_labels:!verbose d;
    print_newline()

let do_match_nfa s d = 
    if NFA_Regexp.accepts d (explode s)
    then print_string "True\n"
    else print_string "False\n"
%}

%token LPAR RPAR PLUS STAR ONE ZERO
%token <char> SYMB

%token <string> STR

%token HASH SLASH TILDE COMMA DOUBLE_EQUAL QUESTION BANG V
%token LBR RBR LT GT LCURL RCURL
%token AMPER PIPE REV DOT
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

    | dfa NEWLINE                                   { DFA_Regexp.print ~show_labels:!verbose $1 ; print_newline () }
    | nfa NEWLINE                                   { NFA_Regexp.print ~show_labels:!verbose $1 ; print_newline () }

    | regexp SLASH STR NEWLINE                      { do_derivative $1 $3 }
    | STR TILDE regexp NEWLINE                      { do_match_regexp $1 $3 }
    | STR TILDE dfa NEWLINE                         { do_match_dfa $1 $3 }
    | STR TILDE nfa NEWLINE                         { do_match_nfa $1 $3 }

    | dfa DOUBLE_EQUAL dfa NEWLINE                  { do_equal $1 $3 }
    | dfa LT dfa NEWLINE                            { do_subset $1 $3 }
    | dfa GT dfa NEWLINE                            { do_subset $3 $1 }

    | V NEWLINE                                     { verbose := not !verbose ;
                                                      raise (Invalid_argument "set verbosity")}

    | EOF                                           { raise Exit }
    | NEWLINE                                       { raise (Invalid_argument "empty") }


dfa:
    | LPAR dfa RPAR             { $2 }
    | LBR regexp RBR            { dfa_from_regexp (simplify $2) }
    | LBR HASH regexp RBR       { dfa_from_regexp $3 }
    | TILDE dfa                 { DFA_Regexp.complement $2 }
    | BANG dfa                  { DFA_Regexp.minimize $2 }
    | dfa PIPE dfa              { DFA_Regexp.union $1 $3 }
    | dfa AMPER dfa             { DFA_Regexp.intersection $1 $3 }

nfa:
    | LPAR nfa RPAR             { $2 }
    | LCURL regexp RCURL        { nfa_from_regexp (simplify $2) }
    | LCURL HASH regexp RCURL   { nfa_from_regexp $3 }
    | nfa PIPE nfa              { NFA_Regexp.union $1 $3 }
    | nfa STAR                  { NFA_Regexp.star $1 }
    | nfa DOT nfa               { NFA_Regexp.concat $1 $3 }
    | REV nfa                   { NFA_Regexp.reverse $2 }

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

