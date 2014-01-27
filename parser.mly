%{
open Misc
open Regexp
open Conversions

let verbose = ref false
let quiet = ref false

let do_help () =
    List.iter print_endline
    [
"                      ML-Regular";
"";
"Small program for experimenting with regular expressions and automata.";
"";
"Commands:";
"  # regexp                     print the regexp";
"  # dfa                        print the table of the automaton";
"  # nfa                        print the table of the automaton";
"  # word                       print the (expanded) word";
"";
"  # REG<n> := regexp           define a regexp";
"  # DFA<n> := dfa              define a deterministic automaton";
"  # NFA<n> := nfa              define a non-deterministic automaton";
"  # NFA<n> := \\n table         define a non-deterministic automaton from a table";
"";
"  # \"word\" IN regexp           matches the string against the regexp";
"  # \"word\" IN dfa              matches the string against the automaton";
"  # \"word\" IN nfa              matches the string against the automaton";
"";
"  # expr == expr               test if the two expressions are equal";
"  # expr >> expr               test if the first expression has a larger language than the second";
"  # expr << expr               test if the second expression has a larger language than the first";
"  # EMPTY expr                 test if a an expression has an empty language";
"  # INFINITE regexp            test if the regexp has an infinite language";
"";
"  # :derivatives regexp        show all the derivatives of a regexp";
"  # :quit                      quit";
"  # :verbose                   toggle verbosity";
"  # :quiet                     toggle printing results of affectations R<n>, D<n> and N<n>";
"  # :help                      help message";
"  # ?                          help message";
"";
"words are obtained from symbols (lowercase letters) as well as repetitions/parenthesis,";
"as in # \"(ab){5}\" IN a(ba)*b";
"";
"Basic regexp are obtained from 0, 1, lowercase letters, +, *, concatenation,";
"complementation (~), user defined regexp (REG<n>) and random regexps (<RANDOM>)";
"";
"Regexps can also be generated with";
"    regexp / \"word\"              the word derivative of the regexp wrt to the string";
"    regexp \\ \"word\"              the word antiderivative of the regexp wrt to the string";
"    TRANS regexp                 the transposition of the regexp";
"    PREF regexp                  regexp of prefixes";
"    <nfa>                        the regexp associated to an automaton";
"    <dfa>                        the regexp associated to an automaton";
"    regexp ?                     the regexp zero or one time";
"    regexp{<n>}                  the regexp <n> times";
"    regexp{<m>,<n>}              the regexp at least <m> times, at most <n> times";
"    regexp{<n>,}                 the regexp at least <n> times";
"    regexp & regexp              the intersection of two regexp (using complements)";
"A regexp can be of the form (# regexp) to prevent simplifying it.";
"";
"dfa are obtained from:";
"     [regexp]                  automaton of the derivatives of the regexp";
"     [nfa]                     determinisation of the automaton";
"     DFA<n>                    user defined automaton";
"     !dfa                      minimization of the automaton";
"     dfa & dfa                 intersection of the two automata";
"     dfa + dfa                 union of the two automata";
"     ~dfa                      complement of the automaton";
"     ~dfa / {a,b,c...}         complement of the automaton, with additional symbols";
"";
"nfa are obtained from:";
"     {I regexp}                automaton inductively obtained from the regexp";
"     {D regexp}                automaton obtained from the derivatives of the regexp";
"     {regexp}                  automaton obtained from the derivatives of the regexp";
"     {dfa}                     the same automaton, seen as non-deterministic";
"     NFA<n>                    user defined automaton";
"     nfa + nfa                 union of the two automata";
"     nfa*                      star of the automaton";
"     nfa . nfa                 concatenation of the automata";
"     TRANS nfa                 reversal of the automaton";
"";
"A table can be used to define a non-deterministic automaton.";
"A table is given in the form";
"           |  _ a  b  c      d   e";
"------------------------------------";
" -> 1 ->  |  _  1  1  {1,2}  {}  {1}";
"    2 ->  |  _  _  _  {2,3}  2   3";
"    3     |  {} 1  3  3      3   4";
" -> 4     |  4  4  4  4      4   4";
"";
    ]

let toggle_verbosity () =
    if !verbose
    then
        begin
            verbose := false;
            if not !quiet
            then print_endline "verbosity is now off"
        end
    else
        begin
            verbose := true;
            if not !quiet
            then
                begin
                    print_endline "verbosity is now on:";
                    print_endline "  - full labels for automata will be displayed";
                    print_endline "  - counter examples given for false assertions"
                end
        end

let toggle_quiet () = quiet := not !quiet

module IntMap = Map.Make(struct type t=int let compare=compare end)

let list_NFA = ref IntMap.empty
let list_DFA = ref IntMap.empty
let list_REG = ref IntMap.empty

let get_REG n =
    try IntMap.find n !list_REG
    with Not_found -> raise(Invalid_argument("no such regexp REG"^(string_of_int n)))
let get_DFA n =
    try IntMap.find n !list_DFA
    with Not_found -> raise(Invalid_argument("no such automaton DFA"^(string_of_int n)))
let get_NFA n =
    try IntMap.find n !list_NFA
    with Not_found -> raise(Invalid_argument("no such automaton NFA"^(string_of_int n)))

(* transform a parsed table into a non-deterministic automaton *)
let make_nfa (symbols:char option list)
             (table:(bool*int*bool*(int list list)) list) =

    let init = List.map (function i,n,_,_ -> i,n) table in
    let init = List.filter fst init in
    let init = List.map (fun n -> Dummy(string_of_int (snd n))) init in

    let accepting = List.map (function _,n,a,_ -> a,n) table in
    let accepting = List.filter fst accepting in
    let accepting = List.map (fun n -> Dummy(string_of_int (snd n))) accepting in

    let state n = Dummy(string_of_int n) in
    let matrix = List.map (function _,n,_,row -> (state n, row)) table in

    let process_row row =
        let rec aux row symbols acc = match row, symbols with
            | [], _ -> acc
            | _, [] -> raise (Invalid_argument("not enough symbols"))
            | l::row, a::symbols ->
                    let l = List.map state l in
                    aux row symbols ((a,l)::acc)
        in
        aux row symbols []
    in
    let matrix = List.map (function s,row -> (s,process_row row)) matrix in

    NFA_Regexp.from_matrix matrix init accepting

let dfa_subset d1 d2 =
    try
        DFA_Regexp.subset ~counterexample:!verbose d1 d2
    with DFA_Regexp.Found(u) ->
        print_string "    <<< found counter-example: \"";
        List.iter print_char u;
        print_endline "\" >>>";
        false

let dfa_empty d =
    try
        DFA_Regexp.is_empty ~counterexample:!verbose d
    with DFA_Regexp.Found(u) ->
        print_string "    <<< found accepting word: \"";
        List.iter print_char u;
        print_endline "\" >>>";
        false

let nfa_empty d =
    try
        NFA_Regexp.is_empty ~counterexample:!verbose d
    with NFA_Regexp.Found(u) ->
        print_string "    <<< found accepting word: \"";
        List.iter print_char u;
        print_endline "\" >>>";
        false


let assertion b =
    if not b
    then
        begin
            let pos = Parsing.symbol_start_pos () in
            let lineNum = pos.Lexing.pos_lnum in
            print_endline ("*** Assertion failed on line " ^ (string_of_int lineNum) ^ " ***");
            exit 1
        end

let prod n r =
    let rec aux n acc =
        if n=1
        then acc
        else aux (n-1) (Product(r,acc))
    in
    if n < 1
    then One
    else aux n r

let sum m n r =
    let rec aux n acc p =
        if n=0
        then acc
        else aux (n-1) (Sum(acc,p)) (Product(r,p))
    in
    if m>n
    then One
    else if m=n then prod m r
    else Product(prod m r, aux (1+n-m) r One)

let show_derivatives r =
    print_string "> derivatives of ";
    print_regexp r;
    print_newline ();
    let der = get_all_derivatives r in
    List.iter
        (function r,w ->
            print_string "  ";
            if w = []
            then print_string "1"
            else List.iter print_char w;
            print_string " --> ";
            print_regexp r;
            print_newline ())
        der

let n_concat l n =
    let rec aux n acc =
        if n < 1
        then acc
        else aux (n-1) (l@acc)
    in
    aux n []
%}

//typed tokens
%token <char> SYMB
%token <int> DFA
%token <int> NFA
%token <int> REG

//grouping
%token LPARHASH LPAR RPAR LBR RBR LCURLI LCURLD LCURL RCURL LANGL RANGL

//constants
%token ONE ZERO
%token INFINITE EMPTY PREF TRANS

//unary
%token STAR TILDE BANG

//binary
%token PLUS AMPER DOT SLASH BACKSLASH

//misc
%token NEWLINE EOF
%token ASSERT VERBOSE QUIT HELP AFFECT QUIET NOT QUESTION
%token DERIVATIVES
%token <int> RANDOM
%token <int> NUM
%token QUOTE

//relations
%token LT GT DOUBLE_EQUAL IN

//parsing tables
%token PIPE ARROW UNDERSCORE COMMA LINE


//priorities and associativity of some operations
%right PIPE PLUS
%right AMPER
%right TILDE BANG
%left STAR

%start toplevel
%type <unit> toplevel
%type <Regexp.regexp> regexp

%%

toplevel:
    | command NEWLINE                               { $1 }

command:
    | HELP                                          { do_help () }
    | QUESTION                                      { do_help () }

    | dfa                                           { DFA_Regexp.print ~show_labels:!verbose $1 ; print_newline () }
    | nfa                                           { NFA_Regexp.print ~show_labels:!verbose $1 ; print_newline () }
    | regexp                                        { print_regexp $1 ; print_newline () }
    | word                                          { print_endline ("\"" ^ $1 ^ "\"") }

    | REG AFFECT regexp                             { list_REG := IntMap.add $1 $3 !list_REG ;
                                                      if not !quiet
                                                      then (print_regexp $3; print_newline ()) }
    | DFA AFFECT dfa                                { list_DFA := IntMap.add $1 $3 !list_DFA ;
                                                      if not !quiet
                                                      then (DFA_Regexp.print ~show_labels:!verbose $3 ; print_newline ()) }
    | NFA AFFECT nfa                                { list_NFA := IntMap.add $1 $3 !list_NFA ;
                                                      if not !quiet
                                                      then (NFA_Regexp.print ~show_labels:!verbose $3 ; print_newline ()) }
    | NFA AFFECT NEWLINE table                      { list_NFA := IntMap.add $1 $4 !list_NFA }

    | assertion                                     { if $1 then print_endline "true" else print_endline "false" }
    | ASSERT assertion                              { assertion $2 }
    | DERIVATIVES raw_regexp                        { show_derivatives $2 }


    | VERBOSE                                       { toggle_verbosity () }
    | QUIET                                         { toggle_quiet () }

    | EOF                                           { raise End_of_file }
    | QUIT                                          { exit 0 }
    |                                               { () }

assertion:
    | NOT assertion                         { not $2 }
    | word IN regexp                        { match_regexp $1 $3 }
    | word IN dfa                           { DFA_Regexp.accepts $3 (explode $1) }
    | word IN nfa                           { NFA_Regexp.accepts $3 (explode $1) }
    | INFINITE regexp                       { is_infinite $2 }

    | EMPTY nfa                             { nfa_empty $2 }
    | EMPTY regexp                          { is_empty $2 }
    | EMPTY dfa                             { dfa_empty $2 }
    | dfa_expr DOUBLE_EQUAL dfa_expr        { (dfa_subset $1 $3) && (dfa_subset $3 $1) }
    | dfa_expr LT dfa_expr                  { dfa_subset $1 $3 }
    | dfa_expr GT dfa_expr                  { dfa_subset $3 $1 }

dfa_expr:
    | dfa       { $1 }
    | nfa       { NFA_Regexp.to_dfa $1 }
    | regexp    { dfa_from_regexp $1 }

dfa:
    | LPAR dfa RPAR             { $2 }
    | LBR regexp RBR            { dfa_from_regexp $2 }
    | TILDE dfa alphabet        { DFA_Regexp.complement $2 ~symbols:$3}
    | BANG dfa                  { DFA_Regexp.minimize $2 }
    | dfa PIPE dfa              { DFA_Regexp.union $1 $3 }
    | dfa AMPER dfa             { DFA_Regexp.intersection $1 $3 }
    | LBR nfa RBR               { NFA_Regexp.to_dfa $2 }
    | DFA                       { get_DFA $1 }

alphabet:
    |                               { [] }
    | SLASH LCURL elements RCURL    { $3 }

elements:
    |                       { [] }
    | SYMB                  { [$1] }
    | SYMB COMMA elements   { $1::$3 }

nfa:
    | LPAR nfa RPAR                 { $2 }
    | LCURL regexp RCURL            { nfa_from_regexp_derivative $2 }
    | LCURLD regexp RCURL           { nfa_from_regexp_derivative $2 }
    | LCURLI regexp RCURL           { nfa_from_regexp_inductive $2 }
    | nfa PIPE nfa                  { NFA_Regexp.union $1 $3 }
    | nfa STAR                      { NFA_Regexp.star $1 }
    | nfa DOT nfa                   { NFA_Regexp.concat $1 $3 }
    | TRANS nfa                     { NFA_Regexp.transpose $2 }
    | LCURL dfa RCURL               { NFA_Regexp.from_dfa $2 }
    | NFA                           { get_NFA $1 }

regexp:
    | raw_regexp                    { simplify $1 }
    | LPARHASH raw_regexp RPAR      { $2 }

raw_regexp:
    | sum_regexp                { $1 }
    | raw_regexp SLASH word     { word_derivative $1 $3 }
    | raw_regexp BACKSLASH word { word_antiderivative $1 $3 }

sum_regexp:
    | product_regexp                { $1 }
    | sum_regexp PLUS sum_regexp    { Sum($1, $3) }
    | sum_regexp AMPER sum_regexp   { Neg(Sum(Neg($1),Neg($3))) }

product_regexp:
    | atomic_regexp                     { $1 }
    | atomic_regexp product_regexp      { Product($1, $2) }

atomic_regexp:
    | ZERO                                  { Zero }
    | ONE                                   { One }
    | SYMB                                  { Symb($1) }
    | LPAR raw_regexp RPAR                  { $2 }
    | atomic_regexp STAR                    { Star($1) }
    | TILDE atomic_regexp                   { Neg($2) }

    | REG                                   { get_REG $1 }
    | TRANS atomic_regexp                   { transpose $2 }
    | PREF atomic_regexp                    { prefix $2 }
    | LANGL nfa RANGL                       { regexp_from_nfa $2 }
    | LANGL dfa RANGL                       { regexp_from_nfa (NFA_Regexp.from_dfa $2) }
    | LANGL RANDOM RANGL                    { random_regexp $2 }

    | atomic_regexp QUESTION                   { Sum(One, $1) }
    | atomic_regexp LCURL num RCURL            { prod $3 $1 }
    | atomic_regexp LCURL num COMMA RCURL      { Product(prod $3 $1,Star($1)) }
    | atomic_regexp LCURL num COMMA num RCURL  { sum $3 $5 $1}

num:
    | NUM   { $1 }
    | ONE   { 1 }
    | ZERO  { 0 }

table:
    | PIPE underscore first_line NEWLINE line end_table { make_nfa ($2@$3) $6 }

line:
    |               {}
    | LINE NEWLINE  {}

underscore:
    |            { [] }
    | UNDERSCORE { [None] }

first_line:
    |                   { [] }
    | SYMB first_line   { (Some($1))::$2 }

end_table:
    |                               { [] }
    | LINE                          { [] }
    | table_line NEWLINE end_table  { $1::$3 }

table_line:
    | arrow num arrow PIPE transitions     { ($1,$2,$3,$5) }

arrow:
    |           { false }
    | ARROW     { true }

transitions:
    |                                 { [] }
    | UNDERSCORE transitions          { []::$2 }
    | num transitions                 { [$1]::$2 }
    | LCURL nums RCURL transitions    { $2::$4 }

nums:
    |                                   { [] }
    | num                             { [$1] }
    | num COMMA nums                { $1::$3 }

word:
    | QUOTE raw_word QUOTE          { implode $2 }

raw_word:
    |                   { [] }
    | atomic_word raw_word     { $1 @ $2 }

atomic_word:
    | SYMB                                      { [$1] }
    | LPAR raw_word RPAR                        { $2 }
    | atomic_word LCURL num RCURL               { n_concat $1 $3 }
