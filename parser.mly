/***************************************************************/
/*  Copyright 2014 Pierre Hyvernat. All rights reserved.       */
/*  This file is distributed under the terms of the            */
/*  GNU General Public License, described in file COPYING.     */
/***************************************************************/

%{
open Common
open Regex
open Conversions

let verbose = ref false
let quiet = ref false
let alphabet = ref []

let do_help () =
    List.iter print_endline
    [
"                      ML-Regular";
"";
"Small program for experimenting with regular expressions and automata.";
"";
"Commands";
"========";
"  # regex                     print the regex";
"  # dfa                       print the table of the automaton";
"  # nfa                       print the table of the automaton";
"  # L<n>                      print the context free language";
"  # word                      print the word";
"";
"  # R<n> := regex             define a regex";
"  # D<n> := dfa               define a deterministic automaton";
"  # N<n> := nfa               define a non-deterministic automaton";
"  # L<n> := language          define a context free language";
"";
"  # \"word\" IN regex           matches the string against the regex";
"  # \"word\" IN dfa             matches the string against the automaton";
"  # \"word\" IN nfa             matches the string against the automaton";
"  # \"word\" IN L<n>(var)       matches the string against the context free language";
"";
"  # expr == expr              test if the two expressions are equal";
"  # expr >> expr              test if the first expression has a larger language than the second";
"  # expr << expr              test if the second expression has a larger language than the first";
"  # EMPTY expr                test if a an expression has an empty language";
"  # INFINITE regex            test if the regex has an infinite language";
"";
"  # :derivatives regex        show all the derivatives of a regex";
"  # :quit                     quit";
"  # :set verbose 0/1          set verbosity";
"  # :set quiet 0/1            toggle printing results of affectations R<n>, D<n> and N<n>";
"  # :set alphabet {a,b,...}   set the default alphabet";
"";
"  # :help                     this message";
"  # :help word                help about words (strings)";
"  # :help regex               help about regexs";
"  # :help dfa                 help about deterministic automata";
"  # :help nfa                 help about non-deterministic automata";
"  # :help lang                help about context free language";
"  # ?                         this message";
"";
"";
    ]

let do_help_word () =
    List.iter print_endline
    [
"Words";
"=====";
"Words are given between quotes '\"' and can only use lowercase letters.";
"Repetition in POSIX style, with grouping, are expanded. For example,";
"\"(ab){5}\" expands to \"ababababab\".";
"";
"";
    ]

let do_help_regex () =
    List.iter print_endline
    [
"Regular expressions";
"===================";
"Basic regular expressions are obtained from 0, 1, lowercase letters, infix '+',";
"infix '.' (or plain concatenation) and postfix '*'";
"Prefix '~' for complementation is available and is considered a regex constructor.";
"(To remove complementation, transform the expression to an automaton, and";
"transform it back to a regular expression...)";
"";
"Expressions are always simplified using the obvious equalities.";
"To prevent that, use '(# regex)'...";
"";
"User defined expressions (R<n>) can be used, and '<RANDOM>' generates a random";
"regular expression.";
"";
"POSIX style constructions are expanded to their definitions:";
"    regex?                      the regex zero or one time";
"    regex{<n>}                  the regex <n> times";
"    regex{<m>,<n>}              the regex at least <m> times, at most <n> times";
"    regex{<n>,}                 the regex at least <n> times";
"";
"Several operations on expressionsn are defined:";
"    regex & regex               the intersection of two regex (expanded using complements)";
"    regex / \"word\"              the word derivative of the regex wrt to the string";
"    regex \\ \"word\"              the word antiderivative of the regex wrt to the string";
"    TRANSPOSE regex             the transposition of the regex";
"    PREFIX regex                regex of prefixes";
"    <nfa>                       the regex associated to an automaton";
"    <dfa>                       the regex associated to an automaton";
"";
"";
    ]

let do_help_dfa () =
    List.iter print_endline
    [
"Deterministic finite automata";
"=============================";
"DFA are obtained from:";
"     [regex]                   automaton of the derivatives of the regex";
"     [nfa]                     determinisation of the automaton";
"     dfa & dfa                 intersection of the two automata";
"     dfa + dfa                 union of the two automata";
"     dfa | dfa                 union of the two automata";
"     ~dfa                      complement of the automaton";
"     ~dfa / {a,b,c...}         complement of the automaton, with additional symbols";
"     D<n>                      user defined automaton";
"     !dfa                      minimization of the automaton";
"";
"";
    ]

let do_help_nfa () =
    List.iter print_endline
    [
"Non deterministic finite automata";
"=================================";
"NFA are obtained from:";
"     {I regex}                 automaton inductively obtained from the regex";
"     {D regex}                 automaton obtained from the derivatives of the regex";
"     {regex}                   automaton obtained from the derivatives of the regex";
"     {dfa}                     the same automaton, seen as non-deterministic";
"     nfa + nfa                 union of the two automata";
"     nfa | nfa                 union of the two automata";
"     nfa*                      star of the automaton";
"     nfa . nfa                 concatenation of the automata";
"     N<n>                      user defined automaton";
"     TRANS nfa                 reversal of the automaton";
"";
"User defined NFA can be given in table form. It takes the form:";
"# N42 :=";
"          |  _  a  b  c      d   e";
"------------------------------------";
" -> 1 ->  |  _  1  1  {1,2}  {}  {1}";
"    2 ->  |  _  _  _  {2,3}  2   3";
"    3     |  {} 1  3  3      3   4";
" -> 4     |  4  4  4  4      4   4";
"";
    ]

let do_help_language () =
    List.iter print_endline
    [
"Context free languages";
"======================";
"Context free languages are defined using equations on regular expressions.";
"For example, to define the language of palindromic words on {a,b,c}:";
"  # L1 :=";
"   X -> 1 + a + b + a X a + b X b + c X c";
"";
"Note that variables are necessarily upper case...";
"";
"We can then get the language derived from a non terminal with";
"  # L1(X) / \"aba\"";
"";
"";
    ]

let toggle_verbosity b =
    if not b
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

module IntMap = Map.Make(struct type t=int let compare=compare end)

let list_NFA = ref IntMap.empty
let list_DFA = ref IntMap.empty
let list_REG = ref IntMap.empty
let list_LANG = ref IntMap.empty

let get_REG n =
    try IntMap.find n !list_REG
    with Not_found -> raise(Invalid_argument("no such regex R"^(string_of_int n)))
let get_DFA n =
    try IntMap.find n !list_DFA
    with Not_found -> raise(Invalid_argument("no such automaton D"^(string_of_int n)))
let get_NFA n =
    try IntMap.find n !list_NFA
    with Not_found -> raise(Invalid_argument("no such automaton N"^(string_of_int n)))
let get_LANG n =
    try IntMap.find n !list_LANG
    with Not_found -> raise(Invalid_argument("no such context-free language L"^(string_of_int n)))

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

    NFA_Regex.from_matrix matrix init accepting

let dfa_subset d1 d2 =
    try
        DFA_Regex.subset ~counterexample:!verbose d1 d2
    with DFA_Regex.Found(u) ->
        print_string "    <<< found counter-example: \"";
        List.iter print_char u;
        print_endline "\" >>>";
        false

let dfa_empty d =
    try
        DFA_Regex.is_empty ~counterexample:!verbose d
    with DFA_Regex.Found(u) ->
        print_string "    <<< found accepting word: \"";
        List.iter print_char u;
        print_endline "\" >>>";
        false

let nfa_empty d =
    try
        NFA_Regex.is_empty ~counterexample:!verbose d
    with NFA_Regex.Found(u) ->
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
    print_regex r;
    print_newline ();
    let der = get_all_derivatives r in
    List.iter
        (function r,w ->
            print_string "  ";
            if w = []
            then print_string "1"
            else List.iter print_char w;
            print_string " --> ";
            print_regex r;
            print_newline ())
        der

let n_concat l n =
    let rec aux n acc =
        if n < 1
        then acc
        else aux (n-1) (l@acc)
    in
    aux n []

let print_set l = match l with
    | [] -> print_string "{}"
    | x::l ->
            print_string "{";
            print_char x;
            List.iter (fun x -> print_char ','; print_char x) l;
            print_string "}"
%}

//typed tokens
%token <char> SYMB
%token <int> DFA
%token <int> NFA
%token <int> REG
%token <int> LANG
%token <string> VAR

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
%token ASSERT VERBOSE QUIT HELP HELP_WORD HELP_REGEX HELP_DFA HELP_NFA HELP_LANG AFFECT
%token QUIET NOT QUESTION SET ALPHABET
%token DERIVATIVES
%token <int> RANDOM
%token <int> NUM
%token QUOTE

//relations
%token LT GT DOUBLE_EQUAL IN

//parsing tables
%token PIPE ARROW UNDERSCORE COMMA LINE


//priorities and associativity of some operations
//TODO: check priorities
%right PIPE PLUS
%right AMPER
%right PREF TRANS
%right DOT
%right TILDE BANG
%left STAR QUESTION LCURL
%left SLASH BACKSLASH

%start toplevel
%type <unit> toplevel
%type <Regex.regex> regex

%%

toplevel:
    | command NEWLINE                               { $1 }

command:
    | HELP                                          { do_help () }
    | HELP_WORD                                     { do_help_word () }
    | HELP_REGEX                                    { do_help_regex () }
    | HELP_DFA                                      { do_help_dfa () }
    | HELP_NFA                                      { do_help_nfa () }
    | HELP_LANG                                     { do_help_language () }
    | QUESTION                                      { do_help () }

    | dfa                                           { DFA_Regex.print ~show_labels:!verbose $1 ; print_newline () }
    | nfa                                           { NFA_Regex.print ~show_labels:!verbose $1 ; print_newline () }
    | regex                                        { print_regex $1 ; print_newline () }
    | word                                          { print_endline ("\"" ^ $1 ^ "\"") }
    | language                                      { print_language $1 }

    | REG AFFECT regex                             { list_REG := IntMap.add $1 $3 !list_REG ;
                                                      if not !quiet
                                                      then (print_regex $3; print_newline ()) }
    | DFA AFFECT dfa                                { list_DFA := IntMap.add $1 $3 !list_DFA ;
                                                      if not !quiet
                                                      then (DFA_Regex.print ~show_labels:!verbose $3 ; print_newline ()) }
    | NFA AFFECT nfa                                { list_NFA := IntMap.add $1 $3 !list_NFA ;
                                                      if not !quiet
                                                      then (NFA_Regex.print ~show_labels:!verbose $3 ; print_newline ()) }
    | NFA AFFECT NEWLINE table                      { list_NFA := IntMap.add $1 $4 !list_NFA }
    | LANG AFFECT NEWLINE language                  { list_LANG := IntMap.add $1 (List.rev $4) !list_LANG}

    | assertion                                     { if $1 then print_endline "true" else print_endline "false" }
    | ASSERT assertion                              { assertion $2 }
    | DERIVATIVES raw_regex                        { show_derivatives $2 }


    | SET VERBOSE ZERO                              { toggle_verbosity false }
    | SET VERBOSE ONE                               { toggle_verbosity true }
    | SET VERBOSE                                   { toggle_verbosity (not !verbose) }
    | SET QUIET ZERO                                { quiet := false }
    | SET QUIET ONE                                 { quiet := true }
    | SET QUIET                                     { quiet := (not !quiet) }
    | SET ALPHABET alphabet                         { alphabet := uniq (List.sort compare $3) }
    | SET ALPHABET                                  { print_string "current alphabet: ";
                                                      print_set !alphabet ;
                                                      print_newline ()}

    | EOF                                           { raise End_of_file }
    | QUIT                                          { exit 0 }
    /* empty command also parsed as empty language |                                               { () } */

assertion:
    | NOT assertion                         { not $2 }
    | word IN regex                        { match_regex $1 $3 }
    | word IN LANG LPAR VAR RPAR            { match_language $1 (get_LANG $3) $5 }
    | word IN dfa                           { DFA_Regex.accepts $3 (explode $1) }
    | word IN nfa                           { NFA_Regex.accepts $3 (explode $1) }
    | INFINITE regex                       { is_infinite $2 }

    | EMPTY nfa                             { nfa_empty $2 }
    | EMPTY regex                          { is_empty $2 }
    | EMPTY dfa                             { dfa_empty $2 }
    | dfa_expr DOUBLE_EQUAL dfa_expr        { (dfa_subset $1 $3) && (dfa_subset $3 $1) }
    | dfa_expr LT dfa_expr                  { dfa_subset $1 $3 }
    | dfa_expr GT dfa_expr                  { dfa_subset $3 $1 }

dfa_expr:
    | dfa       { $1 }
    | nfa       { NFA_Regex.to_dfa $1 }
    | regex    { dfa_from_regex ~alphabet:!alphabet $1 }

dfa:
    | LPAR dfa RPAR             { $2 }
    | LBR regex RBR            { dfa_from_regex ~alphabet:!alphabet $2 }
    | TILDE dfa SLASH alphabet  { DFA_Regex.complement $2 ~alphabet:$4 }
    | TILDE dfa                 { DFA_Regex.complement $2 ~alphabet:!alphabet }
    | BANG dfa                  { DFA_Regex.minimize $2 }
    | dfa PIPE dfa              { DFA_Regex.union $1 $3 }
    | dfa PLUS dfa              { DFA_Regex.union $1 $3 }
    | dfa AMPER dfa             { DFA_Regex.intersection $1 $3 }
    | LBR nfa RBR               { NFA_Regex.to_dfa $2 }
    | DFA                       { get_DFA $1 }

alphabet:
    | LCURL elements RCURL    { $2 }

elements:
    |                       { [] }
    | SYMB                  { [$1] }
    | SYMB COMMA elements   { $1::$3 }

nfa:
    | LPAR nfa RPAR                 { $2 }
    | LCURL regex RCURL            { nfa_from_regex_derivative ~alphabet:!alphabet $2 }
    | LCURLD regex RCURL           { nfa_from_regex_derivative ~alphabet:!alphabet $2 }
    | LCURLI regex RCURL           { nfa_from_regex_inductive $2 }
    | nfa PIPE nfa                  { NFA_Regex.union $1 $3 }
    | nfa PLUS nfa                  { NFA_Regex.union $1 $3 }
    | nfa STAR                      { NFA_Regex.star $1 }
    | nfa DOT nfa                   { NFA_Regex.concat $1 $3 }
    | TRANS nfa                     { NFA_Regex.transpose $2 }
    | LCURL dfa RCURL               { NFA_Regex.from_dfa $2 }
    | NFA                           { get_NFA $1 }

language:
    | LANG                              { get_LANG $1 }
    | LANG LPAR VAR RPAR SLASH word     { language_word_derivative (get_LANG $1) $3 $6 }
    |                                   {[]}
    | VAR ARROW regex NEWLINE language  { ($1,$3)::$5 }


regex:
    | raw_regex                    { simplify $1 }
    | LPARHASH raw_regex RPAR      { $2 }

raw_regex:
    | sum_regex                { $1 }
    | raw_regex SLASH word     { word_derivative $1 $3 }
    | raw_regex BACKSLASH word { word_antiderivative $1 $3 }
    | BANG raw_regex           { simplify_sums $2 }

sum_regex:
    | product_regex                { $1 }
    | sum_regex PLUS sum_regex    { Sum($1, $3) }
    | sum_regex AMPER sum_regex   { Neg(Sum(Neg($1),Neg($3))) }

product_regex:
    | atomic_regex                     { $1 }
    | atomic_regex product_regex      { Product($1, $2) }
    | atomic_regex DOT product_regex  { Product($1, $3) }

atomic_regex:
    | ZERO                                  { Zero }
    | ONE                                   { One }
    | SYMB                                  { Symb($1) }
    | VAR                                   { Var($1) }
    | LPAR raw_regex RPAR                  { $2 }
    | atomic_regex STAR                    { Star($1) }
    | TILDE atomic_regex                   { Neg($2) }

    | REG                                   { get_REG $1 }
    | TRANS atomic_regex                   { transpose $2 }
    | PREF atomic_regex                    { prefix $2 }
    | LANGL nfa RANGL                       { regex_from_nfa $2 }
    | LANGL dfa RANGL                       { regex_from_nfa (NFA_Regex.from_dfa $2) }
    | LANGL RANDOM RANGL                    { random_regex $2 }

    | atomic_regex QUESTION                   { Sum(One, $1) }
    | atomic_regex LCURL num RCURL            { prod $3 $1 }
    | atomic_regex LCURL num COMMA RCURL      { Product(prod $3 $1,Star($1)) }
    | atomic_regex LCURL num COMMA num RCURL  { sum $3 $5 $1}

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

