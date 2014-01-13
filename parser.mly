%{
open Misc
open Regexp
open Conversions

let verbose = ref false

let do_help () =
    List.iter print_endline
    [
"Commands:";
"  > regexp                     simplified form of the regexp";
"  > dfa                        print the table of the automaton";
"";
"  > REG<n> := regexp           define a regexp";
"  > DFA<n> := dfa              define a deterministic automaton";
"  > NFA<n> := nfa              define a non-deterministic automaton";
"  > NFA<n> := \\n table         define a non-deterministic automaton";
"";
"  > \"string\" ~ regexp          matches the string against the regexp";
"  > \"string\" ~ dfa             matches the string against the automaton";
"  > \"string\" ~ nfa             matches the string against the automaton";
"  > dfa == dfa                 test if the two automata are equal";
"  > dfa > dfa                  test if the first automaton has a larger language than the second one";
"  > dfa < dfa                  test if the second automaton has a larger language than the first one";
"  > INFINITE regexp            test if the regexp has an infinite language";
"  > EMPTY regexp               test if the regexp has an empty language";
"";
"  > Q                      quit";
"  > V                      toggle printing labels of states in automata";
"  > ?                      help message";
"";
"Basic regexp are obtained from 0, 1, lowercase letters, +, * and concatenation, and user defined regexp (REG<n>)";
"Extended regexps are obtained from";
"    regexp / \"string\"            the word derivative of the regexp wrt to the string";
"    TRANS regexp                 the transposition of the regexp";
"    PREF regexp                  regexp of prefixes";
"    <nfa>                        the regexp associated to an automaton";
"A regexp can be of the form (# regexp) to prevent simplifying it.";
"";
"dfa are obtained from:";
"     [regexp]                  automaton of the derivatives of the regexp";
"     [#regexp]                 automaton of the derivatives of the raw regexp";
"     dfa & dfa                 intersection of the two automata";
"     dfa | dfa                 union of the two automata";
"     ~dfa                      complement of the automaton";
"     ~dfa / {a,b,c...}         complement of the automaton, with additional symbols";
"     !dfa                      minimization of the automaton";
"     [nfa]                     determinisation of the automaton";
"     DFA<n>                    user defined automaton";
"";
"nfa are obtained from:";
"     {regexp}                  automaton inductively obtained from the regexp";
"     {#regexp}                 automaton inductively obtained from the raw regexp";
"     nfa | nfa                 union of the two automata";
"     nfa*                      star of the automaton";
"     nfa . nfa                 concatenation of the automata";
"     TRANS nfa                 reversal of the automaton";
"     {dfa}                     the same automaton, seen as non-deterministic";
"     NFA<n>                    user defined automaton";
"";
"A table can be used to define a non-deterministic automaton.";
"A table is given in the form";
"           |  _  a   b    c        d    e";
" -> s1 ->  |  !  s1  s1  {s1,s2}   {}  {s1}";
"    s2 ->  |  !  !   !   {s2,s3}   s2  s3";
"    s3     |  {} s1  s3  s3        s3  s4";
" -> s4     |  s4 s4  s4  s4        s4  s4";
"";
    ]

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

%}

%token LPAR RPAR PLUS STAR ONE ZERO
%token <char> SYMB

%token <string> STR
%token <int> DFA
%token <int> NFA
%token <int> REG
%token <int> STATE

%token HASH SLASH TILDE COMMA DOUBLE_EQUAL QUESTION BANG V
%token LBR RBR LT GT LCURL RCURL
%token AMPER PIPE TRANS DOT AFFECT PREF
%token ARROW
%token NEWLINE EOF
%token TABLE
%token UNDERSCORE
%token INFINITE EMPTY

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

    | dfa NEWLINE                                   { DFA_Regexp.print ~show_labels:!verbose $1 ; print_newline () }
    | nfa NEWLINE                                   { NFA_Regexp.print ~show_labels:!verbose $1 ; print_newline () }
    | regexp NEWLINE                                { print_regexp $1 ; print_newline () }

    | REG AFFECT regexp NEWLINE                     { list_REG := IntMap.add $1 $3 !list_REG }
    | DFA AFFECT dfa NEWLINE                        { list_DFA := IntMap.add $1 $3 !list_DFA }
    | NFA AFFECT nfa NEWLINE                        { list_NFA := IntMap.add $1 $3 !list_NFA }
    | NFA AFFECT NEWLINE table                      { list_NFA := IntMap.add $1 $4 !list_NFA }

    | assertion NEWLINE                             { if $1 then print_endline "true" else print_endline "false" }

    | V NEWLINE                                     { verbose := not !verbose ;
                                                      raise (Invalid_argument "set verbosity")}

    | EOF                                           { raise Exit }
    | NEWLINE                                       { raise (Invalid_argument "empty line") }

assertion:
    | STR TILDE regexp                      { match_regexp $1 $3 }
    | STR TILDE dfa                         { DFA_Regexp.accepts $3 (explode $1) }
    | STR TILDE nfa                         { NFA_Regexp.accepts $3 (explode $1) }

    | dfa DOUBLE_EQUAL dfa                  { DFA_Regexp.equal $1 $3 }
    | dfa LT dfa                            { DFA_Regexp.subset $1 $3 }
    | dfa GT dfa                            { DFA_Regexp.subset $3 $1 }
    | EMPTY regexp                          { is_empty $2 }
    | INFINITE regexp                       { is_infinite $2 }


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
    | SLASH LCURL elements RCURL    { print_endline "alphabet"; $3 }
elements:
    |                       { [] }
    | SYMB                  { [$1] }
    | SYMB COMMA elements   { $1::$3 }


nfa:
    | LPAR nfa RPAR                 { $2 }
    | LCURL regexp RCURL            { nfa_from_regexp $2 }
    | nfa PIPE nfa                  { NFA_Regexp.union $1 $3 }
    | nfa STAR                      { NFA_Regexp.star $1 }
    | nfa DOT nfa                   { NFA_Regexp.concat $1 $3 }
    | TRANS nfa                     { NFA_Regexp.transpose $2 }
    | LCURL dfa RCURL               { NFA_Regexp.from_dfa $2 }
    | NFA                           { get_NFA $1 }

regexp:
    | raw_regexp                    { simplify $1 }
    | LPAR HASH raw_regexp RPAR     { $3 }

raw_regexp:
    | sum_regexp { $1 }

sum_regexp:
    | product_regexp                    { $1 }
    | product_regexp PLUS sum_regexp    { Sum($1, $3) }

product_regexp:
    | atomic_regexp                     { $1 }
    | atomic_regexp product_regexp      { Product($1, $2) }

atomic_regexp:
    | ZERO                          { Zero }
    | ONE                           { One }
    | SYMB                          { Symb($1) }
    | LPAR raw_regexp RPAR          { $2 }
    | atomic_regexp STAR            { Star($1) }
    | REG                           { get_REG $1 }
    | TRANS atomic_regexp           { transpose $2 }
    | atomic_regexp SLASH STR       { word_derivative $1 $3 }
    | PREF atomic_regexp            { prefix $2 }
    | LT nfa GT                     { regexp_from_nfa $2 }




table:
    | PIPE underscore first_line NEWLINE end_table NEWLINE { make_nfa ($2@$3) $5 }

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
    | arrow STATE arrow PIPE transitions     { ($1,$2,$3,$5) }

arrow:
    |           { false }
    | ARROW     { true }

transitions:
    |                                   { [] }
    | BANG transitions                  { []::$2 }
    | STATE transitions                 { [$1]::$2 }
    | LCURL states RCURL transitions    { $2::$4 }

states:
    |                                   { [] }
    | STATE                             { [$1] }
    | STATE COMMA states                { $1::$3 }


