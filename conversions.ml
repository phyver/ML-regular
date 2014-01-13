(***
 *** conversions between automata and regular expressions
 **)


open Regexp
open Misc

(* we will need an automaton with regexps as states and characters as symbols
 * those are the corresponding OTypes *)
module ORegexp = struct
    type t = regexp
    let compare = compare
    let to_string = string_of_regexp
end

module OChar = struct
    type t = char
    let compare = compare
    let to_string c = String.make 1 c
end


module DFA_Regexp = DFA.Make(OChar)(ORegexp)
module NFA_Regexp = NFA.Make(OChar)(ORegexp)

(* transform a regexp into an automaton by computing its derivatives *)
let dfa_from_regexp (r:regexp) : DFA_Regexp.dfa =

    (* the symbols appearing in the regexp *)
    let actual_symbols = Regexp.get_symbols r in

    (* we compute all the derivatives and put them in an automaton
     *   - "done_states" contains all the states (regexps) whose derivative we
     *      have already computed the corresponding rows in the automaton are
     *      thus complete
     *   - "matrix" contains the matrix of the automaton as association lists
     *   - "accepting" contains the list of accepting states we have already
     *      encountered
     *   - "todo" contains the states (regexps) whose derivatives we haven't
     *     yet computed
     *)
    let rec aux (done_states:regexp list)
                matrix
                accepting
                (todo:regexp list) =
        match todo with
            | [] -> done_states, matrix, accepting
            | r::todo ->
                    let accepting = if (Regexp.contains_epsilon r)
                                    then Atom(r)::accepting
                                    else accepting
                    in
                    if List.mem r done_states                   (* if we've already done r *)
                    then aux done_states matrix accepting todo  (* we continue *)
                    else                                        (* otherwise *)
                        let row,todo =
                            List.fold_left
                                (fun rt (a:char) ->
                                    let row,todo = rt in
                                    let ra = Regexp.simplify (Regexp.derivative r a) in
                                    let row = (a,Atom(ra))::row in
                                    (row,ra::todo)
                            ) ([],todo) actual_symbols
                        in
                        aux (r::done_states) ((Atom(r),row)::matrix) accepting todo
    in

    let _, matrix, accepting = aux [] [] [] [r] in

    DFA_Regexp.from_matrix matrix (Atom(r)) accepting


let rec nfa_from_regexp r = match r with
    | Zero -> NFA_Regexp.zero_nfa
    | One -> NFA_Regexp.one_nfa
    | Symb(a) -> NFA_Regexp.symbol_nfa a
    | Sum(r1,r2) ->
            let d1 = nfa_from_regexp r1 in
            let d2 = nfa_from_regexp r2 in
            NFA_Regexp.union d1 d2
    | Product(r1,r2) ->
            let d1 = nfa_from_regexp r1 in
            let d2 = nfa_from_regexp r2 in
            NFA_Regexp.concat d1 d2
    | Star(r) ->
            let d = nfa_from_regexp r in
            NFA_Regexp.star d


(* TODO regexp from nfa *)





(* vim600:set textwidth=0: *)
