(* from regexp to dfa *)

open Regexp
open Automata

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


module DFA_Regexp = DFA(OChar)(ORegexp)
module LTS = LTS(OChar)(GeneralizedState(ORegexp))


(* transform a regexp into an automaton by computing its derivatives *)
let dfa_from_regexp (r:regexp) : DFA_Regexp.dfa =

    (* the symbols appearing in the regexp *)
    let actual_symbols = Regexp.get_symbols r in

    (* we compute all the derivatives and put them in an automaton
     *   - done_states contains all the states (regexps) whose derivative we have already computed
     *     the corresponding rows in the automaton are thus complete
     *   - matrix contains the LTS of the automaton
     *   - accepting contains the list of accepting states we have already encountered
     *   - todo contains the states (regexps) whose derivatives we haven't yet computed
     *)
    (* FIXME: use sets for done_states *)
    let rec aux (done_states:regexp list) (matrix:LTS.lts) (accepting:regexp list) (todo:regexp list) =
        match todo with
        | [] -> done_states, matrix, accepting
        | r::todo ->
                let accepting = if (Regexp.contains_epsilon r)
                                then r::accepting
                                else accepting
                in
                if List.mem r done_states                   (* if we've already done r *)
                then aux done_states matrix accepting todo  (* we continue *)
                else                                        (* otherwise *)
                    let matrix,todo =
                        List.fold_left                      (* we compute all its derivatives *)
                            (fun mt (a:char) ->             (* and add the corresponding transitions in "matrix" *)
                                let matrix, todo = mt in
                                let ra = Regexp.simplify (Regexp.derivative r a) in
                                let matrix = LTS.add (Atom(r)) a (Atom(ra)) matrix in
                                let todo = ra::todo in
                                matrix, todo
                            )
                            (matrix,todo)
                            actual_symbols
                    in
                    aux (r::done_states) matrix accepting todo
    in

    let _, matrix, accepting = aux [] LTS.empty [] [r] in

    DFA_Regexp.from_lts matrix r accepting

