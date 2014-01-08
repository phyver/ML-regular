(* from regexp to dfa *)

open Regexp
open DFA

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

module DFA_Regexp = Make(OChar)(ORegexp)


let dfa_from_regexp (r:regexp) : DFA_Regexp.dfa =

    let actual_symbols = Regexp.get_symbols r in

    let rec add_transition_row (a:char) t row =
        try
            let _ = List.assoc a row in
            assert false
        with Not_found -> (a,t)::row
    in

    let rec add_transition s (a:char) t matrix acc =
        match matrix with
        | [] -> (s,[a,t])::acc
        | (st,row)::matrix when st=s -> (s, add_transition_row a t row)::(List.rev_append acc matrix)
        | (st,row)::matrix -> add_transition s a t matrix ((st,row)::acc)
    in

    let rec aux done_states matrix accepting todo =
        match todo with
        | [] -> done_states, matrix, accepting
        | r::todo ->
                let accepting = if (Regexp.contains_epsilon r) then r::accepting else accepting in
                if List.mem r done_states
                then aux done_states matrix accepting todo
                else
                    let matrix,todo =
                        List.fold_left
                            (fun mt (a:char) ->
                                let matrix, todo = mt in
                                let ra = Regexp.simplify (Regexp.derivative r a) in
                                let matrix = add_transition r a ra matrix [] in
                                let todo = ra::todo in
                                matrix, todo
                            )
                            (matrix,todo)
                            actual_symbols
                    in
                    aux (r::done_states) matrix accepting todo
    in

    let _, matrix, accepting = aux [] [] [] [r] in

    DFA_Regexp.from_lts matrix r accepting

