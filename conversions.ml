(***************************************************************)
(*  Copyright 2014 Pierre Hyvernat. All rights reserved.       *)
(*  This file is distributed under the terms of the            *)
(*  GNU General Public License, described in file COPYING.     *)
(***************************************************************)


(***
 *** conversions between automata and regular expressions
 **)


open Regex
open Common

(* we will need an automaton with regexs as states and characters as symbols
 * those are the corresponding OTypes *)
module ORegex = struct
    type t = regex
    let compare = compare
    let to_string = string_of_regex
end

module OChar = struct
    type t = char
    let compare = compare
    let to_string c = String.make 1 c
end


module DFA_Regex = DFA.Make(OChar)(ORegex)
module NFA_Regex = NFA.Make(OChar)(ORegex)

(* transform a regex into an automaton by computing its derivatives *)
let dfa_from_regex ?(alphabet=[]) (r:regex) : DFA_Regex.dfa =

    (* the symbols appearing in the regex *)
    let alphabet = List.sort OChar.compare alphabet in
    let actual_symbols = merge_union alphabet (Regex.get_symbols r) in

    (* we compute all the derivatives and put them in an automaton
     *   - "done_states" contains all the states (regexs) whose derivative we
     *      have already computed the corresponding rows in the automaton are
     *      thus complete
     *   - "matrix" contains the matrix of the automaton as association lists
     *   - "accepting" contains the list of accepting states we have already
     *      encountered
     *   - "todo" contains the states (regexs) whose derivatives we haven't
     *     yet computed
     *)
    let rec aux (done_states:regex list)
                matrix
                accepting
                (todo:regex list) =
        match todo with
            | [] -> done_states, matrix, accepting
            | r::todo ->
                    let accepting = if (Regex.contains_epsilon r)
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
                                    let ra = Regex.simplify (Regex.derivative r a) in
                                    let row = (a,Atom(ra))::row in
                                    (row,ra::todo)
                            ) ([],todo) actual_symbols
                        in
                        aux (r::done_states) ((Atom(r),row)::matrix) accepting todo
    in

    let _, matrix, accepting = aux [] [] [] [r] in

    DFA_Regex.from_matrix matrix (Atom(r)) accepting

(* compute an NFA from a regex using the inductive construction à la Thomson *)
let rec nfa_from_regex_inductive r = match r with
    | Zero -> NFA_Regex.zero_nfa
    | One -> NFA_Regex.one_nfa
    | Symb(a) -> NFA_Regex.symbol_nfa a
    | Neg(r) -> raise (Failure "cannot compute the NFA associated to a negated regex directly")
    | Sum(r1,r2) ->
            let d1 = nfa_from_regex_inductive r1 in
            let d2 = nfa_from_regex_inductive r2 in
            NFA_Regex.union d1 d2
    | Product(r1,r2) ->
            let d1 = nfa_from_regex_inductive r1 in
            let d2 = nfa_from_regex_inductive r2 in
            NFA_Regex.concat d1 d2
    | Star(r) ->
            let d = nfa_from_regex_inductive r in
            NFA_Regex.star d
    | Var(_) -> assert false

(* transform a regex into an NFA by computing its derivatives *)
let nfa_from_regex_derivative ?(alphabet=[]) (r:regex) : NFA_Regex.nfa =

    (* the symbols appearing in the regex *)
    let alphabet = List.sort OChar.compare alphabet in
    let actual_symbols = merge_union alphabet (Regex.get_symbols r) in

    (* we compute all the derivatives and put them in an automaton
     *   - "done_states" contains all the states (regexs) whose derivative we
     *      have already computed the corresponding rows in the automaton are
     *      thus complete
     *   - "matrix" contains the matrix of the automaton as association lists
     *   - "accepting" contains the list of accepting states we have already
     *      encountered
     *   - "todo" contains the states (regexs) whose derivatives we haven't
     *     yet computed
     *)
    let rec aux (done_states:regex list)
                matrix
                accepting
                (todo:regex list) =
        match todo with
            | [] -> done_states, matrix, accepting
            | r::todo ->
                    let accepting = if (Regex.contains_epsilon r)
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
                                    let ra = Regex.simplify (Regex.derivative r a) in
                                    let ras = get_summands ra in
                                    let row = (Some(a),List.map (fun r -> Atom(r)) ras)::row in
                                    (row,ras@todo)
                            ) ([],todo) actual_symbols
                        in
                        aux (r::done_states) ((Atom(r),row)::matrix) accepting todo
    in

    let init = get_summands r in
    let _, matrix, accepting = aux [] [] [] init in

    NFA_Regex.from_matrix matrix (List.map (fun s -> Atom(s)) init) accepting


(* regex from nfa *)
(* FIXME: probably not very optimized *)
module IntIntMap = Map.Make(struct type t=int*int let compare=compare end)
type mat = regex IntIntMap.t

let regex_from_nfa ?(random=true) aut : regex =

    let states = NFA_Regex.get_states aut in
    let symbols = NFA_Regex.get_symbols aut in
    let id s = idx s states in

    (*
    let print_matrix s matrix =
        print_endline (">>> "^s);
        IntIntMap.iter
            (fun st r -> print_int (fst st) ;
                         print_string " --" ;
                         print_regex r ;
                         print_string "--> " ;
                         print_int (snd st) ;
                         print_newline ()
            ) matrix;
        print_endline "<<<"
    in
    *)

    (* we construct the matrix with transition "Symb(a)" from the automaton *)
    let matrix =
        List.fold_left (fun matrix s -> let is = id s in
        List.fold_left (fun matrix a ->
            try
                let ts = NFA_Regex.next aut s (Some(a)) in
                List.fold_left (fun matrix t -> let it = id t in
                    let entry = try IntIntMap.find (is,it) matrix
                                with Not_found -> Zero
                    in
                    IntIntMap.add (is,it) (simplify_sum entry (Symb(a))) matrix
                ) matrix ts
            with Not_found -> matrix
        ) matrix symbols
        ) IntIntMap.empty states
    in
    (* we also add the regex corresponding to epsilon transititions *)
    let matrix =
        List.fold_left (fun matrix s -> let is = id s in
            try
                let ts = NFA_Regex.next aut s None in
                List.fold_left (fun matrix t -> let it = id t in
                    let entry = try IntIntMap.find (is,it) matrix
                                with Not_found -> Zero
                    in
                    IntIntMap.add (is,it) (simplify_sum entry One) matrix
                ) matrix ts
            with Not_found -> matrix
        ) matrix states
    in

    (* we add a new initial state (-1) and a new final state (-2) *)
    let init = -1 in
    let matrix =
        List.fold_left (fun matrix s -> let is = id s in
            IntIntMap.add (init,is) One matrix
        ) matrix (NFA_Regex.get_init aut)
    in
    let final = -2 in
    let matrix =
        List.fold_left (fun matrix s -> let is = id s in
            IntIntMap.add (is,final) One matrix
        ) matrix (List.filter (NFA_Regex.is_accepting aut) states)
    in

    (* remove a state from the matrix *)
    let states = List.map id states in
    let states = if random then shuffle states else states in

    (* we should somehow return the new list of states to avoid going through
     * all of them all the time... *)
    let remove_state matrix x states =
        let xx = try Star(IntIntMap.find (x,x) matrix)
                 with Not_found -> One
        in
        let matrix =
            List.fold_left (fun matrix s ->
            List.fold_left (fun matrix t ->
                try
                    let sx = IntIntMap.find (s,x) matrix in
                    let xt = IntIntMap.find (x,t) matrix in
                    let st = try IntIntMap.find (s,t) matrix
                             with Not_found -> Zero
                    in
                    let new_st = simplify_product sx (Product(xx,xt)) in
                    let new_st = simplify_sum new_st st in
                    IntIntMap.add (s,t) new_st matrix
                with Not_found -> matrix
            ) matrix (init::final::states)
            ) matrix (init::final::states)
        in
        List.fold_left (fun matrix s ->
            let matrix = IntIntMap.remove (s,x) matrix in
            let matrix = IntIntMap.remove (x,s) matrix in
            matrix
        ) matrix (init::final::x::states)
    in

    (* remove all the states from the matrix *)
    let rec remove_all_states matrix states = match states with
        | [] -> matrix
        | s::states ->
                let matrix = remove_state matrix s states in
                remove_all_states matrix states
    in
    let matrix = remove_all_states matrix states in

    (* we get the only entry from the initial state to the final state *)
    assert (2 > IntIntMap.cardinal matrix);
    try
        IntIntMap.find (init,final) matrix
    with Not_found -> Zero


(* the same, but we try it many times and keep the smallest regex *)
let regex_from_nfa aut =
    let rec aux n r =
        if n<0
        then r
        else let rr = regex_from_nfa aut in
             if String.length (string_of_regex r) < String.length (string_of_regex rr)
             then aux (n-1) r
             else aux (n-1) rr
    in
    aux 100 (regex_from_nfa aut)

(* TODO: we seem to get many regexs of the form (1+x)(1+x)* which is equal to x*... Perhaps I could simplify that... *)


(* TODO: a simplification function that removes summands that are less than the sum *)
let simplify_sums r =

    let less r1 r2 =
        let dfa1 = dfa_from_regex r1 in
        let dfa2 = dfa_from_regex r2 in
        DFA_Regex.subset dfa1 dfa2
    in

    let rec aux summands acc = match summands with
        | [] -> list2sum acc
        | r1::summands ->
                let r = list2sum (List.rev_append summands acc) in
                if less r1 r
                then aux summands acc
                else aux summands (r1::acc)
    in

    let rec simplify_aux r = match r with
        | Zero | One | Symb(_) | Var(_) -> r
        | Product(r1, r2) -> Product(simplify_aux r1, simplify_aux r2)
        | Star(r) -> Star(simplify_aux r)
        | Neg(r) -> Neg(simplify_aux r)
        | Sum(_,_) ->
                let summands = get_summands r in
                aux summands []
    in
    simplify_aux r

(* vim600:set textwidth=0: *)
