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

(* compute an NFA from a regexp using the inductive construction à la Thomson *)
let rec nfa_from_regexp_inductive r = match r with
    | Zero -> NFA_Regexp.zero_nfa
    | One -> NFA_Regexp.one_nfa
    | Symb(a) -> NFA_Regexp.symbol_nfa a
    | Neg(r) -> raise (Failure "cannot compute the NFA associated to a negated regexp directly")
    | Sum(r1,r2) ->
            let d1 = nfa_from_regexp_inductive r1 in
            let d2 = nfa_from_regexp_inductive r2 in
            NFA_Regexp.union d1 d2
    | Product(r1,r2) ->
            let d1 = nfa_from_regexp_inductive r1 in
            let d2 = nfa_from_regexp_inductive r2 in
            NFA_Regexp.concat d1 d2
    | Star(r) ->
            let d = nfa_from_regexp_inductive r in
            NFA_Regexp.star d

(* transform a regexp into an NFA by computing its derivatives *)
let nfa_from_regexp_derivative (r:regexp) : NFA_Regexp.nfa =

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
                                    let ras = get_summands ra in
                                    let row = (Some(a),List.map (fun r -> Atom(r)) ras)::row in
                                    (row,ras@todo)
                            ) ([],todo) actual_symbols
                        in
                        aux (r::done_states) ((Atom(r),row)::matrix) accepting todo
    in

    let _, matrix, accepting = aux [] [] [] [r] in

    NFA_Regexp.from_matrix matrix [Atom(r)] accepting


(* regexp from nfa *)
(* FIXME: probably not very optimized *)
module IntIntMap = Map.Make(struct type t=int*int let compare=compare end)
type mat = regexp IntIntMap.t

let regexp_from_nfa ?(random=true) aut : regexp =

    let states = NFA_Regexp.get_states aut in
    let symbols = NFA_Regexp.get_symbols aut in
    let id s = idx s states in

    (*
    let print_matrix s matrix =
        print_endline (">>> "^s);
        IntIntMap.iter
            (fun st r -> print_int (fst st) ;
                         print_string " --" ;
                         print_regexp r ;
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
                let ts = NFA_Regexp.next aut s (Some(a)) in
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
    (* we also add the regexp corresponding to epsilon transititions *)
    let matrix =
        List.fold_left (fun matrix s -> let is = id s in
            try
                let ts = NFA_Regexp.next aut s None in
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
        ) matrix (NFA_Regexp.get_init aut)
    in
    let final = -2 in
    let matrix =
        List.fold_left (fun matrix s -> let is = id s in
            IntIntMap.add (is,final) One matrix
        ) matrix (List.filter (NFA_Regexp.is_accepting aut) states)
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


(* the same, but we try it many times and keep the smallest regexp *)
let regexp_from_nfa aut =
    let rec aux n r =
        if n<0
        then r
        else let rr = regexp_from_nfa aut in
             if String.length (string_of_regexp r) < String.length (string_of_regexp rr)
             then aux (n-1) r
             else aux (n-1) rr
    in
    aux 100 (regexp_from_nfa aut)

(* TODO: we seem to get many regexps of the form (1+x)(1+x)* which is equal to x*... Perhaps I could simplify that... *)


(* vim600:set textwidth=0: *)
