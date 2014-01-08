open Misc


(***
 *** module for ordered types and "to_string" function
 ***)
module type OType = sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
end


(***
 *** module for labelled transition systems, with utility functions
 ***)
module LTS (Label:OType)(State:OType) = struct

    type state = State.t
    type label = Label.t

    module Row = Map.Make (struct type t = label let compare = Label.compare end)
    type row = state Row.t

    module Matrix = Map.Make (struct type t = state let compare = State.compare end)
    type lts = row Matrix.t

    (* empty LTS *)
    let empty = Matrix.empty

    (* next state *)
    let next (m:lts) (s:state) (l:label) : state = Row.find l (Matrix.find s m)

    (* list of all states appearing in an LTS *)
    let get_states (m:lts) : state list =
        let all = Matrix.fold
                    (fun s row acc1 ->
                        s::(Row.fold (fun l t acc2 -> t::acc2) row acc1)
                    )
                    m
                    []
        in uniq (List.sort State.compare all)

    (* list of all labels appearing in an LTS *)
    let get_labels (m:lts) : label list =
        let all = Matrix.fold
                    (fun s row acc1 ->
                        Row.fold (fun l t acc2 -> l::acc2) row acc1
                    )
                    m
                    []
        in uniq (List.sort Label.compare all)

    (* remove some arcs in an lts *)
    let filter (f:state -> label -> state -> bool) (m:lts) : lts =
        let m = Matrix.mapi
                    (fun s row -> Row.filter (fun l t -> f s l t) row)
                    m
        in
        Matrix.filter (fun s row -> not (Row.is_empty row)) m

    (* add a transition *)
    let add (s:state) (l:label) (t:state) (m:lts) : lts =
        let row = try Matrix.find s m
                  with Not_found -> Row.empty in
        Matrix.add s (Row.add l t row) m

    (* applies a function to states
     * if the function isn't a morphism, the result may be unexpected *)
    let map (f:state -> state) (m:lts) =
        Matrix.fold
            (fun s row m1 -> Row.fold (fun l t m2 -> add (f s) l (f t) m2) row m1)
            m
            Matrix.empty
end


(***
 *** module for working with DFA with single state/symbol types
 ***)
module DFA (Symbol:OType) (State:OType) = struct

    (* types for symbols and states *)
    type symbol = Symbol.t
    type state = State.t

    (* finite sets of symbols and states *)
    module SetSymbols = Set.Make (struct type t=Symbol.t let compare = Symbol.compare end)
    module SetStates = Set.Make (struct type t=State.t let compare = State.compare end)

    (* corresponding labeled transition systems *)
    module LTS = LTS(Symbol)(State)
    type lts = LTS.lts

    (* the actual type for deterministic automata *)
    type dfa = {
        init : state                ;
        matrix : lts                ;
        accepting : SetStates.t     ;
        }


    (* utility: get the set of symbols of the automaton *)
    let get_symbols (d:dfa) : SetSymbols.t =
        List.fold_left (fun acc a -> SetSymbols.add a acc) SetSymbols.empty (LTS.get_labels d.matrix)

    (* utility: get the set of states of the automaton *)
    let get_states (d:dfa) : SetStates.t =
        List.fold_left (fun acc s -> SetStates.add s acc) SetStates.empty (LTS.get_states d.matrix)

    (* utility: convert an LTS, with init state and list of states into an automaton *)
    let from_lts (matrix:lts) (init:state) (accepting:state list) : dfa =
        let accepting = List.fold_left (fun acc s -> SetStates.add s acc) SetStates.empty accepting in
        {
            init = init             ;
            matrix = matrix         ;
            accepting = accepting   ;
        }


    (* check if a state is accepting *)
    let is_accepting (d:dfa) (s:state) : bool = SetStates.mem s d.accepting

    (* next state *)
    let next (d:dfa) (s:state) (a:symbol) : state = LTS.next d.matrix s a

    (* compute the restriction of an automaton to its reachable states *)
    let reachable (d:dfa) : dfa =
        (* set of symbols used in the automaton *)
        let actual_symbols = get_symbols d in

        (* depth first search to compute the reachable states of the automaton *)
        let rec dfs current visited =
            if SetStates.mem current visited
            then visited
            else SetSymbols.fold
                    (fun a visited ->
                        try dfs (next d current a) (SetStates.add current visited)
                        with Not_found -> visited)
                    actual_symbols
                    visited
        in
        let reachable_states = dfs d.init SetStates.empty in

        (* we remove the transition that are not reachable
         * Note that it is not necessary to check reachability of the source
         * and target: either they both are reachable, or none of them is *)
        let matrix = LTS.filter (fun s a t -> SetStates.mem s reachable_states) d.matrix
        in
        {
            init = d.init                                       ;
            matrix = matrix                                     ;
            accepting = SetStates.inter d.accepting reachable_states ;
        }

    (* print the automaton in table form
     * We can choose to show the labels as string, or simply with their number
     * with the (optional) argument show_labels *)
    let print ?(show_labels=false) (d:dfa) : unit =
        (* sets of symbols and states of the automaton *)
        let actual_symbols = SetSymbols.elements (get_symbols d) in
        let actual_states = SetStates.elements (get_states d) in

        (* width of the largest state, necessary to align columns
         * we suppose that symbols are smaller than states *)
        let width = if show_labels
                    then List.fold_left (fun w s -> max w (String.length (State.to_string s))) 0 actual_states
                    else String.length (string_of_int (List.length actual_states))
        in

        (* print a single row of the automaton *)
        let print_row s =
            (* the source state *)
            if s = d.init
            then print_string "-> "
            else print_string "   ";
            if show_labels
            then print_string_w (State.to_string s) width
            else print_string_w (string_of_int (idx s actual_states)) width;
            if is_accepting d s
            then print_string " -> | "
            else print_string "    | ";

            (* all the transitions *)
            List.iter
                (fun a ->
                    let t = try if show_labels
                                then State.to_string (next d s a)
                                else string_of_int (idx (next d s a) actual_states)
                            with Not_found -> "!"
                    in
                    print_string_w t (1+width)
                )
                actual_symbols;

            (* we've finished the row *)
            print_newline()
        in

        (* the first row of the table *)
        print_n_char ' ' (6+width);
        print_string " | ";
        List.iter
            (fun a -> print_string_w (Symbol.to_string a) (1+width))
            actual_symbols;
        print_newline ();

        (* a line separating the first row from the actual data *)
        print_n_char '-' (9+width+(1+width)*(List.length actual_symbols));
        print_newline();

        (* we call the row printing function for all states *)
        List.iter print_row actual_states


    (* we will need relation, i.e. sets of pairs of states *)
    module Rel = Set.Make(
        struct
            type t = State.t*State.t
            let compare a b =
                let c = State.compare (fst a) (fst b) in
                if c=0
                then State.compare (snd a) (snd b)
                else c
        end)

    (* we will also need a way to pick a representant of equivalence classes
     * we use a map for that *)
    module MapSt = Map.Make(
        struct
            type t = State.t
            let compare = State.compare
        end)


    (* compute the minimal automaton *)
    let minimize (d:dfa) : dfa =
        (* we first restrict to reachable states *)
        let d = reachable d in

        (* the set of pairs (x,y) with x>=y, where x and y are states *)
        let all_pairs =
            SetStates.fold
                (fun s1 r1 -> SetStates.fold
                                (fun s2 r2 -> Rel.add (max s1 s2, min s1 s2) r2)
                                (get_states d)
                                r1
                )
                (get_states d)
                Rel.empty
        in

        (* "different" is the list of pairs of _surely_ different states,
        * "similar" is the list of pairs of _maybe_ similar states *)
        let different, similar =
            Rel.partition (function x,y -> xor (is_accepting d x) (is_accepting d y))
                          all_pairs
        in

        (* at each step, we want to look among the _maybe_ similar states to see of
        * some of them are in fact _surely_ different: this happens when they
        * have _surely_ different neighboors.
        *   - "different" is the current set of _surely_ different states
        *   - "similar" is the current set of _maybe_ similar states we've
        *      already checked at this step
        *   - "tocheck" is the set of _maybe_ similar states we still need to
        *      check at this step
        *   - "change" is a boolean that records if we have changed some pair
        *     from "tocheck" to "different"
        *)
        let one_step different similar tocheck change =
            Rel.fold
                (fun xy dsc ->
                    let x, y = xy in
                    let different, similar, change = dsc in
                    let b = SetSymbols.exists
                                (fun a ->
                                let xa = next d x a in
                                let ya = next d y a in
                                let xya = (max xa ya, min xa ya) in
                                    Rel.mem xya different)
                                (get_symbols d)
                    in
                    if b
                    then (Rel.add (x,y) different), similar,true
                    else different, (Rel.add (x,y) similar), change
                )
                tocheck
                (different, similar, change)
        in

        (* we call the previous function until no more pair passes from "similar"
        * to "different"
        * The list "similar" should then correspond to an equivalence relation *)
        let rec all_steps different similar =
            let different, similar, change = one_step different Rel.empty similar false in
            if change
            then all_steps different similar
            else similar
        in

        (* we can now compute the equivalence relation of similarity between states *)
        let equiv : Rel.t = all_steps different similar in

        (* to make life easier, we now associate to each state a canonical
         * representant (the smallest in the equivalent class)
         * we use a map for that purpose *)
        let representants : state MapSt.t =
            (* the argument "equiv" is the _list_ of equivalent pairs (x,y)
             * with x>=y, in lexicographic order
             * we just need to look through the list and associate to each x
             * the first y appearing next to x in this list:
             * [ (1,1) ; (2,1) ; (2,2) ; (2,3) ; (2,4) ; (3,2) ; ... ]
             * gives 1->1, 2->1, 3->2 ... *)
            let rec aux equiv acc =
                match equiv with
                | [] -> acc
                | [(x,y)] -> acc
                | (x1,y1)::((x2,y2)::_ as equiv) ->
                        if x1 = x2
                        then aux equiv acc
                        else aux equiv (MapSt.add x2 y2 acc)
            in
            (* the list of equivalent elements, sorted *)
            let pairs_equiv = Rel.elements equiv in

            (* the "aux" function does't get the first pair
             * we thus initialize the accumulator with it *)
            let x1,y1 = List.hd pairs_equiv in
            aux pairs_equiv (MapSt.add x1 y1 MapSt.empty)
        in

        (* we can now easily compute a representent for any state *)
        let repr (s:state) : state = MapSt.find s representants in

        (* we collapse the automaton using this *)
        let matrix = LTS.map repr d.matrix in
        (* we also replace each accepting state by its representing,
         * effectively removing all states that are not equal to their representant *)
        let accepting =
            SetStates.fold (fun s acc -> SetStates.add (repr s) acc)
                           d.accepting
                           SetStates.empty
        in
        {
            init = repr d.init      ;
            matrix = matrix         ;
            accepting = accepting   ;
        }

    (* complement of an automaton
     * we just change the accepting states *)
    let complement d =
        {
            init = d.init                                           ;
            matrix = d.matrix                                       ;
            accepting = SetStates.diff (get_states d) d.accepting   ;
        }

end



