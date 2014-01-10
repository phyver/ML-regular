open Misc


(***
 *** module for ordered types and "to_string" function
 ***)
module type OType = sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
end


(* we will need states to be closed under indexing, pairing and finite sets *)
type 'a generalized_state =
    | Dummy
    | Atom of 'a
    | In of int*'a generalized_state
    | Pair of 'a generalized_state * 'a generalized_state
    | FSet of 'a generalized_state list

module GeneralizedState (State:OType)
  : OType with type t=State.t generalized_state
  = struct
    type t = State.t generalized_state

    let rec list_compare cmp l1 l2 = match l1,l2 with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> +1
        | a1::l1, a2::l2 ->
                let c = cmp a1 a2 in
                if c = 0
                then list_compare cmp l1 l2
                else c

    let rec compare s1 s2 = match s1,s2 with
        | Dummy, Dummy -> 0
        | Dummy, _ -> -1
        | _, Dummy -> +1
        | Atom(a1), Atom(a2) -> State.compare a1 a2
        | Atom(_), _ -> -1
        | _, Atom(_) -> +1
        | In(n1,s1), In(n2,s2) ->
                if n1<n2 then -1
                else if n1>n2 then +1
                else compare s1 s2
        | In(_), _ -> -1
        | _,In(_) -> +1
        | Pair(s1,s2), Pair(p1,p2) ->
                let c = compare s1 p1 in
                if c = 0
                then compare s2 p2
                else c
        | Pair(_), _ -> -1
        | _, Pair(_) -> +1
        | FSet(l1), FSet(l2) ->
                list_compare compare l1 l2

    let rec to_string = function
        | Dummy -> "."
        | Atom(a) -> State.to_string a
        | In(n,s) -> (string_of_int n) ^ ":" ^ (to_string s)
        | Pair(s1,s2) -> "<" ^ (to_string s1) ^ " , " ^ (to_string s2) ^ ">"
        | FSet(l) ->
                begin
                    let lstr = List.map to_string l in
                    match lstr with
                    | [] -> "{}"
                    | hd::tl ->
                            let str = List.fold_right
                                        (fun s ss -> " , " ^ s ^ ss)
                                        tl
                                        ""
                            in
                            "{" ^ hd ^ str ^ "}"
                end
end


(***
 *** module for labelled transition systems, with utility functions
 ***)
module type LTSType = sig
    type state
    type label
    type lts
    val get_states : lts -> state list
    val get_labels : lts -> label list
    val next : lts -> state -> label -> state
    val empty : lts
    val add : state -> label -> state -> lts -> lts
    val fold : (state -> label -> state -> 'a -> 'a) -> lts -> 'a -> 'a
    val filter : (state -> label -> state -> bool) -> lts -> lts
    val map : (state -> state) -> lts -> lts
end

module LTS (Label:OType)(State:OType)
 : LTSType with type state = State.t
            and type label = Label.t
 = struct
    module Row = Map.Make (struct type t = Label.t let compare = Label.compare end)
    module Matrix = Map.Make (struct type t = State.t let compare = State.compare end)

    type state = State.t
    type label = Label.t
    type row = state Row.t
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

    (* fold *)
    let fold (f:state -> label -> state -> 'a -> 'a) (m:lts) (o:'a) : 'a =
        Matrix.fold
            (fun s row acc1 -> Row.fold (fun l t acc2 -> f s l t acc2) row acc1) m o

    (* applies a function to states
     * if the function isn't a morphism, the result may be unexpected *)
    let map (f:state -> state) (m:lts) =
        fold (fun s l t m -> add (f s) l (f t) m) m Matrix.empty
end


(***
 *** module for working with DFA with single atomic_state/symbol types
 ***)
module type DFAType = sig
        type symbol
        type atomic_state
        type state
        type dfa

        val get_states : dfa -> state list
        val get_symbols : dfa -> symbol list
        val get_init : dfa -> state
        val is_accepting : dfa -> state -> bool
        val next : dfa -> state -> symbol -> state

        type lts
        val from_lts : lts -> atomic_state -> atomic_state list -> dfa

        val print : ?show_labels:bool -> dfa -> unit

        val reachable : dfa -> dfa
        val totalify : dfa -> dfa
        val minimize : dfa -> dfa
        val complement : dfa -> dfa
        val union : dfa -> dfa -> dfa
        val intersection : dfa -> dfa -> dfa
        val subset : dfa -> dfa -> bool
        val equal : dfa -> dfa -> bool
end

module DFA (Symbol:OType) (State:OType)
  : DFAType
  with type symbol = Symbol.t
   and type atomic_state = State.t
   and type state = GeneralizedState(State).t
   and type lts = LTS(Symbol)(GeneralizedState(State)).lts
  = struct

    module GState = GeneralizedState(State)
    module SetSymbols = Set.Make (struct type t=Symbol.t let compare = Symbol.compare end)
    module SetStates = Set.Make (struct type t=GState.t let compare = GState.compare end)
    module LTS = LTS(Symbol)(GeneralizedState(State))

    type symbol = Symbol.t
    type atomic_state = State.t
    type state = GState.t
    type lts = LTS.lts

    (* the actual type for deterministic automata *)
    type dfa = {
        init : state               ;
        matrix : lts                ;
        accepting : SetStates.t     ;
        symbols : SetSymbols.t      ;
        }


    (* utility: get the set of symbols of the automaton *)
    let get_symbols (d:dfa) : symbol list = SetSymbols.elements d.symbols

    (* utility: get the set of states of the automaton *)
    let get_states (d:dfa) : state list =
        let rec aux l acc = match l with
        | [] -> List.rev (d.init::acc)
        | s::_ when s=d.init -> raise Exit
        | s::_ when s>d.init -> raise Exit
        | s::l -> aux l (s::acc)
        in
        let states = LTS.get_states d.matrix in
        try
            aux states []
        with Exit -> states

    (* utility: get initial state *)
    let get_init (d:dfa) : state = d.init

    (* utility: convert an LTS, with init atomic_state and list of states into an automaton *)
    let from_lts (matrix:lts) (init:atomic_state) (accepting:atomic_state list) : dfa =
        let accepting = List.fold_left (fun acc s -> SetStates.add (Atom(s)) acc) SetStates.empty accepting in
        let symbols = List.fold_left (fun acc a -> SetSymbols.add (a) acc) SetSymbols.empty (LTS.get_labels matrix) in
        {
            init = Atom(init)       ;
            matrix = matrix         ;
            accepting = accepting   ;
            symbols = symbols       ;
        }

    (* check if a atomic_state is accepting *)
    let is_accepting (d:dfa) (s:state) : bool = SetStates.mem s d.accepting

    (* next atomic_state *)
    let next (d:dfa) (s:state) (a:symbol) : state = LTS.next d.matrix s a

    (* compute the restriction of an automaton to its reachable states *)
    let reachable (d:dfa) : dfa =
        (* set of symbols used in the automaton *)
        let actual_symbols = get_symbols d in

        (* depth first search to compute the reachable states of the automaton *)
        let rec dfs current visited =
            if SetStates.mem current visited
            then visited
            else List.fold_left
                    (fun visited a ->
                        try dfs (next d current a) (SetStates.add current visited)
                        with Not_found -> visited)
                    visited
                    actual_symbols
        in
        let reachable_states = dfs (get_init d) SetStates.empty in

        (* we remove the transition that are not reachable
         * Note that it is not necessary to check reachability of the source
         * and target: either they both are reachable, or none of them is *)
        let matrix = LTS.filter (fun s a t -> SetStates.mem s reachable_states) d.matrix
        in
        {
            init = get_init d                                       ;
            matrix = matrix                                     ;
            accepting = SetStates.inter d.accepting reachable_states ;
            symbols = d.symbols;
        }

    (* print the automaton in table form
     * We can choose to show the labels as string, or simply with their number
     * with the (optional) argument show_labels *)
    let print ?(show_labels=false) (d:dfa) : unit =
        (* sets of symbols and states of the automaton *)
        let actual_symbols = get_symbols d in
        let actual_states = get_states d in

        (* width of the largest atomic_state, necessary to align columns
         * we suppose that symbols are smaller than states *)
        let width = if show_labels
                    then List.fold_left (fun w s -> max w (String.length (GState.to_string s))) 1 actual_states
                    else String.length (string_of_int (List.length actual_states))
        in

        (* print a single row of the automaton *)
        let print_row s =
            (* the source atomic_state *)
            if s = get_init d
            then print_string "-> "
            else print_string "   ";
            if show_labels
            then print_string_w (GState.to_string s) width
            else print_string_w (string_of_int (idx s actual_states)) width;
            if is_accepting d s
            then print_string " -> | "
            else print_string "    | ";

            (* all the transitions *)
            List.iter
                (fun a ->
                    let t = try if show_labels
                                then GState.to_string (next d s a)
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
            type t = GState.t*GState.t
            let compare a b =
                let c = GState.compare (fst a) (fst b) in
                if c=0
                then GState.compare (snd a) (snd b)
                else c
        end)

    (* we will also need a way to pick a representant of equivalence classes
     * we use a map for that *)
    module MapSt = Map.Make(
        struct
            type t = GState.t
            let compare = GState.compare
        end)


    let is_defined d s a = try ignore (next d s a) ; true
                           with Not_found -> false

    (* check if a dfa is total *)
    let is_total (d:dfa) : bool =
        let symbols = get_symbols d in
        let states = get_states d in
            List.for_all (fun s ->
            List.for_all (fun a ->
                is_defined d s a
            ) symbols) states


    (* render a dfa total *)
    let totalify (d:dfa) : dfa =
        let symbols = get_symbols d in
        let states = get_states d in
        (* we rename all the existing states *)
        let matrix = LTS.map (fun s -> In(0,s)) d.matrix in

        (* we define a new, different state *)
        let new_state = In(1,Dummy) in

        (* we add loops around this state *)
        let matrix = List.fold_left (fun matrix a -> LTS.add new_state a new_state matrix) matrix symbols in

        (* we replace non-existant transitions by transitions to this new state *)
        let matrix =
            List.fold_left (fun matrix1 s ->
            List.fold_left (fun matrix2 a -> if is_defined d s a then matrix2 else LTS.add (In(0,s)) a new_state matrix2)
              matrix1 symbols)
              matrix states
        in
        {
            init = In(0,d.init)       ;
            matrix = matrix         ;
            accepting = SetStates.fold (fun s acc -> SetStates.add (In(0,s)) acc) d.accepting SetStates.empty ;
            symbols = d.symbols       ;
        }


    (* compute the minimal automaton *)
    let minimize (d:dfa) : dfa =
        (* we first restrict to reachable states *)
        let d = reachable d in

        (* the set of pairs (x,y) with x>=y, where x and y are states *)
        let all_pairs =
            let actual_states = get_states d in
            List.fold_left
                (fun r1 s1 -> List.fold_left
                                (fun r2 s2 -> Rel.add (max s1 s2, min s1 s2) r2)
                                r1
                                actual_states
                )
                Rel.empty
                actual_states
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
                    let b = List.exists
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

        (* to make life easier, we now associate to each atomic_state a canonical
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
            match pairs_equiv with
            | [] -> MapSt.empty (* no states are similar, the result should be
            empty *)
            | (x1,y1)::_ -> aux pairs_equiv (MapSt.add x1 y1 MapSt.empty)
        in

        (* we can now easily compute a representent for any atomic_state *)
        let repr (s:state) : state = MapSt.find s representants in

        (* we collapse the automaton using this *)
        let matrix = LTS.map repr d.matrix in
        (* we also replace each accepting atomic_state by its representing,
         * effectively removing all states that are not equal to their representant *)
        let accepting =
            if MapSt.is_empty representants
            then SetStates.empty
            else SetStates.fold (fun s acc -> SetStates.add (repr s) acc)
                                d.accepting
                                SetStates.empty
        in
        {
            init = (try repr (get_init d) with Not_found -> (get_init d))    ;
            matrix = matrix         ;
            accepting = accepting   ;
            symbols = d.symbols     ;
        }

    (* complement of an automaton
     * we just change the accepting states *)
    let complement d =
        let states = List.fold_left (fun acc s -> SetStates.add s acc) SetStates.empty (get_states d) in
        {
            init = get_init d                                           ;
            matrix = d.matrix                                       ;
            accepting = SetStates.diff states d.accepting   ;
            symbols = d.symbols ;
        }

    let intersection (d1:dfa) (d2:dfa) : dfa =
        let states1 = get_states d1 in
        let states2 = get_states d2 in
        let symbols = uniq (List.merge Symbol.compare (get_symbols d2) (get_symbols d1)) in
        let matrix : lts =
            List.fold_left
                (fun matrix1 s1 ->
                    List.fold_left
                        (fun matrix2 s2 ->
                            List.fold_left
                                (fun matrix3 a ->
                                    try
                                        let sa1 = next d1 s1 a in
                                        let sa2 = next d2 s2 a in
                                        (LTS.add (Pair(s1,s2)) a (Pair(sa1,sa2)) matrix3)
                                    with Not_found -> matrix3
                                )
                                matrix2
                                symbols)
                        matrix1
                        states2)
                LTS.empty
                states1
        in

        (* cartesian product of the accepting states *)
        let accepting =
            let accepting1 = SetStates.elements (d1.accepting) in
            let accepting2 = SetStates.elements (d2.accepting) in
            List.fold_left
                (fun acc1 s1 ->
                    List.fold_left
                        (fun acc2 s2 -> SetStates.add (Pair(s1,s2)) acc2)
                        acc1
                        accepting2
                )
                SetStates.empty
                accepting1
        in

        {
            init = Pair(get_init d1, get_init d2)   ;
            matrix = matrix                 ;
            accepting = accepting           ;
            symbols = SetSymbols.union d1.symbols d2.symbols ;
        }

    let union (d1:dfa) (d2:dfa) : dfa =
        let states1 = get_states d1 in
        let states2 = get_states d2 in
        let symbols = uniq (List.merge Symbol.compare (get_symbols d2) (get_symbols d1)) in
        let matrix : lts =
            List.fold_left
                (fun matrix1 s1 ->
                    List.fold_left
                        (fun matrix2 s2 ->
                            List.fold_left
                                (fun matrix3 a ->
                                    try
                                        let sa1 = next d1 s1 a in
                                        let sa2 = next d2 s2 a in
                                        (LTS.add (Pair(s1,s2)) a (Pair(sa1,sa2)) matrix3)
                                    with Not_found -> matrix3
                                )
                                matrix2
                                symbols)
                        matrix1
                        states2)
                LTS.empty
                states1
        in

        let accepting =
            let accepting1 = SetStates.elements (d1.accepting) in
            List.fold_left
                (fun acc1 s1 ->
                    List.fold_left
                        (fun acc2 s2 -> SetStates.add (Pair(s1,s2)) acc2)
                        acc1
                        states2
                )
                SetStates.empty
                accepting1
        in
        let accepting =
            let accepting2 = SetStates.elements (d2.accepting) in
            List.fold_left
                (fun acc1 s1 ->
                    List.fold_left
                        (fun acc2 s2 -> SetStates.add (Pair(s1,s2)) acc2)
                        acc1
                        accepting2
                )
                accepting
                states1
        in

        {
            init = Pair(get_init d1, get_init d2)   ;
            matrix = matrix                 ;
            accepting = accepting           ;
            symbols = SetSymbols.union d1.symbols d2.symbols ;
        }


    let subset (d1:dfa) (d2:dfa) : bool =
        let d1 =
        {
            init = d1.init       ;
            matrix = d1.matrix         ;
            accepting = d1.accepting   ;
            symbols = SetSymbols.union d1.symbols d2.symbols       ;
        }
        in
        let d2 =
        {
            init = d2.init       ;
            matrix = d2.matrix         ;
            accepting = d2.accepting   ;
            symbols = SetSymbols.union d1.symbols d2.symbols       ;
        }
        in
        let d1 = minimize (totalify d1) in
        let cd2 = minimize (complement (totalify d2)) in
        let d = minimize (totalify (intersection d1 cd2)) in
        SetStates.is_empty d.accepting

    let equal (d1:dfa) (d2:dfa) : bool = (subset d1 d2) && (subset d2 d1)

end
