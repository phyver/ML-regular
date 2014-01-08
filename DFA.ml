open Misc


module type OType = sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
end


type ('a,'b) lts = ('a*(('b*'a) list)) list


module type DFAType = sig
    exception Invalid_state
    exception Invalid_symbol
    exception Undefined_transition
    type symbol_universe
    type state_universe
    type dfa
    val actual_states : dfa -> state_universe list
    val actual_symbols : dfa -> symbol_universe list
    val init_state : dfa -> state_universe
    val transition_matrix : dfa -> (state_universe, symbol_universe) lts
    val accepting : dfa -> state_universe -> bool
    val transition : dfa -> state_universe -> symbol_universe -> state_universe
    val total : dfa -> bool
    val from_lts : (state_universe, symbol_universe) lts -> state_universe -> state_universe list -> dfa
    val reachable : dfa -> dfa
    val print : ?show_labels:bool -> dfa -> unit
    val cardinal_states : dfa -> int
    val cardinal_symbols : dfa -> int
    val minimize : dfa -> dfa
    val complement : dfa -> dfa
end


module Make (Symb:OType) (St:OType) = struct

    type symbol_universe = Symb.t
    type state_universe = St.t

    exception Invalid_state
    exception Invalid_symbol
    exception Undefined_transition

    module FSSymb = Set.Make (struct type t=Symb.t let compare = Symb.compare end)
    module FSSt = Set.Make (struct type t=St.t let compare = St.compare end)



    type dfa = {
        init : state_universe                          ;
        matrix : (state_universe,symbol_universe) lts  ;
        accepting : FSSt.t                             ;
        actual_states : FSSt.t                         ;
        actual_symbols : FSSymb.t                      ;
        }

    let actual_states d = FSSt.elements d.actual_states

    let actual_symbols d = FSSymb.elements d.actual_symbols

    let init_state d = d.init

    let from_lts matrix init accepting =
        let states =
            List.fold_left
            (fun acc row ->
                FSSt.add (fst row) (FSSt.union acc (List.fold_left (fun acc at -> FSSt.add (snd at) acc) acc (snd row))))
            FSSt.empty
            matrix
        in
        let symbols =
            List.fold_left
            (fun acc row ->
                FSSymb.union acc (List.fold_left (fun acc at -> FSSymb.add (fst at) acc) acc (snd row)))
            FSSymb.empty
            matrix
        in
        let accepting = List.fold_left (fun acc s -> FSSt.add s acc) FSSt.empty accepting in
        {
            init = init                 ;
            matrix = matrix             ;
            accepting = accepting       ;
            actual_states =  states     ;
            actual_symbols = symbols    ;
        }


    let transition_matrix d = d.matrix

    let accepting d s = FSSt.mem s d.accepting

    let transition d s a =
        if not (List.mem s (actual_states d))
        then raise Invalid_state
        else if not (List.mem a (actual_symbols d))
        then raise Invalid_symbol
        else try
            List.assoc a (List.assoc s d.matrix)
        with Not_found -> raise Undefined_transition

    let cardinal_states d = FSSt.cardinal d.actual_states

    let cardinal_symbols d = FSSymb.cardinal d.actual_symbols

    let total d =
        try
            FSSt.mem d.init d.actual_states &&
            FSSt.for_all
            (fun s ->
                FSSymb.for_all
                (fun a -> List.mem (transition d s a) (actual_states d)
                )
                d.actual_symbols
            )
            d.actual_states
        with Undefined_transition | Invalid_state | Invalid_symbol -> false

    (* use sets instead of lists *)
    let reachable d =
        let rec dfs current visited =
            if FSSt.mem current visited
            then visited
            else FSSymb.fold
                    (fun a visited ->
                        try dfs (transition d current a) (FSSt.add current visited)
                        with Undefined_transition | Invalid_state | Invalid_symbol -> visited)
                    d.actual_symbols
                    visited
        in
        let reachable_states =  dfs d.init FSSt.empty in
        let matrix = List.filter (function s,_ -> FSSt.mem s reachable_states) d.matrix in
        let matrix = List.map (function s, row -> s,List.filter (function _, t -> FSSt.mem t reachable_states) row) matrix in
        {
            init = d.init                                       ;
            matrix = matrix                                     ;
            accepting = FSSt.inter d.accepting reachable_states ;
            actual_states =  reachable_states                   ;
            actual_symbols = d.actual_symbols                   ;
        }

    let print ?(show_labels=false) d =
        let symbols = actual_symbols d in
        let states = actual_states d in
        let width = if show_labels
                    then List.fold_left (fun w s -> max w (String.length (St.to_string s))) 0 states
                    else String.length (string_of_int (List.length states))
        in

        let print_row s =
            (* state *)
            if s = init_state d
            then print_string "-> "
            else print_string "   ";
            if show_labels
            then print_string_w (St.to_string s) width
            else print_string_w (string_of_int (idx s states)) width;
            if accepting d s
            then print_string " -> | "
            else print_string "    | ";

            (* transitions *)
            List.iter
                (fun a ->
                    let t = try
                                if show_labels
                                then St.to_string (transition d s a)
                                else string_of_int (idx (transition d s a) states)
                            with Undefined_transition | Invalid_state | Invalid_symbol -> "!"
                    in
                    print_string_w t (1+width)
                )
                symbols;

            print_newline()
        in

        print_n_char ' ' (6+width);
        print_string " | ";
        List.iter
            (fun a -> print_string_w (Symb.to_string a) (1+width))
            symbols;
        print_newline ();
        print_n_char '-' (9+width+(1+width)*(List.length symbols));

        print_newline();

        List.iter print_row states

    (* sets of pairs of states *)
    module FSSt2 = Set.Make(
        struct
            type t = St.t*St.t
            let compare a b =
                let c = St.compare (fst a) (fst b) in
                if c=0
                then St.compare (snd a) (snd b)
                else c
        end)
    module MapSt = Map.Make(
        struct
            type t = St.t
            let compare = St.compare
        end)


    let minimize d =
        let d = reachable d in

        (* get the set of pairs (x,y) with x>y, where x and y are in l *)
        let pairs = FSSt.fold (fun s1 r1 -> FSSt.fold (fun s2 r2 -> FSSt2.add (max s1 s2, min s1 s2) r2) d.actual_states r1) d.actual_states FSSt2.empty in

        (* xor function: exactly one of a and b is true *)
        let xor a b = (a && not b) || (not a && b) in

        (* "different" is the list of pairs of _surely_ different states,
        * "similar" is the list of pairs of _maybe_ similar states *)
        let different, similar = FSSt2.partition (function x,y -> xor (accepting d x) (accepting d y)) pairs in

        (* at each step, we want to look among the _maybe_ similar state_universe to see of
        * some of them are in fact _surely_ different: this happens when they
        * have _surely_ different neighboors.
        *   - "different" is the current list of _surely_ different states
        *   - "similar" is the current list of _maybe_ similar states we've
        *      already checked at this step
        *   - "tocheck" is the list of _maybe_ similar states we still need to
        *      check at this step
        *   - "change" is a boolean that records if we have changed some pair
        *     from "tocheck" to "different"
        *)
        let one_step different similar tocheck change =
            FSSt2.fold
                (fun xy dsc ->
                    let different, similar, change = dsc in
                    let x, y = xy in
                    let b = FSSymb.exists
                                (fun a ->
                                let xa = transition d x a in
                                let ya = transition d y a in
                                let xya = (max xa ya, min xa ya) in
                                    FSSt2.mem xya different)
                                d.actual_symbols
                    in
                    if b
                    then (FSSt2.add (x,y) different), similar,true
                    else different, (FSSt2.add (x,y) similar), change

                )
                tocheck
                (different, similar, change)
        in

        (* we call the previous function until no more pair from "similar" passes
        * into the list of pairs "different"
        * The list "similar" should then correspond to an equivalence relation *)
        let rec all_steps different similar =
            let different, similar, change = one_step different FSSt2.empty similar false in
            if change
            then all_steps different similar
            else similar
        in

        (* equiv is now the equivalence relation of similarity between states *)
        let equiv = all_steps different similar in

        (*
        let check_is_equiv relation =
            List.iter (fun x ->
            List.iter (fun y ->
            List.iter (fun z ->
                if List.mem (min x y, max x y) relation &&
                List.mem (min x z, max x z) relation
                then if y=z || (List.mem (min y z, max y z) relation)
                    then ()
                    else print_endline ("problem:\n   " ^ x ^ "\n   " ^ y ^ "\n   " ^ z ^ "\n"))
            states)
            states)
            states in
        let _ = check_is_equiv equiv in
        *)

        (* an association list that give, for each state_universe, the smallest element in
         * its equivalence classe *)
        let representants =
            let rec aux equiv map =
                match equiv with
                | [] -> map
                | [(x,y)] -> map
                | (x1,y1)::((x2,y2)::_ as equiv) ->
                        if x1 = x2
                        then aux equiv map
                        else aux equiv (MapSt.add x2 y2 map)
            in
            let lequiv = FSSt2.elements equiv in
            let x1,y1 = List.hd lequiv in
            aux lequiv (MapSt.add x1 y1 MapSt.empty)

        in
        let repr s = MapSt.find s representants in

        (* the states of the quotiented DFA are the representants *)
        let quotient_states = MapSt.fold (fun k v set -> FSSt.add v set) representants FSSt.empty in

        let matrix = List.filter (function s,_ -> FSSt.mem s quotient_states) d.matrix in
        let matrix = List.map (function s, row -> s, List.map (function a, t -> a, repr t) row) matrix in
        let accepting = FSSt.fold (fun s acc -> FSSt.add (repr s) acc) d.accepting FSSt.empty in

        {
            init = repr d.init                                  ;
            matrix = matrix                                     ;
            accepting = accepting ;
            actual_states = quotient_states                     ;
            actual_symbols = d.actual_symbols                   ;
        }


    let complement d =
        {
            init = d.init                                       ;
            matrix = d.matrix                                   ;
            accepting = FSSt.diff d.actual_states d.accepting   ;
            actual_states = d.actual_states                     ;
            actual_symbols = d.actual_symbols                   ;
        }

end



