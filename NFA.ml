open Misc


(****************************************************************************
 *** module for labelled transition systems, with utility functions
 ***)
module type LTSType = sig
(*<<<1*)
    type state
    type label
    module SetStates : Set.S
    type setstates
    type lts
    val get_states : lts -> setstates
    val get_labels : lts -> label list
    val next : lts -> state -> label -> setstates
    val empty : lts
    val add : state -> label -> state -> lts -> lts
    val fold : (state -> label -> state -> 'a -> 'a) -> lts -> 'a -> 'a
    val filter : (state -> label -> state -> bool) -> lts -> lts
    val map : (state -> state) -> lts -> lts
end
(*>>>1*)

module OType2OrderedType(T:OType) : Set.OrderedType with type t = T.t = struct
    type t = T.t
    let compare = T.compare
end
module LTS (Label:OType)(State:OType)
 : LTSType with type state = State.t
            and type label = Label.t
            and type setstates = Set.Make(OType2OrderedType(State)).t
(*<<<1*)
 = struct
    module SetStates = Set.Make (OType2OrderedType(State))
    module Row = Map.Make (struct type t = Label.t let compare = Label.compare end)
    module Matrix = Map.Make (struct type t = State.t let compare = State.compare end)

    type state = State.t
    type setstates = SetStates.t
    type label = Label.t
    type row = setstates Row.t
    type lts = row Matrix.t

    (* empty LTS *)
    let empty = Matrix.empty

    (* next state *)
    let next (m:lts) (s:state) (l:label) : setstates =
        Row.find l (Matrix.find s m)

    (* list of all states appearing in an LTS *)
    let get_states (m:lts) : setstates =
        Matrix.fold
            (fun s row acc1 ->
                SetStates.add s (Row.fold (fun l ss acc2 -> SetStates.union ss acc2) row acc1)
            )
            m
            SetStates.empty

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
                    (fun s row -> Row.mapi
                    (fun l ss -> SetStates.filter
                    (fun t -> f s l t)
                    ss) row) m
        in
        Matrix.filter (fun s row -> not (Row.is_empty row)) m

    (* add a transition *)
    let add (s:state) (l:label) (t:state) (m:lts) : lts =
        let row = try Matrix.find s m
                  with Not_found -> Row.empty in
        let ss = try Row.find l row
                 with Not_found -> SetStates.empty in
        Matrix.add s (Row.add l (SetStates.add t ss) row) m

    (* fold *)
    let fold (f:state -> label -> state -> 'a -> 'a) (m:lts) (o:'a) : 'a =
        Matrix.fold
            (fun s row acc1 -> Row.fold
            (fun l ss acc2  -> SetStates.fold
            (fun t acc3     -> f s l t acc3) ss acc2) row acc1) m o

    (* applies a function to states
     * if the function isn't a morphism, the result may be unexpected *)
    let map (f:state -> state) (m:lts) =
        fold (fun s l t m -> add (f s) l (f t) m) m Matrix.empty
end
(*>>>1*)



module type NFAType = sig
(*<<<1*)
    type symbol
    type atomic_state
    type state
    type dfa
    type nfa

    val get_states : nfa -> state list
    val get_symbols : nfa -> symbol list
    val get_init : nfa -> state list
    val is_accepting : nfa -> state -> bool
    val next : nfa -> state -> symbol option -> state list

    val print : ?show_labels:bool -> nfa -> unit

    val zero_nfa : nfa
    val one_nfa : nfa
    val symbol_nfa : symbol -> nfa
    val union : nfa -> nfa -> nfa
    val concat : nfa -> nfa -> nfa
    val star : nfa -> nfa
    val reverse : nfa -> nfa
    val accepts : nfa -> symbol list -> bool

    val from_matrix : (state * (symbol option * (state list)) list) list ->
                      (state list) ->
                      (state list) -> nfa
    val from_dfa : dfa -> nfa
    val to_dfa : nfa -> dfa
(*
    val reachable : nfa -> nfa
    val remove_epsilon : nfa -> nfa
*)
end
(*>>>1*)

module Make (Symbol:OType) (State:OType)
  : NFAType
  with type symbol = Symbol.t
   and type atomic_state = State.t
   and type state = GeneralizedState(State).t
   and type dfa = DFA.Make(Symbol)(State).dfa
(*<<<1*)
 = struct

    module GState = GeneralizedState(State)
    module SetSymbols = Set.Make (OType2OrderedType(Symbol))
    module SetStates = Set.Make (OType2OrderedType(GeneralizedState(State)))
    module SymbolOption = struct
        type t=Symbol.t option
        let compare a b = match a,b with
        | None,None -> 0
        | None,_ -> -1
        | _,None -> +1
        | Some(a),Some(b) -> Symbol.compare a b
        let to_string = function
            | None -> "_"
            | Some(a) -> Symbol.to_string a
    end

    module LTS = LTS(SymbolOption)(GeneralizedState(State))

    type symbol = Symbol.t
    type atomic_state = State.t
    type state = GState.t

    module DFA = DFA.Make(Symbol)(State)
    type dfa = DFA.dfa

    type setstates = SetStates.t
    type setsymbols = SetSymbols.t
    type lts = LTS.lts

    (* the actual type for deterministic automata *)
    type nfa = {
        init : setstates            ;
        matrix : lts                ;
        accepting : setstates       ;
        symbols : setsymbols        ;
        }

    let get_states (aut:nfa) : state list =
        SetStates.elements (LTS.get_states aut.matrix)

    let get_symbols (aut:nfa) : symbol list =
        SetSymbols.elements (aut.symbols)

    let get_init (aut:nfa) : state list =
        SetStates.elements (aut.init)

    let is_accepting (aut:nfa) (s:state) : bool =
        SetStates.mem s aut.accepting

    let next (aut:nfa) (s:state) (a:symbol option) : state list =
        try
            SetStates.elements (LTS.next aut.matrix s a)
        with Not_found -> []

    (* print the automaton in table form
     * We can choose to show the labels as string, or simply with their number
     * with the (optional) argument show_labels *)
    let print ?(show_labels=false) (d:nfa) : unit =
        (* sets of symbols and states of the automaton *)
        let actual_symbols = None::(List.map (fun a->Some(a)) (get_symbols d)) in
        let actual_states = get_states d in

        let state_to_string s =
            if show_labels
            then GState.to_string s
            else (string_of_int (idx s actual_states))
        in

        let set_to_string ss =
            match ss with
            | [] -> "!"
            | s::ss ->
                    (state_to_string s) ^
                    (List.fold_right (fun s str -> "," ^ (state_to_string s) ^ str) ss "")
        in

        (* width of the largest state, necessary to align columns
         * we suppose that symbols are smaller than states *)
        let width_state =
            List.fold_left (fun w s -> max w (String.length (state_to_string s)))
                           1
                           actual_states
        in
        let width_set =
            List.fold_left (fun w s ->
            List.fold_left (fun w a ->
                max (String.length (set_to_string (next d s a)))
                    w) w actual_symbols) 1 actual_states
        in


        (* print a single row of the automaton *)
        let print_row s =
            (* the source atomic_state *)
            if SetStates.mem s d.init
            then print_string "-> "
            else print_string "   ";
            print_string_w (state_to_string s) width_state;
            if is_accepting d s
            then print_string " -> | "
            else print_string "    | ";

            (* all the transitions *)
            List.iter
                (fun a ->
                    let t = set_to_string (next d s a)
                    in
                    print_string_w t (1+width_set)
                )
                actual_symbols;

            (* we've finished the row *)
            print_newline()
        in

        (* the first row of the table *)
        print_string " NFA ";
        print_n_char ' ' (1+width_state);
        print_string " | ";
        List.iter
            (fun a -> print_string_w (SymbolOption.to_string a) (1+width_set))
            actual_symbols;
        print_newline ();

        (* a line separating the first row from the actual data *)
        print_n_char '-' (9+width_state+(1+width_set)*(List.length actual_symbols));
        print_newline();

        (* we call the row printing function for all states *)
        List.iter print_row actual_states

    (* OK *)
    let zero_nfa : nfa =
        {
            init = SetStates.empty          ;
            matrix = LTS.empty              ;
            accepting = SetStates.empty     ;
            symbols = SetSymbols.empty      ;
        }

    let one_nfa : nfa =
        {
            init = SetStates.singleton Dummy        ;
            matrix = LTS.empty                      ;
            accepting = SetStates.singleton Dummy   ;
            symbols = SetSymbols.empty              ;
        }

    (* OK *)
    let symbol_nfa (a:symbol) : nfa =
        {
            init = SetStates.singleton (In(0,Dummy))  ;
            matrix = LTS.add (In(0,Dummy)) (Some(a)) (In(1,Dummy)) LTS.empty        ;
            accepting = SetStates.singleton (In(1,Dummy))                           ;
            symbols = SetSymbols.singleton (a)                                      ;
        }

    let union (d1:nfa) (d2:nfa) : nfa =
        let matrix1 = LTS.map (fun s -> In(1,s)) d1.matrix in
        let matrix = LTS.fold (fun s a t matrix -> LTS.add (In(2,s)) a (In(2,t)) matrix) d2.matrix matrix1 in
        let matrix = List.fold_left (fun matrix i1 -> LTS.add Dummy None (In(1,i1)) matrix) matrix (get_init d1) in
        let matrix = List.fold_left (fun matrix i2 -> LTS.add Dummy None (In(2,i2)) matrix) matrix (get_init d2) in
        let accepting = SetStates.fold (fun s1 acc -> SetStates.add (In(1,s1)) acc) d1.accepting SetStates.empty in
        let accepting = SetStates.fold (fun s2 acc -> SetStates.add (In(2,s2)) acc) d2.accepting accepting in
        {
            init = SetStates.singleton Dummy                        ;
            matrix = matrix                                         ;
            accepting = accepting                                   ;
            symbols = SetSymbols.union (d1.symbols) (d2.symbols)    ;
        }


    let concat (d1:nfa) (d2:nfa) : nfa =
        let matrix1 = LTS.map (fun s -> In(1,s)) d1.matrix in
        let matrix = LTS.fold (fun s a t matrix -> LTS.add (In(2,s)) a (In(2,t)) matrix) d2.matrix matrix1 in
        let matrix = List.fold_left (fun matrix f1 ->
                     List.fold_left (fun matrix i2 ->
                         LTS.add (In(1,f1)) None (In(2,i2)) matrix) matrix (get_init d2)) matrix (SetStates.elements (d1.accepting)) in
        let init = SetStates.fold (fun s acc -> SetStates.add (In(1,s)) acc) d1.init SetStates.empty in
        {
            init = init                                             ;
            matrix = matrix                                         ;
            accepting = SetStates.fold (fun s acc -> SetStates.add (In(2,s)) acc) d2.accepting SetStates.empty                                ;
            symbols = SetSymbols.union (d1.symbols) (d2.symbols)    ;
        }

    let star (d:nfa) : nfa =
        let init = get_init d in
        let accepting = SetStates.elements d.accepting in
        let matrix =
            List.fold_left (fun matrix i ->
            List.fold_left (fun matrix f -> LTS.add f None i matrix) matrix accepting) d.matrix init
        in
        {
            init = d.init               ;
            matrix = matrix             ;
            accepting = d.accepting     ;
            symbols = d.symbols         ;
        }

    let reverse (d:nfa) : nfa =
        let matrix = LTS.fold (fun s a t matrix -> LTS.add t a s matrix) d.matrix LTS.empty in
        {
            init = d.accepting          ;
            matrix = matrix             ;
            accepting = d.init          ;
            symbols = d.symbols         ;
        }

    let epsilon_closure (d:nfa) (ss:setstates) : setstates =
        let rec dfs closure todo =
            match todo with
            | [] -> closure
            | s::todo ->
                    if SetStates.mem s closure
                    then dfs closure todo
                    else
                        let closure = SetStates.add s closure in
                        let todo = List.rev_append (next d s None) todo in
                        dfs closure todo
        in
        dfs SetStates.empty (SetStates.elements ss)

    let accepts (d:nfa) (w:symbol list) : bool =
        let rec trans (ss:setstates) (w:symbol list) : setstates =
            match w with
            | [] -> epsilon_closure d ss
            | a::w ->
                let ss = epsilon_closure d ss in
                let after = SetStates.fold
                                (fun s acc -> try SetStates.union (LTS.next d.matrix s (Some(a))) acc
                                              with Not_found -> acc)
                                ss
                                SetStates.empty
                in
                trans after w
        in
        let ss = trans d.init w in
        not (SetStates.is_empty (SetStates.inter d.accepting ss))

    let from_matrix (matrix:(state * (symbol option * (state list)) list) list)
                    (init:state list)
                    (accepting:state list) : nfa
        = let matrix = List.fold_left
                        (fun matrix1 srow ->
                            let s,row = srow in
                            List.fold_left
                                (fun matrix2 at ->
                                    let a,st = at in
                                        List.fold_left (fun matrix3 t -> LTS.add s a t matrix3) matrix2 st
                                )
                                matrix1
                                row

                        )
                        LTS.empty
                        matrix
        in
        let accepting = List.fold_left (fun acc s -> SetStates.add s acc) SetStates.empty accepting in
        let init = List.fold_left (fun acc s -> SetStates.add s acc) SetStates.empty init in
        let symbols = List.fold_left (fun acc a -> match a with Some(a) -> SetSymbols.add (a) acc | None -> acc) SetSymbols.empty (LTS.get_labels matrix) in
        {
            init = init             ;
            matrix = matrix         ;
            accepting = accepting   ;
            symbols = symbols       ;
        }


    let from_dfa (d:dfa) : nfa =
        let states = DFA.get_states d in
        let symbols = DFA.get_symbols d in
        let accepting = List.filter (DFA.is_accepting d) states in
        let matrix =
            List.fold_left
                (fun acc1 s ->
                    List.fold_left
                        (fun acc2 a -> try LTS.add s (Some(a)) (DFA.next d s a) acc2
                                        with Not_found -> acc2
                        )
                        acc1
                        symbols
                )
                LTS.empty
                states
        in
        {
            init = SetStates.singleton (DFA.get_init d);
            accepting = List.fold_left (fun acc s -> SetStates.add s acc) SetStates.empty accepting;
            symbols = List.fold_left (fun acc a -> SetSymbols.add a acc) SetSymbols.empty symbols;
            matrix = matrix;
        }

    let to_dfa (d:nfa) : dfa =

        let symbols = get_symbols d in

        let add_matrix s a t m =
            let row = try List.assoc s m
                      with Not_found -> []
            in
            let matrix = List.remove_assoc s m in
            (s,((a,t)::row))::matrix
        in

        let (init:state list) = SetStates.elements (epsilon_closure d d.init) in

        let rec aux (matrix:(state*((symbol*state)list))list)
                    (accepting:state list)
                    (ok:state list list)
                    (todo:state list list) =
            match todo with
            | [] -> matrix, accepting
            | ss::todo ->
                    if List.mem ss ok
                    then aux matrix accepting ok todo
                    else
                        let matrix,todo =
                            List.fold_left
                                (fun mt a -> let matrix,todo = mt in
                                    let next_state : setstates = List.fold_left (fun acc s ->
                                        try SetStates.union acc (LTS.next d.matrix s (Some(a)))
                                        with Not_found -> acc) SetStates.empty ss in
                                    let next_state : setstates = epsilon_closure d next_state in
                                    let next_state : state list = SetStates.elements next_state in
                                    let matrix = add_matrix (FSet(ss)) a (FSet(next_state)) matrix in
                                    let todo = next_state::todo in
                                    matrix, todo
                                )
                                (matrix,todo)
                                symbols
                        in
                        let accepting = if List.exists (fun s -> is_accepting d s) ss
                                        then FSet(ss)::accepting
                                        else accepting
                        in
                        aux matrix accepting (ss::ok) todo

        in

        let matrix, accepting = aux [] [] [] [init]
        in

        DFA.from_matrix matrix (FSet(init)) accepting

end
(*>>>1*)
(* determinisation... *)

(* vim600:set foldmarker=<<<,>>> foldmethod=marker textwidth=0: *)
