module type OType = sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
end

type ('a,'b) lts = ('a*(('b*'a) list)) list

module type DFAType = sig
    type symbol_universe
    type state_universe
    type dfa

    exception Invalid_state
    exception Invalid_symbol
    exception Undefined_transition

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


module Make (Symb:OType) (St:OType) : DFAType with
    type symbol_universe = Symb.t
    and
    type state_universe = St.t

