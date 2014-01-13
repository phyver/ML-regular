module type DFAType = sig
    type symbol
    type atomic_state
    type state
    type dfa

    val from_matrix : (state * (symbol * state) list) list ->
                       state ->
                       state list -> dfa

    val get_states : dfa -> state list
    val get_symbols : dfa -> symbol list
    val get_init : dfa -> state
    val is_accepting : dfa -> state -> bool
    val next : dfa -> state -> symbol -> state

    val print : ?show_labels:bool -> dfa -> unit

    val accepts : dfa -> symbol list -> bool

    val reachable : dfa -> dfa
    val make_total : ?symbols:symbol list -> dfa -> dfa
    val accessible : dfa -> dfa
    val minimize : dfa -> dfa

    val complement : ?symbols:symbol list -> dfa -> dfa
    val union : dfa -> dfa -> dfa
    val intersection : dfa -> dfa -> dfa

    val subset : dfa -> dfa -> bool
    val equal : dfa -> dfa -> bool
  end

module Make (Symbol:Misc.OType) (State:Misc.OType)
    : DFAType
    with type symbol = Symbol.t
     and type atomic_state = State.t
     and type state = Misc.GeneralizedState(State).t

