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

    val accepts : dfa -> symbol list -> bool
    val from_matrix : (state * (symbol * state) list) list ->
                       state ->
                       state list -> dfa

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

module Make (Symbol:Misc.OType) (State:Misc.OType)
    : DFAType
    with type symbol = Symbol.t
     and type atomic_state = State.t
     and type state = Misc.GeneralizedState(State).t

