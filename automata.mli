module type OType = sig
    type t
    val compare : t -> t -> int
    val to_string : t -> string
end

type 'a generalized_state =
  | Dummy
  | Atom of 'a
  | In of int * 'a generalized_state
  | Pair of 'a generalized_state * 'a generalized_state
  | FSet of 'a generalized_state list

module GeneralizedState (State:OType)
    : OType
    with type t=State.t generalized_state


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
    val from_lts : (atomic_state * (symbol * atomic_state) list) list ->
                   atomic_state ->
                   atomic_state list -> dfa
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

