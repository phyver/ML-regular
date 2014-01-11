module type NFAType =
  sig
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
  end

module Make (Symbol:Misc.OType) (State:Misc.OType)
  : NFAType
  with type symbol = Symbol.t
   and type atomic_state = State.t
   and type state = Misc.GeneralizedState(State).t
   and type dfa = DFA.Make(Symbol)(State).dfa