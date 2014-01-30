(***************************************************************)
(*  Copyright 2014 Pierre Hyvernat. All rights reserved.       *)
(*  This file is distributed under the terms of the            *)
(*  GNU General Public License, described in file COPYING.     *)
(***************************************************************)

open Common

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
    val make_total : ?alphabet:symbol list -> dfa -> dfa
    val collapse: dfa -> dfa
    val minimize : dfa -> dfa

    val complement : ?alphabet:symbol list -> dfa -> dfa
    val union : dfa -> dfa -> dfa
    val intersection : dfa -> dfa -> dfa

    exception Found of symbol list
    val is_empty : ?counterexample:bool -> dfa -> bool
    val subset : ?counterexample:bool -> dfa -> dfa -> bool
    val equal : ?counterexample:bool -> dfa -> dfa -> bool
  end

module Make (Symbol:OType) (State:OType)
    : DFAType
    with type symbol = Symbol.t
     and type atomic_state = State.t
     and type state = GeneralizedState(State).t

