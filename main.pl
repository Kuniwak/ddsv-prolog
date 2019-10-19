:- table transit_composition_transitive/4.

% TODO: Valid end state

deadlock(Ls_n, Vs_n) :-
    reachable(Ls_n, Vs_n),
    \+ transit_composition(_, Ls_n, _, Vs_n, _).

reachable(Ls_n, Vs_n) :-
    init_locations(Ls_0),
    init_vars(Vs_0),
    transit_composition_transitive(Ls_0, Ls_n, Vs_0, Vs_n).
reachable(Ls_0, Vs_0) :-
    init_locations(Ls_0),
    init_vars(Vs_0).

transit_composition_transitive(Ls, Ls_1, Vs, Vs_1) :-
    transit_composition(_, Ls, Ls_1, Vs, Vs_1).
transit_composition_transitive(Ls, Ls_n, Vs, Vs_n) :-
    transit_composition(_, Ls, Ls_1, Vs, Vs_1),
    transit_composition_transitive(Ls_1, Ls_n, Vs_1, Vs_n).

transit_composition(T, Ls, Ls_1, Vs, Vs_1) :-
    select(L, Ls, L_1, Ls_1),
    transit(T, L, L_1, Vs, Vs_1).

write_composition :-
    writeln("strict digraph {"),
    findall(_, (reachable(Ls0, Vs0), transit_composition(T, Ls0, Ls1, Vs0, Vs1), write_composition_edge(T, Ls0, Ls1, Vs0, Vs1)), _),
    findall(_, (deadlock(Ls, Vs), write("  "), write_state(Ls, Vs), write("[style=\"solid,filled\", fillcolor=\"#FF7777\"]\n")), _),
    writeln("}").
write_composition_edge(T, Ls0, Ls1, Vs0, Vs1) :-
    write("  "),
    write_state(Ls0, Vs0),
    write(" -> "),
    write_state(Ls1, Vs1),
    format(" [label=~q]\n", [T]).
write_state(Ls, Vs) :- write_atoms(Ls, "_"), write("_"), write_atoms(Vs, "_").

write_thread :-
    writeln("strict digraph {"),
    findall(_, (transit(T, L0, L1, _, _), write_edge(T, L0, L1)), _),
    writeln("}").
write_edge(T, L0, L1) :- format("  ~w -> ~w [label=~q]\n", [L0, L1, T]).
write_atoms([X0,X1|Xs], C) :- write(X0), write(C), write_atoms([X1|Xs], C).
write_atoms([X], _) :- write(X).

% ---- Target specific ----
init_locations(['P0', 'Q0']).
init_vars([0, 0]).

transit(T, L0, L1, Vs, Vs_1) :-
    transit_lock_0_P(T, L0, L1, Vs, Vs_1);
    transit_lock_1_P(T, L0, L1, Vs, Vs_1);
    transit_unlock_0_P(T, L0, L1, Vs, Vs_1);
    transit_unlock_1_P(T, L0, L1, Vs, Vs_1);
    transit_lock_0_Q(T, L0, L1, Vs, Vs_1);
    transit_lock_1_Q(T, L0, L1, Vs, Vs_1);
    transit_unlock_0_Q(T, L0, L1, Vs, Vs_1);
    transit_unlock_1_Q(T, L0, L1, Vs, Vs_1).

transit_lock_0_P('lock_0_P', 'P0', 'P1', [0, M2], [1, M2]).
transit_lock_1_P('lock_1_P', 'P1', 'P2', [M1, 0], [M1, 1]).
transit_unlock_0_P('unlock_0_P', 'P2', 'P3', [1, M2], [0, M2]).
transit_unlock_1_P('unlock_1_P', 'P3', 'P0', [M1, 1], [M1, 0]).
transit_lock_1_Q('lock_1_Q', 'Q0', 'Q1', [M1, 0], [M1, 1]).
transit_lock_0_Q('lock_0_Q', 'Q1', 'Q2', [0, M2], [1, M2]).
transit_unlock_0_Q('unlock_0_Q', 'Q2', 'Q3', [1, M2], [0, M2]).
transit_unlock_1_Q('unlock_1_Q', 'Q3', 'Q0', [M1, 1], [M1, 0]).

% vim:ft=prolog
