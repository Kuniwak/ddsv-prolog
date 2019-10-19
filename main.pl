:- table transit_composition_transitive/8.

% TODO: SVars = (X_n, T1_n, T2_n)

reachable(Ls_n, X_n, T1_n, T2_n) :-
    init_locations(Ls),
    init_vars(X, T1, T2),
    transit_composition_transitive(Ls, Ls_n, X, T1, T2, X_n, T1_n, T2_n).

deadlock(Ls_n, X_n, T1_n, T2_n) :-
    reachable(Ls_n, X_n, T1_n, T2_n),
    \+ transit_composition(_, Ls_n, _, X_n, T1_n, T2_n, _, _, _).

transit_composition_transitive(Ls, Ls_1, X, T1, T2, X_1, T1_1, T2_1) :-
    transit_composition(_, Ls, Ls_1, X, T1, T2, X_1, T1_1, T2_1).
transit_composition_transitive(Ls, Ls_n, X, T1, T2, X_n, T1_n, T2_n) :-
    transit_composition(_, Ls, Ls_1, X, T1, T2, X_1, T1_1, T2_1),
    transit_composition_transitive(Ls_1, Ls_n, X_1, T1_1, T2_1, X_n, T1_n, T2_n).

transit_composition(T, Ls, Ls_1, X, T1, T2, X_1, T1_1, T2_1) :-
    select(L, Ls, L_1, Ls_1),
    transit(T, L, L_1, X, T1, T2, X_1, T1_1, T2_1).

write_digraph :-
    writeln("strict digraph {"),
    findall((T, Ls0, Ls1), (transit_composition(T, Ls0, Ls1, _, _, _, _, _, _), write_edge(T, Ls0, Ls1)), _),
    writeln("}").

write_edge(T, L0, L1) :- write("  "), write(L0), write("->"), write(L1), write(" [label = "), write(T), writeln("]").

% ---- Target specific ----

init_locations(['P0']).
init_vars(0, 0, 0).

location('P0').
location('P1').
location('P3').

transit(T, L0, L1, X, T1, T2, X_1, T1_1, T2_1) :-
    transit_read_1(T, L0, L1, X, T1, T2, X_1, T1_1, T2_1);
    transit_inc_1(T, L0, L1, X, T1, T2, X_1, T1_1, T2_1);
    transit_write_1(T, L0, L1, X, T1, T2, X_1, T1_1, T2_1).

transit_read_1('read_1', 'P0', 'P1', X, _1, T2, X, X, T2).
transit_inc_1('inc_1', 'P1', 'P2', X, T1, T2, X, T1_1, T2) :- T1_1 = T1 + 1.
transit_write_1('write_1', 'P2', 'P3', _, T1, T2, T1, T1, T2).

% vim:ft=prolog
