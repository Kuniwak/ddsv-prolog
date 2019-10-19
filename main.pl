:- table transit_transitive/8.

deadlock(L_n, X_n, T1_n, T2_n) :-
    init_vars(X, T1, T2),
    transit_transitive('P0', L_n, X, T1, T2, X_n, T1_n, T2_n),
    \+ valid_end(L_n),
    \+ transit(_, L_n, _, X_n, T1_n, T2_n, _, _, _).

transit_transitive(L, L_1, X, T1, T2, X_1, T1_1, T2_1) :- transit(_, L, L_1, X, T1, T2, X_1, T1_1, T2_1).
transit_transitive(L, L_n, X, T1, T2, X_n, T1_n, T2_n) :-
    transit(_, L, L_1, X, T1, T2, X_1, T1_1, T2_1),
    transit_transitive(L_1, L_n, X_1, T1_1, T2_1, X_n, T1_n, T2_n).

write_digraph :-
    writeln("strict digraph {"),
    findall((T, L0, L1), (transit(T, L0, L1, _, _, _, _, _, _), write_edge(T, L0, L1)), _),
    writeln("}").

write_edge(T, L0, L1) :- write("  "), write(L0), write("->"), write(L1), write(" [label = "), write(T), writeln("]").

% ---- Target specific ----

init_vars(0, 0, 0).

location('P0').
location('P1').
location('P3').

valid_end('P3').

transit(T, L0, L1, X, T1, T2, X_1, T1_1, T2_1) :-
    transit_read_1(T, L0, L1, X, T1, T2, X_1, T1_1, T2_1);
    transit_inc_1(T, L0, L1, X, T1, T2, X_1, T1_1, T2_1);
    transit_write_1(T, L0, L1, X, T1, T2, X_1, T1_1, T2_1).

transit_read_1('read_1', 'P0', 'P1', X, _1, T2, X, X, T2).
transit_inc_1('inc_1', 'P1', 'P2', X, T1, T2, X, T1_1, T2) :- T1_1 = T1 + 1.
transit_write_1('write_1', 'P2', 'P3', _, T1, T2, T1, T1, T2).

% vim:ft=prolog
