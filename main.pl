:- table transit_transitive/8.

deadlock(L_n, X_n, T1_n, T2_n) :-
    init_vars(X, T1, T2),
    transit_transitive('P0', L_n, X, T1, T2, X_n, T1_n, T2_n),
    \+ valid_end(L_n),
    \+ transit(L_n, _, X_n, T1_n, T2_n, _, _, _).

transit_transitive(L, L_1, X, T1, T2, X_1, T1_1, T2_1) :- transit(L, L_1, X, T1, T2, X_1, T1_1, T2_1).
transit_transitive(L, L_n, X, T1, T2, X_n, T1_n, T2_n) :-
    transit(L, L_1, X, T1, T2, X_1, T1_1, T2_1),
    transit_transitive(L_1, L_n, X_1, T1_1, T2_1, X_n, T1_n, T2_n).

transit(L0, L1, X, T1, T2, X_1, T1_1, T2_1) :-
    transit_read_1(L0, L1, X, T1, T2, X_1, T1_1, T2_1);
    transit_inc_1(L0, L1, X, T1, T2, X_1, T1_1, T2_1);
    transit_write_1(L0, L1, X, T1, T2, X_1, T1_1, T2_1).

% ---- Target specific ----

init_vars(0, 0, 0).

location('P0').
location('P1').
location('P3').

valid_end('P3').

transit_read_1('P0', 'P1', X, _1, T2, X, X, T2).
transit_inc_1('P1', 'P2', X, T1, T2, X, T1_1, T2) :- T1_1 = T1 + 1.
transit_write_1('P2', 'P3', _, T1, T2, T1, T1, T2).

% vim:ft=prolog
