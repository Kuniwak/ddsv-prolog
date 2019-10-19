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
    writeln("}").
write_composition_edge(T, Ls0, Ls1, Vs0, Vs1) :-
    write("  "),
    write_state(Ls0, Vs0),
    write(" -> "),
    write_state(Ls1, Vs1),
    format(" [label = ~q]\n", [T]).
write_state(Ls, Vs) :- write_atoms(Ls, "_"), write("_"), write_atoms(Vs, "_").

write_thread :-
    writeln("strict digraph {"),
    findall(_, (transit(T, L0, L1, _, _), write_edge(T, L0, L1)), _),
    writeln("}").
write_edge(T, L0, L1) :- format("  ~w -> ~w [label = ~q]\n", [L0, L1, T]).
write_atoms([X0,X1|Xs], C) :- write(X0), write(C), write_atoms([X1|Xs], C).
write_atoms([X], _) :- write(X).

% ---- Target specific ----
init_locations(['P0', 'Q0']).
init_vars([0, 0, 0]).

transit(T, L0, L1, Vs, Vs_1) :-
    transit_read_1(T, L0, L1, Vs, Vs_1);
    transit_inc_1(T, L0, L1, Vs, Vs_1);
    transit_write_1(T, L0, L1, Vs, Vs_1);
    transit_read_2(T, L0, L1, Vs, Vs_1);
    transit_inc_2(T, L0, L1, Vs, Vs_1);
    transit_write_2(T, L0, L1, Vs, Vs_1).

transit_read_1('read_1', 'P0', 'P1', [X, _, T2], [X, X, T2]).
transit_inc_1('inc_1', 'P1', 'P2', [X, T1, T2], [X, T1_1, T2]) :- plus(T1, 1, T1_1).
transit_write_1('write_1', 'P2', 'P3', [_, T1, T2], [T1, T1, T2]).

transit_read_2('read_2', 'Q0', 'Q1', [X, T1, _], [X, T1, X]).
transit_inc_2('inc_2', 'Q1', 'Q2', [X, T1, T2], [X, T1, T2_1]) :- plus(T2, 1, T2_1).
transit_write_2('write_2', 'Q2', 'Q3', [_, T1, T2], [T2, T1, T2]).

% vim:ft=prolog
