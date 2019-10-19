:- table transit_composition_transitive/4.

% TODO: Valid end state

run(Ls_n, Vs_n) :-
    init_locations(Ls_0),
    init_vars(Vs_0),
    transit_composition_transitive(Ls_0, Ls_n, Vs_0, Vs_n).

deadlock(Ls_n, Vs_n) :-
    reachable(Ls_n, Vs_n),
    \+ transit_composition(_, Ls_n, _, Vs_n, _).

reachable(Ls_n, Vs_n) :-
    init_locations(Ls_0),
    init_vars(Vs_0),
    transit_composition_transitive(Ls_0, Ls_n, Vs_0, Vs_n).

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
    findall((T, Ls0), (reachable(Ls0, _), transit_composition(T, Ls0, Ls1, _, _), write_composition_edge(T, Ls0, Ls1)), _),
    writeln("}").

write_thread :-
    writeln("strict digraph {"),
    findall((T, L0, L1), (transit(T, L0, L1, _, _), write_edge(T, L0, L1)), _),
    writeln("}").

write_composition_edge(T, Ls0, Ls1) :- write("  "), write_composition_node(Ls0), write("->"), write_composition_node(Ls1), write(" [label = "), write(T), writeln("]").
write_composition_node(Ls) :- findall(L, (member(L, Ls), write(L), write("_")), _).
write_edge(T, L0, L1) :- write("  "), write(L0), write("->"), write(L1), write(" [label = "), write(T), writeln("]").

% ---- Target specific ----
init_locations(['P0', 'Q0']).
init_vars([0, 0, 0]).

transit(T, L0, L1, Vs, Vs_1) :-
    transit_read_1(T, L0, L1, Vs, Vs_1);
    transit_inc_1(T, L0, L1, Vs, Vs_1);
    transit_write_1(T, L0, L1, Vs, Vs_1).

transit_read_1('read_1', 'P0', 'P1', [X, _, T2], [X, X, T2]).
transit_inc_1('inc_1', 'P1', 'P2', [X, T1, T2], [X, T1+1, T2]).
transit_write_1('write_1', 'P2', 'P3', [_, T1, T2], [T1, T1, T2]).

% vim:ft=prolog
