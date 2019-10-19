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
    format(" [label=~w]\n", [T]).
write_state(Ls, Vs) :- write_atoms(Ls, "_"), write("_"), write_atoms(Vs, "_").

write_thread :-
    writeln("strict digraph {"),
    findall(_, (transit(T, L0, L1, _, _), write_edge(T, L0, L1)), _),
    writeln("}").
write_edge(T, L0, L1) :- format("  ~w -> ~w [label=~w]\n", [L0, L1, T]).
write_atoms([X0,X1|Xs], C) :- write(X0), write(C), write_atoms([X1|Xs], C).
write_atoms([X], _) :- write(X).

% ---- Target specific ----
init_locations(['P0', 'Q0']).
init_vars([0, 0]).

transit(T, L0, L1, Vs, Vs_1) :-
    transit_Foo(T, L0, L1, Vs, Vs_1);
    transit_Bar(T, L0, L1, Vs, Vs_1).

transit_Foo('Foo', 'P0', 'P1', [0, M2], [1, M2]).
transit_Bar('Bar', 'P1', 'P2', [M1, 0], [M1, 1]).

% vim:ft=prolog
