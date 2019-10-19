# Toy Deadlock Finder
## Usage
### Model
```console
$ edit ./main.pl
```

```prolog
...

% ---- Target specific ----
init_locations(['P0', 'Q0']).
init_vars([0, 0]).

transit(T, L0, L1, Vs, Vs_1) :-
    transit_Foo(T, L0, L1, Vs, Vs_1);
    transit_Bar(T, L0, L1, Vs, Vs_1).

transit_Foo('Foo', 'P0', 'P1', [0, M2], [1, M2]).
transit_Bar('Bar', 'P1', 'P2', [M1, 0], [M1, 1]).
```



### Visualize
```
$ ./viz-composition
strict digraph {
  P3_Q0_0_1 -> P0_Q0_0_0 [label=unlock_1_P]
  P2_Q0_1_1 -> P3_Q0_0_1 [label=unlock_0_P]
}
```

You can view state diagram using Graphviz facilities such as [dreampuf/GraphvizOnline](https://dreampuf.github.io/GraphvizOnline/).



## Requirements
- SWI-Prolog 8.0.2



# Acknowledges
- [Implement Deadlock Finder To Learn Multi-Thread Programming](https://principia.connpass.com/event/143181/)