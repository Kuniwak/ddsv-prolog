# Toy Deadlock Finder
## Usage
### Model
```console
$ edit ./main.pl
```

You must write at least 4 predicates on the target specific part:

| Predicate | Description | Example |
|:--|:--|:--|
| `init_locations(Ls).` | Define an initial locations. | `init_locations(['P0', 'Q0']).` |
| `init_vars(Vs).` | Define an initial shared variables. | `init_vars([0, 0]).` |
| `transit(T, L0, L1, Vs0, Vs1).` | Define a transition. The transition can occure if the transition is true. It means, you can write a guard as `Vs0`. <dl><dt><code>T</code></dt><dd>Label for the transition.</dd><dt><code>L0</code></dt><dd>Previous location.</dd><dt><code>L1</code></dt><dd>Next location.</dd><dt><code>Vs0</code></dt><dd>Preious values of shared variables.</dd><dt><code>Vs1</code></dt><dd>Next values of shared variables.</dd></dl> | `transit_Foo('Foo', 'P0', 'P1', [0, M2], [1, M2]).` |

This is an example of a target specific part:

```prolog
...

% ---- Target specific ----
init_locations(['P0', 'Q0']).
init_vars([0, 0]).

transit('Foo', 'P0', 'P1', [0, M2], [1, M2]).
transit('Bar', 'P1', 'P2', [M1, 0], [M1, 1]).
```



### Visualize
```console
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
