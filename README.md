# Erlang: the power of functional programming
Functional programming in Erlang, including streams, lazy evaluation and dependent types.

Code for the presentation at [Erlang/Elixir Factory, Buenos Aires](http://www.erlang-factory.com/eflba2017).

## Notes on different approaches

**lazy.erl** Definitive version of lazy examples in which references stored in a named ETS table. 
References are allocated sequentially, with value at key 0 being the current index used.

**lazy0.erl** Version in which the id of a table is passed explicitly into all functions that might use it.

**lazy2.erl** Incomplete experiment in which use a hashed version of an expression as its address in an ETS table. Unsuccessful: hasing appears to be too eager to work.

**lazy.erl** Definitive version of lazy examples which doesn't use ETS tables or other side-effecting constructs. References are stored in a map, and allocated sequentially, with value at key 0 being the current index used. The map is threaded through the computation.

**memo.erl** Two experiments in memoisation: using ETS and in the data.

**rps.erl** Rock-Paper-Scissors, a basic implementation in Erlang.

**maze.erl** List of routes through a maze: a good candidate for making lazy: want to try it?

**stream.erl** Demand-driven evaluation for streams: first iteration with no attempt to optimise.

