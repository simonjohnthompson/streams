-module(memo).
-compile(export_all).

% Experiments in memoisation and macros.

setup() ->
  ets:new(tab,[named_table]).

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

fibM(0) -> 0;
fibM(1) -> 1;
fibM(N) ->
    case ets:lookup(tab,N) of
        [] -> V = fibM(N-1) + fibM(N-2),
              ets:insert(tab,{N,V}),
              V;
        [{N,V}] -> V
    end.
            

% meomising in the data: 
% vectors are lists with an accompnying length.

% computing length is trival
% other functions have shadow definitions tracking length
% correach new function has macro to extract the value

-type vector(T) :: {integer(),list(T)}.

-define(mkV(Xs),{length(Xs),Xs}).

-define(length(V),element(1,V)).

-spec joinV(T,vector(T)) -> vector(T).

joinV(Sep,{M,Xs}) -> {2*M-1,lists:join(Sep,Xs)}.

-define(join(Sep,V),element(2,joinV(Sep,V))).




