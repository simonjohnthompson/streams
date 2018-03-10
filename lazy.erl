-module(lazy).
-compile([export_all]).

% To use, need an ETS table. This does the trick, and names it tab.
% Once named, it doesn't need to be passed around.

setup() ->
  ets:new(tab,[named_table]),
  ets:insert(tab,{0,1}).

% Macro for constructing a new cell.

-define(cons(X,Xs), begin ets:insert(tab,{0,next_ref()+1}) ,
                          ets:insert(tab, {next_ref(), {thunk, fun () -> {X,Xs} end}}), 
                          % io:format("done cons insert~n"), 
                          {ref,next_ref()} end).

% Get the next free cell, assuming that value stored in location 0 
% keeps the locations used so far.

next_ref() ->
   [{0,Ref}]=ets:lookup(tab,0),
   Ref.
  
nil() -> {}.

% Head of a stream.

head({ref,Ref}) ->
  % io:format("entered head~n"),
  case ets:lookup(tab,Ref) of
    [{Ref,{thunk,F}}] -> Val = F(),
                         ets:insert(tab,{Ref,Val}),
                         {H,_} = Val,
                          H;
    [{Ref,{H,_}}] -> % io:format("head success~n"),
                     H
  end.              

% Tail of a stream.

tail({ref,Ref}) ->
    % io:format("entered tail~n"),
  case ets:lookup(tab,Ref) of
    [{Ref,{thunk,F}}] -> Val = F(),
                         ets:insert(tab,{Ref,Val}),
                         {_,T} = Val,
                          T;
    [{Ref,{_,T}}] -> % io:format("tail success~n"),
                     T
  end.              


                 

% Infinite stream of ones.

ones() ->
  ?cons(1,ones()).

% Circular representation of the infinite stream of ones.

onesC() ->
  This = next_ref()+1,
  ?cons(1,{ref,This}).

% Infinite stream of n, n+1, … .

ns(N) ->
  ?cons(N,ns(N+1)).

% Circular version

nsC(N) ->
  This = next_ref()+1,
  ?cons(N,addOne({ref,This})).

addOne(Xs) ->
  ?cons(head(Xs)+1,addOne(tail(Xs))).
  
% Infinite stream of primes, using a sieve algorithm.

primes() -> sieve(ns(2)).

% The top-level sieve function: workhorse of primes/1.

sieve(Ns) ->
  H = head(Ns),
  ?cons(H,sieve(cull(H,tail(Ns)))).

% Remove all the multiples of N from the stream Ns.

cull(N,Ns) ->
  H = head(Ns),
  case H rem N of
    0 -> cull(N,tail(Ns));
    _ -> ?cons(H,cull(N,tail(Ns)))
  end.

% Get the Nth element of a stream.

index(N,Ls) ->
  case N of 
    0 -> head(Ls);
    _ -> index(N-1,tail(Ls))
  end.  

% Zip together two streams of numbers with +.

addZip(Xs,Ys) ->
   ?cons(head(Xs)+head(Ys), addZip(tail(Xs),tail(Ys))).

% The infinite stream of fibs.

fibs() ->
  ?cons(0,
    ?cons(1,
      addZip(fibs(),tail(fibs())))).

% Circular representation of the infinite stream of fibs.

fibsC() ->
  This = next_ref()+1,
  Next = This+1,
  ?cons(0,
    ?cons(1,
      addZip({ref,This},{ref,Next}))).

% Circular representation of the infinite stream of fibs (variant).

fibsCVar() ->
  This = next_ref()+1,
  ?cons(0,
    ?cons(1,
      addZip({ref,This},tail({ref,This})))).

% Print the first N values of a stream, one per line.    

ps(_Xs,0) -> ok;

ps(Xs,N) ->
  io:format("~w~n",[head(Xs)]),
  ps(tail(Xs),N-1).

% Print the first N values of a stream on one line.    

  ss(_Xs,0) -> 
      io:format("~n");
  
  ss(Xs,N) ->
    io:format("~w, ",[head(Xs)]),
  ss(tail(Xs),N-1).
  
  
