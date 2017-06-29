-module(lazy0).
-compile([export_all]).

% THIS IS REPLACED IN lazy.erl BY A NAMED TABLE: NO NEED TO PASS IT IN EXPLICITLY.

% To use, need an ETS table. This does the trick; call thus:
%   Tab = lazy:setup().
% Tab needs to be passed as a parameter to all functions; 
% could alternatively do with 

setup() ->
  Tab = ets:new(foo,[]),
  ets:insert(Tab,{0,1}),
  Tab.


% Macro for constructing a new cell.

-define(cons(X,Xs,Tab), begin ets:insert(Tab,{0,next_ref(Tab)+1}) ,
                              ets:insert(Tab, {next_ref(Tab), {thunk, fun () -> {X,Xs} end}}), 
                              % io:format("done cons insert~n"), 
                              {ref,next_ref(Tab)} end).

% Get the next free cell, assuming that value stored in location 0 
% keeps the locations used so far.

next_ref(Tab) ->
   [{0,Ref}]=ets:lookup(Tab,0),
   Ref.
  
nil() -> {}.

% Head of a stream.

head({ref,Ref},Tab) ->
  % io:format("entered head~n"),
  case ets:lookup(Tab,Ref) of
    [{Ref,{thunk,F}}] -> Val = F(),
                 ets:insert(Tab,{Ref,Val}),
                 {H,_} = Val,
                H;
    [{Ref,{H,_}}] -> % io:format("head success~n"),
               H
  end.              

% Tail of a stream.

tail({ref,Ref},Tab) ->
    % io:format("entered tail~n"),
  case ets:lookup(Tab,Ref) of
    [{Ref,{thunk,F}}] -> Val = F(),
                 ets:insert(Tab,{Ref,Val}),
                 {_,T} = Val,
                T;
    [{Ref,{_,T}}] -> % io:format("tail success~n"),
               T
  end.              


                 

% Infinite stream of ones.

ones(Tab) ->
  ?cons(1,ones(Tab),Tab).

% Circular representation of the infinite stream of ones.

onesC(Tab) ->
  This = next_ref(Tab)+1,
  ?cons(1,{ref,This},Tab).

% Infinite stream of n, n+1, … .

ns(N,Tab) ->
  ?cons(N,ns(N+1,Tab),Tab).

% Infinite stream of primes, using a sieve algorithm.

primes(Tab) -> sieve(ns(2,Tab),Tab).

% The top-level sieve function: workhorse of primes/1.

sieve(Ns,Tab) ->
  H = head(Ns,Tab),
  ?cons(H,sieve(cull(H,tail(Ns,Tab),Tab),Tab),Tab).

% Remove all the multiples of N from the stream Ns.

cull(N,Ns,Tab) ->
  H = head(Ns,Tab),
  case H rem N of
    0 -> cull(N,tail(Ns,Tab),Tab);
    _ -> ?cons(H,cull(N,tail(Ns,Tab),Tab),Tab)
  end.

% Get the Nth element of a stream.

index(N,Ls,Tab) ->
  case N of 
    0 -> head(Ls,Tab);
    _ -> index(N-1,tail(Ls,Tab),Tab)
  end.  

% Zip together two streams of numbers with +.

addZip(Xs,Ys,Tab) ->
   ?cons(head(Xs,Tab)+head(Ys,Tab), addZip(tail(Xs,Tab),tail(Ys,Tab),Tab),Tab).

% The infinite stream of fibs.

fibs(Tab) ->
  ?cons(0,
    ?cons(1,
      addZip(fibs(Tab),tail(fibs(Tab),Tab),Tab),Tab),Tab).

% Circular representation of the infinite stream of fibs.

fibsC(Tab) ->
  This = next_ref(Tab)+1,
  Next = This+1,
  ?cons(0,
    ?cons(1,
      addZip({ref,This},{ref,Next},Tab),Tab),Tab).

% Circular representation of the infinite stream of fibs (variant).

fibsCVar(Tab) ->
  This = next_ref(Tab)+1,
  ?cons(0,
    ?cons(1,
      addZip({ref,This},tail({ref,This},Tab),Tab),Tab),Tab).

% Print the first N values of a stream, one per line.    

ps(_Xs,0,_T) -> ok;

ps(Xs,N,Tab) ->
  io:format("~w~n",[head(Xs,Tab)]),
  ps(tail(Xs,Tab),N-1,Tab).
  
