-module(stream).
-compile([export_all]).

% nil() -> {}.

-define(cons(X,Xs),
        fun() -> {X,Xs} end).

cons(X,Xs) ->
  fun() -> {X,Xs} end.

head(L) ->
  case (L()) of
    {H,_} -> H
  end.

tail(L) ->
  case (L()) of
    {_,T} -> T
  end.

% list1() ->
%   fun() -> 
%     {23,
%      fun() -> 1/0 end}
%   end.   

% 
% Infinite stream of ones.
% 


%  cons(1,ones()).

ones() ->
  ?cons(1,ones()).

% 
% Infinite stream of nunbers from N.
% 

ns(N) ->
  ?cons(N,ns(N+1)).

% 
% Infinite stream of nunbers from 0, cicular definition.
% 

ns() ->
  ?cons(0,addOne(ns())).

addOne(Xs) ->
  ?cons(head(Xs)+1, addOne(tail(Xs))).

%
% Infiinite stream of primes.
%

primes() -> sieve(ns(2)).

sieve(Ns) ->
  H = head(Ns),
  ?cons(H,sieve(cut(H,tail(Ns)))).

cut(N,Ns) ->
  H = head(Ns),
  case H rem N of
    0 ->         cut(N,tail(Ns));
    _ -> ?cons(H,cut(N,tail(Ns)))
  end.

% Alternative definition
%
% sieve(Ns) ->
%   H = head(Ns),
%   ?cons(H,sieve(cull(H,tail(Ns)))).


%
% Infinite stream of Fibaonacci numbers, knot-typing.
%

fibs() ->
  ?cons(0,
    ?cons(1,
      addZip(fibs(),tail(fibs())))).

addZip(Xs,Ys) ->
   ?cons(head(Xs)+head(Ys), addZip(tail(Xs),tail(Ys))).


%
% Inspection functions.
%

ps(_Xs,0) -> ok;

ps(Xs,N) ->
  io:format("~w~n",[head(Xs)]),
ps(tail(Xs),N-1).

index(N,Ls) ->
  case N of 
    0 -> stream:head(Ls);
    _ -> index(N-1,stream:tail(Ls))
  end.  
