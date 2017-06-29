-module(lazy2).
-compile([export_all]).

-define(cons(X,Xs,Tab), begin ets:insert(Tab, {?ref(X,Xs), {thunk, fun () -> {X,Xs} end}}), 
                              io:format("done cons insert~n"), 
                              {ref,?ref(X,Xs)} end).

-define(ref(X,Xs), splat({??X,??Xs})).                        
                        
% -define(test(T),begin io:format("??T is ~w~n",[??T]), 
%                       io:format("length(??T) is ~w~n",[length(??T)]),
%                     io:format("splat(??T) is ~w~n",[splat(??T)]) end ).
                    
% test() -> ?test(primes({})).                    

% cons(X,Xs) ->
%   fun() -> {X,Xs} end

next_ref(Tab) ->
   [{0,Ref}]=ets:lookup(Tab,0),
   Ref.
  
nil() -> {}.

head({ref,Ref},Tab) ->
  io:format("entered head~n"),
  case ets:lookup(Tab,Ref) of
    [{Ref,{thunk,F}}] -> Val = F(),
                 ets:insert(Tab,{Ref,Val}),
                 {H,_} = Val,
                H;
    [{Ref,{H,_}}] -> io:format("head success~n"),
               H
  end.              

tail({ref,Ref},Tab) ->
    io:format("entered tail~n"),
  case ets:lookup(Tab,Ref) of
    [{Ref,{thunk,F}}] -> Val = F(),
                 ets:insert(Tab,{Ref,Val}),
                 {_,T} = Val,
                T;
    [{Ref,{_,T}}] -> io:format("tail success~n"),
               T
  end.              


                 

% list1() ->
%   fun() -> 
%     {23,
%      fun() -> 1/0 end}
%   end.   

%ones() ->
%  cons(1,ones()).

ones(Tab) ->
  ?cons(1,ones(Tab),Tab).

ns(N,Tab) ->
  ?cons(N,ns(N+1,Tab),Tab).

primes(Tab) -> sieve(ns(2,Tab),Tab).

% sieve(Ns) ->
%   H = head(Ns),
%   ?cons(H,sieve(cull(H,tail(Ns)))).

sieve(Ns,Tab) ->
  H = head(Ns,Tab),
  ?cons(H,sieve(cull(H,tail(Ns,Tab),Tab),Tab),Tab).

cull(N,Ns,Tab) ->
  H = head(Ns,Tab),
  case H rem N of
    0 -> cull(N,tail(Ns,Tab),Tab);
    _ -> ?cons(H,cull(N,tail(Ns,Tab),Tab),Tab)
  end.

index(N,Ls,Tab) ->
  case N of 
    0 -> head(Ls,Tab);
    _ -> index(N-1,tail(Ls,Tab),Tab)
  end.  

addZip(Xs,Ys,Tab) ->
   ?cons(head(Xs,Tab)+head(Ys,Tab), addZip(tail(Xs,Tab),tail(Ys,Tab),Tab),Tab).

fibs(Tab) ->
  ?cons(0,
    ?cons(1,
      addZip(fibs(Tab),tail(fibs(Tab),Tab),Tab),Tab),Tab).



ps(_Xs,0,_T) -> ok;

ps(Xs,N,Tab) ->
  io:format("~w~n",[head(Xs,Tab)]),
  ps(tail(Xs,Tab),N-1,Tab).
  

     


splat(F) ->
  crypto:hash(md5,binary:bin_to_list(term_to_binary(F))).
  


