-module(lazy3).
-compile([export_all]).

% Replaces the ETS table with a map that is threaded through all calls.
% The state variable for the map is called a store.

% Different representation of cons: head is eager, tail is suspended.
% Also requires a different suspension, to which a state is passed. 

% Macro for constructing a new cons cell. Takes
%   Value for the head
%   Closure constructing the tail (and new store) from a store
%   Current store
% Returns a new ref cell, paired with the updated store.

% The commented out version of the cons macro fails because the variables introduced get re-assigned. Sigh.
% The version available introduces no variables.

% -define(cons(X,F,Sto), begin Ref = next_ref(Sto),
%                              Sto1 = Sto#{ Ref => {X, {thunk, F}}},
%                              Sto2 = Sto1#{ 0 => next_ref(Sto1)+1 },
%                              {{ref,Ref}, Sto2} end).

-define(cons(X,F,Sto), begin {{ref,next_ref(Sto)}, 
                              Sto#{next_ref(Sto) => {X, {thunk, F}}, 0 => next_ref(Sto)+1}} end).


% Get the next free cell, assuming that value stored in location 0 
% keeps the locations used so far.

next_ref(Sto) ->
   maps:get(0,Sto).
  
start() -> #{ 0 => 1}.

% Head of a stream.

head({ref,Ref},Sto) ->
  case maps:get(Ref,Sto) of
    % {ref,R}    -> 
    %              Hd = head({ref,R}, Sto),
    %              {Tl,Sto1} = tail({ref,R},Sto),
    %              Sto2 = Sto1# {Ref => {Hd,Tl}},
    %              {Hd,Sto2};
    {H,_}   -> H
  end.              

% Tail of a stream.

tail({ref,Ref},Sto) ->
  case maps:get(Ref,Sto) of
    {ref,R}    -> 
                 Hd = head({ref,R}, Sto),
                 {Tl,Sto1} = tail({ref,R},Sto),
                 Sto2 = Sto1# {Ref => {Hd,Tl}},
                 {Tl,Sto2};
    {Hd,{thunk,F}} -> 
                 {Tl,StoC} = F(Sto),
                 Sto1 = StoC#{Ref => {Hd,Tl}},
                 {Tl,Sto1};
    {_,T} ->     
                 {T,Sto}
  end.              

% Circular representation of the infinite stream of ones.

onesC(Sto) ->
  This = next_ref(Sto),
  ?cons(1,fun (T) -> {{ref,This},T} end,Sto).

% Circular version of Infinite stream of n, n+1, … .

nsC(N,Sto) ->
  Ref = next_ref(Sto),
  ?cons(N,fun (T) -> addOne({ref,Ref},T) end,Sto).
  
% Add one to each element of a stream.

addOne(Xs,Sto) ->
  Hd = head(Xs,Sto),
  ?cons(Hd+1, fun (T) -> begin {Tl,T1} = tail(Xs,T), addOne(Tl,T1) end end, Sto).

% Circular defintion of the Fibonacci sequence.

fibsC(Sto) ->
  This = next_ref(Sto),
  ?cons(0, fun(T) -> 
        ?cons(1, fun(S) -> begin {Tl,S1} = tail({ref,This},S), 
                                 addZip({ref,This},Tl,S1) end end, T)  end, Sto).

% Add two lists together, elementwise.

addZip(Xs,Ys,Sto) ->
   HX = head(Xs,Sto),
   HY = head(Ys,Sto),
   ?cons(HX+HY, fun(T) -> begin {TX,T1} = tail(Xs,T), {TY,T2} = tail(Ys,T1), addZip(TX,TY,T2) end end, Sto).
 
% Get the Nth element of a stream.

index(N,Ls,Sto) ->
  case N of 
    0 -> head(Ls,Sto);
    _ -> {Tl,Sto1} = tail(Ls,Sto),
         index(N-1,Tl,Sto1)
  end.  

% Print the first N values of a stream, one per line.    

ps(_Xs,0,_Sto) -> ok;

ps(Xs,N,Sto) ->
  io:format("~w~n",[head(Xs,Sto)]),
  {T,Sto1} = tail(Xs,Sto),
  ps(T,N-1,Sto1).

% Print the first N values of a stream on one line.    

ss(_Xs,0,_Sto) -> 
  io:format("~n");

ss(Xs,N,Sto) ->
  io:format("~w, ",[head(Xs,Sto)]),
  {T,Sto1} = tail(Xs,Sto),
  ss(T,N-1,Sto1).
  
show(Str,N) ->
  {Stream,Sto} = (Str(start())),
  ss(Stream,N,Sto). 
% Calling ps

% 10> {Stream,Sto} = lazy3:onesC(lazy3:start()).
% {{ref,2},#{0 => 2,2 => {thunk,#Fun<lazy3.0.26054295>}}}
% 11> lazy3:ps(Stream,20,Sto).
% 1
% 1
% 1 …