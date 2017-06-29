-module(lazy3).
-compile([export_all]).

% Replaces the ETS table with a map that is threaded through all calls.
% The state variable for the map is still called a table.

% Different representation of cons: head is eager, tail is suspended.
% Also requires a different suspension, to which a state is passed. 

% Macro for constructing a new cons cell. Takes
%   Value for the head
%   Closure constructing the tail (and new table) from a table
%   Current table
% Returns a new ref cell, paired with the updated Table.

% The commented out version of the cons macro fails because the variables introduced get re-assigned. Sigh.
% The version available introduces no variables.

% -define(cons(X,F,Tab), begin Ref = next_ref(Tab),
%                              Tab1 = Tab#{ Ref => {X, {thunk, F}}},
%                              Tab2 = Tab1#{ 0 => next_ref(Tab1)+1 },
%                              {{ref,Ref}, Tab2} end).

-define(cons(X,F,Tab), begin {{ref,next_ref(Tab)}, Tab#{next_ref(Tab) => {X, {thunk, F}}, 0 => next_ref(Tab)+1}} end).


% Get the next free cell, assuming that value stored in location 0 
% keeps the locations used so far.

next_ref(Tab) ->
   maps:get(0,Tab).
  
start() -> #{ 0 => 1}.

% Head of a stream.

head({ref,Ref},Tab) ->
  case maps:get(Ref,Tab) of
    % {ref,R}    -> 
    %              Hd = head({ref,R}, Tab),
    %              {Tl,Tab1} = tail({ref,R},Tab),
    %              Tab2 = Tab1# {Ref => {Hd,Tl}},
    %              {Hd,Tab2};
    {H,_}   -> H
  end.              

% Tail of a stream.

tail({ref,Ref},Tab) ->
  case maps:get(Ref,Tab) of
    {ref,R}    -> 
                 Hd = head({ref,R}, Tab),
                 {Tl,Tab1} = tail({ref,R},Tab),
                 Tab2 = Tab1# {Ref => {Hd,Tl}},
                 {Tl,Tab2};
    {Hd,{thunk,F}} -> 
                 {Tl,TabC} = F(Tab),
                 Tab1 = TabC#{Ref => {Hd,Tl}},
                 {Tl,Tab1};
    {_,T} ->     
                 {T,Tab}
  end.              

% Circular representation of the infinite stream of ones.

onesC(Tab) ->
  This = next_ref(Tab),
  ?cons(1,fun (T) -> {{ref,This},T} end,Tab).

% Circular version of Infinite stream of n, n+1, … .

nsC(N,Tab) ->
  Ref = next_ref(Tab),
  ?cons(N,fun (T) -> addOne({ref,Ref},T) end,Tab).
  
% Add one to each element of a stream.

addOne(Xs,Tab) ->
  Hd = head(Xs,Tab),
  ?cons(Hd+1, fun (T) -> begin {Tl,T1} = tail(Xs,T), addOne(Tl,T1) end end, Tab).

% Circular defintion of the Fibonacci sequence.

fibsC(Tab) ->
  This = next_ref(Tab),
  ?cons(0, fun(T) -> ?cons(1, fun(S) -> begin {Tl,S1} = tail({ref,This},S), addZip({ref,This},Tl,S1) end end, T)  end, Tab).

% Add two lists together, elementwise.

addZip(Xs,Ys,Tab) ->
   HX = head(Xs,Tab),
   HY = head(Ys,Tab),
   ?cons(HX+HY, fun(T) -> begin {TX,T1} = tail(Xs,T), {TY,T2} = tail(Ys,T1), addZip(TX,TY,T2) end end, Tab).
 
% Get the Nth element of a stream.

index(N,Ls,Tab) ->
  case N of 
    0 -> head(Ls,Tab);
    _ -> {Tl,Tab1} = tail(Ls,Tab),
         index(N-1,Tl,Tab1)
  end.  

% Print the first N values of a stream, one per line.    

ps(_Xs,0,_T) -> ok;

ps(Xs,N,Tab) ->
  io:format("~w~n",[head(Xs,Tab)]),
  {T,Tab1} = tail(Xs,Tab),
  ps(T,N-1,Tab1).
  
% Calling ps

% 10> {Stream,Tab} = lazy3:onesC(lazy3:start()).
% {{ref,2},#{0 => 2,2 => {thunk,#Fun<lazy3.0.26054295>}}}
% 11> lazy3:ps(Stream,20,Tab).
% 1
% 1
% 1 …