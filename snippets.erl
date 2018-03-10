-module(snippets).
-compile(export_all).



area({circle,_,R}) ->
  math:pi()*R*R;
area({tri,_,A,B,C}) ->
  S = (A+B+C)/2,
  math:sqrt(S*(S-A)*(S-B)*(S-C)).

echo(Pid,N) ->
  receive
    Msg -> Pid!Msg
  end,
  echo(Pid,N+1).

-spec switch(number(),T,T) -> T.

switch(N,Pos,Neg) ->
    case N>0 of
        true -> Pos;
        _    -> Neg
    end.


-spec sum_first_two(list(number())) -> number().

sum_first_two([A,B|_Rest]) 
    -> A+B.


-spec lswitch(number(),fun(() ->T),fun(() ->T)) -> T.

lswitch(N,Pos,Neg) ->
    case N>0 of
        true -> Pos();
        _    -> Neg()
    end.

lex1() -> lswitch(1,fun() -> 3+4 end,fun () -> 1/0 end).

-define(switch(N,Pos,Neg),
        lswitch(N,fun() -> Pos end,fun() -> Neg end)).

lex2() -> ?switch(1,3+4,1/0).



  
  

