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



