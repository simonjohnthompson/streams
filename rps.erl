-module(rps). 
-export([play1/0,series_result/1,result/1,interact/1,echo/1,paper/1,random/1,beat/1]).

% A simple implementation of rock-paper-scissors in Erlang.

% The result of a single play, from the point of view of the first player.
% Two versions: one takes a pair as a single argument,
% the other takes the two values separately.

% The "pair" version ...

-type play() :: rock | paper | scissors.
-type result() :: win | lose | draw.
-type game() :: {play(),play()}.

-spec result(game()) -> result().

result({X,Y}) -> result(X,Y).

% ... is defined in terms of the "two argument" version:

-spec result(play(),play()) -> result().

result(paper,paper)       -> draw;
result(paper,rock)        -> win;
result(paper,scissors)    -> lose;

result(rock,paper)        -> lose;
result(rock,rock)         -> draw;
result(rock,scissors)     -> win;

result(scissors,paper)    -> win;
result(scissors,rock)     -> lose;
result(scissors,scissors) -> draw.

% Getting a numeric result: win=+1, draw=0, lose=-1.

-spec num_result(game()) -> integer().

num_result(Z) -> res_to_num(result(Z)).

-spec res_to_num(result()) -> integer().

res_to_num(win)  -> 1;
res_to_num(lose) -> -1;
res_to_num(draw) -> 0.

% What is the result of a series of games, from the first
% player's point of view?

-type series() :: [game()].

-spec series_result(series()) -> result().

series_result(Games) ->
    Scores = lists:map(fun num_result/1, Games),
    Outcome = lists:sum(Scores),
    if
	Outcome > 0  -> win;
	Outcome == 0 -> draw;
	Outcome < 0  -> lose
    end.

% An example of a series of games.

-spec play1() -> series().

play1() ->
   [{rock,rock},{paper,scissors},{rock,scissors},{paper,paper},{scissors,paper}].

% Play interactively.

% The argument is a strategy for the machine to play.
% For example,
%    rps:interact(fun rps:random/1)

-type plays() :: [play()].

-type strategy() :: fun((plays()) -> play()).

-spec interact(strategy()) -> ok.

interact(Strategy) ->
    interact(Strategy,[]).

% The second argument here is the accumulated input from the player
% Note that this function doesn't cheat: the Response is chosen
% before the Play from the player.

-spec interact(strategy(),[play()]) -> ok.

interact(Strategy,Xs) ->
    Response = Strategy(Xs), 
    {ok,[Play|_]} = io:fread('play one of rock, paper, scissors, or stop: ',"~a"),
    case Play of
	stop -> ok;
	_ ->
	    Result = result({Play,Response}),
	    io:format("Machine has played ~p, result is ~p~n",[Response,Result]),
	    interact(Strategy,[Play|Xs])
    end.



% strategies

% the random strategy

-spec random(plays()) -> play().

random(_) ->
    random_play().

% echo the previous choice of your opponent

-spec echo(plays()) -> play().

echo([]) ->
     random_play();
echo([X|_Xs]) ->
    X.

% assume that your opponent doesn't repeat themselves.

-spec beat(plays()) -> play().

beat([]) ->
    random_play();
beat([X|_]) ->
    case X of
	rock -> scissors;
	paper -> rock;
	scissors -> paper
    end.

% always play paper (not much of a strategy ...)

-spec paper(plays()) -> play().

paper(_) ->
    paper.

% A single random play, i.e. a random choice of rock, paper, scissors

-spec random_play() -> play().

random_play() ->
    case rand:uniform(3) of
	1 -> rock;
	2 -> paper;
	3 -> scissors
    end.

    


-spec vote([strategy()]) -> strategy().

vote(_Strats) -> fun paper/1.  % DUMMY

-type ast() :: any().

-type parser() :: fun((string()) -> [{ast(),string()}]).

-spec sequence(parser(),parser()) -> parser().

sequence(P1,_P2) -> P1.  % DUMMY
