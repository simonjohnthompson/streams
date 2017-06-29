-module(maze).
-compile(export_all).

%%	A module solving mazes.

%%	Type of Mazes
%% 	It is globally assumed that mazes are rectangular,
%%	so that all the lines are of the same length.
 
-type maze() :: list(list(boolean())).

%%	Mazes as Strings

mazeSt1() ->
    ["..#..####",
     "#...#...#",
     "..#...#.#",
     "##.##.#.#",
     "....#...#",
     "#.#...#.#",
     "#.##.##.#",
     "#..#....#",
     "##...##.#",
     "..#..##.."].

miniMazeSt()->
    ["..######",
     "#..#...#",
     "##...#.#",
     "######.."].

%% Maze coordinates measured from top left hand corner, starting from 0.
%% For example, 4,2} is shown by X:
%%
%%    ..#######
%%    #...#...#
%%    ###...#.#
%%    ##.##.###
%%    #.X.#...#
%%    #.#...#.#
%%    #.##.####
%%    #..#....#
%%    ##...##.#
%%    #######..

-spec makeMaze(list(string())) -> maze().

makeMaze(StList) ->
    lists:map(fun (Line) -> lists:map(fun(Ch) -> Ch == $. end,Line) end,StList).

-spec maze1() -> maze().

maze1() -> makeMaze(mazeSt1()).

-spec miniMaze() -> maze().

miniMaze() -> makeMaze(miniMazeSt()).

-spec maze0() -> maze().

maze0() -> 
    Top = lists:zipwith(fun (L1,L2) -> L1++L2 end, maze1(), maze1()),
    Top ++Top. 

%% A (potential) path is given by a list of points.

-type point() :: {integer(),integer()}.

-type path() :: list(point()).

-spec path1() -> path().
-spec path2() -> path().

path1() -> 
        [ {0,0}, {0,1}, {1,1}, {1,2}, {1,3}, {2,3}, {2,4}, {2,5},
          {3,5}, {4,5}, {5,5}, {5,4}, {5,3}, {4,3}, {4,2}, {4,1},
          {5,1}, {6,1}, {7,1}, {7,2}, {8,2}, {8,3}, {8,4}, {7,4},         
          {7,5}, {7,6}, {7,7}, {8,7}, {9,7}, {9,8}].

path2() -> 
        [ {0,0}, {0,1}, {1,1}, {1,2}, {1,3}, {2,3}, {2,4}, {2,5},
          {3,5}, {4,5}, {5,5}, {5,4}, {5,3}, {5,2}, {4,2}, {4,1},
          {5,1}, {6,1}, {7,1}, {7,2}, {8,2}, {8,3}, {8,4}, {7,4},         
          {7,5}, {7,6}, {7,7}, {8,7}, {9,7}, {9,8}].


%% Is it a path through the maze, from its beginning point to its 
%% end point. It must satsify a number of conditions:
%%	each point is in the grid;
%%	each point is unoccupied {True};
%%	each point is adjacent to its successor.

-spec isPath(maze(),path()) -> boolean().

isPath(_Maze,[]) ->
  true;
isPath(Maze,[P]) -> 
  inGrid(Maze,P);
isPath(Maze,[P1,P2|Ps]) -> 
    inGrid(Maze,P1) andalso
    isEmpty(Maze,P1) andalso
    adjacent(P1,P2) andalso
    isPath(Maze,[P2|Ps]).

%% Does a point lie in the rectangular area of the maze?

-spec inGrid(maze(),point()) -> boolean().

inGrid(Maze,{Y,X}) ->
   0 =< Y andalso Y < length(Maze) andalso
   0 =< X andalso X < length(hd(Maze)).

%% Is a point {assumed to be within the grid) empty in the maze?

-spec isEmpty(maze(),point()) -> boolean().

isEmpty(Maze,{Y,X}) ->
  index(X,index(Y,Maze)).

%% Are two points adjacent?

-spec adjacent(point(),point()) -> boolean().

adjacent({Y1,X1},{Y2,X2}) ->
  (abs (Y1-Y2) + abs (X1-X2)) == 1.

%% All the points adjacent to a given point.

-spec adjPoints(maze(),point()) -> list(point()).

adjPoints(Maze,{Y,X}) -> 
    Potential = [ {Y+1,X}, {Y-1,X}, {Y,X+1}, {Y,X-1} ],
    [ P || P <- Potential, inGrid(Maze,P), isEmpty(Maze,P)].
        

%% Path finding: avoids loops by keeping track of the points
%% already visited in an `avoid' list. The work is done by
%% the allPaths function.

-spec paths(maze(),point(),point()) -> list(path()).

paths(Maze,P1,P2) ->
    allPaths(Maze,P1,P2,[]).

-spec allPaths(maze(),point(),point(),list(point())) -> list(path()).

allPaths(Maze,P1,P2,Avoid) ->
    case P1 == P2 of 
        true -> case lists:member(P1,Avoid) of
                    true -> [];
                    _    -> [[P1]]
                end;
        _ ->    PossPoints = lists:filter( fun (X) -> 
                                           not lists:member(X,Avoid) end,adjPoints(Maze,P1)),
                lists:concat(lists:map(fun (P)-> 
                    [ [P1|Path] || Path <- allPaths(Maze,P,P2,[P1|Avoid]) ] end, PossPoints))
            end.
            
index(X,Xs) -> lists:nth(X+1,Xs).        
   
%%
%% Properties: Haskell Quick Check
%%

% prop_path maze x y z w =
%         (inGrid maze {x,y} && inGrid maze {z,w} ) ==> all (isPath maze) {paths maze {x,y} {z,w})

% prop_wrong' maze x y =
%         inGrid maze {x,y}  ==> elem [{x,y}] $ paths maze {x,y} {x,y}

% prop_wrong maze x y = 
%         elem [{x',y'}] $ paths maze {x',y'} {x',y'}
%         where
%         x' = x `mod` (length maze)
%         y' = y `mod` (length $ head maze)


