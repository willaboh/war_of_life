:- ensure_loaded('war_of_life.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Wrapper for the play/5 predicate
test_strategy(N, FirstPlayerStrategy, SecondPlayerStrategy) :-
  run_games(N, FirstPlayerStrategy, SecondPlayerStrategy, Results, Moves, Runtimes),
  sum(b, Results, FirstPlayerWins),
  sum(r, Results, SecondPlayerWins),
  sum(draw, Results, DrawNum),
  sum(stalemate, Results, StalemateNum),
  DrawTotal is DrawNum + StalemateNum,
  longest(Moves, MaxMove),
  shortest(Moves, MinMove),
  average(Moves, AvgMove),
  average(Runtimes, AvgRuntime),
  format('Number of Draws: ~w ~N', [DrawTotal]),
  format('Player 1 Wins: ~w ~N', [FirstPlayerWins]),
  format('Player 2 Wins: ~w ~N' , [SecondPlayerWins]),
  format('Longest Moves: ~w ~N', [MaxMove]),
  format('Shortest Moves: ~w ~N', [MinMove]),
  format('Average Moves: ~w ~N', [AvgMove]),
  format('Average Runtime: ~w ~N', [AvgRuntime]).
  %halt.

% Runs the game N times adding statistics to the Results, Moves and Runtimes Lists
run_games(0, _, _, [], [], []).

run_games(N, FirstPlayerStrategy, SecondPlayerStrategy, [Result | Results], [GameMoves | Moves], [Runtime | Runtimes]) :-
  N > 0,
  statistics(runtime, [T0|_]),
  play(quiet, FirstPlayerStrategy, SecondPlayerStrategy, GameMoves, Result),
  statistics(runtime, [T1|_]),
  Runtime is T1 - T0,
  NewN is N - 1,
  run_games(NewN, FirstplayerStrategy, SecondPlayerStrategy, Results, Moves, Runtimes).

% Sums up the number of first argument elements in the list provided in the second argument
sum(_, [], 0) :- !.

sum(P, [P|L], N) :-
  sum(P, L, N1),
  N is N1 + 1.

sum(P, [Q|L], N) :-
  P \= Q,
  sum(P, L, N).

% Returns the maximum value in the list
longest([P], P).

longest([P, Q|L], Max) :-
  P > Q -> longest([P|L], Max) ; longest([Q|L], Max).

% Returns the minimum value in the list
shortest([P], P).

shortest([P, Q|L], Min) :-
  P < Q -> shortest([P|L], Min) ; shortest([Q|L], Min).

% Calculates the average of all values in the list
average(Values, Avg) :-
  length(Values, Length),
  add_all(Values, Total),
  Avg is Total / Length.

% Sums up the values in a list
add_all([], 0).

add_all([P|L], Total) :-
  add_all(L, LTotal),
  Total is P + LTotal.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions for strategies
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_all_moves(PlayerColour, [AliveBlues, AliveReds], PossMoves) :-
  PlayerColour == 'b' -> AliveCells = AliveBlues ; AliveCells = AliveReds,
  findall([A, B, MA, MB],
            (member([A, B], AliveCells),
            neighbour_position(A, B, [MA, MB]),
            \+member([MA, MB], AliveBlues),
            \+member([MA, MB], AliveReds)),
          PossMoves),
  !.

% Picks the best move according to the strategy lookahead function provided
% run_strategy_lookahead/5
% run_strategy_lookahead(+StrategyLookahead, +PlayerColour, +CurrentBoardState, +PossibleMoves, -BestMove)
run_strategy_lookahead(_, _, _, [Move], Move).

run_strategy_lookahead(StrategyLookahead, PlayerColour, CurrentBoardState, [MoveA, MoveB | Moves], FinalMove) :-
  try_move(PlayerColour, CurrentBoardState, MoveA, MoveABoardState),
  try_move(PlayerColour, CurrentBoardState, MoveB, MoveBBoardState),
  compare_board_states(StrategyLookahead, PlayerColour, MoveA, MoveABoardState, MoveB, MoveBBoardState, BestMove),
  run_strategy_lookahead(StrategyLookahead, PlayerColour, CurrentBoardState, [BestMove | Moves], FinalMove).

try_move(PlayerColour, CurrentBoardState, Move, NewBoardState) :-
  return_new_board(PlayerColour, CurrentBoardState, Move, PreCrankedBoard),
  next_generation(PreCrankedBoard, NewBoardState).

compare_board_states(bloodlust_lookahead, PlayerColour, MoveA, MoveABoardState, MoveB, MoveBBoardState, BestMove) :-
  bloodlust_lookahead(PlayerColour, MoveA, MoveABoardState, MoveB, MoveBBoardState, BestMove).

compare_board_states(self_preservation_lookahead, PlayerColour, MoveA, MoveABoardState, MoveB, MoveBBoardState, BestMove) :-
  self_preservation_lookahead(PlayerColour, MoveA, MoveABoardState, MoveB, MoveBBoardState, BestMove).

% Returns new board state after move but before Conway's crank
return_new_board('b', [AliveBlues, AliveReds], Move, [NewAliveBlues, AliveReds]) :-
  alter_board(Move, AliveBlues, NewAliveBlues).

return_new_board('r', [AliveBlues, AliveReds], Move, [AliveBlues, NewAliveReds]) :-
  alter_board(Move, AliveReds, NewAliveReds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Strategies
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Returns the move and board state which results in the fewest number of opponent's pieces after Conway's crank
bloodlust(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
  get_all_moves(PlayerColour, CurrentBoardState, PossMoves),
  run_strategy_lookahead(bloodlust_lookahead, PlayerColour, CurrentBoardState, PossMoves, Move),
  return_new_board(PlayerColour, CurrentBoardState, Move, NewBoardState).

bloodlust_lookahead('b', MoveA, [_, RedA], MoveB, [_, RedB], BestMove) :-
  length(RedA, NumRedsA),
  length(RedB, NumRedsB),
  NumRedsA < NumRedsB -> BestMove = MoveA ; BestMove = MoveB.

bloodlust_lookahead('r', MoveA, [BlueA, _], MoveB, [BlueB, _], BestMove) :-
  length(BlueA, NumBluesA),
  length(BlueB, NumBluesB),
  NumBluesA < NumBluesB -> BestMove = MoveA ; BestMove = MoveB.

% Returns the move and board state which results in the largest number of this player's pieces after Conway's crank
self_preservation(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
  get_all_moves(PlayerColour, CurrentBoardState, PossMoves),
  run_strategy_lookahead(self_preservation_lookahead, PlayerColour, CurrentBoardState, PossMoves, Move),
  return_new_board(PlayerColour, CurrentBoardState, Move, NewBoardState).

self_preservation_lookahead('r', MoveA, [_, RedA], MoveB, [_, RedB], BestMove) :-
  length(RedA, NumRedsA),
  length(RedB, NumRedsB),
  NumRedsA > NumRedsB -> BestMove = MoveA ; BestMove = MoveB.

self_preservation_lookahead('b', MoveA, [BlueA, _], MoveB, [BlueB, _], BestMove) :-
  length(BlueA, NumBluesA),
  length(BlueB, NumBluesB),
  NumBluesA > NumBluesB -> BestMove = MoveA ; BestMove = MoveB.
