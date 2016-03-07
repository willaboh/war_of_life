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
  sum(exhaust, Results, ExhaustNum),
  DrawTotal is DrawNum + StalemateNum + ExhaustNum,
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
  run_games(NewN, FirstPlayerStrategy, SecondPlayerStrategy, Results, Moves, Runtimes).

% Sums up the number of first argument elements in the list provided in the second argument
sum(_, [], 0) :- !.

sum(P, [P|L], N) :-
  sum(P, L, N1),
  N is N1 + 1.

sum(P, [Q|L], N) :-
  P \= Q,
  sum(P, L, N).

% Returns the maximum value in the list
longest(Moves, Max) :-
  max_member(Max, Moves),
  Max < 250.

% Returns the minimum value in the list
shortest(Moves, Min) :-
  min_member(Min, Moves).

% Calculates the average of all values in the list
average(Values, Avg) :-
  length(Values, Length),
  add_all(Values, Total),
  Avg is Total / Length.

% Sums up the values in a list
add_all(List, Total) :-
  sumlist(List, Total).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions for strategies
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_all_moves(PlayerColour, [AliveBlues, AliveReds], PossMoves) :-
  PlayerColour == 'b' -> AliveCells = AliveBlues ; AliveCells = AliveReds,
  findall([A, B, MA, MB],
          (
            member([A, B], AliveCells),
            neighbour_position(A, B, [MA, MB]),
            \+ member([MA, MB], AliveBlues),
            \+ member([MA, MB], AliveReds)
          ),
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

compare_board_states(land_grab_lookahead, PlayerColour, MoveA, MoveABoardState, MoveB, MoveBBoardState, BestMove) :-
  land_grab_lookahead(PlayerColour, MoveA, MoveABoardState, MoveB, MoveBBoardState, BestMove).

% Returns new board state after move but before Conway's crank
return_new_board('b', [AliveBlues, AliveReds], Move, [NewAliveBlues, AliveReds]) :-
  alter_board(Move, AliveBlues, NewAliveBlues).

return_new_board('r', [AliveBlues, AliveReds], Move, [AliveBlues, NewAliveReds]) :-
  alter_board(Move, AliveReds, NewAliveReds).

% Returns the colour of this player's opponent
opponent_colour('b', 'r').
opponent_colour('r', 'b').

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

% Returns the move and board state which after Conway's crank, maximises the function
% 'Number of Player's pieces - Number of Opponent's pieces'

land_grab(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
  get_all_moves(PlayerColour, CurrentBoardState, PossMoves),
  run_strategy_lookahead(land_grab_lookahead, PlayerColour, CurrentBoardState, PossMoves, Move),
  return_new_board(PlayerColour, CurrentBoardState, Move, NewBoardState).

land_grab_lookahead(PlayerColour, MoveA, [AliveBlueA, AliveRedA], MoveB, [AliveBlueB, AliveRedB], BestMove) :-
  length(AliveRedA, NumRedsA),
  length(AliveRedB, NumRedsB),
  length(AliveBlueA, NumBluesA),
  length(AliveBlueB, NumBluesB),
  land_grab_max_func(PlayerColour, NumBluesA, NumRedsA, NumBluesB, NumRedsB, DiffA, DiffB),
  DiffA >= DiffB -> BestMove = MoveA ; BestMove = MoveB.

land_grab_max_func('b', NumBluesA, NumRedsA, NumBluesB, NumRedsB, DiffA, DiffB) :-
  DiffA is NumBluesA - NumRedsA,
  DiffB is NumBluesB - NumRedsB.

land_grab_max_func('r', NumBluesA, NumRedsA, NumBluesB, NumRedsB, DiffA, DiffB) :-
  DiffA is NumRedsA - NumBluesA,
  DiffB is NumRedsB - NumBluesB.

% Returns the best move and board after utilising the minimax principle on the land grab heuristic
minimax(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
  get_all_moves(PlayerColour, CurrentBoardState, PossMoves),
  find_best_move(PlayerColour, CurrentBoardState, PossMoves, Move),
  return_new_board(PlayerColour, CurrentBoardState, Move, NewBoardState).

score_board(PlayerColour, [AliveBlues, AliveReds], Score) :-
  length(AliveBlues, BL),
  length(AliveReds, RL),
  score(PlayerColour, BL, RL, Score).

score('b', BL, RL, Score) :-
  Score is BL - RL.

score('r', BL, RL, Score) :-
  Score is RL - BL.

find_best_move(_, _, [Move], Move).

find_best_move(PlayerColour, CurrentBoardState, [MoveA, MoveB | Moves], BestMove) :-
  opponent_colour(PlayerColour, OpponentColour),
  try_move(PlayerColour, CurrentBoardState, MoveA, MoveABoard),
  try_move(PlayerColour, CurrentBoardState, MoveB, MoveBBoard),
  get_all_moves(OpponentColour, MoveABoard, OppMoveAMoves),
  get_all_moves(OpponentColour, MoveBBoard, OppMoveBMoves),
  run_strategy_lookahead(land_grab_lookahead, OpponentColour, MoveABoard, OppMoveAMoves, BestOppAMove),
  run_strategy_lookahead(land_grab_lookahead, OpponentColour, MoveBBoard, OppMoveBMoves, BestOppBMove),
  try_move(OpponentColour, MoveABoard, BestOppAMove, BestOppAMoveBoard),
  try_move(OpponentColour, MoveBBoard, BestOppBMove, BestOppBMoveBoard),
  score_board(PlayerColour, BestOppAMoveBoard, ScoreA),
  score_board(PlayerColour, BestOppBMoveBoard, ScoreB),
  ScoreA > ScoreB ->
    find_best_move(PlayerColour, CurrentBoardState, [MoveA | Moves], BestMove)
    ;
    find_best_move(PlayerColour, CurrentBoardState, [MoveB | Moves], BestMove).
