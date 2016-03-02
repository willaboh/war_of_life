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
  format('Average Runtime: ~w ~N', [AvgRuntime]),
  halt.

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
