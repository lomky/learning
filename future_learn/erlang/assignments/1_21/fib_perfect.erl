-module(fib_perfect).
-export([fibonacci/1,perfect/1]).

fibonacci(N) ->
  fibonacci(N, 0, 1).

fibonacci(0, P1, P2) ->
  P1;
fibonacci(N, P1, P2) when N>0 ->
  fibonacci(N-1, P2, P1 + P2).

perfect(N) ->
  perfect(N, N-1, 0).

perfect(N, 0, S) ->
  N == S;
perfect(N, D, S) when N rem D == 0 ->
  perfect(N, D-1, S + D);
perfect(N, D, S) when D>0 ->
  perfect(N, D-1, S).
