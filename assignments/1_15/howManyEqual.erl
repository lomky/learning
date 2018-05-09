-module(howManyEqual).
-export([howManyEqual/3]).

howManyEqual(A,A,A) ->
  3;
howManyEqual(A,A,B) ->
  2;
howManyEqual(A,B,A) ->
  2;
howManyEqual(B,A,A) ->
  2;
howManyEqual(A,B,C) ->
  0.

