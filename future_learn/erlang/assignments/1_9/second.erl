-module(second).
-export([hypotenuseSize/2,perimeter/2,areaTriangle/2]).

hypotenuseSize(A,B) ->
    math:sqrt(first:square(A) + first:square(B)).

perimeter(A,B) ->
    A+B+hypotenuseSize(A,B).

areaTriangle(A,B) ->
    first:mult(A,B)/2.



