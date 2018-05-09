-module(week_1_section_1_7).
-export([double/1,mult/2]).

mult(X, Y) ->
    X*Y.

double(X) ->
    mult(2, X).


