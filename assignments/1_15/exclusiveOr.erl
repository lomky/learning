-module(exclusiveOr).
-export([xOr1/2,xOr2/2,xOr3/2]).

xOrA(true,false) ->
  true;
xOrA(false,true) ->
  true;
xOrA(X,Y) ->
  false.

xOrB(true,false) ->
  true;
xOrB(false,true) ->
  true;
xOrB(_,_) ->
  false.

xOr1(false,false) ->
  false;
xOr1(true,true) ->
  false;
xOr1(_,_) ->
  true.

xOr2(false,false) ->
  not true;
xOr2(true,true) ->
  not true;
xOr2(_,_) ->
  not false.

xOr3(false,true) ->
  not false;
xOr3(true,false) ->
  not false;
xOr3(_,_) ->
  false.
