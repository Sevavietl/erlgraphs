%% @author V V Vietluzhskykh <sevavietl@gmail.com>
%% @doc Functions for math calculations
%% @copyright 2014 V V Vietluzhskykh
%% @doc module that implements redifined math functions
%% @version 0.1

-module(maths).
-compile(export_all).

%% @doc Function to compare distances in the algorithm.
%% In opposite to < operator can deal with infinity, wich is vital.
%% Helps not to use cheking if distance is infinity.
less(infinity, infinity) -> false;
less(_, infinity) -> true;
less(infinity, _) -> false;
less(A, B) -> A < B.

%% @doc Round up division
round_up_div(A, B) when A rem B =/= 0 ->
    A div B + 1;
round_up_div(A, B) ->
    A div B.
