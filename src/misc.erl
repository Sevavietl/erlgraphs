%% @author V V Vietluzhskykh <sevavietl@gmail.com>
%% @doc Misc Functions.
%% @copyright 2014 V V Vietluzhskykh
%% @doc module that implements miscellaneous functions.
%% @version 0.1

-module(misc).
-compile(export_all).

%% @doc Function that parses graph file into list of look
%% [{A1, L1, B1}, {A2, L2, B2}, ..., {AN, LN, BN}]
parse_graph(Bin) when is_binary(Bin) ->
    parse_graph(binary_to_list(Bin));
parse_graph(Str) when is_list(Str) ->
    Values = [X || X <- string:tokens(Str, "\n")],
    group_vals(Values, []).

%% @doc Function that parses graph file into sublists
group_vals([], Acc) ->
    lists:reverse(Acc);
group_vals([Value|Rest], Acc) ->
    [S, L, D] = string:tokens(Value, ";"),
    group_vals(Rest, [{S, str_to_num(L), D}|Acc]).

%% @doc Function to convert string to number.
%% Deals with
str_to_num(N) ->
    case string:to_float(N) of
        {error, no_float} -> list_to_integer(N);
        {F, _} -> F
    end.
