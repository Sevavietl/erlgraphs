-module(graphs).
-export([main/0]).

main() ->
        File = "graph.txt",
        {ok, Bin} = file:read_file(File),
        Graph = parse_graph(Bin),
	djkstra_main_loop("a", "f", Graph, create_dist(Graph, maps:new()), create_previous(Graph, maps:new())).

djkstra_main_loop(_, _, [], Dist, Previous) ->
	{Dist, Previous};
djkstra_main_loop(Source, Target, Graph, Dist, Previous) when maps:get(Source, Dist) =:= inf ->
	NewGraph = [Node || Node <- Graph, {U, _, _} = Node, Source =/= U],
	maps:update(Source, 0, Dist),
	djkstra_main_loop(Source, Target, NewCGraph, Dist, Previous);
djkstra_main_loop(Source, Target, [{N, _, _}], Dist, Previous) when maps:get(N, Dist) =:= inf ->
	io:format("All remaining nodes are inaccessible from node ~p~n", [N]);
djkstra_main_loop(Source, Target, [{N, L, D}], Dist, Previous) when maps:get(N, Dist) =/= inf ->
	Alt = maps:get
	djkstra_main_loop(Source, Target, NewGraph, Dist, Previous); 
djkstra_main_loop(Source, Target, Graph, Dist, Previous) when maps:get(Source, Dist) =/= inf ->
	U = lists:foldl(
		fun({N1, _, _}, {N2, _, _}) when maps:get(N1, Dist) =:= inf ->
			N2;
		({N1, _, _}, {N2, _, _}) when maps:get(N2, Dist) =:= inf ->
			N1;
		({N1, _, _}, {N2, _, _}) when maps:get(N1, Dist) < maps:get(N2, Dist) ->
			N1;
		({N1, _, _}, {N2, _, _}) when maps:get(N1, Dist) >= maps:get(N2, Dist) ->
			N2
		end, hd(Graph), tl(Graph)),
	NewGraph = [Node || Node <- Graph, {N, _, _} = Node, N =/= U],
	if maps:get(U, Dist) =:= inf ->
		io:format("All remaining nodes are inaccessible from node ~p~n", [U])
	; maps:get(U, Dist) =/= inf ->
		Neighbors = [Node || Node <- Graph, {Source, _, _} = Node],
	 	{G, D, P} = djkstra_sub_loop(Source, Neighbors, NewGraph, Dist, Previous),
		dijkstra_main_loop(Source, Target, G, D, P)
	end.

		

djkstra_sub_loop(_, [], _, Graph, Dist, Previous) ->
	{Graph, Dist, Previous};
djkstra_sub_loop(Source, [Neighbor|Neighbors], Graph, Dist, Previous) when maps:get(Target, Dist) =:= inf ->
	maps:update(Neighbor, (maps:get(Source, Dist) + dist_between(Source, Neighbor, Graph)), Dist),
	maps:update(Neighbor, Source, Previous),
	djkstra_sub_loop(Source, Neighbors, Graph, Dist, Previous);
djkstra_sub_loop(Source, [Neighbor|Neighbors], Graph, Dist, Previous) when maps:get(Target, Dist) =/= inf -> 
	Alt = maps:get(Source, Dist) + dist_between(Source, Neighbor, Graph),
	if Alt < maps:get(Neighbor, Dist) ->
		 maps:update(Neighbor, (maps:get(Source, Dist) + dist_between(Source, Neighbor, Graph)), Dist),
        	maps:update(Neighbor, Source, Previous),
        	djkstra_sub_loop(Source, Neighbors, Graph, Dist, Previous)
	; Alt >= maps:get(Neighbor, Dist) ->
		djkstra_sub_loop(Source, Neighbors, Graph, Dist, Previuos)
	end.

%% @doc Function to get distance between two neighboring nodes
dist_between(U, V, Graph) ->
	{_, D, _} = [Node || Node <- Graph, Node = {U, _, V}]
	D.	
	
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

%% @doc Function to convert string to number
str_to_num(N) ->
        case string:to_float(N) of
                {error, no_float} -> list_to_integer(N);
                {F, _} -> F
        end.

%% @doc Function creates dist array - assign to every node a tentative 
%% distance value (uses map structure)
create_dist([], Acc) -> 
	Acc;
create_dist([Vertex|Graph], Acc) when maps:is_key(Vertex, Acc) ->
	create_dist(Graph, Acc);
create_dist([Vertex|Graph], Acc) ->
	create_dist(Graph, maps:put(Vertex, inf, Acc)).

%% @doc Function creates previous array - assign to every node a previous node 
%% (uses map structure)
create_previous([], Acc) ->
	Acc;
create_previous([Vertex|Graph], Acc) when maps:find(Vertex, Acc) ->
	create_previous(Graph, Acc);
create_previous([Vertex|Graph], Acc) ->
	create_previous(Graph, maps:put(Vertex, undefined, Acc)).
	
