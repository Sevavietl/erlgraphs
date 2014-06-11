%% @author V V Vietluzhskykh <sevavietl@gmail.com>
%% @doc Functions for calculating areas of geometric shapes.
%% @copyright 2014 V V Vietluzhskykh
%% Some comments were taken from http://en.wikipedia.org/wiki/Dijkstra's_algorithm
%% @doc module that implements Dijkstra alforithm 
%% @version 0.1

-module(dijkstra).
-compile(export_all).

main() ->
	File = "graph.txt",
	{ok, Bin} = file:read_file(File),
	Graph = parse_graph(Bin),
	Source = "h",
	Target = "f",
	Dist = create_dist(Graph, maps:new()),
	DistNew = maps:update(Source, 0, Dist),
	Previous = create_previous(Graph, maps:new()),
	{DistResult, PreviousResult} = loop(Source, Target, Graph, DistNew, Previous),
	{maps:get(Target, DistResult), shortest_path_from_previous(Source, Target, PreviousResult, [])}.

%% @doc The main loop of Dijkstra algorithm.
%% Returns tuple of maps with Distances and Previuos Nodes.
loop(_Source, _Target, [], Dist, Previous) -> % The base case that hits when there're no more nodes in the graph.
	{Dist, Previous};
loop(Source, Target, Graph, Dist, Previous) ->
	{U, _, _} = lists:foldl(fun({N1, _D1, _V1}, {N2, _D2, _V2}) -> % Source node in first case.
					case less(maps:get(N1, Dist), maps:get(N2, Dist)) of
						true -> {N1, _D1, _V1};
			   			false -> {N2, _D2, _V2}
					end
				end, hd(Graph), tl(Graph)),
	if 
		U =:= Target ->
			loop(Source, Target, [], Dist, Previous); % Terminate search if current constant node is the target node.
		U =/= Target ->
			case maps:get(U, Dist) =:= infinity of 
				true ->
					loop(Source, Target, Graph, Dist, Previous); % All remainign nodes are inaccessible from the source node. TODD: Implement wright message passing
				false ->
					Neighbors = get_neighbors(U, Graph), % Get all the neighboring nodes of current constant node.
					NewGraph = remove_vertices(U, Graph), % Remove current constant node from the graph list.
					case Neighbors of 
						[] ->
							loop(Source, Target, NewGraph, Dist, Previous);
						_ ->	
							{NewDist, NewPrevious} = lists:foldl(fun({_, D, V}, {DistAcc, PreviousAcc}) ->                                                      
														Alt = maps:get(U, Dist) + D,
														case less(Alt, maps:get(V, Dist)) of
															true -> % Relax (u,v,d), where u - current constant node, v - current neighbor, d - distance 
																{maps:update(V, Alt, DistAcc),
																maps:update(V, U, PreviousAcc)};
															false ->
																{DistAcc, PreviousAcc}
														end
			      	      					      	end, {Dist, Previous}, Neighbors),
							loop(Source, Target, NewGraph, NewDist, NewPrevious)
					end
			end
	end.
	
%% @doc Function creates dist array - assign to every node a tentative 
%% distance value - infinity, as for now (uses map structure).
create_dist([], Acc) ->
	Acc;
create_dist([{Vertex1, _, Vertex2}|Graph], Acc) ->
	case maps:is_key(Vertex1, Acc) of
		true -> 
			case maps:is_key(Vertex2, Acc) of
				true ->
					create_dist(Graph, Acc);
				false ->
					create_dist(Graph, maps:put(Vertex2, infinity, Acc))
			end;
		false -> 
			case maps:is_key(Vertex2, Acc) of
				true ->
					create_dist(Graph, maps:put(Vertex1, infinity, Acc));
				false ->
					create_dist(Graph, maps:put(Vertex2, infinity,  maps:put(Vertex1, infinity, Acc)))
			end
	end.
	

%% @doc Function creates previous array - assign to every node a previous node - undefined, as for now 
%% (uses map structure)
create_previous([], Acc) ->
        Acc;
create_previous([{Vertex1, _, Vertex2}|Graph], Acc) ->
	case maps:is_key(Vertex1, Acc) of
		true ->
			case maps:is_key(Vertex2, Acc) of
				true ->
					create_previous(Graph, Acc);
				false ->
					create_previous(Graph, maps:put(Vertex2, undefined, Acc))
			end;
		false ->
			case maps:is_key(Vertex2, Acc) of
				true ->
					create_previous(Graph, maps:put(Vertex1, undefined, Acc));
				false ->
					create_previous(Graph, maps:put(Vertex2, undefined, maps:put(Vertex1, undefined, Acc)))
			end
        end.

%% @doc Function to compare distances in the algorithm.
%% In opposite to < operator can deal with infinity, wich is vital.
%% Helps not to use cheking if distance is infinity.
less(infinity, infinity) -> false;
less(_, infinity) -> true;
less(infinity, _) -> false;
less(A, B) -> A < B.

%% @doc Function that removes used nodes from the Graph list. 
remove_vertices(Vertice, Graph) ->
	[{U, D, V} || {U, D, V} <- Graph, not(U == Vertice) andalso not(V == Vertice)].

%% @doc Function to get a list of neighboring nodes in the Graph.
get_neighbors(Vertice, Graph) ->
	[{U, D, V} || {U, D, V} <- Graph, U == Vertice].


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
%% Deals with float and integers.
str_to_num(N) ->
	case string:to_float(N) of
		{error, no_float} -> list_to_integer(N);
		{F, _} -> F
	end.

%% @doc Function to construct the shortest path with a stack.
%% We push the vertex into the stack and then traverse from target to source.
shortest_path_from_previous(S, S, _, Path) ->
	[S|Path]; 
shortest_path_from_previous(S, T, Previous, Path) ->
	shortest_path_from_previous(S, maps:get(T, Previous), maps:remove(T, Previous), [T|Path]).	
