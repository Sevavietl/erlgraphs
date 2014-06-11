%% @author V V Vietluzhskykh <sevavietl@gmail.com>
%% @doc Functions for calculating areas of geometric shapes.
%% @copyright 2014 V V Vietluzhskykh
%% Some comments were taken from http://en.wikipedia.org/wiki/Dijkstra's_algorithm
%% @doc module that implements Dijkstra alforithm 
%% @version 0.1

-module(parallel_dijkstra).
-compile(export_all).

main() ->
	File = "graph.txt",
	{ok, Bin} = file:read_file(File),
	Graph = parse_graph(Bin),
	Source = "a",
	Target = "f",
	NumOfCores = erlang:system_info(logical_processors_available), % Get the number of cores.
	NodesList = get_all_nodes(Graph, []), % Get the list of all nodes in the Graph.
	PerCore = round_up_div(length(Graph), NumOfCores), % Calculate the number of nodes per core.
	ListOfProcs = create_procs(Source, Target, Graph, NodesList, PerCore, []), % Create list of processes and store the pids in the lists.
	server(ListOfProcs, ListOfProcs, [], {initial, {Source, 0}}, Target, []).

server(ListOfProcs, ListOfProcs, ClosestVertexes, {initial, N}, Target, Previouses) ->
	ok = send_message_to_all(ListOfProcs, {self(), constant_node, N}),
	server(ListOfProcs, ListOfProcs, ClosestVertexes, {}, Target, Previouses);
server([], _, _, Distance, Target, Previouses) ->
	erlang:display(Previouses);
server(ListOfProcs, [], ClosestVertexes, Distance, Target, Previouses) ->
	io:format("ClosestVertexes: ~p~n~n", [ClosestVertexes]),
	{CV, CD} = global_closest(ClosestVertexes), % Find globally closest vertex	
	if
		CV =:= Target ->
			ok = send_message_to_all(ListOfProcs, {self(), final, {CV, CD}}),
			server(ListOfProcs, ListOfProcs, [{CV, CD}], CD, Target, Previouses);	
		CV =/= Target ->	
			io:format("CV: ~p~n~n", [CV]),
				case CV =:= infinity of
					true ->
			 			io:format("DeadEnd, could not reach Target from Source");
					false -> 
			 			ok = send_message_to_all(ListOfProcs, {self(), constant_node, {CV, CD}}), % TODO: Implement this function.
						server(ListOfProcs, ListOfProcs, [], Distance, Target, Previouses)
				end
	end;
server(ListOfProcs, TempListOfProcs, ClosestVertexes, Distance, Target, Previouses) -> 	
	receive
		{From, {closest, U}} ->
			server(ListOfProcs, lists:delete(From, TempListOfProcs), [U|ClosestVertexes], Distance, Target, Previouses);
		{From, {final, U, Previous}} ->
			server(lists:delete(From, ListOfProcs), lists:delete(From, TempListOfProcs), [U|ClosestVertexes], Distance, Target, [Previous|Previouses]);
		{From, _} ->
			io:format("Unknown message! From: ~p~n", [From])
	end.

%% @doc Function to send constant node to all processes
send_message_to_all([], _) ->
	ok;
send_message_to_all([Proc|ListOfProcs], Message) ->
	Proc ! Message,
	send_message_to_all(ListOfProcs, Message).

%% @doc Function to find global closest Vertex
global_closest([]) ->
	[];
global_closest(Vertices) ->
	lists:foldl(fun ({V1, D1}, {V2, D2}) -> 
					case less(D1, D2) of
						true ->
							{V1, D1};
						false ->
							{V2, D2}
					end
				end, hd(Vertices), tl(Vertices)).
 
%% @doc Function to create all the processes for graph travesing.
%% Returns te list of Pid's.
create_procs(_, _, [], _, _, Acc) ->
	Acc;
create_procs(Source, Target, Graph, NodesList, PerCore, ListOfProcs) ->
	GraphPortion = lists:sublist(Graph, PerCore),
	NewGraph = lists:subtract(Graph, GraphPortion),
 	Dist = create_dist(NodesList, maps:new()),
	Previous = create_previous(NodesList, maps:new()),
	case
		maps:is_key(Source, Dist) of
			true ->
				NewDist = maps:update(Source, 0, Dist);
			false ->
				NewDist = Dist
	end,
	Pid = spawn(?MODULE, loop, [GraphPortion, NewDist, Previous]),
	create_procs(Source, Target, NewGraph, NodesList, PerCore, [Pid|ListOfProcs]).	
	 
%% @doc The main loop of Dijkstra algorithm.
%% Returns tuple of maps with Distances and Previuos Nodes.	
loop(Graph, Dist, Previous) ->
	io:format("Pid: ~p~n Graph: ~p~n Dist: ~p~n Previous: ~p~n~n", [self(), Graph, Dist, Previous]),
	receive
		{From, constant_node, {U, UD}} ->
			io:format("Pid: ~p~n constant node: ~p~n~n", [self(), U]),
			Neighbors = get_neighbors(U, Graph), % Get all the neighboring nodes of current constant node.
			NewGraph = remove_vertices(U, Graph), % Remove current constant node from the graph list.
			TempDist = maps:update(U, UD, Dist),
			io:format("Pid: ~p~n Neighbors: ~p~n~n", [self(), Neighbors]),
			case Neighbors of 
				[] ->
                     {M, _, _} = lists:foldl(fun({N1, _D1, _V1}, {N2, _D2, _V2}) ->
                                                 case less(maps:get(N1, TempDist), maps:get(N2, TempDist)) of
                                                     true -> {N1, _D1, _V1};
                                                     false -> {N2, _D2, _V2}
                                                 end
                                             end, hd(NewGraph), tl(NewGraph)),
                    io:format("Pid: ~p~n M: ~p~n~n", [self(), M]),
                    From ! {self(), {closest, {M, maps:get(M, TempDist)}}},
					loop(NewGraph, Dist, Previous);
				_ ->	
					{NewDist, NewPrevious} = lists:foldl(fun({_, D, V}, {DistAcc, PreviousAcc}) ->                                                      
															Alt = maps:get(U, TempDist) + D,
															case less(Alt, maps:get(V, Dist)) of
																true -> % Relax (u,v,d), where u - current constant node, v - current neighbor, d - distance 
																	{maps:update(V, Alt, DistAcc),
																	maps:update(V, U, PreviousAcc)};
																false ->
																	{DistAcc, PreviousAcc}
															end
														end, {TempDist, Previous}, Neighbors),
					io:format("Pid: ~p~n Graph: ~p~n U: ~p~n~n", [self(), NewGraph, U]),
					io:format("Pid: ~p~n Dist: ~p~n Previous: ~p~n Graph: ~p~n~n", [self(), NewDist, NewPrevious, NewGraph]),
                    {M, _, _} = lists:foldl(fun({N1, _D1, _V1}, {N2, _D2, _V2}) ->
				 		case less(maps:get(N1, NewDist), maps:get(N2, NewDist)) of
					 		true -> {N1, _D1, _V1};
							false -> {N2, _D2, _V2}
				 		end
					 end, hd(Neighbors), tl(Neighbors)),
					 io:format("Pid: ~p~n M: ~p~n~n", [self(), M]),
					 case NewGraph of 
						[] ->
							From ! {self(), {final, {M, maps:get(M, NewDist)}, NewPrevious}};
						_ ->
					 		From ! {self(), {closest, {M, maps:get(M, NewDist)}}},
                     		loop(NewGraph, NewDist, NewPrevious)
					end
			end;
		{From, final, N} ->
			From ! {self(), {final, N, Previous}};
		{From, _} ->
			io:format("Unknown message! From: ~p~n", [From])
	end.

%% @doc Get list of all nodes in the Graph.
get_all_nodes([], Acc) ->
	Acc;
get_all_nodes([{Vertex1, _, Vertex2}|Graph], Acc) ->
	case lists:member(Vertex1, Acc) of
		true ->
			case lists:member(Vertex2, Acc) of
				true ->
					get_all_nodes(Graph, Acc);
				false ->
					get_all_nodes(Graph, [Vertex2|Acc])
			end;
		false ->
			case lists:member(Vertex2, Acc) of
				true ->
					get_all_nodes(Graph, [Vertex1|Acc]);
				false ->
					get_all_nodes(Graph, [Vertex2, Vertex1|Acc])
			end
	end.


%% @doc Function creates dist array - assign to every node a tentative 
%% distance value - infinity, as for now (uses map structure).
create_dist([], Acc) ->
	Acc;
create_dist([Vertex|Nodes], Acc) ->
	create_dist(Nodes, maps:put(Vertex, infinity, Acc)).
	

%% @doc Function creates previous array - assign to every node a previous node - undefined, as for now 
%% (uses map structure)
create_previous([], Acc) ->
	Acc;
create_previous([Vertex|Nodes], Acc) ->
	create_previous(Nodes, maps:put(Vertex, undefined, Acc)).

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

%% @doc Round up division
round_up_div(A, B) when A rem B =/= 0 ->
	A div B + 1;
round_up_div(A, B) ->
	A div B.
