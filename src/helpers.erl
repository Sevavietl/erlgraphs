%% @author V V Vietluzhskykh <sevavietl@gmail.com>
%% @doc Helper functions
%% @copyright 2014 V V Vietluzhskykh
%% Some comments were taken form http://en.wikipedia.org/
%% @doc module that implements helper functions for graph
%% processing
%% @version 0.1

-module(helpers).
-compile(export_all).

%% @doc Function to create all the processes for graph travesing.
%% Returns the list of Pid's.
create_procs(_, _, _, [], _, Acc) ->
    Acc;
create_procs(Source, Target, Graph, NodeList, PerCore, ListOfProcs) ->
    NodesPortion = lists:sublist(NodeList, PerCore),
    NewNodeList = lists:subtract(NodeList, NodesPortion),
    Dist = create_dist(NodesPortion, maps:new()),
    Previous = create_previous(NodesPortion, maps:new()),
    Pid = spawn(?MODULE, loop, [Source, Target, Graph, Dist, Previous]),
    create_procs(Source, Target, Graph, NewNodeList, PerCore, [Pid|ListOfProcs]).

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


%% @doc Function to construct the shortest path with a stack.
%% We push the vertex into the stack and then traverse from target to source.
shortest_path_from_previous(S, S, _, Path) ->
    [S|Path];
shortest_path_from_previous(S, T, Previous, Path) ->
    shortest_path_from_previous(S, maps:get(T, Previous), maps:remove(T, Previous), [T|Path]).

