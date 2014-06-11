%%% Records used for Mnesia database

%% Record for the graphs table
-record(graphs, {name,      % id of the graph
				 id}). % name of the graph

%% Record for the nodes table
-record(vertecies, {graph_id,       % id of the node
				id, % id of the graph wich consists node
				name,	  % name of the node 
				x,		  % x screen coordinates of the node
				y}).	  % y screen coordinates of the node

%% Record for edges table
-record(edges, {u_id,       % id of the edge
				id,	  % id of the source node
				v_id, 	  % id of the target node
				i}).      % weight of the edge
