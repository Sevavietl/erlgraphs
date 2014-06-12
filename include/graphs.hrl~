%%% Records used for Mnesia database

%% Record for the graphs table
-record(graphs, {id,      % id of the graph
				 name}). % name of the graph

%% Record for the nodes table
-record(vertecies, {id,       % id of the node
				graph_id, % id of the graph wich consists node
				name,	  % name of the node 
				x,		  % x screen coordinates of the node
				y}).	  % y screen coordinates of the node

%% Record for edges table
-record(edges, {id,       % id of the edge
				u_id,	  % id of the source node
				v_id, 	  % id of the target node
				i}).      % weight of the edge
