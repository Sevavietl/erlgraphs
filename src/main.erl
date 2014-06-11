-module(main).
-compile(export_all).

-include("graphs.hrl").

init() ->
	try
		ok = mnesia:create_schema([node()]),
		ok = mnesia:start(),

		{atomic, ok} = mnesia:create_table(graphs,
									   [{attributes, record_info(fields, graphs)},
						 				{index, [#graphs.id]},
						 				{disc_copies, [node()]}]),
		{atomic, ok} = mnesia:create_table(vertecies,
									   [{attributes, record_info(fields, vertecies)},
						 				{index, [#vertecies.id]},
						 				{disc_copies, [node()]}]),
		{atomic, ok} = mnesia:create_table(edges,
									   [{attributes, record_info(fields, edges)},
						 				{index, [#edges.id]},
						 				{disc_copies, [node()]}])
	catch
		_ ->
			mnesia:stop(),
			mnesia:delete_scheme([node()])
	end.
