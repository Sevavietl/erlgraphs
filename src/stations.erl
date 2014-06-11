-module(sations).

-record(stations, {id,
				 	name,
					x,
					y}).

-record(relation, {source_id,
					target_id,
					weight}).


