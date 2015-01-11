-module(recon_monitor_json_util).

-export([encode_object/1]).

encode_object(Object) ->
	{struct, lists:map(fun({Key, Value}) ->
							{Key, Value}
						end)}.