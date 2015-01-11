-module(recon_monitor_controller).

-export([process/1]).

process(Req) ->
	Route = recon_monitor_uri_router:route(Req),

	case Route of
		undefined ->
			Req:not_found();
		Ewc ->
			ewc(Ewc)
	end.


ewc({ewc, Component, FuncName, Req}) ->
    ewc({ewc, Component, FuncName, Req, []});
ewc({ewc, Component, FuncName, Req, _Options}) ->
    try
        Time1 = now(),
        Controller = list_to_atom(atom_to_list(Component) ++ "_controller"),

        do_process(Controller, FuncName, Req),

        Time2 = now(),
        ewc_log(Controller, FuncName, Req, Time1, Time2)
    catch
        Type:Error ->
            lager:error("recon_monitor_apply error, the Type is ~p, the Error is ~p~n", [Type, Error]),
            lager:error("recon_monitor_apply error, the stack is ~p~n", [erlang:get_stacktrace()]),
        	Req:respond({500, [{"Content-Type", "text/plain"}],
								"request failed, sorry\n"})
    end;
ewc(Other) ->
    Other.

do_process(Controller, FuncName, Req) ->
	Response = recon_monitor_apply(Controller, FuncName, Req),
	do_response(Response, Req),
	ok.

do_response({error, Error}, Req) ->
	lager:error("process error, the Error is ~p~n", [Error]),
    Req:respond({500, [{"Content-Type", "text/plain"}],
						"request failed, sorry\n"});
do_response({json, Data}, Req) ->
    Req:ok({"application/json",[],[Data]}).

ewc_log(Controller, FuncName, Req, Time1, Time2) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_universal_time(Time1),

    Log = ["Processing ", Controller, "#", FuncName, " at ", Year, "-", Month, "-", Day, " ", Hour, ":", Min,
           ":", Sec, ") [", atom_to_list(Req:get(method)), "]", " \nCompleted in ",
           io_lib:format("~f",[timer:now_diff(Time2, Time1)/1000]), "ms"],

    lager:info(lists:concat(Log),[]).

%%
%% {json, Data} || {error, Error}
%%
recon_monitor_apply(Module, Function, Req) ->
    Response = Module:Function(Req),
    Response.
