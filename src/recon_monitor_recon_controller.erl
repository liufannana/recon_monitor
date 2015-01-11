-module(recon_monitor_recon_controller).

-export([files/1,
		 socket/1,
         proc_count/1,
         port_types/1]).

-define(FORMAT(X), io_lib:format("~p", [X])).
files(_Req) ->
    Result = recon_monitor_server:rpc(recon_monitor_api, files, []),

    {json, ?FORMAT(Result)}.

socket(_Req) ->
    Result = recon_monitor_server:rpc(recon_monitor_api, socket, []),

    {json, ?FORMAT(Result)}.

proc_count(Req) ->
    Query = Req:parse_qs(),
    Attr = proplists:get_value("attr", Query,undefined),
    Num = proplists:get_value("num", Query,undefined),

    case {Attr, Num} of
        {undefined, _} ->
            {error, "request attr is undefined"};
        {_, undefined} ->
            {error, "request num is undefined"};
        _ ->
            Result = recon_monitor_server:rpc(recon_monitor_api, proc_count, [list_to_atom(Attr), list_to_integer(Num)]),
            {json, ?FORMAT(Result)}
    end.

port_types(_Req) ->
    Result = recon_monitor_server:rpc(recon_monitor_api, port_types, []),

    {json, ?FORMAT(Result)}.
