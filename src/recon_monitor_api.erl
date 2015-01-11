-module(recon_monitor_api).

-export([files/0,
		 socket/0,
		 proc_count/2,
		 port_types/0]).

-define(NODE, to_binary(node())).

files() ->
	{?NODE, lists:map(fun(X) ->
							{X, [recon:port_info(X, memory_used), recon:port_info(X, io)]}
						end, recon:files())}.

socket() ->
    TcpPorts = lists:map(fun(X) ->
							{X, [recon:port_info(X, memory_used), recon:port_info(X, io)]}
						end, recon:tcp()),
    UdpPorts = lists:map(fun(X) ->
							{X, [recon:port_info(X, memory_used), recon:port_info(X, io)]}
						end, recon:udp()),
    SctpPorts = lists:map(fun(X) ->
							{X, [recon:port_info(X, memory_used), recon:port_info(X, io)]}
						end, recon:sctp()),

    {?NODE, [{"tcp", TcpPorts}, {"udp", UdpPorts}, {"sctp", SctpPorts}]}.

proc_count(Attr, Num) ->

	ProcCount = recon:proc_count(Attr, Num),

	{?NODE, ProcCount}.

port_types() ->

	ProcTypes = recon:port_types(),

	{?NODE, ProcTypes}.







to_binary(X) when is_integer(X) ->
	list_to_binary(erlang:integer_to_list(X));
to_binary(X) when is_list(X) ->
	list_to_binary(X);
to_binary(X) when is_atom(X) ->
	list_to_binary(erlang:atom_to_list(X));
to_binary(X) when is_binary(X) ->
	X;
to_binary(X) when is_float(X) ->
	list_to_binary(erlang:float_to_list(X));
to_binary(X) when is_function(X) ->
	list_to_binary(erlang:fun_to_list(X));
to_binary(X) when is_pid(X) ->
	list_to_binary(erlang:pid_to_list(X));
to_binary(X) when is_port(X) ->
	list_to_binary(erlang:port_to_list(X));
to_binary(X) when is_port(X) ->
	list_to_binary(erlang:port_to_list(X));
to_binary(X) when is_reference(X) ->
	list_to_binary(erlang:ref_to_list(X));
to_binary(_) ->
	<<"type error">>.
		