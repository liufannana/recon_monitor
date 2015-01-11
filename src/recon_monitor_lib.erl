-module(recon_monitor_lib).

-export([rpc/3,
		 rpc/4,
		 rpc/5,
		 request_unvalid/0,
     get_env/3]).

rpc(Module, Func, Args) when
  is_atom(Module),
  is_atom(Func),
  is_list(Args) ->
	rpc([node()|nodes()], Module, Func, Args, infinity).

rpc(Nodes, Module, Func, Args) when
  is_list(Nodes), 
  is_atom(Module),
  is_atom(Func),
  is_list(Args) ->
	rpc(Nodes, Module, Func, Args, infinity).

rpc(Nodes = [_|_], Module, Func, Args, Timeout) when
  is_list(Nodes), 
  is_atom(Module),
  is_atom(Func),
  is_list(Args),
  is_integer(Timeout);Timeout =:= infinity ->
	rpc:multicall(Nodes, Module, Func, Args, Timeout);
rpc(Node, Module, Func, Args, Timeout) when 
  is_atom(Node), 
  is_atom(Module),
  is_atom(Func),
  is_list(Args),
  is_integer(Timeout); Timeout =:= infinity ->
	rpc([Node], Module, Func, Args, Timeout).

request_unvalid() ->
	"{\"code\":\"0001\",\"msg\":\"unvalid message\"}".

get_env(App, K, Def) ->
  case application:get_env(App, K) of
    {ok, V} -> V;
    _ -> Def
  end.