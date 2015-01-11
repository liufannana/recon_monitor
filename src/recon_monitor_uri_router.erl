-module(recon_monitor_uri_router).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(ROUTER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
		 start_link/1,
		 register_ws/1,
		 unregister_ws/1,
         route/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Arg) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Arg, []).

%% @doc Register api for web servcies
register_ws(WebServices) when is_list(WebServices) ->
    gen_server:call(?MODULE, {register, WebServices}).

%% @doc Unregister api for web servcies
unregister_ws(WebServices) when is_list(WebServices) ->
    gen_server:call(?MODULE, {unregister, WebServices}).

%% --------------------------------------------------------------------
%% Function: route/1
%% Description: distribute http requests to respective controllers 
%%              according to the mapping rules, which are loaded from
%%              app resource files. Decoding of parameters is also
%%              done here.
%% Args: 
%%      Req
%% Return: {ewc, Component::atom(), Func::atom(), Req}
%%      | undefined
%% --------------------------------------------------------------------
route(Req) ->
    Prefix = Req:get(path),
    case string:tokens(Prefix, "/") of
        [] ->
            gen_ewc({undefined, "index", []}, Req);
        [ComponentStr] ->
            %% if FuncStr is not specified,
            %% it will be setted to "index" as default.
            gen_ewc({ComponentStr}, Req);
        [ComponentStr, FuncStr | Params] ->
            gen_ewc({ComponentStr, FuncStr, Params}, Req);
        _ ->
            undefined
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    lager:info("uri router starting!~n", []), 
    process_flag(trap_exit, true),
    Router = ets:new(?ROUTER, [protected, named_table]),
    ets:insert(Router, Args),
    {ok, Router}.

handle_call({register, WebServices}, _From, Router) ->
    try 
        lists:foldl(
            fun(WebService, Acc) ->
                    case ets:insert_new(Router, WebService) of
                        false ->
                            %% rollback the registered web service here
                            clean_up_web_services(Acc),
                            throw(
                                lists:flatten(
                                io_lib:format(
                                "web service ~p has already been registered!", 
                                [WebService])));
                        _ ->
                            [WebService |Acc]
                    end
            end, [], WebServices),
        {reply, ok, Router}
    catch _:Msg ->
            {reply, {error, Msg}, Router}
    end;

handle_call({unregister, WebServices}, _From, State) ->
    clean_up_web_services(WebServices),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

clean_up_web_services(WebServices) when is_list(WebServices) ->
    lists:foreach(fun(WebService) ->
                          ets:delete_object(?ROUTER, WebService)
                  end, WebServices).

gen_ewc({ComponentStr}, Req) ->
    gen_ewc({ComponentStr, "index", []}, Req);
gen_ewc({ComponentStr, FuncStr}, Req) ->
    gen_ewc({ComponentStr, FuncStr, []}, Req);
gen_ewc({ComponentStr, FuncStr, Params}, Req) ->
	Component =
	    case ComponentStr of
	        undefined ->
	            "recon_monitor_controller";
	        _ ->
	            ComponentStr
	    end,
	MapRes = route_mapping({Component, FuncStr}),
	ComponentRoute = component_route(MapRes,Params, Req),
	ComponentRoute.

route_mapping({Component, Func}) when is_list(Component), is_list(Func) ->
    case ets:lookup(?ROUTER, {Component, Func}) of
        [{{Component, Func}, {Comp, Fun}, Options}] when is_list(Options) ->
            {Comp, Fun, Options};
        _ ->
            case ets:lookup(?ROUTER, Component) of
                [{Component, {Comp, Fun}, Options}] when is_list(Options) ->
                    {Comp, Fun, Options};
                [{Component, Comp, Options}] when is_list(Options) ->
                    {Comp, list_to_atom(Func), Options};
                _ ->
                    undefined
            end
    end.

component_route(undefined, _Params, _Req) ->
    undefined;
component_route({Component, Fun, Options}, _, Req) ->
    {ewc, Component, Fun, Req, Options}.