-module(recon_monitor_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         state/0,
         rpc/3]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {nodes, done = [], last_done = [], undo = []}).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Arg) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Arg, []).

state() ->
    gen_server:call(?SERVER, state).

rpc(M, F, Arg) ->
    case gen_server:call(?SERVER, {rpc, M, F, Arg}) of
        R = {[{badrpc, _}|_], _} ->
            lager:error("the rpc failed, the result is ~p~n", [R]),
            "request unvalid";
        Result ->
            Result
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    lager:info("this is server init, arg is ~p~n", [Args]),
    gen_server:cast(?SERVER, init),
    timer:apply_interval(1000, gen_server, cast, [?SERVER, init]),
    {ok, #state{nodes = Args}}.

handle_call(state, _From, State) ->
    {reply, State, State};
handle_call({rpc, M, F, Arg}, _From, State = #state{done = DoneNodes, nodes = Nodes}) ->
    FailNodes = Nodes -- DoneNodes,
    {Success, Fail} = recon_monitor_lib:rpc(DoneNodes, M, F, Arg),
    Result = {Success, FailNodes ++ Fail},
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(init, State = #state{nodes = Nodes, done = DoneNodes}) ->
    gen_server:cast(?SERVER,init_cluster),
    {noreply, State#state{last_done = DoneNodes, done = [], undo = Nodes}};
handle_cast(init_cluster, State = #state{undo = [], done = DoneNodes, last_done = LastDoneNodes}) ->
    ReloadNodes = DoneNodes -- LastDoneNodes,
    recon:remote_load(ReloadNodes, [recon, recon_lib, recon_trace, recon_alloc, recon_monitor_api]),

    {noreply, State};
handle_cast(init_cluster, State = #state{undo = [Node|Rest], done = Done}) ->
    case net_adm:ping(Node) of
        pong ->
            NewState = State#state{undo = Rest, done = [Node|Done]},
            gen_server:cast(?SERVER, init_cluster),
            {noreply, NewState};
        _ ->
            gen_server:cast(?SERVER, init_cluster),
            NewState = State#state{undo = Rest},
            {noreply, NewState}
    end;
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

