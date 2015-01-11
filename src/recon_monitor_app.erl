%% @author Mochi Media <dev@mochimedia.com>
%% @copyright recon_monitor Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the recon_monitor application.

-module(recon_monitor_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for recon_monitor.
start(_Type, _StartArgs) ->
    recon_monitor_deps:ensure(),
    lager:start(),
    recon_monitor_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for recon_monitor.
stop(_State) ->
    ok.
