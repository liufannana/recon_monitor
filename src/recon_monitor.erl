%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc recon_monitor.

-module(recon_monitor).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the recon_monitor server.
start() ->
    recon_monitor_deps:ensure(),
    ensure_started(crypto),
    application:start(recon_monitor).


%% @spec stop() -> ok
%% @doc Stop the recon_monitor server.
stop() ->
    application:stop(recon_monitor).
