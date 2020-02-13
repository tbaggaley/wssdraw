%%%-------------------------------------------------------------------
%% @doc ws_server public API
%% @end
%%%-------------------------------------------------------------------

-module(ws_server_app).

-behaviour(application).

-export([start/2, stop/1, routes/0]).

routes() -> [
 { '_', [
  {"/", cowboy_static, {priv_file, ws_server, "www/index.html"}},
  {"/ws", ws_handler, #{}},
  {"/[...]", cowboy_static, {priv_dir, ws_server, "www"}}
 ] }
].

start(_StartType, _StartArgs) ->
  Routes = cowboy_router:compile(routes()),
  
  {ok, _} = cowboy:start_clear(httpd, [{port, 8080}], #{env =>
    #{dispatch => Routes}
  }),
  
  ws_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
