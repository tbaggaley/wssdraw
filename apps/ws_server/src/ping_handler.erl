-module(ping_handler).
-export([init/2]).

init(Req, State) ->
    Req2 = cowboy_req:reply(200, #{
             <<"content-type">> => <<"text/plain">>,
             <<"access-control-allow-origin">> => <<"*">>
            }, <<"ok">>, Req),
    {ok, Req2, State}.
