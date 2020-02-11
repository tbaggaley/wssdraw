-module(broadcaster).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, register_self/1, deregister_self/1, send_all/1, start_link/0, request_introductions/0]).

init(_) ->
  register(?MODULE, self()),
  {ok, dict:new()}.

handle_call({register, ClientID}, {From, _Tag}, ClientDict) ->
  NewDict = dict:store(ClientID, From, ClientDict),
  {reply, ok, NewDict};
handle_call(introduce, {From, _Tag}, ClientDict) ->
  F = fun(_ID, ClientPID) -> 
    if
      ClientPID == From ->
        %% Don't attempt to 'introduce' self to oneself -> causes issues!
        ok;
      ClientPID /= From ->
        Ref = make_ref(),
        ClientPID ! {introduce, From, Ref},
        receive 
          {ok, Ref} -> ok
        after 500 ->
          timeout
        end
    end
  end,
  dict:map(F, ClientDict),
  {reply, ok, ClientDict}.

handle_cast({send_all, Msg}, ClientDict) ->
  dict:map(fun(_ID, ClientPID) -> ClientPID ! {send, Msg} end, ClientDict),
  {noreply, ClientDict};
handle_cast({deregister, ClientID}, ClientDict) ->
  {noreply, dict:erase(ClientID, ClientDict)}.

%% Public API

start_link() ->
  gen_server:start_link(?MODULE, [], []).

register_self(ClientID) ->
  ok = gen_server:call(?MODULE, {register, ClientID}).

deregister_self(ClientID) -> 
  gen_server:cast(?MODULE, {deregister, ClientID}).

send_all(Msg) -> 
  gen_server:cast(?MODULE, {send_all, Msg}).

request_introductions() -> 
  ok = gen_server:call(?MODULE, introduce).
