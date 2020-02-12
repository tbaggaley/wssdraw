-module(broadcaster).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, register_self/1, deregister_self/1, send_all/1, send_all/2, start_link/0, request_introductions/0]).

init(_) ->
  register(?MODULE, self()),
  {ok, #{clients => dict:new(), history => queue:new()}}.

handle_call({register, ClientID}, {From, _Tag}, State = #{clients := Clients, history := History}) ->
  NewDict = dict:store(ClientID, From, Clients),
  [From ! {send, Msg} || Msg <- queue:to_list(History)],
  {reply, ok, State#{clients => NewDict}}.

handle_cast({send_all, Msg, #{log := Logging}}, #{clients := Clients, history := History}) ->
  dict:map(fun(_ID, ClientPID) -> ClientPID ! {send, Msg} end, Clients),
  NewHistory = if Logging == true -> queue:snoc(History, Msg);
                  Logging /= true -> History
               end,
  {noreply, #{clients => Clients, history => NewHistory}};
handle_cast({deregister, ClientID}, State = #{clients := ClientDict}) ->
  {noreply, State#{clients => dict:erase(ClientID, ClientDict)}}.

%% Public API

start_link() ->
  gen_server:start_link(?MODULE, [], []).

register_self(ClientID) ->
  ok = gen_server:call(?MODULE, {register, ClientID}).

deregister_self(ClientID) -> 
  gen_server:cast(?MODULE, {deregister, ClientID}).

send_all(Msg, Options) -> 
  gen_server:cast(?MODULE, {send_all, Msg, Options}).

send_all(Msg) ->
  send_all(Msg, #{log => true}).

request_introductions() -> 
  ok = gen_server:call(?MODULE, introduce).
