-module(broadcaster).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, register_self/1, deregister_self/1, send_all/1, send_all/2, start_link/0, request_introductions/0]).

init(_) ->
  register(?MODULE, self()),
  {ok, #{clients => dict:new(), history => queue:new(), timer => undefined}}.

handle_call({register, ClientID}, {From, _Tag}, State = #{timer := Timer, clients := Clients, history := History}) ->
  io:format("Registering clientID: ~p~n", [ClientID]),
  timer:cancel(Timer),
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
    NewClients = dict:erase(ClientID, ClientDict),
    NoClients = dict:size(NewClients),
    {ok, Timer} = if NoClients =:= 0 ->
                         timer:apply_after(120000, os, cmd, ["sudo shutdown -hP 0"]);
                     true ->
                         {ok, undefined}
    end,
    {noreply, State#{clients => NewClients, timer => Timer}}.

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
