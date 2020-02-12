-module(ws_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, State) ->
  {cowboy_websocket, Req, 
   State#{
     id => base64:encode(crypto:strong_rand_bytes(16)),
     mouseCoords => "0,0",
     name => "Unknown",
     mouseDown => false
    },
   #{compress => true, idle_timeout => 120000}}.

websocket_init(State = #{id := ClientID}) ->
  %% So we need all other websockets to introduce themselves
  %% while still within websocket_init (so these messages are seen first by the client)
  broadcaster:register_self(ClientID),
  broadcaster:send_all(["NEW,", ClientID], #{log => true}),
  {reply, {text, [<<"YOUR_ID,">>, ClientID]}, State}.

websocket_handle({text, Msg}, State = #{id := ClientID, mouseDown := MouseDown}) ->
  NewState = case binary_to_list(Msg) of
    "M_MOV," ++ Coords -> 
      %% Only add events to history if actively drawing, i.e. mouse is depressed
      broadcaster:send_all([<<"M_MOV,">>, ClientID, $,, Coords], #{log => MouseDown}),
      State#{mouseCoords => Coords};
    "M_UP" ->
      broadcaster:send_all([<<"M_UP,">>, ClientID], #{log => true}),
      State#{mouseDown => false};
    "M_DOWN" ->
      broadcaster:send_all([<<"M_DOWN,">>, ClientID], #{log => true}),
      State#{mouseDown => true};
    "SIZE," ++ Size ->
      broadcaster:send_all([<<"SIZE,">>, ClientID, $,, Size]),
      State;
    "COLOR," ++ Color ->
      broadcaster:send_all([<<"COLOR,">>, ClientID, $,, Color]),
      State;
    "NAME," ++ Name ->
      broadcaster:send_all([<<"NAME,">>, ClientID, $,, Name], #{log => true}),
      State#{name => Name};
    _Else ->
      io:format("Received OOB text message: ~p~n", [Msg]),
      State
  end, 
  {ok, NewState};
websocket_handle(_Frame, State) ->
  {ok, State}.

websocket_info({send, Msg}, State) ->
  {reply, {text, Msg}, State};
websocket_info(Msg, State) ->
  io:format("Websocket: OOB message ~p~n", [Msg]),
  {ok, State}.

terminate(_Reason, _Req, #{id := ClientID}) ->
  broadcaster:deregister_self(ClientID),
  broadcaster:send_all([<<"DISCONNECT,">>, ClientID], #{log => true}),
  ok.
