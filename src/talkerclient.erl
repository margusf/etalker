-module(talkerclient).

-export([connect/1,
         controlling_process/2,
         join/2,
         leave/2,
         send/3,
         shutdown/1]).

% TODO: hostname argument
% After login, controlling process will receive update messages:
% {talker, connected}
% {talker, login_failed, message}
% {talker, msg, nick, message}
% {talker, event, nick, message}
% {talker, shutdown}
connect(Nick) ->
    Self = self(),
    spawn(fun() -> do_login(Nick, Self) end).

controlling_process(Client, Pid) ->
    none.

join(Client, Channel) ->
    none.

leave(Client, Channel) ->
    none.

send(Client, Channel, Message) ->
    none.

shutdown(Client) ->
    none.

% Implementation stuff

do_login(Nick, Control) ->
    talker_server ! {login, Nick},
    receive
        {Server, login_ok} ->
            Control ! {talker, connected};
        {Server, login_failed, Message} ->
            Control ! {talker, login_failed, Message}
    end.