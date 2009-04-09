-module(talkerclient).

-export([connect/1,
         connect/2,
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
    connect(Nick, self()).

connect(Nick, Control) ->
    Pid = spawn(fun() -> do_login(Nick, Control) end),
    io:format("talkerclient ~p: spawned worker ~p for nick ~p~n",
              [Control, Pid, Nick]),
    Pid.

controlling_process(Client, Pid) ->
    Client ! {self(), controlling_process, Pid}.

join(Client, Channel) ->
    Client ! {self(), join, Channel}.

leave(Client, Channel) ->
    Client ! {self(), leave, Channel}.

send(Client, Channel, Message) ->
    Client ! {self(), send, Channel, Message}.

shutdown(Client) ->
    exit(Client, shutdown).

% Implementation stuff

do_login(Nick, Control) ->
    talker_server ! {self(), login, Nick},
    io:format("Sent login to server~n"),
    receive
        {Server, login_ok} ->
            io:format("Login ok~n"),
            Control ! {talker, connected},
            do_loop(Server, Control);
        {_Server, login_failed, Message} ->
            Control ! {talker, login_failed, Message}
    end.

do_loop(Server, Control) ->
    receive
        {Control, controlling_process, NewPid} ->
            io:format("New controlling process: ~p~n", [NewPid]),
            do_loop(Server, NewPid);
        {_Pid, join, Channel} ->
            Server ! {self(), join, Channel },
            do_loop(Server, Control);
        {_Pid, leave, Channel} ->
            Server ! {self(), leave, Channel},
            do_loop(Server, Control);
        {_Pid, send, Channel, Message} ->
            Server ! {self(), send, Channel, Message},
            do_loop(Server, Control);

        % Messages from server.
        {Server, channel_msg, Message} ->
            Control ! {talker, channel_msg, Message}
    end.