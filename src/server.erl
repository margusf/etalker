-module(server).

-export([start/0]).

start() ->
    log("START", []),
    register(talker_server,
             spawn(fun() -> init_tables(),
                            process_flag(trap_exit, true),
                            loop() end)).

% TODO: detect if client dies and remove it from the tables.

loop() ->
    io:format("server loop~n"),
    receive
        {Pid, login, Nick} ->
            log("LOGIN: nick ~s", [Nick]),
            % Link to client process so that when it dies,
            % we can remove it from tables
            Pid ! {self(), login_ok},
            add_nick(Pid, Nick),
            link(Pid),
            loop();

        {Pid, join, Channel} ->
            Nick = get_nick(Pid),
            log("JOIN: chan ~s, nick ~s", [Channel, Nick]),
            send_channel(Channel, Nick, "joins"),
            join_channel(Pid, Channel),
            loop();

        {Pid, leave, Channel} ->
            Nick = get_nick(Pid),
            log("LEAVE: chan ~s, nick ~s", [Channel, Nick]),
            % leave_channel sends notification message.
            leave_channel(Pid, Channel),
            loop();

        {Pid, send, Channel, Message} ->
            Nick = get_nick(Pid),
            log("SEND: chan ~s, nick ~s, msg ~s", [Channel, Nick, Message]),
            send_channel(Channel, Nick, Message),
            loop();

        {'EXIT', Pid, Reason} ->
            log("DISCONNECT: client ~s, reason ~p", [get_nick(Pid), Reason]),
            remove_client(Pid),
            loop();

        Other ->
            log("UNKNOWN MSG: ~p", [Other]),
            loop()
    end.

% Helper functions for managing nick and channel tables.

init_tables() ->
    ets:new(chanlist, [named_table, bag]),
    ets:new(nicklist, [named_table, set]).

add_nick(Pid, Nick) ->
    ets:insert_new(nicklist, {Pid, Nick}).

% Removes all data associated with this PID.
remove_client(Pid) ->
    Channels = ets:match(chanlist, {'$1', Pid}),
    lists:foreach(fun([Chan]) -> leave_channel(Pid, Chan) end,
                  Channels),
    ets:delete(nicklist, Pid).

get_nick(Pid) ->
    [{Pid, Nick}] = ets:lookup(nicklist, Pid),
    Nick.

join_channel(Pid, Channel) ->
    ets:insert(chanlist, {Channel, Pid}).

leave_channel(Pid, Channel) ->
    ets:delete_object(chanlist, {Channel, Pid}),
    send_channel(Channel, get_nick(Pid), "leaves").

send_channel(Channel, Nick, Message) ->
    Members = ets:lookup(chanlist, Channel),
    lists:foreach(fun({_Chan, Pid}) ->
                          log("SEND TO: pid ~p, nick ~s",
                              [Pid, get_nick(Pid)]),
                          Pid ! {self(), channel_msg, Channel, Nick, Message}
                  end,
                  Members).

log(Str, Arg) ->
    io:format("SRV: " ++ Str ++ "~n", Arg).