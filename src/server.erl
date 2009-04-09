-module(server).

-export([start/0]).

start() ->
    log("START", []),
    register(talker_server,
             spawn(fun() -> init_tables(),
                            loop() end)).

% TODO: detect if client dies and remove it from the tables.

loop() ->
    io:format("server loop~n"),
    receive
        {Pid, login, Nick} ->
            log("LOGIN: nick ~s", [Nick]),
            Pid ! {self(), login_ok},
            add_nick(Pid, Nick),
            loop();
        {Pid, join, Channel} ->
            Nick = get_nick(Pid),
            log("JOIN: chan ~s, nick ~s", [Channel, Nick]),
            send_channel(Channel, Nick, "joins"),
            join_channel(Pid, Channel),
            loop();
        {Pid, send, Channel, Message} ->
            Nick = get_nick(Pid),
            log("SEND: chan ~s, nick ~s, msg ~s", [Channel, Nick, Message]),
            send_channel(Channel, Nick, Message),
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

get_nick(Pid) ->
    [{Pid, Nick}] = ets:lookup(nicklist, Pid),
    Nick.

join_channel(Pid, Channel) ->
    ets:insert(chanlist, {Channel, Pid}).

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