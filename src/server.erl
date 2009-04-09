-module(server).

-export([start/0]).

start() ->
    io:format("starting server~n"),
    register(talker_server,
             spawn(fun() -> init_tables(),
                            loop() end)).

% TODO: detect if client dies and remove it from the tables.

loop() ->
    io:format("server loop~n"),
    receive
        {Pid, login, Nick} ->
            io:format("Login request from ~s~n", [Nick]),
            Pid ! {self(), login_ok},
            add_nick(Pid, Nick),
            loop();
        {Pid, join, Channel} ->
            Nick = get_nick(Pid),
            io:format("~s joins channel ~s~n", [Nick, Channel]),
            send_channel(Channel, Nick ++ " joins channel"),
            join_channel(Pid, Channel),
            loop();
        {Pid, send, Channel, Message} ->
            Nick = get_nick(Pid),
            send_channel(Channel, Nick ++ " " ++ Message),
            loop();
        Other ->
            io:format("Received other: ~p~n", [Other]),
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

send_channel(Channel, Message) ->
    Members = ets:lookup(chanlist, Channel),
    lists:foreach(fun({_Chan, Pid}) ->
                          Pid ! {channel_msg, Channel, Message} end,
                  Members).