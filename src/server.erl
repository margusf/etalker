-module(server).

-export([start/0]).

start() ->
    io:format("starting server~n"),
    register(talker_server, spawn(fun() -> loop() end)).

loop() ->
    io:format("server loop~n"),
    receive
        {Pid, login, Nick} ->
            io:format("Login request: ~p~n", [Nick]),
            Pid ! {self(), login_ok},
            loop();
        Other ->
            io:format("Received other: ~p~n", [Other]),
            loop()
    end.