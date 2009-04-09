-module(client).

-export([main/1]).

main(Nick) ->
    Listener = spawn(fun() -> print_messages() end),
    Client = talkerclient:connect(Nick, Listener),
    run_loop(Client, none).

run_loop(Client, Channel) ->
    case io:get_line("irc> ") of
        eof -> done;
        Str -> io:format("got string: '~s'~n", [Str]),
               process_line(string:strip(Str, right), Client, Channel)
    end.

print_messages() ->
    receive
        F -> io:format("IRC: ~s~n", [F])
    end,
    print_messages().

process_line("", Client, Channel) ->
    run_loop(Client, Channel);

process_line([$/ | Rest], Client, Channel) ->
    Parts = string:tokens(Rest, " \t"),
    case Parts of
        ["join", JoinChan] ->
            talkerclient:join(Client, JoinChan),
            run_loop(Client, Channel);
        ["chan", SelectChan] ->
            run_loop(Client, SelectChan);
            
        [Command | _] ->
            io:format("Invalid command: ~s~n", [Command]),
            run_loop(Client, Channel);
        [] ->
            run_loop(Client, Channel)
    end;

process_line(Line, Client, Channel) ->
    talkerclient:send(Client, Channel, Line),
    run_loop(Client, Channel).