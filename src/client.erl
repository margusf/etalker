-module(client).

-export([main/1, main/2]).

main(Nick) ->
    main(Nick, node()).

main(Nick, ServerNode) ->
    Listener = spawn_link(fun() -> print_messages() end),
    Client = talkerclient:connect(Nick, ServerNode, Listener),
    run_loop(Client, "none").

run_loop(Client, Channel) ->
    case io:get_line(Channel ++ "> ") of
        eof -> done;
        Str -> process_line(string:strip(Str, right, $\n), Client, Channel)
    end.

print_messages() ->
    receive
        F -> io:format("IRC: ~p~n", [F])
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
        ["q"] ->
            talkerclient:shutdown(Client);
            
        [Command | _] ->
            io:format("Invalid command: ~s~n", [Command]),
            run_loop(Client, Channel);
        [] ->
            run_loop(Client, Channel)
    end;

process_line(Line, Client, Channel) ->
    talkerclient:send(Client, Channel, Line),
    run_loop(Client, Channel).