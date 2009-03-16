-module(client).

-export([main/1]).

main(Nick) ->
    Client = talkerclient:connect(Nick),
    spawn(fun() -> talkerclient:controlling_process(Client, self()),
                   print_messages(Client) end),
    run_loop(Client).

run_loop(Client) ->
    case io:get_line("irc> ") of
        eof -> done;
        Str -> io:format("got string: '~p'~n", [Str]),
               run_loop(Client)
    end.

print_messages(Client) ->
    receive
        F -> io:format("~p~n", [F])
    end,
    print_messages(Client).