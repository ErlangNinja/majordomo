%%%-------------------------------------------------------------------
%%% @author erikh
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. apr 2014 09:14
%%%-------------------------------------------------------------------
-module(majordomo_tests).
-author("erikh").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

majordomo_test_() ->
    {setup,
        spawn,
        fun setup/0,
        fun teardown/1,
        fun tests/1}.

setup() ->
    application:start(sasl),
    application:start(gen_listener_tcp),
    application:start(ezmq),
    majordomo_broker:start(5555),
    majordomo_worker:start("127.0.0.1", 5555, <<"echo">>, fun(Request) -> Request end),
    majordomo_worker:start("127.0.0.1", 5555, <<"echo">>, fun(Request) -> Request end),
    majordomo_worker:start("127.0.0.1", 5555, <<"echo">>, fun(Request) -> Request end),
    majordomo_worker:start("127.0.0.1", 5555, <<"echo">>, fun(Request) -> Request end),
    Counter = spawn(fun() -> count_loop(0, 0) end),
    Socket = majordomo_client:start("127.0.0.1", 5555, Counter),
    {Socket, Counter}.

teardown({Socket, Counter}) ->
    Counter ! stop,
    majordomo_client:close(Socket),
    application:stop(ezmq).

tests({Socket, Callback}) ->
    [{timeout, 60, fun() -> test_client(Socket, Callback) end}].

test_client(Socket, Counter) ->
    Count = 10000,
    Start = now(),
    spawn(fun() -> send_loop(Socket, Counter, Count) end),
    wait_loop(Counter, Count),
    ?debugFmt("~p msg/sec", [Count * 1000000 / timer:now_diff(now(), Start)]),
    ok.

send_loop(_Socket, _Counter, 0) ->
    ok;

send_loop(Socket, Counter, Count) ->
    Counter ! majordomo_client:send(Counter, Socket, <<"echo">>, <<"test">>),
    send_loop(Socket, Counter, Count - 1).

count_loop(Requests, Replies) ->
    receive
        {ok, _CorrelationID} ->
            count_loop(Requests + 1, Replies);
        {reply, _Service, _Reply} ->
            count_loop(Requests, Replies + 1);
        {reply, _CorrelationID, _Service, _Reply} ->
            count_loop(Requests, Replies + 1);
        {From, get_count} ->
            From ! {count, Requests, Replies},
            count_loop(Requests, Replies);
        stop ->
            ok;
        Response ->
            ?debugFmt("Response: ~p", [Response]),
            count_loop(Requests, Replies)
    end.

wait_loop(Counter, Count) ->
    Counter ! {self(), get_count},
    receive
        {count, _Requests, Count} ->
            Count;
        {count, Requests, Replies} ->
            ?debugFmt("~p requests, ~p replies", [Requests, Replies]),
            timer:sleep(100),
            wait_loop(Counter, Count)
    end.
