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
    %%{ok, Broker} = majordomo_broker:start(5555),
    majordomo_worker:start("127.0.0.1", 5555, <<"echo">>, fun(Request) -> Request end),
    majordomo_worker:start("127.0.0.1", 5555, <<"echo">>, fun(Request) -> Request end),
    majordomo_worker:start("127.0.0.1", 5555, <<"echo">>, fun(Request) -> Request end),
    majordomo_worker:start("127.0.0.1", 5555, <<"echo">>, fun(Request) -> Request end),
    Count = fun Loop(Requests, Replies) ->
        receive
            {request, _Service, _Request} ->
                Loop(Requests + 1, Replies);
            {reply, _Service, _Reply} ->
                Loop(Requests, Replies + 1);
            {From, get_count} ->
                From ! {count, Requests, Replies},
                Loop(Requests, Replies);
            stop ->
                ok;
            Response ->
                ?debugFmt("Response: ~p", [Response]),
                Loop(Requests, Replies)
        end
    end,
    Counter = spawn(fun() -> Count(0, 0) end),
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
    Send = fun
        Loop(0) ->
            ok;
        Loop(C) ->
            majordomo_client:send(Socket, <<"echo">>, <<"test">>),
            Counter ! {request, <<"echo">>, <<"test">>},
            Loop(C - 1)
    end,
    spawn(fun() -> Send(Count) end),
    wait_for_responses(Counter, Count),
    ?debugFmt("~p msg/sec", [Count * 1000000 / timer:now_diff(now(), Start)]),
    ok.

wait_for_responses(Receiver, Count) ->
    Receiver ! {self(), get_count},
    receive
        {count, _Requests, Count} ->
            Count;
        {count, Requests, Replies} ->
            ?debugFmt("~p requests, ~p replies", [Requests, Replies]),
            timer:sleep(100),
            wait_for_responses(Receiver, Count)
    end.
