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
    lager:start(),
    application:start(ezmq),

    majordomo_broker:start(5555),
    Socket = majordomo_client:start("127.0.0.1", 5555, self()),

    majordomo_worker:start("127.0.0.1", 5555, <<"echo">>, fun(Request) -> Request end),
    majordomo_worker:start("127.0.0.1", 5555, <<"echo">>, fun(Request) -> Request end),
    majordomo_worker:start("127.0.0.1", 5555, <<"echo">>, fun(Request) -> Request end),
    majordomo_worker:start("127.0.0.1", 5555, <<"echo">>, fun(Request) -> Request end),

    Socket.

teardown(Socket) ->
    majordomo_client:close(Socket),
    application:stop(ezmq).

tests(Socket) ->
    [{timeout, 60, fun() -> test_client(Socket) end}].

test_client(Socket) ->
    Count = 10000,
    Start = now(),
    ReplyTo = self(),
    spawn(fun() -> send_loop(Socket, ReplyTo, Count) end),
    {_Requests, _Replies, Size} = recv_loop(Count, 0, 0, 0),
    ?debugFmt("~p msg/sec", [Count * 1000000 / timer:now_diff(now(), Start)]),
    ?debugFmt("~p bytes/sec", [Size * 1000000 / timer:now_diff(now(), Start)]),
    ok.

send_loop(_Socket, _Counter, 0) ->
    ok;

send_loop(Socket, ReplyTo, Count) ->
    Data = crypto:strong_rand_bytes(random:uniform(1000)),
    ReplyTo ! majordomo_client:send(ReplyTo, Socket, <<"echo">>, Data),
    send_loop(Socket, ReplyTo, Count - 1).

recv_loop(Limit, Requests, Replies, Size) when Limit == Replies ->
    {Requests, Replies, Size};

recv_loop(Limit, Requests, Replies, Size) ->
    receive
        {ok, _CorrelationID} ->
            recv_loop(Limit, Requests + 1, Replies, Size);
        {reply, <<"echo">>, Reply} ->
            recv_loop(Limit, Requests, Replies + 1, Size + size(Reply));
        {reply, _CorrelationID, <<"echo">>, Reply} ->
            recv_loop(Limit, Requests, Replies + 1, Size + size(Reply));
        Response ->
            ?debugFmt("Response: ~p", [Response]),
            recv_loop(Limit, Requests, Replies, Size)
    end.