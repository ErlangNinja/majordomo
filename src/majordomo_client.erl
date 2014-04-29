%%%-------------------------------------------------------------------
%%% @author erikh
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. apr 2014 21:02
%%%-------------------------------------------------------------------
-module(majordomo_client).
-author("erikh").

-include("majordomo.hrl").

%% API
-export([start/2, start/3, close/1, send/3, send/4]).

start(Address, Port) ->
    start(Address, Port, self()).

start(Address, Port, Dispatch) when is_pid(Dispatch) ->
    start(Address, Port, fun(Message) -> Dispatch ! Message end);

start(Address, Port, Dispatch) ->
    {ok, Socket} = ezmq:start([{type, dealer}]),
    spawn(fun() -> recv_loop(Socket, Dispatch) end),
    ok = ezmq:connect(Socket, tcp, Address, Port, []),
    Socket.

close(Socket) ->
    ezmq:close(Socket).

send(Socket, Service, Request) ->
    ezmq:send(Socket, [?MDP_CLIENT_HEADER, Service, Request]).

send(ReplyTo, Socket, Service, Request) ->
    CorrelationID = erlang:make_ref(),
    From = erlang:term_to_binary({ReplyTo, CorrelationID}),
    case ezmq:send(Socket, [?MDP_CLIENT_HEADER, From, Service, Request]) of
        ok ->
            {ok, CorrelationID};
        Other ->
            Other
    end.

recv_loop(Socket, Dispatch) ->
    case catch ezmq:recv(Socket) of
        {ok, [?MDP_CLIENT_HEADER, From, Service, Response]} ->
            {ReplyTo, CorrelationID} = erlang:binary_to_term(From),
            ReplyTo ! {reply, CorrelationID, Service, Response},
            recv_loop(Socket, Dispatch);
        {ok, [?MDP_CLIENT_HEADER, Service, Response]} ->
            Dispatch({reply, Service, Response}),
            recv_loop(Socket, Dispatch);
        {'EXIT', {normal, {gen_server, call, [Socket, {recv, infinity}, infinity]}}} ->
            Dispatch(closed);
        {'EXIT', Reason} ->
            Dispatch({'EXIT', Reason});
        Other ->
            error_logger:warning_msg("Unexpected response: ~p", [Other]),
            recv_loop(Socket, Dispatch)
    end.
