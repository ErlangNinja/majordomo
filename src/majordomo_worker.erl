%%%-------------------------------------------------------------------
%%% @author erikh
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. apr 2014 21:25
%%%-------------------------------------------------------------------
-module(majordomo_worker).
-author("erikh").

-include("majordomo.hrl").

%% API
-export([start/4, close/1]).

-record(state, {
    address = {127, 0, 0, 1},
    port = 5555,
    service = undefined,
    dispatch = undefined,
    socket = undefined,
    liveness = ?HEARTBEAT_LIVENESS
}).

start(Address, Port, Service, Dispatch) ->
    InitialState = #state{
        address = Address,
        port = Port,
        service = Service,
        dispatch = Dispatch
    },
    Worker = spawn(fun() -> work_loop(InitialState) end),
    timer:send_interval(?HEARTBEAT_INTERVAL, Worker, {heartbeat, send}),
    Worker.

close(Worker) ->
    Worker ! close,
    ok.

work_loop(State = #state{socket = undefined}) ->
    Worker = self(),
    {ok, Socket} = ezmq:start([{type, dealer}]),
    spawn(fun() -> recv_loop(Socket, Worker) end),
    ezmq:connect(Socket, tcp, State#state.address, State#state.port, []),
    ezmq:send(Socket, [?MDP_WORKER_HEADER, ?MDP_READY_CMD, State#state.service]),
    work_loop(State#state{socket = Socket});

work_loop(State = #state{dispatch = Dispatch}) ->
    receive
        {request, Client, Request} when is_pid(Dispatch) ->
            Dispatch ! {request, self(), Client, Request},
            work_loop(State#state{liveness = ?HEARTBEAT_LIVENESS});
        {request, Client, Request} ->
            ezmq:send(State#state.socket, [?MDP_WORKER_HEADER, ?MDP_REPLY_CMD, Client, <<>>, Dispatch(Request)]),
            work_loop(State#state{liveness = ?HEARTBEAT_LIVENESS});
        {reply, Client, Response} ->
            ezmq:send(State#state.socket, [?MDP_WORKER_HEADER, ?MDP_REPLY_CMD, Client, <<>>, Response]),
            work_loop(State#state{liveness = ?HEARTBEAT_LIVENESS});
        {heartbeat, recv} ->
            work_loop(State#state{liveness = ?HEARTBEAT_LIVENESS});
        {heartbeat, send} ->
            if
                State#state.liveness < 1 ->
                    ezmq:close(State#state.socket),
                    work_loop(State#state{socket = undefined, liveness = ?HEARTBEAT_LIVENESS});
                true ->
                    ezmq:send(State#state.socket, [?MDP_WORKER_HEADER, ?MDP_HEARTBEAT_CMD]),
                    work_loop(State#state{liveness = State#state.liveness - 1})
            end;
        disconnect ->
            ezmq:close(State#state.socket),
            work_loop(State#state{socket = undefined, liveness = ?HEARTBEAT_LIVENESS});
        close ->
            ezmq:close(State#state.socket);
        Cmd ->
            error_logger:warning_msg("Unexpected command: ~p", [Cmd]),
            work_loop(State)
    end.

recv_loop(Socket, Worker) ->
    case catch ezmq:recv(Socket) of
        {ok, [?MDP_WORKER_HEADER, ?MDP_REQUEST_CMD, Client, <<>>, Request]} ->
            Worker ! {request, Client, Request},
            recv_loop(Socket, Worker);
        {ok, [?MDP_WORKER_HEADER, ?MDP_HEARTBEAT_CMD]} ->
            Worker ! {heartbeat, recv},
            recv_loop(Socket, Worker);
        {ok, [?MDP_WORKER_HEADER, ?MDP_DISCONNECT_CMD]} ->
            Worker ! disconnect,
            recv_loop(Socket, Worker);
        {'EXIT', _Reason} ->
            ok;
        Reply ->
            error_logger:warning_msg("Unexpected reply: ~p", [Reply]),
            recv_loop(Socket, Worker)
    end.