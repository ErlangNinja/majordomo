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
    Pid = spawn(fun() -> Loop = work_loop(), Loop(InitialState) end),
    timer:send_interval(?HEARTBEAT_INTERVAL, Pid, {heartbeat, send}),
    Pid.

close(Worker) ->
    Worker ! close,
    ok.

work_loop() ->
    Worker = self(),
    fun
        Loop(State = #state{socket = undefined}) ->
            {ok, Socket} = ezmq:start([{type, dealer}]),
            spawn(fun() -> RecvLoop = recv_loop(), RecvLoop(Socket, Worker) end),
            ezmq:connect(Socket, tcp, State#state.address, State#state.port, []),
            ezmq:send(Socket, [?MDP_WORKER_HEADER, ?MDP_READY_CMD, State#state.service]),
            Loop(State#state{socket = Socket});
        Loop(State = #state{dispatch = Dispatch}) ->
            receive
                {request, Client, Request} when is_pid(Dispatch) ->
                    Dispatch ! {request, Worker, Client, Request},
                    Loop(State#state{liveness = ?HEARTBEAT_LIVENESS});
                {request, Client, Request} ->
                    ezmq:send(State#state.socket, [?MDP_WORKER_HEADER, ?MDP_REPLY_CMD, Client, <<>>, Dispatch(Request)]),
                    Loop(State#state{liveness = ?HEARTBEAT_LIVENESS});
                {reply, Client, Response} ->
                    ezmq:send(State#state.socket, [?MDP_WORKER_HEADER, ?MDP_REPLY_CMD, Client, <<>>, Response]),
                    Loop(State#state{liveness = ?HEARTBEAT_LIVENESS});
                {heartbeat, recv} ->
                    Loop(State#state{liveness = ?HEARTBEAT_LIVENESS});
                {heartbeat, send} ->
                    if
                        State#state.liveness < 1 ->
                            ezmq:close(State#state.socket),
                            Loop(State#state{socket = undefined, liveness = ?HEARTBEAT_LIVENESS});
                        true ->
                            ezmq:send(State#state.socket, [?MDP_WORKER_HEADER, ?MDP_HEARTBEAT_CMD]),
                            Loop(State#state{liveness = State#state.liveness - 1})
                    end;
                disconnect ->
                    ezmq:close(State#state.socket),
                    Loop(State#state{socket = undefined, liveness = ?HEARTBEAT_LIVENESS});
                close ->
                    ezmq:close(State#state.socket);
                Cmd ->
                    error_logger:warning_msg("Unexpected command: ~p", [Cmd]),
                    Loop(State)
            end
    end.

recv_loop() ->
    fun Loop(Socket, Worker) ->
        case catch ezmq:recv(Socket) of
            {ok, [?MDP_WORKER_HEADER, ?MDP_REQUEST_CMD, Client, <<>>, Request]} ->
                Worker ! {request, Client, Request},
                Loop(Socket, Worker);
            {ok, [?MDP_WORKER_HEADER, ?MDP_HEARTBEAT_CMD]} ->
                Worker ! {heartbeat, recv},
                Loop(Socket, Worker);
            {ok, [?MDP_WORKER_HEADER, ?MDP_DISCONNECT_CMD]} ->
                Worker ! disconnect,
                Loop(Socket, Worker);
            {'EXIT', _Reason} ->
                ok;
            Reply ->
                error_logger:warning_msg("Unexpected reply: ~p", [Reply]),
                Loop(Socket, Worker)
        end
    end.