%%%-------------------------------------------------------------------
%%% @author erikh
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. apr 2014 09:40
%%%-------------------------------------------------------------------
-module(majordomo_broker).
-author("erikh").

-include("majordomo.hrl").

%% API
-export([start/1, start_link/1, close/1]).

-record(state, {
    socket = undefined,
    workers,
    dispatchers,
    waiting = []
}).

start(Port) ->
    case ezmq:start([{type, router}]) of
        {ok, Socket} ->
            case ezmq:bind(Socket, tcp, Port, []) of
                ok ->
                    spawn(fun() ->
                        State = #state{
                            socket = Socket,
                            workers = ets:new(workers, [public, {write_concurrency, true}]),
                            dispatchers = ets:new(dispatchers, [])
                        },
                        RecvLoop = recv_loop(),
                        RecvLoop(State)
                    end),
                    {ok, Socket};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

start_link(Port) ->
    case start(Port) of
        {ok, Socket} ->
            link(Socket),
            {ok, Socket};
        {error, Reason} ->
            {error, Reason}
    end.

close(Socket) ->
    ezmq:close(Socket).

recv_loop() ->
    fun Loop(State) ->
        case catch ezmq:recv(State#state.socket) of
            {ok, {Id, [?MDP_CLIENT_HEADER | [<<"mmi.", MMIService/binary>> | Request]]}} ->
                Loop(mmi_dispatch(Id, [<<"mmi.", MMIService/binary>> | Request], State));
            {ok, {Id, [?MDP_CLIENT_HEADER | Request]}} ->
                Loop(client_dispatch(Id, Request, State));
            {ok, {Id, [?MDP_WORKER_HEADER | Reply]}} ->
                Loop(worker_dispatch(Id, Reply, State));
            {'EXIT', _Reason} ->
                ok;
            Message ->
                error_logger:warning_msg("Unhandled message ~p", [Message]),
                Loop(State)
        end
    end.

mmi_dispatch(Id, [?MMI_SERVICE, Service], State = #state{socket = Socket}) ->
    {Dispatcher, NewState} = get_dispatcher_for_service(Service, State),
    Dispatcher ! {mmi_service, Id},
    NewState;

mmi_dispatch(Id, [MMIService | _Request], State = #state{socket = Socket}) ->
    ezmq:send(Socket, {Id, [?MDP_CLIENT_HEADER, MMIService, ?MMI_NOT_IMPLEMENTED]}),
    State.

client_dispatch(Id, [Service, Request], State) ->
    {Dispatcher, NewState} = get_dispatcher_for_service(Service, State),
    Dispatcher ! {request, Id, Request},
    NewState;

client_dispatch(_Id, _Request, State) ->
    State.

worker_dispatch(Id, [?MDP_READY_CMD, Service], State) ->
    {Dispatcher, NewState} = get_dispatcher_for_service(Service, State),
    Dispatcher ! {ready, Id},
    NewState;

worker_dispatch(Id, [?MDP_REPLY_CMD, Client, <<>>, Reply], State) ->
    get_dispatcher_for_worker(Id, State) ! {reply, Id, Client, Reply},
    State;

worker_dispatch(Id, [?MDP_HEARTBEAT_CMD], State) ->
    get_dispatcher_for_worker(Id, State) ! {heartbeat, Id},
    State;

worker_dispatch(Id, [?MDP_DISCONNECT_CMD], State) ->
    get_dispatcher_for_worker(Id, State) ! {disconnect, Id},
    State;

worker_dispatch(_Id, _Reply, State) ->
    State.

dispatcher() ->
    fun
        Dispatch(Service, State = #state{socket = Socket, workers = Workers}, Queue, [Worker | IdlePool], ActivePool) when Queue /= {[], []} ->
            {{value, {Id, Request}}, SmallerQueue} = queue:out(Queue),
            case ezmq:send(Socket, {Worker, [?MDP_WORKER_HEADER, ?MDP_REQUEST_CMD, erlang:term_to_binary(Id), <<>>, Request]}) of
                ok ->
                    Dispatch(Service, State, SmallerQueue, IdlePool, lists:append(ActivePool, [Worker]));
                invalid_identity ->
                    ets:delete(Workers, Worker),
                    Dispatch(Service, State, Queue, IdlePool, ActivePool);
                _ ->
                    Dispatch(Service, State, Queue, lists:append(IdlePool, [Worker]), ActivePool)

            end;
        Dispatch(Service, State = #state{socket = Socket, workers = Workers}, Queue, IdlePool, ActivePool) ->
            receive
                {ready, Id} ->
                    IsMember = lists:member(Id, IdlePool) or lists:member(Id, ActivePool),
                    if
                        IsMember ->
                            ezmq:send(Socket, {Id, [?MDP_WORKER_HEADER, ?MDP_DISCONNECT_CMD]}),
                            ets:delete(Workers, Id),
                            Dispatch(Service, State, Queue, lists:delete(Id, IdlePool), lists:delete(Id, ActivePool));
                        true ->
                            ets:insert(Workers, {Id, self(), get_expiration()}),
                            Dispatch(Service, State, Queue, lists:append(IdlePool, [Id]), ActivePool)
                    end;
                {request, Id, Request} ->
                    Dispatch(Service, State, queue:in({Id, Request}, Queue), IdlePool, ActivePool);
                {reply, Id, Client, Reply} ->
                    ets:insert(State#state.workers, {Id, self(), get_expiration()}),
                    ezmq:send(Socket, {erlang:binary_to_term(Client), [?MDP_CLIENT_HEADER, Service, Reply]}),
                    Dispatch(Service, State, Queue, lists:append(IdlePool, [Id]), lists:delete(Id, ActivePool));
                {heartbeat, Id} ->
                    ets:insert(State#state.workers, {Id, self(), get_expiration()}),
                    case lists:member(Id, IdlePool) of
                        true ->
                            Dispatch(Service, State, Queue, lists:append(lists:delete(Id, IdlePool), [Id]), ActivePool);
                        false ->
                            Dispatch(Service, State, Queue, IdlePool, lists:append(lists:delete(Id, ActivePool), [Id]))
                    end;
                {disconnect, Id} ->
                    ets:delete(Workers, Id),
                    Dispatch(Service, State, Queue, lists:delete(Id, IdlePool), lists:delete(Id, ActivePool));
                {heartbeat_send} ->
                    {Expired, IdleRemaining} = get_expired_workers(IdlePool, State),
                    [ets:delete(Workers, Id) || Id <- Expired],
                    [ezmq:send(Socket, {Id, [?MDP_WORKER_HEADER, ?MDP_HEARTBEAT_CMD]}) || Id <- IdleRemaining],
                    Dispatch(Service, State, Queue, IdleRemaining, ActivePool);
                {mmi_service, Id} ->
                    WorkerCount = length(IdlePool) + length(ActivePool),
                    if
                        WorkerCount == 0 ->
                            ezmq:send(Socket, {Id, [?MDP_CLIENT_HEADER, ?MMI_SERVICE, ?MMI_NOT_FOUND]});
                        true ->
                            ezmq:send(Socket, {Id, [?MDP_CLIENT_HEADER, ?MMI_SERVICE, ?MMI_FOUND]})
                    end,
                    Dispatch(Service, State, Queue, IdlePool, ActivePool);
                Message ->
                    error_logger:warning_msg("Unexpected message: ~p", [Message]),
                    Dispatch(Service, State, Queue, IdlePool, ActivePool)
            end
    end.

get_expiration() ->
    get_timestamp(?HEARTBEAT_INTERVAL * ?HEARTBEAT_LIVENESS).

get_timestamp(Offset) ->
    {Mega, Sec, Micro} = erlang:now(),
    (Mega * 1000000 + Sec) * 1000000 + Micro + Offset * 1000.

get_dispatcher_for_service(Service, State = #state{dispatchers = Dispatchers}) ->
    case ets:lookup(Dispatchers, Service) of
        [{Service, Dispatcher}] ->
            {Dispatcher, State};
        [] ->
            Dispatcher = spawn(fun() -> D = dispatcher(), D(Service, State, queue:new(), [], []) end),
            timer:send_interval(?HEARTBEAT_INTERVAL, Dispatcher, {heartbeat_send}),
            ets:insert(Dispatchers, {Service, Dispatcher}),
            {Dispatcher, State}
    end.

get_dispatcher_for_worker(Worker, #state{workers = Workers}) ->
    ets:lookup_element(Workers, Worker, 2).

get_expired_workers(Remaining, State) ->
    get_expired_workers(get_timestamp(0), Remaining, [], State).

get_expired_workers(_Timestamp, [], Expired, _State) ->
    {Expired, []};

get_expired_workers(Timestamp, [Worker | Remaining], Expired, State = #state{workers = Workers}) ->
    Expiration = ets:lookup_element(Workers, Worker, 3),
    if
        Expiration < Timestamp ->
            get_expired_workers(Timestamp, Remaining, [Worker | Expired], State);
        true ->
            {Expired, [Worker | Remaining]}
    end.