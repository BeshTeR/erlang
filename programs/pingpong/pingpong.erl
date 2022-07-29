%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Реализации PingPong с одним loop
%%% N - количество циклов. При N < 0 работает в бесконечном цикле
%%% @end
%%% ----------------------------------------------------------------------------
-module(pingpong).

%% API -------------------------------------------------------------------------
-export([start1/1, start2/1, start3/1]).

%% -----------------------------------------------------------------------------
%% @doc Вариант 1: Отдельные функции для ping и pong.
%% @end
%% -----------------------------------------------------------------------------
start1(N) ->
    spawn(fun() -> ping(spawn(fun pong/0), N) end),
    ok.

ping(Pid, 0) ->
    exit(Pid);
ping(Pid, N) ->
    Pid ! {ping, self()},
    receive
        pong -> out(Pid, pong, self())
    end,
    ping(Pid, N-1).

pong() ->
    receive
        {ping, Pid} ->
            out(Pid, ping, self()),
            Pid ! pong,
            pong()
    end.

%% -----------------------------------------------------------------------------
%% @doc Вариант 2: Один loop. Передача стейта только через сообщения.
%% @end
%% -----------------------------------------------------------------------------
start2(N) ->
    spawn(fun loop/0) ! {self(), {ping, N}},
    loop().

loop() ->
    receive
        {From, {_, 0}} ->
            From ! {self(), {stop, 0}},
            ok;
        {From, {Msg, N}} ->
            out(From, Msg, self()),
            From ! {self(),
                case Msg of
                    ping -> {pong, N};
                    pong -> {ping, N-1}
                end},
            loop()
    end.

%% -----------------------------------------------------------------------------
%% @doc Вариант 3: Один loop. Сообщения без кортежей (только атомы ping или pong).
%% @end
%% -----------------------------------------------------------------------------
start3(0) -> ok;
start3(N) ->
    register(ping, spawn(fun() -> loop(N) end)),
    register(pong, spawn(fun() -> loop(N) end)),
    pong ! ping,
    ok.

loop(0) ->
    unregister(ping),
    unregister(pong);
loop(N) ->
    {In, Out, D} =
    receive
        ping -> {ping, pong, 1};
        pong -> {pong, ping, 0}
    end,
    In ! Out,
    out(whereis(In), In, whereis(Out)),
    loop(N-D).

%%% private --------------------------------------------------------------------

out(From, Msg, To) -> io:format("~-9w --> ~w --> ~w~n", [From, Msg, To]).
