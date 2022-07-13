%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Реализации PingPong с одним loop
%%% N - количество циклов. При N < 0 работает в бесконечном цикле
%%% @end
%%% ----------------------------------------------------------------------------
-module(pingpong).

%% API
-export([start1/1, start2/1, start3/1]).


%% -----------------------------------------------------------------------------
%% @doc Вариант 1: отдельные функции для ping и pong
%% @end
%% -----------------------------------------------------------------------------
start1(N) ->
    spawn(fun() -> pong(spawn(fun ping/0), N) end),
    ok.

pong(Pid, 0) -> Pid ! stop;
pong(Pid, N) ->
    Pid ! {pong, self()},
    receive
        ping -> out(self(), pong, Pid)
    end,
    pong(Pid, N-1).

ping() ->
    receive
        stop -> ok;
        {pong, Pid} ->
            out(self(), ping, Pid),
            Pid ! ping,
            ping()
    end.

%% -----------------------------------------------------------------------------
%% @doc Вариант 2: один loop, передача стейта через сообщения
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
%% @doc Вариант 3: один loop, передача стейта через loop, регистрация процессов
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

out(From, Msg, To) ->
    io:format("~-9w --> ~w --> ~w~n", [From, Msg, To]).
