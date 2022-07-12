%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Реализации PingPong с одним loop
%%% N - количество циклов. При N < 0 работает в бесконечном цикле
%%% @end
%%% ----------------------------------------------------------------------------
-module(pingpong).

%% API
-export([start1/1, start2/1]).

%% Tests
-include("tests/pingpong_tests.erl").

%% -----------------------------------------------------------------------------
%% @doc Вариант 1: передача состояния через сообщения
%% @end
%% -----------------------------------------------------------------------------
-spec start1(N) -> Return when
    N      :: integer(),
    Return :: ok.

start1(N) ->
    spawn(fun loop/0) ! {self(), {ping, N}},
    loop().

loop() ->
    receive
        {From, {_, 0}} ->
            From ! {self(), {stop, 0}},
            ok;
        {From, {Msg, N}} ->
            io:format("~-9w --> ~w --> ~w~n", [From, Msg, self()]),
            From ! {self(),
                case Msg of
                    ping -> {pong, N};
                    pong -> {ping, N-1}
                end},
            loop()
    end.

%% -----------------------------------------------------------------------------
%% @doc Вариант 2: передача состояния через loop и регистрация процессов
%% @end
%% -----------------------------------------------------------------------------
-spec start2(N) -> Return when
    N      :: integer(),
    Return :: ok.

start2(0) -> ok;
start2(N) ->
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
    io:format("~-9w --> ~w --> ~w~n", [whereis(In), In, whereis(Out)]),
    loop(N-D).
