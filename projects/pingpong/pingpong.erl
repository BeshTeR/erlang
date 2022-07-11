%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Реализация PingPong с одним loop и передачей состояния через сообщения
%%% @end
%%% ----------------------------------------------------------------------------
-module(pingpong).

%% API
-export([start/1]).

%% Tests
-include("tests/pingpong_tests.erl").

%% -----------------------------------------------------------------------------
%% @doc Запуск теста PingPong
%% N - количество циклов. При N < 0 работает в бесконечном цикле
%% @end
%% -----------------------------------------------------------------------------
-spec start(N) -> Return when
    N      :: integer(),
    Return :: ok.
start(N) ->
    spawn(fun loop/0) ! {self(), {ping, N}},
    loop().

loop() ->
    receive
        {From, {_, 0}} ->
            From ! {self(), {stop, 0}},
            ok;
        {From, {Msg, N}} ->
            io:format("~w --> ~w --> ~w~n", [From, Msg, self()]),
            From ! {self(),
                case Msg of
                    ping -> {pong, N};
                    pong -> {ping, N-1}
                end},
            loop()
    end.
