%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Кольцевой тест
%%% @end
%%% ----------------------------------------------------------------------------

-module(chain).
-export([start/2]).

%% -----------------------------------------------------------------------------
%% @doc Старт теста
%% N - количество процессов
%% M - количество циклов передачи сообщений по кольцу процессов
%% @end
%% -----------------------------------------------------------------------------
-spec start(N, M) -> Return when
    N      :: pos_integer(),
    M      :: pos_integer(),
    Return :: ok.

start(N, M) when is_integer(N), is_integer(M), N > 0, M > 0 ->
    spawn(fun() -> new(1, N, self()) end) ! M*N,
    ok.

loop(I, Pid) ->
    receive
        0 -> ok;
        N ->
            Pid ! N-1,
            io:format("from ~w to ~w --> ~w~n", [self(), Pid, N]),
            loop(I, Pid)
    end.

new(N, N, Pid) ->
    loop(N, Pid);
new(I, N, Pid) ->
    loop(I, spawn(fun() -> new(I+1, N, Pid) end)).



