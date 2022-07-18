%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Библиотека работы с таймером
%%% @end
%%% ----------------------------------------------------------------------------
-module(clock).

%% API
-export([start/3, stop/1, sleep/1]).

%% -----------------------------------------------------------------------------
%% @doc Периодические запуски функции F(), именованной как Name, через интервалы в T миллисекунд
%% @end
%% -----------------------------------------------------------------------------

%% остановка таймера
-spec stop(Name) -> Result when
    Name   :: atom(),
    Result :: boolean().

stop(Name) -> Name ! stop.

%% запуск таймера
-spec start(Name, T, F) -> Result when
    Name   :: atom(),
    T      :: non_neg_integer(),
    F      :: fun(),
    Result :: boolean().

start(Name, T, F) when is_atom(Name), is_function(F), is_integer(T), T >= 0 ->
    register(Name, spawn(fun() -> tick(Name, T, F) end)).

tick(Name, T, F) ->
    receive
        stop -> ok
    after T ->
        F(),
        tick(Name, T, F)
    end.

%% -----------------------------------------------------------------------------
%% @doc Остановка текущего процесса на T миллисекунд
%% @end
%% -----------------------------------------------------------------------------
-spec sleep(T) -> Return when
    T      :: non_neg_integer,
    Return :: ok.

sleep(T) when is_integer(T), T >= 0 ->
    receive
        after T -> ok
    end.
