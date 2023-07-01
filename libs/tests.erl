%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Модуль для тестирования
%%% @end
%%% ----------------------------------------------------------------------------
-module(tests).

%% API -------------------------------------------------------------------------

%% Тестирование корректности работы
-export([run/3, run/1]).

%% Тестирование скорости работы
-export([tc/4, fast/3]).

%% Для отладки
-export([os/0, dbg/0]).

%% Tests -----------------------------------------------------------------------
-include("tests/tests_tests.erl").

%% -----------------------------------------------------------------------------
%% @doc Тестирование функции
%% @end
%% -----------------------------------------------------------------------------
-spec run(M, F, Arity) -> Return when
    M      :: atom(),
    F      :: atom(),
    Arity  :: non_neg_integer(),
    Return :: ok.

run(M, F, Arity) ->
    case  ListTests = M:tests(F, Arity) of
        [] -> ok;
        _ ->
            ListErrors = [{Args, Res, ResGood} || {Args, ResGood} <- ListTests, (Res = apply(M, F, Args)) =/= ResGood],
            io:format("*** test ~w/~w is ", [F, Arity]),
            case ListErrors of
                [] ->
                    io:format("ok~n");
                _ ->
                    io:format("error~n"),
                    [io:format("~w~w \t--> ~w~ncorrect --> ~w~n", [F, Args, Res, ResGood]) || {Args, Res, ResGood} <- ListErrors]
            end
    end.

%% -----------------------------------------------------------------------------
%% @doc Тестирование модуля
%% @end
%% -----------------------------------------------------------------------------
-spec run(M) -> Return when
    M      :: atom(),
    Return :: ok.

run(M) ->
    [run(M, F, Arity) || {F, Arity} <- M:module_info(exports)],
    ok.

%% -----------------------------------------------------------------------------
%% @doc Среднее время применения функции по нескольким испытаниям (микросекунд)
%% @end
%% -----------------------------------------------------------------------------
-spec tc(M, F, Args, N) -> Return when
    M      :: atom(),
    F      :: atom(),
    Args   :: [any()],
    N      :: pos_integer(),
    Return :: float().

tc(M, F, Args, N) when is_integer(N), N > 0 ->
    timer:start(),
    tc({M, F, Args}, N, 0) / N.

tc(_, 0, Sum) -> Sum;
tc({M, F, Args}, N, Sum) ->
    {T, _} = timer:tc(M, F, Args),
    tc({M, F, Args}, N-1, Sum+T).

%% -----------------------------------------------------------------------------
%% @doc Сравнение скорости работы списка функций на одинаковых аргументах
%% @end
%% -----------------------------------------------------------------------------
-spec fast(Funs, Args, N) -> Return when
    Funs   :: [{atom(),atom()}],
    Args   :: [any()],
    N      :: pos_integer(),
    Return :: [ok] | {error, no_functions | one_function}.

fast([], _, _)  ->
    {error, no_functions};
fast([_], _, _) ->
    {error, one_function};
fast(Funs, Args, N) when N > 0 ->
    [io:format("~s:~s/~w \t==> ~.3f mks~n", [M, F, length(Args), T]) ||
    {{M, F}, T} <- lists:sort(fun({_,A}, {_,B}) -> A < B end, [{{M, F}, tc(M, F, Args, N)} || {M, F} <- Funs])].

%% -----------------------------------------------------------------------------
%% @doc Запуск мониторинга системы
%% @end
%% -----------------------------------------------------------------------------
-spec os() -> Return when
    Return :: ok.

os() -> observer:start().

%% -----------------------------------------------------------------------------
%% @doc Запуск отладчика
%% @end
%% -----------------------------------------------------------------------------
-spec dbg() -> Return when
    Return :: ok.

dbg() -> debugger:start().
