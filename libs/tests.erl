%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Модуль для тестирования
%%% @end
%%% ----------------------------------------------------------------------------
-module(tests).

%% API
-export([run/3, run/1, tc/4, fast/3, os/0]).

%% Tests
-include("tests/tests_tests.erl").

%% -----------------------------------------------------------------------------
%% @doc Тестирование функции
%% @end
%% -----------------------------------------------------------------------------
-spec run(Mod, Fun, Arity) -> Return when
    Mod    :: atom(),
    Fun    :: atom(),
    Arity  :: non_neg_integer(),
    Return :: ok.

run(Mod, Fun, Arity) ->
    case  ListTests = Mod:tests(Fun, Arity) of
        [] -> ok;
        _ ->
            ListErrors = [{Args, Res, ResGood} || {Args, ResGood} <- ListTests, (Res = apply(Mod, Fun, Args)) =/= ResGood],
            io:format("*** test ~w/~w is ", [Fun, Arity]),
            case ListErrors of
                [] ->
                    io:format("ok~n");
                _ ->
                    io:format("error~n"),
                    [io:format("~w~w \t--> ~w~ncorrect --> ~w~n", [Fun, Args, Res, ResGood]) || {Args, Res, ResGood} <- ListErrors]
            end
    end.

%% -----------------------------------------------------------------------------
%% @doc Тестирование модуля
%% @end
%% -----------------------------------------------------------------------------
-spec run(Mod) -> Return when
    Mod    :: atom(),
    Return :: ok.

run(Mod) ->
    [run(Mod, Fun, Arity) || {Fun, Arity} <- Mod:module_info(exports)],
    ok.

%% -----------------------------------------------------------------------------
%% @doc Среднее время применения функции по нескольким испытаниям (микросекунд):
%% {T2, T1}, где T1 - процессорное время, T2 - время по часам
%% @end
%% -----------------------------------------------------------------------------
-spec tc(Mod, Fun, Args, N) -> Return when
    Mod    :: atom(),
    Fun    :: atom(),
    Args   :: [any()],
    N      :: pos_integer(),
    Return :: {float(), float()}.

tc(Mod, Fun, Args, N) when is_integer(N), N > 0 ->
    statistics(runtime),
    statistics(wall_clock),
    tc({Mod, Fun, Args}, N),
    {_, T1} = statistics(runtime),
    {_, T2} = statistics(wall_clock),
    {T2/N, T1/N}.

tc(_, 0) ->
    ok;
tc({M, F, Args}, N) ->
    apply(M, F, Args),
    tc({M, F, Args}, N-1).

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
    [io:format("~s:~s/~w \t==> ~.2f mks (~.2f mks)~n", [M, F, length(Args), T1, T2]) ||
    {{M, F}, {T1, T2}} <- lists:sort(fun({_,{A,_}},{_,{B,_}}) -> A < B end, [{{M, F}, tc(M, F, Args, N)} || {M, F} <- Funs])].

%% -----------------------------------------------------------------------------
%% @doc Запуск мониторинга системы
%% @end
%% -----------------------------------------------------------------------------
-spec os() -> Return when
    Return :: ok.

os() ->
    observer:start().
