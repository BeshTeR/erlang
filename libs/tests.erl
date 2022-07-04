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
    ListTests = Mod:tests(Fun, Arity),
    case ListTests of
        [] -> ok;
        _  -> ListErrors = [{Args, Res, ResGood} || {Args, ResGood} <- ListTests, (Res = apply(Mod, Fun, Args)) =/= ResGood],
              Status = case ListErrors of
                  [] -> "ok";
                  _  -> "error"
              end,
              io:format("*** test ~w is ~s~n", [Fun, Status]),
              print_err(ListErrors)
    end.

%% вывод списка ошибок
print_err([]) ->
    ok;
print_err([{Args, Res, ResGood} | T]) ->
    io:format("~w \t--> ~w~ncorrect --> ~w~n", [Args, Res, ResGood]),
    print_err(T).

%% -----------------------------------------------------------------------------
%% @doc Тестирование модуля
%% @end
%% -----------------------------------------------------------------------------
-spec run(Mod) -> Return when
    Mod    :: atom(),
    Return :: ok.

run(Mod) ->
    for_all(Mod, Mod:module_info(exports)).

%% выполняем тесты для всех функций Mod:Fun/Arity
for_all(_, []) ->
    ok;
for_all(Mod, [{Fun, Arity} | T]) ->
    run(Mod, Fun, Arity),
    for_all(Mod, T).

%% -----------------------------------------------------------------------------
%% @doc Среднее время применения функции по нескольким испытаниям (микросекунд)
%% @end
%% -----------------------------------------------------------------------------
-spec tc(Mod, Fun, Args, Num) -> Return when
    Mod    :: atom(),
    Fun    :: atom(),
    Args   :: [any()],
    Num    :: pos_integer(),
    Return :: float().

tc(Mod, Fun, Args, Num) when is_integer(Num), Num > 0 ->
    tc(Mod, Fun, Args, Num, 0)/Num.

%% тело tc с аккумулятором
tc(_, _, _, 0, Acc) ->
    Acc;
tc(Mod, Fun, Args, Num, Acc) ->
    tc(Mod, Fun, Args, Num-1, Acc+time_call(Mod, Fun, Args)).

%% время одного вызова Mod:Fun(Args)
time_call(Mod, Fun, Args) ->
    StartTime = erlang:system_time(microsecond),
    apply(Mod, Fun, Args),
    erlang:system_time(microsecond)-StartTime.

%% -----------------------------------------------------------------------------
%% @doc Сравнение скорости работы списка функций на одинаковых аргументах
%% @end
%% -----------------------------------------------------------------------------
-spec fast(Funs, Args, Num) -> Return when
    Funs   :: [{atom(),atom()}],
    Args   :: [any()],
    Num    :: pos_integer(),
    Return :: ok | {error, no_functions | one_function}.

fast([], _, _)  ->
    {error, no_functions};
fast([_], _, _) ->
    {error, one_function};
fast(Funs, Args, Num) when Num > 0 ->
    FunOrder = fun({_, A}, {_, B}) -> A < B end,
    NoSortRes = [{{Mod, Fun}, tc(Mod, Fun, Args, Num)} || {Mod, Fun} <- Funs],
    ResList = [{_, Min}|_] = lists:sort(FunOrder, NoSortRes),
    print_res(ResList, Min).

%% вывод результатов тестирования
print_res([], _) ->
    ok;
print_res([{{Mod, Fun}, TotalTime} | T], Min) ->
    io:format("~s:~s \t==> ~.2f~n", [Mod, Fun, TotalTime/Min]),
    print_res(T, Min).

%% -----------------------------------------------------------------------------
%% @doc Запуск мониторинга системы
%% @end
%% -----------------------------------------------------------------------------
-spec os() -> Return when
    Return :: ok.

os() ->
    observer:start().
