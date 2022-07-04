%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Реализация различных алгоритмов
%%% @end
%%% ----------------------------------------------------------------------------
-module(mix).

%% API
-export([fizzbuzz1/0, fizzbuzz2/0, pythag/1, qsort/1]).

%% Tests
-include("tests/mix_tests.erl").

%% -----------------------------------------------------------------------------
%% @doc FizzBuzz - вариант 1
%% @end
%% -----------------------------------------------------------------------------
-spec fizzbuzz1() -> Return when
    Return :: ok.

fizzbuzz1() ->
    [io:format(fizzbuzz1(N) ++ "\n") || N <-lists:seq(1,100)],
    ok.

fizzbuzz1(N) when N rem 15 == 0 ->
    "FizzBuzz";
fizzbuzz1(N) when N rem 3  == 0 ->
    "Fizz";
fizzbuzz1(N) when N rem 5  == 0 ->
    "Buzz";
fizzbuzz1(N) ->
    integer_to_list(N).

%% -----------------------------------------------------------------------------
%% @doc FizzBuzz - вариант 2
%% @end
%% -----------------------------------------------------------------------------
-spec fizzbuzz2() -> Return when
    Return :: ok.

fizzbuzz2() ->
    fizzbuzz2(1).

fizzbuzz2(N) when N > 100 ->
    ok;
fizzbuzz2(N) ->
    Fizz = N rem 3 == 0,
    Buzz = N rem 5 == 0,
    io:format("~s~n", [case {Fizz, Buzz} of
                            {true, true}  -> "FizzBuzz";
                            {true, false} -> "Fizz";
                            {false, true} -> "Buzz";
                            _ -> integer_to_list(N)
                        end]),
    fizzbuzz2(N+1).

%% -----------------------------------------------------------------------------
%% @doc Пифагоровы тройки со сторонами треугольника не больше заданного натурального числа
%% @end
%% -----------------------------------------------------------------------------
-spec pythag(Num) -> Return when
    Num    :: pos_integer(),
    Return :: [{pos_integer(), pos_integer(), pos_integer()}].

pythag(Num) when is_integer(Num), Num > 0 ->
    L0 = lists:seq(1, Num),
    L1 = [{A, B, C} || A <- L0, B <- L0, C <- L0, A < B, A*A+B*B == C*C],                          % формируем тройки
    L2 = [{A div G, B div G, C div G} || {A, B, C} <- L1, (G = lib:gcd(A, lib:gcd(B, C))) > 0],    % сокрашаем числа в тройках на их НОД
    F = fun(Elem, Acc) ->
            case lists:member(Elem, Acc) of
                true  -> Acc;
                false -> [Elem|Acc]
            end
        end,
    lists:reverse(lists:foldl(F, [], L2)).                                                         % удаляем повторяющиеся тройки

%% -----------------------------------------------------------------------------
%% @doc Быстрая сортировка
%% @end
%% -----------------------------------------------------------------------------
-spec qsort(List) -> Return when
    List   :: [any()],
    Return :: [any()].

qsort(List) when is_list(List) ->
    qsort(List, []).

%% тело qsort с аккумулятором
qsort([], Acc) ->
    Acc;
qsort([X|T], Acc) ->
    partition(T, Acc, X, {[], [X], []}).

partition([], Acc, _, {S, E, L}) ->
    qsort(S, E ++ qsort(L, Acc));
partition([H|T], Acc, X, {S, E, L}) ->
    if H < X ->
           partition(T, Acc, X, {[H|S], E, L});
       H > X ->
           partition(T, Acc, X, {S, E, [H|L]});
       H == X ->
           partition(T, Acc, X, {S, [H|E], L})
    end.
