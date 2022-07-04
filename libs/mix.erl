%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Реализация различных алгоритмов
%%% @end
%%% ----------------------------------------------------------------------------
-module(mix).

%% API
-export([fizzbuzz/0, pythag/1, qsort/1]).

%% Tests
-include("tests/mix_tests.erl").

%% -----------------------------------------------------------------------------
%% @doc FizzBuzz
%% @end
%% -----------------------------------------------------------------------------
-spec fizzbuzz() -> Return when
    Return :: ok.

fizzbuzz() ->
    [io:format(fizzbuzz(N) ++ "\n") || N <-lists:seq(1,100)],
    ok.

fizzbuzz(N) when N rem 15 == 0 ->
    "FizzBuzz";
fizzbuzz(N) when N rem 3  == 0 ->
    "Fizz";
fizzbuzz(N) when N rem 5  == 0 ->
    "Buzz";
fizzbuzz(N) ->
    integer_to_list(N).

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
