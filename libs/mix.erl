%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Реализация различных алгоритмов
%%% @end
%%% ----------------------------------------------------------------------------
-module(mix).

%% API
-export([fizzbuzz/0, pythag/1, qsort/1, fib/1, bin/2, perms/1]).

%% Tests
-include("tests/mix_tests.erl").

%% -----------------------------------------------------------------------------
%% @doc FizzBuzz
%% @end
%% -----------------------------------------------------------------------------
-spec fizzbuzz() -> Return when
    Return :: ok.

fizzbuzz() ->
    [io:format("~s~n",
        [case {N rem 3, N rem 5} of
            {0, 0} -> "FizzBuzz";
            {0, _} -> "Fizz";
            {_, 0} -> "Buzz";
            _ -> integer_to_list(N)
        end]) || N <- lists:seq(1,100)],
    ok.

%% -----------------------------------------------------------------------------
%% @doc Числа Фибоначчи
%% С ростом n затраты времени на вычисления растут логарифмически.
%% Если A и B - пара соседних чисел Фибоначчи, то трансформации на каждой итерации при переходе к следующей паре:
%% A ← B*Q+A*Q+A*P
%% B ← B*P+A*Q
%% где P и Q - коэффициенты трансформации
%% @end
%% -----------------------------------------------------------------------------
-spec fib(Num) -> Return when
    Num    :: non_neg_integer(),
    Return :: pos_integer().

fib(Num) when is_integer(Num), Num >= 0 ->
    fib(Num, {1, 0, 0, 1}).

%% тело fib с аккумулятором
fib(0, {_, B, _, _}) ->
    B;
fib(N, {A, B, P, Q}) when N rem 2 == 0 ->
    fib(N div 2, {A, B, P*P+Q*Q, 2*P*Q+Q*Q});
fib(N, {A, B, P, Q}) ->
    fib(N-1, {B*Q+A*Q+A*P, B*P+A*Q, P, Q}).

%% -----------------------------------------------------------------------------
%% @doc Биномиальные коэффициенты
%% @end
%% -----------------------------------------------------------------------------
-spec bin(N, K) -> Return when
    N      :: non_neg_integer(),
    K      :: non_neg_integer(),
    Return :: non_neg_integer().

bin(N, K) when is_integer(N), is_integer(K), N >= 0, K >= 0 ->
    bin(N, K, {1, 1}).

%% тело binomial с аккумулятором
bin(N, K, {_, _}) when K > N ->
    0;
bin(_, 0, {P1, P2}) ->
    P1 div P2;
bin(N, K, {P1, P2}) ->
    bin(N-1, K-1, {P1*N, P2*K}).

%% -----------------------------------------------------------------------------
%% @doc Список всех перестановок из элементов исходного списка
%% @end
%% -----------------------------------------------------------------------------
-spec perms(L) -> Return when
    L      :: [any()],
    Return :: [[any()]].

perms([]) ->
    [[]];
perms(L) ->
    [[H|T] || H <- L, T <- perms(L--[H])].

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
