%%% ----------------------------------------------------------------------------
%%% @author Oleg Muraviev <avesh.net@bk.ru>
%%%
%%% @doc Реализация различных алгоритмов
%%% @end
%%% ----------------------------------------------------------------------------
-module(mix).

%% API -------------------------------------------------------------------------
-export([fizzbuzz/0, pythag/1, qsort/1, msort/1, fib/1]).

%% Tests -----------------------------------------------------------------------
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
%% С ростом N затраты времени на вычисления растут логарифмически.
%% Если A и B - пара соседних чисел Фибоначчи, то трансформации на каждой итерации при переходе к следующей паре:
%% A ← B*Q+A*Q+A*P
%% B ← B*P+A*Q
%% где P и Q - коэффициенты трансформации
%% @end
%% -----------------------------------------------------------------------------
-spec fib(N) -> Return when
    N      :: non_neg_integer(),
    Return :: pos_integer().

fib(N) when is_integer(N), N >= 0 ->
    fib(N, {1, 0, 0, 1}).

%% тело fib с аккумулятором
fib(0, {_, B, _, _}) ->
    B;
fib(N, {A, B, P, Q}) when N rem 2 =:= 0 ->
    fib(N div 2, {A, B, P*P+Q*Q, 2*P*Q+Q*Q});
fib(N, {A, B, P, Q}) ->
    fib(N-1, {B*Q+A*Q+A*P, B*P+A*Q, P, Q}).

%% -----------------------------------------------------------------------------
%% @doc Пифагоровы тройки со сторонами треугольника не больше заданного натурального числа
%% @end
%% -----------------------------------------------------------------------------
-spec pythag(N) -> Return when
    N      :: pos_integer(),
    Return :: [{pos_integer(), pos_integer(), pos_integer()}].

pythag(N) when is_integer(N), N > 0 ->
    L0 = lists:seq(1, N),
    L1 = [{A, B, C} || A <- L0, B <- L0, C <- L0, A < B, A*A+B*B =:= C*C],                         % формируем тройки
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
-spec qsort(L) -> Return when
    L      :: [any()],
    Return :: [any()].

qsort(L) when is_list(L) ->
    qsort(L, []).

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

%% -----------------------------------------------------------------------------
%% @doc Сортировка слиянием
%% @end
%% -----------------------------------------------------------------------------
-spec msort(L) -> Return when
    L      :: [any()],
    Return :: [any()].

msort([]) ->
    [];
msort([H]) ->
    [H];
msort(L) ->
    {L1, L2} = split(L),
    merge(msort(L1), msort(L2)).

split(L) ->
    split(L, L, []).
split([], L1, L2) ->
    {lists:reverse(L2), L1};
split([_], L1, L2) ->
    {lists:reverse(L2), L1};
split([_,_|T1], [H|T2], L) ->
    split(T1, T2, [H|L]).

merge([], L) ->
    L;
merge(L, []) ->
    L;
merge([H1|T1], [H2|T2]) when H1=<H2 ->
    [H1 | merge(T1, [H2|T2])];
merge([H1|T1], [H2|T2]) when H1>H2 -> 
    [H2 | merge([H1|T1], T2)].
